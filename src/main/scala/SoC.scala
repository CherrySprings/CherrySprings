import chisel3._
import difftest._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import sifive.blocks.devices.chiplink._

object ChipLinkParam {
  val mem      = AddressSet(0x80000000L, 0x80000000L - 1)
  val mmio     = AddressSet(0x00000000L, 0x70000000L - 1)
  val allSpace = Seq(mem, mmio)
  val idBits   = 4
}

class SoC(implicit p: Parameters) extends LazyModule {
  val icache  = LazyModule(new ICache)
  val dcache  = LazyModule(new DCache)
  val uncache = LazyModule(new UncacheCachePortToTileLinkBridge)
  val xbar    = LazyModule(new TLXbar(policy = TLArbiter.highestIndexFirst))

  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val plic_int_sink  = IntSinkNode(IntSinkPortSimple(2, 1))

  // don't modify order of following nodes
  xbar.node := icache.node // 0
  xbar.node := dcache.node // 1
  xbar.node := uncache.node // 2

  // ChipLink
  val chiplink = LazyModule(
    new ChipLink(
      ChipLinkParams(
        TLUH   = List(ChipLinkParam.mmio),
        TLC    = List(ChipLinkParam.mem),
        syncTX = true
      )
    )
  )
  chiplink.node := xbar.node

  val err = LazyModule(
    new TLError(
      DevNullParams(Seq(AddressSet(0x70000000L, 0x1000L - 1)), 64, 64, region = RegionType.TRACKED),
      beatBytes = 4
    )
  )
  err.node := chiplink.node

  val chiplink_sink = chiplink.ioNode.makeSink

  lazy val module = new LazyModuleImp(this) {
    val chiplink_io = chiplink_sink.makeIO()
    dontTouch(chiplink_io)

    val core = Module(new Core)

    // interrupt input
    core.io.interrupt.msip := clint_int_sink.in.head._1(0)
    core.io.interrupt.mtip := clint_int_sink.in.head._1(1)
    core.io.interrupt.meip := plic_int_sink.in.head._1(0)
    core.io.interrupt.seip := plic_int_sink.in.last._1(0)

    // instruction cache
    icache.module.io.cache   <> core.io.imem
    icache.module.io.fence_i := core.io.fence_i

    // data cache
    dcache.module.io.fence_i := core.io.fence_i
    core.io.fence_i_ok       := dcache.module.io.fence_i_ok

    // connect ptw port to data cache
    val xbar = Module(new CachePortXBarNto1(3))
    xbar.io.in(0)          <> core.io.dmem
    xbar.io.in(1)          <> core.io.iptw
    xbar.io.in(2)          <> core.io.dptw
    dcache.module.io.cache <> xbar.io.out

    // uncache
    uncache.module.io.in <> core.io.uncache
  }
}
