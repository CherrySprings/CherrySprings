import chisel3._
import difftest._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._

class SoC(implicit p: Parameters) extends LazyModule {
  val icache  = LazyModule(new ICache)
  val dcache  = LazyModule(new DCache)
  val uncache = LazyModule(new UncacheCachePortToTileLinkBridge)
  val xbar    = LazyModule(new TLXbar(policy = TLArbiter.highestIndexFirst))
  val node    = TLIdentityNode()

  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val plic_int_sink  = IntSinkNode(IntSinkPortSimple(2, 1))

  // don't modify order of following nodes
  xbar.node := icache.node // 0
  xbar.node := dcache.node // 1
  xbar.node := uncache.node // 2
  node      := xbar.node

  lazy val module = new LazyModuleImp(this) {

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
