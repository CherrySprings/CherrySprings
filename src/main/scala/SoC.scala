import chisel3._
import difftest._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._

class SoC(implicit p: Parameters) extends LazyModule {
  val icache      = LazyModule(new ICache)
  val bridge_dmem = LazyModule(new CachePortToTileLinkBridge("dmem"))
  val bridge_iptw = LazyModule(new CachePortToTileLinkBridge("iptw"))
  val bridge_dptw = LazyModule(new CachePortToTileLinkBridge("dptw"))
  val xbar        = LazyModule(new TLXbar(policy = TLArbiter.highestIndexFirst))
  val node        = TLIdentityNode()

  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val plic_int_sink  = IntSinkNode(IntSinkPortSimple(2, 1))

  // don't modify order of following nodes
  xbar.node := icache.node // 0
  xbar.node := bridge_iptw.node // 1
  xbar.node := bridge_dmem.node // 2
  xbar.node := bridge_dptw.node // 3
  node      := xbar.node

  lazy val module = new LazyModuleImp(this) {

    val core = Module(new Core)

    core.io.interrupt.msip      := clint_int_sink.in.head._1(0)
    core.io.interrupt.mtip      := clint_int_sink.in.head._1(1)
    core.io.interrupt.meip      := plic_int_sink.in.head._1(0)
    core.io.interrupt.seip      := plic_int_sink.in.last._1(0)
    icache.module.io.fence_i    := core.io.fence_i
    icache.module.io.cache      <> core.io.imem
    bridge_dmem.module.io.cache <> core.io.dmem
    bridge_iptw.module.io.cache <> core.io.iptw
    bridge_dptw.module.io.cache <> core.io.dptw
  }
}
