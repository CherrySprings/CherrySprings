import chisel3._
import chisel3.util._
import difftest._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.axi4._

class FpgaTop(implicit p: Parameters) extends LazyModule with BindingScope {
  lazy val dts = DTS(bindingTree)

  val soc   = LazyModule(new SoC)
  val xbar  = LazyModule(new TLXbar)
  val clint = LazyModule(new CLINT(CLINTParams(), 8))
  val plic  = LazyModule(new TLPLIC(PLICParams(), 8))
  val node  = AXI4IdentityNode()

  xbar.node := soc.node

  clint.node := xbar.node
  plic.node  := xbar.node
  node       := AXI4UserYanker() := AXI4Deinterleaver(64) := TLToAXI4() := xbar.node

  soc.clint_int_sink := clint.intnode
  soc.plic_int_sink :*= plic.intnode

  class IntSourceNodeToModule(val num: Int)(implicit p: Parameters) extends LazyModule {
    val sourceNode = IntSourceNode(IntSourcePortSimple(num, ports = 1, sources = 1))
    lazy val module = new LazyModuleImp(this) {
      val in = IO(Input(Vec(num, Bool())))
      in.zip(sourceNode.out.head._1).foreach { case (i, s) => s := i }
    }
  }

  val plicSource = LazyModule(new IntSourceNodeToModule(16))

  plic.intnode := plicSource.sourceNode

  lazy val module = new LazyModuleImp(this) {
    ElaborationArtefacts.add("dts", dts)

    // CLINT
    val freq = 100
    val cnt  = RegInit(freq.U)
    val tick = (cnt === 0.U)
    cnt                     := Mux(tick, freq.U, cnt - 1.U)
    clint.module.io.rtcTick := tick
  }
}

class FpgaTopWithMemory(implicit p: Parameters) extends LazyModule with BindingScope {
  val fpga_top = LazyModule(new FpgaTop)
  val mem      = LazyModule(new AXI4RAM(AddressSet(BigInt("080000000", 16), BigInt("7fffffff", 16)), beatBytes = 8))

  mem.node := fpga_top.node

  lazy val module = new LazyModuleImp(this) {}
}
