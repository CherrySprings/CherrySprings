import chisel3._
import chisel3.util._
import difftest._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

class SimTop(implicit p: Parameters) extends LazyModule with BindingScope {
  lazy val dts = DTS(bindingTree)

  val soc   = LazyModule(new SoC)
  val xbar  = LazyModule(new TLXbar)
  val clint = LazyModule(new CLINT(CLINTParams(), 8))
  val plic  = LazyModule(new TLPLIC(PLICParams(), 8))
  val uart  = LazyModule(new UART)
  val mem   = LazyModule(new TLVirtualRam)

  xbar.node := soc.node

  clint.node := xbar.node
  plic.node  := xbar.node
  uart.node  := xbar.node
  mem.node   := TLDelayer(0.1) := xbar.node

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

    val io = IO(new Bundle {
      val logCtrl  = new LogCtrlIO
      val perfInfo = new PerfInfoIO
      val uart     = new UARTIO
    })

    uart.module.io <> io.uart

    // CLINT
    val freq = 100
    val cnt  = RegInit(freq.U)
    val tick = (cnt === 0.U)
    cnt                     := Mux(tick, freq.U, cnt - 1.U)
    clint.module.io.rtcTick := tick
  }
}
