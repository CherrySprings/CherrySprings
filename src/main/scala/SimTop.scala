import chisel3._
import chisel3.util._
import difftest._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

class SimTop(implicit p: Parameters) extends LazyModule with BindingScope with HasCherrySpringsParameters {
  lazy val dts = DTS(bindingTree)

  val soc   = LazyModule(new SoC)
  val xbar  = LazyModule(new TLXbar(policy = TLArbiter.highestIndexFirst))
  val clint = LazyModule(new CLINT(CLINTParams(), 8))
  val plic  = LazyModule(new TLPLIC(PLICParams(), 8))
  val uart  = LazyModule(new UART)
  val mem   = LazyModule(new TLVirtualRam)

  xbar.node := TLBuffer(abcde = BufferParams(depth = 2, flow = false, pipe = false)) := soc.node

  // don't modify order of following nodes
  mem.node   := TLDelayer(0.1)   := xbar.node
  clint.node := xbar.node
  plic.node  := xbar.node
  uart.node  := TLWidthWidget(8) := xbar.node

  soc.clint_int_sink := clint.intnode
  soc.plic_int_sink :*= plic.intnode

  class IntSourceNodeToModule(val num: Int)(implicit p: Parameters) extends LazyModule {
    val sourceNode = IntSourceNode(IntSourcePortSimple(num, ports = 1, sources = 1))
    lazy val module = new LazyModuleImp(this) {
      val in = IO(Input(Vec(num, Bool())))
      in.zip(sourceNode.out.head._1).foreach { case (i, s) => s := i }
    }
  }

  val plicSource = LazyModule(new IntSourceNodeToModule(2))

  plic.intnode := plicSource.sourceNode

  lazy val module = new LazyModuleImp(this) {
    ElaborationArtefacts.add("dts", dts)

    // sync external interrupts
    val ext_intrs = Wire(UInt(2.W))
    ext_intrs := Cat(uart.module.io.intr, 0.U)
    require(plicSource.module.in.length == ext_intrs.getWidth)
    for ((plic_in, interrupt) <- plicSource.module.in.zip(ext_intrs.asBools)) {
      val ext_intr_sync = RegInit(0.U(3.W))
      ext_intr_sync := Cat(ext_intr_sync(1, 0), interrupt)
      plic_in       := ext_intr_sync(2)
    }

    val io = IO(new Bundle {
      val logCtrl  = new LogCtrlIO
      val perfInfo = new PerfInfoIO
      val uart     = new UARTIO
    })

    uart.module.io.uart <> io.uart

    // CLINT
    val cnt  = RegInit(fpgaTimerFreq.U)
    val tick = (cnt === 0.U)
    cnt                     := Mux(tick, fpgaTimerFreq.U, cnt - 1.U)
    clint.module.io.rtcTick := tick
  }
}
