import chisel3._
import difftest._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SimTop(implicit p: Parameters) extends LazyModule with BindingScope {
  lazy val dts = DTS(bindingTree)

  val soc  = LazyModule(new SoC)
  val fpga = LazyModule(new FPGA)

  soc.clint_int_sink := fpga.clint.intnode
  soc.plic_int_sink :*= fpga.plic.intnode

  lazy val module = new LazyModuleImp(this) {
    ElaborationArtefacts.add("dts", dts)

    val io = IO(new Bundle {
      val logCtrl  = new LogCtrlIO
      val perfInfo = new PerfInfoIO
      val uart     = new UARTIO
    })

    fpga.module.io.tl <> soc.module.io.tl
    io.uart           <> fpga.module.io.uart

    dontTouch(fpga.module.io.tl)
    dontTouch(soc.module.io.tl)
  }
}
