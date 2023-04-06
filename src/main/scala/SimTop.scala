import chisel3._
import chisel3.util._
import difftest._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class SimTop(implicit p: Parameters) extends LazyModule with BindingScope with HasCherrySpringsParameters {
  lazy val dts = DTS(bindingTree)

  val soc  = LazyModule(if (enableSerdes) (new SoC) else (new SoCImp))
  val fpga = LazyModule(if (enableSerdes) (new FPGA) else (new FPGAImp))

  soc.clint_int := fpga.clint_int
  soc.plic_int :*= fpga.plic_int

  if (!enableSerdes) {
    fpga.node.get := soc.node.get
  }

  lazy val module = new LazyModuleImp(this) {
    ElaborationArtefacts.add("dts", dts)

    val io = IO(new Bundle {
      val logCtrl  = new LogCtrlIO
      val perfInfo = new PerfInfoIO
      val uart     = new UARTIO
    })

    io.uart <> fpga.module.io.uart
    if (enableSerdes) {
      val clock_divider = Module(new ClockDivider2)
      val io_clock      = clock_divider.io.clk_out
      clock_divider.io.clk_in := clock

      soc.module.io.io_clock.get  := io_clock
      soc.module.io.io_reset.get  := reset
      fpga.module.io.io_clock.get := io_clock
      fpga.module.io.io_reset.get := reset

      fpga.module.io.in.get  <> soc.module.io.out.get
      fpga.module.io.out.get <> soc.module.io.in.get
    }
  }
}
