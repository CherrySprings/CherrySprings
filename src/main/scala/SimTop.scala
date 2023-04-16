import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config._

class SimTop(implicit p: Parameters) extends LazyModule with BindingScope with HasCherrySpringsParameters {
  lazy val dts = DTS(bindingTree)

  val soc = for (i <- 0 until numHarts) yield {
    val p_ = p.alterPartial({
      case HartID => i
    })
    val soc = LazyModule(if (enableSerdes) (new SoC()(p_)) else (new SoCImp()(p_)))
    soc
  }
  val fpga = LazyModule(if (enableSerdes) (new FPGA) else (new FPGAImp))

  for (i <- 0 until numHarts) {
    if (!enableSerdes) {
      fpga.node.get(i) := soc(i).node.get
    }
  }

  lazy val module = new LazyModuleImp(this) {
    ElaborationArtefacts.add("dts", dts)

    val io = IO(new Bundle {
      val logCtrl  = new LogCtrlIO
      val perfInfo = new PerfInfoIO
      val uart     = new UARTIO
    })

    // uart
    io.uart <> fpga.module.io.uart

    // interrupt
    for (i <- 0 until numHarts) {
      soc(i).module.io.intr := fpga.module.io.intr(i)
    }

    // data
    if (enableSerdes) {
      val clock_divider = Module(new ClockDivider2)
      val io_clock      = clock_divider.io.clk_out
      clock_divider.io.clk_in := clock

      fpga.module.io.io_clock.get := io_clock
      fpga.module.io.io_reset.get := reset
      for (i <- 0 until numHarts) {
        soc(i).module.io.io_clock.get := io_clock
        soc(i).module.io.io_reset.get := reset

        fpga.module.io.in.get(i)  <> soc(i).module.io.out.get
        fpga.module.io.out.get(i) <> soc(i).module.io.in.get
      }
    }
  }
}
