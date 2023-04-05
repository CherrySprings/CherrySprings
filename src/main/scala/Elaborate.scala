import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.util.HasRocketChipStageUtils

object Elaborate extends App with HasRocketChipStageUtils {
  implicit val p: Config = new FastSimulationConfig
  val top     = LazyModule(new SimTop())
  val verilog = chisel3.stage.ChiselStage.emitVerilog(top.module)
  writeOutputFile(".", "SimTop.v", verilog)
}
