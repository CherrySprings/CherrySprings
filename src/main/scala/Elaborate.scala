import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.util.HasRocketChipStageUtils

object Elaborate extends App with HasRocketChipStageUtils {
  val target = args(0)
  assert(Seq("fast-sim", "slow-sim", "syn").contains(target))

  implicit val p: Config = (target match {
    case "fast-sim" => new FastSimulationConfig
    case "slow-sim" => new SlowSimulationConfig
    case "syn"      => new SynthesisConfig
  }).toInstance

  val top     = LazyModule(new SimTop())
  val verilog = chisel3.stage.ChiselStage.emitVerilog(top.module)
  writeOutputFile(".", "SimTop.v", verilog)
}
