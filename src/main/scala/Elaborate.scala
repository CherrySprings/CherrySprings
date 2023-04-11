import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.util.HasRocketChipStageUtils
import org.chipsalliance.cde.config._

case class CLIConfig(numHarts: Int = 1, target: String = "fast-sim")

object Elaborate extends App with HasRocketChipStageUtils {
  val parser = new scopt.OptionParser[CLIConfig]("Elaborate") {
    override def errorOnUnknownArgument: Boolean = false
    override def reportWarning(msg: String): Unit = {}
    opt[Int]("numHarts")
      .abbr("n")
      .valueName("<sim-num-harts>")
      .validate { x =>
        if (x > 0) success else failure("numHarts must be positive")
      }
      .action { (x, c) =>
        c.copy(numHarts = x)
      }
      .text("Number of harts")
    opt[String]("target")
      .valueName("<sim-target>")
      .validate { x =>
        if (x == "fast-sim" || x == "slow-sim" || x == "syn") success
        else failure("Elaborate target must be either fast-sim, slow-sim, or syn")
      }
      .action { (x, c) =>
        c.copy(target = x)
      }
      .text("Elaborate target (must be either fast-sim, slow-sim, or syn)")
  }

  parser.parse(args, CLIConfig()) match {
    case Some(config) =>
      implicit val p = (config.target match {
        case "fast-sim" => new FastSimulationConfig
        case "slow-sim" => new SlowSimulationConfig
        case "syn"      => new SynthesisConfig
      }).toInstance.alterPartial({
        case NumHarts => config.numHarts
      })
      val top     = LazyModule(new SimTop())
      val verilog = chisel3.stage.ChiselStage.emitVerilog(top.module)
      writeOutputFile(".", "SimTop.v", verilog)
      writeOutputFile(".", "cs.graphml", top.graphML)
    case None =>
      sys.exit(1)
  }
}
