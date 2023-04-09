import chisel3._
import chisel3.util._
import freechips.rocketchip.subsystem._
import org.chipsalliance.cde.config._

case object HartID extends Field[Int]
case object ResetPC extends Field[BigInt]
case object BootROMImage extends Field[String]
case object CacheNumSets extends Field[Int]
case object EnableBPU extends Field[Boolean]
case object PHTSize extends Field[Int]
case object BTBSize extends Field[Int]

class CoreConfig
    extends Config((site, here, up) => {
      case ResetPC      => BigInt("80000000", radix = 16)
      case BootROMImage => "./bootrom/bootrom.img"
      case CacheNumSets => 512 // 16 KB
      case EnableBPU    => true
      case PHTSize      => 512
      case BTBSize      => 16
    })

case object NumHarts extends Field[Int]
case object CoreTimerFreq extends Field[Int]
case object FpgaTimerFreq extends Field[Int]

class SystemConfig
    extends Config((site, here, up) => {
      case NumHarts      => 2
      case CoreTimerFreq => 2 // suppose 200 MHz core frequency => 200 / 2 = 100 MHz timer frequency
      case FpgaTimerFreq => 2 // suppose 200 MHz FPGA frequency => 200 / 2 = 100 MHz timer frequency
    })

case object EnableDifftest extends Field[Boolean](false)

class EnableDifftestConfig
    extends Config((site, here, up) => {
      case EnableDifftest => true
    })

case object EnableSerdes extends Field[Boolean](false)

class EnableSerdesConfig
    extends Config((site, here, up) => {
      case EnableSerdes => true
    })

class FastSimulationConfig extends Config(new CoreConfig ++ new SystemConfig ++ new EnableDifftestConfig)

class SlowSimulationConfig
    extends Config(new CoreConfig ++ new SystemConfig ++ new EnableDifftestConfig ++ new EnableSerdesConfig)

class SynthesisConfig
    extends Config(new CoreConfig ++ new SystemConfig ++ new EnableSerdesConfig ++ new WithoutTLMonitors)

trait HasCherrySpringsParameters {
  implicit val p: Parameters
  def numHarts:         Int     = p(NumHarts)
  def hartID:           Int     = p(HartID)
  def resetPC:          BigInt  = p(ResetPC)
  def bootROMImage:     String  = p(BootROMImage)
  def xLen:             Int     = 64
  def paddrLen:         Int     = 32
  def vaddrLen:         Int     = 39
  def sourceRange:      Int     = 4
  def cacheNumSets:     Int     = p(CacheNumSets)
  def phtSize:          Int     = p(PHTSize)
  def enableBPU:        Boolean = p(EnableBPU)
  def btbSize:          Int     = p(BTBSize)
  def enableDifftest:   Boolean = p(EnableDifftest)
  def ghrLen:           Int     = log2Up(phtSize)
  def coreTimerFreq:    Int     = p(CoreTimerFreq)
  def fpgaTimerFreq:    Int     = p(FpgaTimerFreq)
  def tlSourceBits:     Int     = 4
  def tlSerWidth:       Int     = 8
  def enableSerdes:     Boolean = p(EnableSerdes)
  def debugPCTrace:     Boolean = false
  def debugInstrFetch:  Boolean = false
  def debugInstrCommit: Boolean = false
  def debugICache:      Boolean = false
  def debugDCache:      Boolean = false
  def debugUncache:     Boolean = false
  def debugPortProxy:   Boolean = false
  def debugTLB:         Boolean = false
}

abstract class CherrySpringsModule(implicit val p: Parameters) extends Module with HasCherrySpringsParameters

class ParameterizedBundle(implicit p: Parameters) extends Bundle

abstract class CherrySpringsBundle(implicit val p: Parameters)
    extends ParameterizedBundle()(p)
    with HasCherrySpringsParameters
