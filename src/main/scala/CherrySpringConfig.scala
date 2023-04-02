import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.subsystem._

case object EnableDifftest extends Field[Boolean]
case object ResetPC extends Field[BigInt]
case object HartID extends Field[Int]
case object CacheNumSets extends Field[Int]
case object EnableBPU extends Field[Boolean]
case object PHTSize extends Field[Int]
case object BTBSize extends Field[Int]
case object CoreTimerFreq extends Field[Int]
case object FpgaTimerFreq extends Field[Int]

class CoreConfig
    extends Config((site, here, up) => {
      case EnableDifftest => true
      case ResetPC        => BigInt("80000000", 16)
      case HartID         => 0
      case CacheNumSets   => 512 // 16 KB
      case EnableBPU      => false
      case PHTSize        => 512
      case BTBSize        => 16
      case CoreTimerFreq  => 10 // suppose 100 MHz core frequency => 10 MHz timer frequency
      case FpgaTimerFreq  => 10 // suppose 100 MHz FPGA frequency => 10 MHz timer frequency
    })

class CherrySpringsConfig extends Config(new CoreConfig)

trait HasCherrySpringsParameters {
  implicit val p: Parameters
  def enableDifftest:   Boolean = p(EnableDifftest)
  def resetPC:          BigInt  = p(ResetPC)
  def hartID:           Int     = p(HartID)
  def xLen:             Int     = 64
  def paddrLen:         Int     = 32
  def vaddrLen:         Int     = 39
  def sourceRange:      Int     = 4
  def cacheNumSets:     Int     = p(CacheNumSets)
  def phtSize:          Int     = p(PHTSize)
  def enableBPU:        Boolean = p(EnableBPU)
  def btbSize:          Int     = p(BTBSize)
  def ghrLen:           Int     = log2Up(phtSize)
  def coreTimerFreq:    Int     = p(CoreTimerFreq)
  def fpgaTimerFreq:    Int     = p(FpgaTimerFreq)
  def enablePCTrace:    Boolean = false
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
