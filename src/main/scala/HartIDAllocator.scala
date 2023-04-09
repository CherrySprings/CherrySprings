import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import org.chipsalliance.cde.config._

class HartIDAllocator(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val device = new SimpleDevice("hart-id-allocator", Seq("hart-id-allocator-0"))
  val node = TLRegisterNode(
    address   = Seq(AddressSet(BigInt("00020000", 16), BigInt("ffff", 16))),
    device    = device,
    beatBytes = 8
  )

  lazy val module = new LazyModuleImp(this) {
    val hart_id = RegInit(0.U(64.W))

    def hartIDAllocate(ready: Bool): (Bool, UInt) = {
      when(ready) { hart_id := hart_id + 1.U }
      // (ready, bits)
      (true.B, hart_id)
    }

    node.regmap(
      0x0 -> Seq(RegField.r(64, hartIDAllocate(_)))
    )
  }
}
