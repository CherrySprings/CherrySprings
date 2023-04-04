import chisel3._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class TLDummyClient(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name     = s"TLDummyClient",
            sourceId = IdRange(0, 1)
          )
        )
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val (tl, _) = node.out.head

    tl.a.valid := false.B
    tl.a.bits  := 0.U.asTypeOf(tl.a.bits)
    tl.d.ready := false.B
  }
}
