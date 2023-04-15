import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._

class TLSilentClient(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name     = s"tl-silent-client",
            sourceId = IdRange(0, 1)
          )
        )
      )
    )
  )

  lazy val module = new TLSilentClientModule(this)
}

class TLSilentClientModule(outer: TLSilentClient) extends LazyModuleImp(outer) {
  val (tl, edge) = outer.node.out.head

  tl.a.valid := false.B
  tl.b.ready := false.B
  tl.c.valid := false.B
  tl.d.ready := false.B
  tl.e.valid := false.B
}

object TLSilentClient {
  def apply()(implicit p: Parameters): TLNode = {
    val slient_client = LazyModule(new TLSilentClient)
    slient_client.node
  }
}
