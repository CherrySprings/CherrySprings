import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class CachePortToTileLinkBridge(source: Int)(implicit p: Parameters)
    extends LazyModule
    with HasCherrySpringsParameters {
  require(xLen == 64)

  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name     = s"CachePort$source",
            sourceId = IdRange(source, source + 1)
          )
        )
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val cache = Flipped(new CachePortIO)
    })
    val (tl, edge) = node.out.head

    val req  = io.cache.req
    val resp = io.cache.resp

    tl.a.valid := req.valid
    req.ready  := tl.a.ready
    resp.valid := tl.d.valid
    tl.d.ready := resp.ready

    // 64-bit CachePortIO to 64-bit TileLink
    val req_addr  = Cat(req.bits.addr(paddrLen - 1, 3), Fill(3, 0.U))
    val req_wdata = req.bits.wdata
    val req_wmask = req.bits.wmask

    val (_, get_bits) = edge.Get(source.U, req_addr, 3.U)
    val (_, put_bits) = edge.Put(source.U, req_addr, 3.U, req_wdata, req_wmask)

    tl.a.bits            := Mux(req.bits.wen, put_bits, get_bits)
    resp.bits.rdata      := tl.d.bits.data
    resp.bits.page_fault := false.B
  }
}
