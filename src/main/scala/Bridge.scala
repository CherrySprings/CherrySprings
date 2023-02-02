import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._

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

    // 64-bit CachePortIO to 64-bit TileLink/AXI4
    val req_addr  = Cat(req.bits.addr(31, 3), Fill(3, 0.U))
    val req_wdata = req.bits.wdata
    val req_wmask = req.bits.wmask

    val (_, get_bits) = edge.Get(source.U, req_addr, 3.U)
    val (_, put_bits) = edge.Put(source.U, req_addr, 3.U, req_wdata, req_wmask)

    tl.a.bits            := Mux(req.bits.wen, put_bits, get_bits)
    resp.bits.rdata      := tl.d.bits.data
    resp.bits.page_fault := false.B
  }
}

class DiplomacyToAXI4Bridge(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val device    = new SimpleDevice("memory", Seq("memory"))
  val beatBytes = 8
  val node = AXI4SlaveNode(
    Seq(
      AXI4SlavePortParameters(
        Seq(
          AXI4SlaveParameters(
            address       = Seq(AddressSet(BigInt("080000000", 16), BigInt("7fffffff", 16))),
            resources     = device.reg,
            regionType    = RegionType.UNCACHED,
            executable    = true,
            supportsWrite = TransferSizes(1, beatBytes * 4),
            supportsRead  = TransferSizes(1, beatBytes * 4)
          )
        ),
        beatBytes  = beatBytes,
        minLatency = 0
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val axi4 = new AXI4IO
    })

    val (axi, _) = node.in.head
    io.axi4.awaddr  := axi.aw.bits.addr
    io.axi4.awlen   := axi.aw.bits.len
    io.axi4.awsize  := axi.aw.bits.size
    io.axi4.awburst := axi.aw.bits.burst
    io.axi4.awprot  := axi.aw.bits.prot
    io.axi4.awlock  := axi.aw.bits.lock
    io.axi4.awcache := axi.aw.bits.cache
    io.axi4.awqos   := axi.aw.bits.qos
    io.axi4.awid    := axi.aw.bits.id
    io.axi4.awuser  := 0.U
    io.axi4.awvalid := axi.aw.valid
    axi.aw.ready    := io.axi4.awready

    io.axi4.wdata  := axi.w.bits.data
    io.axi4.wstrb  := axi.w.bits.strb
    io.axi4.wlast  := axi.w.bits.last
    io.axi4.wuser  := 0.U
    io.axi4.wvalid := axi.w.valid
    axi.w.ready    := io.axi4.wready

    axi.b.bits.resp := io.axi4.bresp
    axi.b.bits.id   := io.axi4.bid
    axi.b.valid     := io.axi4.bvalid
    io.axi4.bready  := axi.b.ready

    io.axi4.araddr  := axi.ar.bits.addr
    io.axi4.arlen   := axi.ar.bits.len
    io.axi4.arsize  := axi.ar.bits.size
    io.axi4.arburst := axi.ar.bits.burst
    io.axi4.arprot  := axi.ar.bits.prot
    io.axi4.arlock  := axi.ar.bits.lock
    io.axi4.arcache := axi.ar.bits.cache
    io.axi4.arqos   := axi.ar.bits.qos
    io.axi4.arid    := axi.ar.bits.id
    io.axi4.aruser  := 0.U
    io.axi4.arvalid := axi.ar.valid
    axi.ar.ready    := io.axi4.arready

    axi.r.bits.resp := io.axi4.rresp
    axi.r.bits.data := io.axi4.rdata
    axi.r.bits.last := io.axi4.rlast
    axi.r.bits.id   := io.axi4.rid
    axi.r.valid     := io.axi4.rvalid
    io.axi4.rready  := axi.r.ready

    if (debugAXI4) {
      when(axi.aw.fire) {
        printf("%d [AXI4-AW] addr=%x\n", DebugTimer(), axi.aw.bits.addr)
      }
      when(axi.w.fire) {
        printf("%d [AXI4-W ] data=%x strb=%x\n", DebugTimer(), axi.w.bits.data, axi.w.bits.strb)
      }
      when(axi.b.fire) {
        printf("%d [AXI4-B ] resp=%x\n", DebugTimer(), axi.b.bits.resp)
      }
      when(axi.ar.fire) {
        printf("%d [AXI4-AR] addr=%x\n", DebugTimer(), axi.ar.bits.addr)
      }
      when(axi.r.fire) {
        printf("%d [AXI4-R ] data=%x\n", DebugTimer(), axi.r.bits.data)
      }
    }
  }
}
