import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._

class CachePortReq(implicit p: Parameters) extends CherrySpringsBundle {
  val addr  = Output(UInt(vaddrLen.W))
  val wdata = Output(UInt(xLen.W))
  val wmask = Output(UInt((xLen / 8).W))
  val wen   = Output(Bool())
  val len   = Output(UInt(2.W)) // only used for uncache (mmio) and lr/sc/amo for dcache
  val lrsc  = Output(Bool()) // only used for dcache (lr when wen = 0 and sc when wen = 1)
  val amo   = Output(UInt(Constant.LSU_X.length.W)) // only used for amo, and wen = 1

  def isAmo(): Bool = Constant.isAmo(amo)

  override def toPrintable: Printable = {
    cf"addr=$addr%x wdata=$wdata%x wmask=$wmask%x wen=$wen len=$len"
  }
}

class CachePortResp(implicit p: Parameters) extends CherrySpringsBundle {
  val rdata        = Output(UInt(xLen.W))
  val page_fault   = Output(Bool())
  val access_fault = Output(Bool())
  val mmio         = Output(Bool())

  override def toPrintable: Printable = {
    cf"rdata=$rdata%x pf=$page_fault af=$access_fault mmio=$mmio"
  }
}

class CachePortIO(implicit p: Parameters) extends CherrySpringsBundle {
  val req  = Decoupled(new CachePortReq)
  val resp = Flipped(Decoupled(new CachePortResp))
}

class CachePortXBarNto1(n: Int)(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val in  = Flipped(Vec(n, new CachePortIO))
    val out = new CachePortIO
  })

  val arbiter = Module(new RRArbiter(new CachePortReq, n))

  for (i <- 0 until n) {
    arbiter.io.in(i) <> io.in(i).req
  }

  // record in flight source (suppose responses are returned in-order)
  val id_queue = Module(new Queue(UInt(log2Ceil(n).W), entries = 4))
  id_queue.io.enq.valid := io.out.req.fire
  id_queue.io.enq.bits  := arbiter.io.chosen
  id_queue.io.deq.ready := io.out.resp.fire

  // req logic
  for (i <- 0 until n) {
    io.in(i).req.ready := (arbiter.io.chosen === i.U) && io.out.req.ready && id_queue.io.enq.ready
  }
  (io.out.req, arbiter.io.out) match {
    case (l, r) => {
      l.bits  := r.bits
      l.valid := r.valid
      r.ready := l.ready
    }
  }

  // resp logic
  for (i <- 0 until n) {
    io.in(i).resp.bits  := io.out.resp.bits
    io.in(i).resp.valid := false.B
    io.out.resp.ready   := false.B
  }
  for (i <- 0 until n) {
    when(id_queue.io.deq.valid && (id_queue.io.deq.bits === i.U)) {
      io.out.resp.ready   := io.in(i).resp.ready
      io.in(i).resp.valid := io.out.resp.valid
    }
  }
}

class CachePortXBar1to2(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val in   = Flipped(new CachePortIO)
    val out  = Vec(2, new CachePortIO)
    val to_1 = Input(Bool())
  })

  // assume only one in-flight request
  val to_1_r = RegEnable(io.to_1, false.B, io.in.req.fire)

  // req logic
  io.out(0).req.bits  := io.in.req.bits
  io.out(1).req.bits  := io.in.req.bits
  io.out(0).req.valid := io.in.req.valid && !io.to_1
  io.out(1).req.valid := io.in.req.valid && io.to_1
  io.in.req.ready     := Mux(io.to_1, io.out(1).req.ready, io.out(0).req.ready)

  // resp logic
  io.in.resp.bits      := Mux(to_1_r, io.out(1).resp.bits, io.out(0).resp.bits)
  io.in.resp.valid     := Mux(to_1_r, io.out(1).resp.valid, io.out(0).resp.valid)
  io.in.resp.bits.mmio := to_1_r
  io.out(0).resp.ready := io.in.resp.ready
  io.out(1).resp.ready := io.in.resp.ready
}

class Uncache(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  require(xLen == 64)

  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name     = s"uncache-${hartID}",
            sourceId = IdRange(0, sourceRange)
          )
        )
      )
    )
  )

  lazy val module = new UncacheModule(this)
}

class UncacheModule(outer: Uncache) extends LazyModuleImp(outer) with HasCherrySpringsParameters {
  val io = IO(new Bundle {
    val in = Flipped(new CachePortIO)
  })

  // TL-UL for uncache data port
  val (tl, edge) = outer.node.out.head

  val req  = io.in.req
  val resp = io.in.resp

  tl.a.valid := req.valid
  req.ready  := tl.a.ready
  resp.valid := tl.d.valid
  tl.d.ready := resp.ready

  val source = Counter(tl.a.fire, sourceRange)._1

  val (_, get_bits) = edge.Get(source, req.bits.addr, req.bits.len)
  val (_, put_bits) = edge.Put(source, req.bits.addr, req.bits.len, req.bits.wdata, req.bits.wmask)

  tl.a.bits       := Mux(req.bits.wen, put_bits, get_bits)
  resp.bits       := 0.U.asTypeOf(new CachePortResp)
  resp.bits.rdata := tl.d.bits.data

  if (debugUncache) {
    when(req.fire) {
      printf(cf"${DebugTimer()} [ MMIO ] [in -req ] ${req.bits}\n")
    }
    when(resp.fire) {
      printf(cf"${DebugTimer()} [ MMIO ] [in -resp] ${resp.bits}\n")
    }
  }
}
