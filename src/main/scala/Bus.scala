import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import Constant._

class CachePortReq(implicit p: Parameters) extends CherrySpringsBundle {
  val addr  = Output(UInt(vaddrLen.W))
  val wdata = Output(UInt(xLen.W))
  val wmask = Output(UInt((xLen / 8).W))
  val wen   = Output(Bool())

  override def toPrintable: Printable = {
    cf"addr=$addr%x wdata=$wdata%x wmask=$wmask%x wen=$wen"
  }
}

class CachePortResp(implicit p: Parameters) extends CherrySpringsBundle {
  val rdata        = Output(UInt(xLen.W))
  val page_fault   = Output(Bool())
  val access_fault = Output(Bool())

  override def toPrintable: Printable = {
    cf"rdata=$rdata%x pf=$page_fault af=$access_fault"
  }
}

class CachePortIO(implicit p: Parameters) extends CherrySpringsBundle {
  val req  = Decoupled(new CachePortReq)
  val resp = Flipped(Decoupled(new CachePortResp))
}

class DataPortReq(implicit p: Parameters) extends CachePortReq {
  val len    = Output(UInt(2.W))
  val signed = Output(Bool())

  override def toPrintable: Printable = {
    cf"addr=$addr%x wdata=$wdata%x wmask=$wmask%x wen=$wen len=$len"
  }
}

class DataPortResp(implicit p: Parameters) extends CachePortResp {}

class DataPortIO(implicit p: Parameters) extends CachePortIO {
  override val req  = Decoupled(new DataPortReq)
  override val resp = Flipped(Decoupled(new DataPortResp))
}

class DataPort2CachePortBridge(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val in  = Flipped(new DataPortIO)
    val out = new CachePortIO
  })

  val addr_offset = io.in.req.bits.addr(2, 0)

  // req
  io.out.req.bits.addr  := Cat(io.in.req.bits.addr(vaddrLen - 1, 3), 0.U(3.W))
  io.out.req.bits.wdata := (io.in.req.bits.wdata << (addr_offset << 3))(xLen - 1, 0)
  io.out.req.bits.wmask := (io.in.req.bits.wmask << addr_offset)(xLen / 8 - 1, 0)
  io.out.req.bits.wen   := io.in.req.bits.wen
  io.out.req.valid      := io.in.req.valid
  io.in.req.ready       := io.out.req.ready

  // resp
  val addr_offset_r = RegEnable(addr_offset, 0.U, io.in.req.fire)
  val len_r         = RegEnable(io.in.req.bits.len, 0.U, io.in.req.fire)
  val signed_r      = RegEnable(io.in.req.bits.signed, false.B, io.in.req.fire)
  val resp_data     = io.out.resp.bits.rdata >> (addr_offset_r << 3)
  val sign = (signed_r && MuxLookup(
    len_r,
    false.B,
    Array(
      s"b$MEM_BYTE".U -> resp_data(7),
      s"b$MEM_HALF".U -> resp_data(15),
      s"b$MEM_WORD".U -> resp_data(31)
    )
  ).asBool).asUInt
  val rdata = MuxLookup(
    len_r,
    0.U,
    Array(
      s"b$MEM_BYTE".U  -> Cat(Fill(56, sign), resp_data(7, 0)),
      s"b$MEM_HALF".U  -> Cat(Fill(48, sign), resp_data(15, 0)),
      s"b$MEM_WORD".U  -> Cat(Fill(32, sign), resp_data(31, 0)),
      s"b$MEM_DWORD".U -> resp_data
    )
  )
  io.in.resp.bits.rdata        := rdata
  io.in.resp.bits.page_fault   := io.out.resp.bits.page_fault
  io.in.resp.bits.access_fault := io.out.resp.bits.access_fault
  io.in.resp.valid             := io.out.resp.valid
  io.out.resp.ready            := io.in.resp.ready

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
