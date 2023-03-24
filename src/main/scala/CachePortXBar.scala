import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

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
