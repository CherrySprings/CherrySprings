import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

class IFU(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)

  val io = IO(new Bundle {
    val jmp_packet = Input(new JmpPacket)
    val imem       = new CachePortIO
    val out        = Output(new FDPacket)
    val out_ready  = Input(Bool())
  })

  val pc_next   = Wire(UInt(xLen.W))
  val pc_update = io.jmp_packet.valid || io.imem.req.fire
  val pc        = RegEnable(pc_next, resetPC.U, pc_update)
  pc_next := Mux(io.jmp_packet.valid, io.jmp_packet.target, pc + 4.U)

  val s_req :: s_resp :: s_wait :: Nil = Enum(3)
  val state                            = RegInit(s_req)

  val jmp_r = BoolStopWatch(
    io.jmp_packet.valid && !io.imem.resp.fire && (state =/= s_wait),
    io.imem.resp.fire
  )

  switch(state) {
    is(s_req) {
      when(io.imem.req.fire) {
        state := s_resp
      }
    }
    is(s_resp) {
      when(io.imem.resp.fire) {
        state := Mux(io.imem.resp.bits.page_fault && !jmp_r, s_wait, s_req)
      }
    }
    is(s_wait) {
      when(io.jmp_packet.valid) { // jump to page fault handler
        state := s_req
      }
    }
  }

  val req_addr = Wire(UInt(vaddrLen.W))
  req_addr              := Mux(io.jmp_packet.valid, io.jmp_packet.target, pc)
  io.imem.req.bits      := 0.U.asTypeOf(new CachePortReq)
  io.imem.req.bits.addr := Cat(req_addr(vaddrLen - 1, 3), 0.U(3.W))
  io.imem.req.valid     := io.out_ready && (state === s_req)
  io.imem.resp.ready    := (io.out_ready || io.jmp_packet.valid) && (state === s_resp)

  val pc_queue = Module(new Queue(UInt(vaddrLen.W), 2))
  pc_queue.io.enq.bits  := req_addr
  pc_queue.io.enq.valid := io.imem.req.fire
  pc_queue.io.deq.ready := io.imem.resp.fire

  io.out.pc         := SignExt39_64(pc_queue.io.deq.bits)
  io.out.instr      := Mux(io.out.pc(2), io.imem.resp.bits.rdata(63, 32), io.imem.resp.bits.rdata(31, 0))
  io.out.page_fault := io.imem.resp.bits.page_fault
  io.out.valid      := io.imem.resp.fire && !io.jmp_packet.valid && !jmp_r
}
