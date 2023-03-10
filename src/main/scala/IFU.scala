import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

class IF0(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val jmp_packet = Input(new JmpPacket)
    val req        = Decoupled(new CachePortReq)
    val req_addr   = Output(UInt(vaddrLen.W))
    val stall_b    = Input(Bool())
  })

  val pc_next   = Wire(UInt(xLen.W))
  val pc_update = (io.jmp_packet.valid || io.req.fire)
  val pc        = RegEnable(pc_next, resetPC.U, pc_update)
  pc_next := Mux(io.jmp_packet.valid, io.jmp_packet.target, pc + 4.U)

  io.req_addr      := Mux(io.jmp_packet.valid, io.jmp_packet.target, pc)
  io.req.bits      := 0.U.asTypeOf(new CachePortReq)
  io.req.bits.addr := Cat(io.req_addr(vaddrLen - 1, 3), 0.U(3.W))
  io.req.valid     := io.stall_b
}

class IF1(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val jmp_packet = Input(new JmpPacket)
    val resp       = Flipped(Decoupled(new CachePortResp))
    val stall_b    = Input(Bool())
    val out        = Output(new FDPacket)
    val ready      = Output(Bool())
    val req_fire   = Input(Bool())
  })

  val s_init :: s_resp :: s_wait :: Nil = Enum(3)
  val state                             = RegInit(s_init)

  val jmp_r = BoolStopWatch(
    io.jmp_packet.valid && !io.resp.fire && (state =/= s_wait),
    io.resp.fire
  )

  val state_to_wait = io.resp.bits.page_fault && !jmp_r && !io.jmp_packet.valid

  switch(state) {
    is(s_init) {
      when(io.req_fire) {
        state := s_resp
      }
    }
    is(s_resp) {
      when(io.resp.fire) {
        state := Mux(state_to_wait, s_wait, Mux(io.jmp_packet.valid, s_init, s_resp))
      }
    }
    is(s_wait) {
      when(io.jmp_packet.valid) { // jump to page fault handler
        state := s_init
      }
    }
  }

  io.resp.ready := (io.stall_b || io.jmp_packet.valid) && (state === s_resp)

  io.out       := 0.U.asTypeOf(new FDPacket)
  io.out.valid := io.resp.fire && !io.jmp_packet.valid && !jmp_r

  io.ready := (io.resp.fire && !state_to_wait && !io.jmp_packet.valid) || (state === s_init)
}

class IFU(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)

  val io = IO(new Bundle {
    val jmp_packet = Input(new JmpPacket)
    val imem       = new CachePortIO
    val out        = Output(new FDPacket)
    val stall_b    = Input(Bool())
  })

  val if0 = Module(new IF0)
  val if1 = Module(new IF1)

  if0.io.jmp_packet := io.jmp_packet
  if0.io.req        <> io.imem.req
  if0.io.stall_b    := io.stall_b && if1.io.ready

  if1.io.jmp_packet := io.jmp_packet
  if1.io.resp       <> io.imem.resp
  if1.io.stall_b    := io.stall_b
  if1.io.req_fire   := io.imem.req.fire

  val pc_queue = Module(new Queue(UInt(vaddrLen.W), 2))
  pc_queue.io.enq.bits  := if0.io.req_addr
  pc_queue.io.enq.valid := io.imem.req.fire
  pc_queue.io.deq.ready := io.imem.resp.fire

  io.out            := if1.io.out
  io.out.pc         := SignExt39_64(pc_queue.io.deq.bits)
  io.out.instr      := Mux(io.out.pc(2), io.imem.resp.bits.rdata(63, 32), io.imem.resp.bits.rdata(31, 0))
  io.out.page_fault := io.imem.resp.bits.page_fault
}

class IFUBackup(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)

  val io = IO(new Bundle {
    val jmp_packet = Input(new JmpPacket)
    val imem       = new CachePortIO
    val out        = Output(new FDPacket)
    val stall_b    = Input(Bool())
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
  io.imem.req.valid     := io.stall_b && (state === s_req)
  io.imem.resp.ready    := (io.stall_b || io.jmp_packet.valid) && (state === s_resp)

  val pc_queue = Module(new Queue(UInt(vaddrLen.W), 2))
  pc_queue.io.enq.bits  := req_addr
  pc_queue.io.enq.valid := io.imem.req.fire
  pc_queue.io.deq.ready := io.imem.resp.fire

  io.out.pc         := SignExt39_64(pc_queue.io.deq.bits)
  io.out.instr      := Mux(io.out.pc(2), io.imem.resp.bits.rdata(63, 32), io.imem.resp.bits.rdata(31, 0))
  io.out.page_fault := io.imem.resp.bits.page_fault
  io.out.valid      := io.imem.resp.fire && !io.jmp_packet.valid && !jmp_r
}
