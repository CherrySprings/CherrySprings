import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

class CachePortProxy(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)

  val io = IO(new Bundle {
    val prv_mpp  = Input(UInt(2.W))
    val sv39_en  = Input(Bool())
    val satp_ppn = Input(UInt(44.W))
    val in       = Flipped(new CachePortIO)
    val out      = new CachePortIO
    val ptw      = new CachePortIO
  })

  val s_in_req :: s_ptw_req :: s_ptw_resp :: s_out_req :: Nil = Enum(4)
  val state_req                                               = RegInit(s_in_req)

  val in_req_bits = RegInit(0.U.asTypeOf(new CachePortReq))
  when(io.in.req.fire) {
    in_req_bits := io.in.req.bits
  }

  val ptw = Module(new PTW)
  ptw.io.prv                       := io.prv_mpp
  ptw.io.satp_ppn                  := io.satp_ppn
  ptw.io.ptw                       <> io.ptw
  ptw.io.addr_trans.req.bits.vaddr := in_req_bits.addr
  ptw.io.addr_trans.req.bits.wen   := in_req_bits.wen
  ptw.io.addr_trans.req.valid      := (state_req === s_ptw_req)
  ptw.io.addr_trans.resp.ready     := (state_req === s_ptw_resp)

  val ptw_en = (io.prv_mpp =/= PRV.M.U) && io.sv39_en

  // record state when input fires, in case satp changes when accessing page table
  val ptw_en_reg = RegEnable(ptw_en, false.B, io.in.req.fire)

  switch(state_req) {
    is(s_in_req) {
      when(io.in.req.fire) {
        state_req := Mux(ptw_en, s_ptw_req, s_out_req)
      }
    }
    is(s_ptw_req) {
      when(ptw.io.addr_trans.req.fire) {
        state_req := s_ptw_resp
      }
    }
    is(s_ptw_resp) {
      when(ptw.io.addr_trans.resp.fire) {
        state_req := Mux(ptw.io.addr_trans.resp.bits.page_fault, s_in_req, s_out_req)
      }
    }
    is(s_out_req) {
      when(io.out.req.fire) {
        state_req := s_in_req
      }
    }
  }

  val paddr      = RegEnable(ptw.io.addr_trans.resp.bits.paddr, 0.U, ptw.io.addr_trans.resp.fire)
  val page_fault = ptw.io.addr_trans.resp.bits.page_fault && ptw.io.addr_trans.resp.fire
  io.in.req.ready  := (state_req === s_in_req)
  io.out.req.valid := (state_req === s_out_req)
  io.out.req.bits  := in_req_bits
  when(ptw_en_reg) {
    io.out.req.bits.addr := paddr
  }

  val s_in_resp :: s_out_resp :: Nil = Enum(2)
  val state_resp                     = RegInit(s_out_resp)

  val out_resp_bits = RegInit(0.U.asTypeOf(new CachePortResp))
  when(io.out.resp.fire) {
    out_resp_bits := io.out.resp.bits
  }

  val page_fault_r = BoolStopWatch(page_fault, io.in.resp.fire)

  switch(state_resp) {
    is(s_out_resp) {
      when(io.out.resp.fire || page_fault) {
        state_resp := s_in_resp
      }
    }
    is(s_in_resp) {
      when(io.in.resp.fire) {
        state_resp := s_out_resp
      }
    }
  }
  io.in.resp.bits            := out_resp_bits
  io.in.resp.bits.page_fault := page_fault_r
  io.in.resp.valid           := (state_resp === s_in_resp)
  io.out.resp.ready          := (state_resp === s_out_resp)
}
