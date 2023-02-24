import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

class CachePortProxy(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)

  val io = IO(new Bundle {
    val prv_mpp    = Input(UInt(2.W))
    val sv39_en    = Input(Bool())
    val satp_ppn   = Input(UInt(44.W))
    val sfence_vma = Input(Bool())
    val in         = Flipped(new CachePortIO)
    val out        = new CachePortIO
    val ptw        = new CachePortIO
  })

  val s_in_req :: s_tlb_req :: s_tlb_resp :: s_out_req :: Nil = Enum(4)
  val state_req                                               = RegInit(s_in_req)

  val in_req_bits_reg = RegInit(0.U.asTypeOf(new CachePortReq))
  when(io.in.req.fire) {
    in_req_bits_reg := io.in.req.bits
  }

  val tlb = Module(new TLB)
  tlb.io.prv                       := io.prv_mpp
  tlb.io.satp_ppn                  := io.satp_ppn
  tlb.io.sfence_vma                := io.sfence_vma
  tlb.io.ptw                       <> io.ptw
  tlb.io.addr_trans.req.bits.vaddr := in_req_bits_reg.addr.asTypeOf(new Sv39VirtAddr)
  tlb.io.addr_trans.req.bits.wen   := in_req_bits_reg.wen
  tlb.io.addr_trans.req.valid      := (state_req === s_tlb_req)

  // address translation & protection enable
  val atp_en = (io.prv_mpp =/= PRV.M.U) && io.sv39_en

  // record state when input fires, in case satp changes when accessing page table
  val atp_en_reg = RegEnable(atp_en, false.B, io.in.req.fire)

  switch(state_req) {
    is(s_in_req) {
      when(io.in.req.fire) {
        state_req := Mux(atp_en, s_tlb_req, s_out_req)
      }
    }
    is(s_tlb_req) {
      when(tlb.io.addr_trans.req.fire) {
        state_req := s_tlb_resp
      }
    }
    is(s_tlb_resp) {
      when(tlb.io.addr_trans.resp.fire) {
        state_req := Mux(tlb.io.addr_trans.resp.bits.page_fault, s_in_req, s_out_req)
      }
    }
    is(s_out_req) {
      when(io.out.req.fire) {
        state_req := s_in_req
      }
    }
  }

  val paddr      = RegEnable(tlb.io.addr_trans.resp.bits.paddr.asTypeOf(UInt(paddrLen.W)), 0.U, tlb.io.addr_trans.resp.fire)
  val page_fault = tlb.io.addr_trans.resp.bits.page_fault && tlb.io.addr_trans.resp.fire
  io.in.req.ready  := (state_req === s_in_req)
  io.out.req.valid := (state_req === s_out_req)
  io.out.req.bits  := in_req_bits_reg
  when(atp_en_reg) {
    io.out.req.bits.addr := paddr
  }

  val page_fault_reg = BoolStopWatch(page_fault, io.in.resp.fire)

  io.in.resp.bits            := io.out.resp.bits
  io.in.resp.bits.page_fault := page_fault_reg
  io.in.resp.valid           := io.out.resp.valid || page_fault_reg
  io.out.resp.ready          := io.in.resp.ready
}
