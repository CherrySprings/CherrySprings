import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.rocket.Causes

class LSU(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 32 || xLen == 64)

  val io = IO(new Bundle {
    val uop      = Input(new MicroOp)
    val is_mem   = Input(Bool())
    val is_store = Input(Bool())
    val is_amo   = Input(Bool())
    val addr     = Input(UInt(xLen.W))
    val wdata    = Input(UInt(xLen.W))
    val rdata    = Output(UInt(xLen.W))
    val valid    = Output(Bool())
    val exc_code = Output(UInt(4.W))
    val dmem     = new CachePortIO
    val ready    = Output(Bool())
  })

  val req    = io.dmem.req
  val resp   = io.dmem.resp
  val is_lr  = io.uop.lsu_op === s"b$LSU_LR".U
  val is_sc  = io.uop.lsu_op === s"b$LSU_SC".U
  val is_ldu = io.uop.lsu_op === s"b$LSU_LDU".U

  val s_idle :: s_req :: s_resp :: s_exc :: s1 = Enum(8)
  val s_amo_ld_req :: s_amo_ld_resp :: s2      = s1
  val s_amo_st_req :: s_amo_st_resp :: Nil     = s2
  val state                                    = RegInit(s_idle)

  val addr_offset = if (xLen == 32) io.addr(1, 0) else io.addr(2, 0)
  val wmask = if (xLen == 32) {
    MuxLookup(
      io.uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U -> "b0001".U(4.W),
        s"b$MEM_HALF".U -> "b0011".U(4.W),
        s"b$MEM_WORD".U -> "b1111".U(4.W)
      )
    )
  } else {
    MuxLookup(
      io.uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U  -> "b00000001".U(8.W),
        s"b$MEM_HALF".U  -> "b00000011".U(8.W),
        s"b$MEM_WORD".U  -> "b00001111".U(8.W),
        s"b$MEM_DWORD".U -> "b11111111".U(8.W)
      )
    )
  }

  val resp_data       = resp.bits.rdata >> (addr_offset << 3)
  val resp_page_fault = resp.bits.page_fault
  val ld_out          = Wire(UInt(xLen.W))
  val ldu_out         = Wire(UInt(xLen.W))
  val ld_out_amo      = RegInit(0.U(xLen.W))
  val st_data         = (io.wdata << (addr_offset << 3))(xLen - 1, 0)
  val st_data_amo_raw = Wire(UInt(xLen.W))
  val st_data_amo     = (st_data_amo_raw << (addr_offset << 3))(xLen - 1, 0)

  req.bits.addr  := io.addr
  req.bits.wdata := Mux(state === s_req, st_data, st_data_amo)
  req.bits.wmask := (wmask << addr_offset)(xLen / 8 - 1, 0)
  req.bits.wen   := io.is_store || (state === s_amo_st_req)
  req.valid      := state === s_req || state === s_amo_ld_req || state === s_amo_st_req
  resp.ready     := state === s_resp || state === s_amo_ld_resp || state === s_amo_st_resp

  val lrsc_reserved = RegInit(false.B)
  val lrsc_addr     = RegInit(0.U(xLen.W))
  val sc_succeed    = is_sc && lrsc_reserved && (lrsc_addr === io.addr)
  val sc_completed  = is_sc && Mux(sc_succeed, resp.fire, state === s_resp)

  when(req.fire && is_lr) {
    lrsc_reserved := true.B
    lrsc_addr     := req.bits.addr
  }
  when(is_sc && sc_completed) {
    lrsc_reserved := false.B
  }

  val misaligned = Wire(Bool())
  misaligned := MuxLookup(
    io.uop.mem_len,
    false.B,
    Array(
      s"b$MEM_HALF".U  -> (io.addr(0) =/= 0.U),
      s"b$MEM_WORD".U  -> (io.addr(1, 0) =/= 0.U),
      s"b$MEM_DWORD".U -> (io.addr(2, 0) =/= 0.U)
    )
  )

  switch(state) {
    is(s_idle) {
      when(io.is_mem) {
        when(misaligned) {
          state := s_exc
        }.elsewhen(is_sc) {
          state := Mux(sc_succeed, s_req, s_resp)
        }.otherwise {
          state := Mux(io.is_amo, s_amo_ld_req, s_req)
        }
      }
    }
    is(s_req) {
      when(req.fire) {
        state := s_resp
      }
    }
    is(s_resp) {
      when(resp.fire || sc_completed) {
        state := Mux(resp_page_fault, s_exc, s_idle)
      }
    }
    is(s_amo_ld_req) {
      when(req.fire) {
        state := s_amo_ld_resp
      }
    }
    is(s_amo_ld_resp) {
      when(resp.fire) {
        state := Mux(resp_page_fault, s_exc, s_amo_st_req)
      }
    }
    is(s_amo_st_req) {
      when(req.fire) {
        state := s_amo_st_resp
      }
    }
    is(s_amo_st_resp) {
      when(resp.fire) {
        state := Mux(resp_page_fault, s_exc, s_idle)
      }
    }
    is(s_exc) {
      state := s_idle
    }
  }

  if (xLen == 32) {
    ld_out := MuxLookup(
      io.uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U -> Cat(Fill(24, resp_data(7)), resp_data(7, 0)),
        s"b$MEM_HALF".U -> Cat(Fill(16, resp_data(15)), resp_data(15, 0)),
        s"b$MEM_WORD".U -> resp_data
      )
    )

    ldu_out := MuxLookup(
      io.uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U -> Cat(Fill(24, 0.U), resp_data(7, 0)),
        s"b$MEM_HALF".U -> Cat(Fill(16, 0.U), resp_data(15, 0))
      )
    )
  } else {
    ld_out := MuxLookup(
      io.uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U  -> Cat(Fill(56, resp_data(7)), resp_data(7, 0)),
        s"b$MEM_HALF".U  -> Cat(Fill(48, resp_data(15)), resp_data(15, 0)),
        s"b$MEM_WORD".U  -> Cat(Fill(32, resp_data(31)), resp_data(31, 0)),
        s"b$MEM_DWORD".U -> resp_data
      )
    )

    ldu_out := MuxLookup(
      io.uop.mem_len,
      0.U,
      Array(
        s"b$MEM_BYTE".U -> Cat(Fill(56, 0.U), resp_data(7, 0)),
        s"b$MEM_HALF".U -> Cat(Fill(48, 0.U), resp_data(15, 0)),
        s"b$MEM_WORD".U -> Cat(Fill(32, 0.U), resp_data(31, 0))
      )
    )
  }

  when(state === s_amo_ld_resp && resp.fire) {
    ld_out_amo := ld_out
  }

  // todo: optimize this logic
  st_data_amo_raw := MuxLookup(
    io.uop.lsu_op,
    0.U,
    Array(
      s"b$LSU_AMOSWAP".U -> io.wdata,
      s"b$LSU_AMOADD".U  -> (io.wdata + ld_out_amo),
      s"b$LSU_AMOAND".U  -> (io.wdata & ld_out_amo),
      s"b$LSU_AMOOR".U   -> (io.wdata | ld_out_amo),
      s"b$LSU_AMOXOR".U  -> (io.wdata ^ ld_out_amo),
      s"b$LSU_AMOMAX".U  -> Mux(io.wdata.asSInt > ld_out_amo.asSInt, io.wdata, ld_out_amo),
      s"b$LSU_AMOMAXU".U -> Mux(io.wdata.asUInt > ld_out_amo.asUInt, io.wdata, ld_out_amo),
      s"b$LSU_AMOMIN".U  -> Mux(io.wdata.asSInt < ld_out_amo.asSInt, io.wdata, ld_out_amo),
      s"b$LSU_AMOMINU".U -> Mux(io.wdata.asUInt < ld_out_amo.asUInt, io.wdata, ld_out_amo)
    )
  )

  io.rdata := Mux(is_sc, (!sc_succeed).asUInt, Mux(io.is_amo, ld_out_amo, Mux(is_ldu, ldu_out, ld_out)))
  io.valid := (state === s_resp || state === s_amo_st_resp) && (resp.fire && !resp.bits.page_fault) || sc_completed || (state === s_exc) // assert for only 1 cycle
  io.ready := ((state === s_idle) && !io.is_mem) || io.valid

  /*
   * exc_code Description
   *        0 Load/store/AMO instruction is executed without exception (different from ISA, 0 for Instruction address misaligned)
   *        4 Load address misaligned
   *        5 Load access fault             (not implemented yet)
   *        6 Store/AMO address misaligned
   *        7 Store/AMO access fault        (not implemented yet)
   *       13 Load page fault
   *       15 Store/AMO page fault
   */
  val exc_code =
    Mux(
      io.is_store || io.is_amo,
      Mux(misaligned, Causes.misaligned_store.U, Causes.store_page_fault.U),
      Mux(misaligned, Causes.misaligned_load.U, Causes.load_page_fault.U)
    )
  io.exc_code := Mux(state === s_exc, exc_code, 0.U)
}
