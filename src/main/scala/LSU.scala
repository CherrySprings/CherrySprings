import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.rocket._
import org.chipsalliance.cde.config._
import Constant._

class LSU(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)

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
    val is_mmio  = Output(Bool())
    val hartid   = Input(UInt(8.W)) // only for difftest
  })

  val req  = io.dmem.req
  val resp = io.dmem.resp

  val s_idle :: s_req :: s_resp :: s_exc :: Nil = Enum(4)
  val state                                     = RegInit(s_idle)

  val addr_offset = io.addr(2, 0)

  val wdata = (io.wdata << (addr_offset << 3))(xLen - 1, 0)
  val wmask = (MuxLookup(
    io.uop.mem_len,
    0.U,
    Seq(
      s"b$MEM_BYTE".U  -> "b00000001".U(8.W),
      s"b$MEM_HALF".U  -> "b00000011".U(8.W),
      s"b$MEM_WORD".U  -> "b00001111".U(8.W),
      s"b$MEM_DWORD".U -> "b11111111".U(8.W)
    )
  ) << addr_offset)(xLen / 8 - 1, 0)

  req.bits.addr  := io.addr
  req.bits.wdata := wdata
  req.bits.wmask := wmask
  req.bits.len   := io.uop.mem_len
  req.bits.wen   := io.is_store || io.is_amo
  req.bits.lrsc  := isLrSc(io.uop.lsu_op)
  req.bits.amo   := io.uop.lsu_op
  req.valid      := (state === s_req)
  resp.ready     := (state === s_resp)

  val misaligned = Wire(Bool())
  misaligned := MuxLookup(
    io.uop.mem_len,
    false.B,
    Seq(
      s"b$MEM_HALF".U  -> (io.addr(0) =/= 0.U),
      s"b$MEM_WORD".U  -> (io.addr(1, 0) =/= 0.U),
      s"b$MEM_DWORD".U -> (io.addr(2, 0) =/= 0.U)
    )
  )

  switch(state) {
    is(s_idle) {
      when(io.is_mem) {
        state := Mux(misaligned, s_exc, s_req)
      }
    }
    is(s_req) {
      when(req.fire) {
        state := s_resp
      }
    }
    is(s_resp) {
      when(resp.fire) {
        state := Mux(resp.bits.page_fault || resp.bits.access_fault, s_exc, s_idle)
      }
    }
    is(s_exc) {
      state := s_idle
    }
  }

  val resp_data = resp.bits.rdata >> (addr_offset << 3)
  val sign = (!isLdu(io.uop.lsu_op) && MuxLookup(
    io.uop.mem_len,
    false.B,
    Seq(
      s"b$MEM_BYTE".U -> resp_data(7),
      s"b$MEM_HALF".U -> resp_data(15),
      s"b$MEM_WORD".U -> resp_data(31)
    )
  ).asBool).asUInt
  val rdata = MuxLookup(
    io.uop.mem_len,
    0.U,
    Seq(
      s"b$MEM_BYTE".U  -> Cat(Fill(56, sign), resp_data(7, 0)),
      s"b$MEM_HALF".U  -> Cat(Fill(48, sign), resp_data(15, 0)),
      s"b$MEM_WORD".U  -> Cat(Fill(32, sign), resp_data(31, 0)),
      s"b$MEM_DWORD".U -> resp_data
    )
  )

  io.rdata   := rdata
  io.valid   := ((state === s_resp) && (resp.fire && !resp.bits.page_fault && !resp.bits.access_fault)) || (state === s_exc) // assert for only 1 cycle
  io.is_mmio := io.valid && resp.bits.mmio
  io.ready   := ((state === s_idle) && !io.is_mem) || io.valid

  /*
   * exc_code Description
   *        0 Load/store/AMO instruction is executed without exception (different from ISA, 0 for Instruction address misaligned)
   *        4 Load address misaligned
   *        5 Load access fault
   *        6 Store/AMO address misaligned
   *        7 Store/AMO access fault
   *       13 Load page fault
   *       15 Store/AMO page fault
   */
  val exc_code =
    Mux(
      io.is_store || io.is_amo,
      Mux(
        misaligned,
        Causes.misaligned_store.U,
        Mux(RegNext(resp.bits.access_fault), Causes.store_access.U, Causes.store_page_fault.U)
      ),
      Mux(
        misaligned,
        Causes.misaligned_load.U,
        Mux(RegNext(resp.bits.access_fault), Causes.load_access.U, Causes.load_page_fault.U)
      )
    )
  io.exc_code := Mux(state === s_exc, exc_code, 0.U)

  if (enableDifftest) {
    val lsu_ok        = (state === s_resp) && resp.fire && !resp.bits.page_fault && !resp.bits.access_fault && !resp.bits.mmio
    val paddr_aligned = Cat(resp.bits.paddr(paddrLen - 1, 3), 0.U(3.W))

    val dt_sb = Module(new DifftestSbufferEvent)
    dt_sb.io.clock       := clock
    dt_sb.io.coreid      := io.hartid
    dt_sb.io.index       := 0.U
    dt_sb.io.sbufferResp := RegNext(RegNext(lsu_ok && (io.is_store || io.is_amo)))
    dt_sb.io.sbufferAddr := RegNext(RegNext(paddr_aligned))
    dt_sb.io.sbufferMask := RegNext(RegNext(wmask))
    for (i <- 0 until 64) {
      if (i < 8) {
        dt_sb.io.sbufferData(i) := RegNext(RegNext(resp.bits.wdata(i * 8 + 7, i * 8)))
      } else {
        dt_sb.io.sbufferData(i) := 0.U
      }
    }

    val dt_ld = Module(new DifftestLoadEvent)
    dt_ld.io.clock  := clock
    dt_ld.io.coreid := io.hartid
    dt_ld.io.index  := 0.U
    dt_ld.io.valid  := RegNext(lsu_ok && (isLoad(io.uop.lsu_op) || io.is_amo))
    dt_ld.io.paddr  := RegNext(resp.bits.paddr)
    dt_ld.io.opType := RegNext(io.uop.mem_len)
    dt_ld.io.fuType := Mux(io.is_amo, 0xf.U /* amo */, 0xc.U /* load */ )

    val dt_st = Module(new DifftestStoreEvent)
    dt_st.io.clock     := clock
    dt_st.io.coreid    := io.hartid
    dt_st.io.index     := 0.U
    dt_st.io.storeAddr := RegNext(RegNext(paddr_aligned))
    dt_st.io.storeData := RegNext(RegNext(MaskData(0.U(64.W), resp.bits.wdata, MaskExpand(wmask))))
    dt_st.io.storeMask := RegNext(RegNext(wmask))
    dt_st.io.valid := RegNext(
      RegNext(lsu_ok && ((io.is_store && (!isLrSc(io.uop.lsu_op) || !io.rdata(0).asBool)) || io.is_amo))
    )

    val dt_am = Module(new DifftestAtomicEvent)
    dt_am.io.clock      := clock
    dt_am.io.coreid     := io.hartid
    dt_am.io.atomicResp := RegNext(lsu_ok && io.is_amo)
    dt_am.io.atomicAddr := RegNext(paddr_aligned)
    dt_am.io.atomicData := RegNext(wdata)
    dt_am.io.atomicMask := RegNext(wmask)
    dt_am.io.atomicOut  := RegNext(io.rdata)
    dt_am.io.atomicFuop := RegNext(
      Cat(
        MuxLookup(
          io.uop.lsu_op,
          0.U,
          Seq(
            s"b$LSU_AMOSWAP".U -> "b00101".U,
            s"b$LSU_AMOADD".U  -> "b00111".U,
            s"b$LSU_AMOXOR".U  -> "b01001".U,
            s"b$LSU_AMOAND".U  -> "b01011".U,
            s"b$LSU_AMOOR".U   -> "b01101".U,
            s"b$LSU_AMOMIN".U  -> "b01111".U,
            s"b$LSU_AMOMAX".U  -> "b10001".U,
            s"b$LSU_AMOMINU".U -> "b10011".U,
            s"b$LSU_AMOMAXU".U -> "b10101".U
          )
        ),
        (io.uop.mem_len === s"b$MEM_DWORD".U).asUInt
      )
    )

    val diff_ls = Module(new DifftestLrScEvent)
    diff_ls.io.clock   := clock
    diff_ls.io.coreid  := io.hartid
    diff_ls.io.valid   := RegNext(lsu_ok && isLrSc(io.uop.lsu_op))
    diff_ls.io.success := RegNext(!io.rdata(0).asBool)
  }
}
