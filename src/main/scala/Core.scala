import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chipsalliance.rocketchip.config._
import Constant._
import difftest._

class Core(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val imem      = new CachePortIO
    val dmem      = new CachePortIO
    val iptw      = new CachePortIO
    val dptw      = new CachePortIO
    val fence_i   = Output(Bool())
    val interrupt = new ExternalInterruptIO
  })

  def isAmo(lsu_op:   UInt) = lsu_op(4).asBool
  def isStore(lsu_op: UInt) = lsu_op === s"b$LSU_ST".U || lsu_op === s"b$LSU_SC".U

  val prv        = Wire(UInt(2.W))
  val sv39_en    = Wire(Bool())
  val satp_ppn   = Wire(UInt(44.W))
  val sfence_vma = Wire(Bool())

  /* ----- Stage 1 - Instruction Fetch (IF) -------- */

  val ifu = Module(new IFU)
  val imem_proxy = Module(new CachePortProxy()(p.alterPartial({
    case IsITLB => true
    case IsDTLB => false
  })))
  val alu_jmp_packet = Wire(new JmpPacket) // from EX stage
  val sys_jmp_packet = Wire(new JmpPacket) // from MEM stage
  imem_proxy.io.in         <> ifu.io.imem
  imem_proxy.io.out        <> io.imem
  imem_proxy.io.ptw        <> io.iptw
  imem_proxy.io.prv_mpp    := prv
  imem_proxy.io.sv39_en    := sv39_en
  imem_proxy.io.satp_ppn   := satp_ppn
  imem_proxy.io.sfence_vma := sfence_vma
  ifu.io.jmp_packet.valid  := alu_jmp_packet.valid || sys_jmp_packet.valid
  ifu.io.jmp_packet.target := Mux(sys_jmp_packet.valid, sys_jmp_packet.target, alu_jmp_packet.target)

  /* ----- Stage 2 - Instruction Buffer (IB) ------- */

  val stall_b = Wire(Bool())
  val flush   = Wire(Bool())

  val instr_buffer = Module(new Queue(new FDPacket, entries = 4, pipe = true, hasFlush = true))
  ifu.io.stall_b            := instr_buffer.io.enq.ready
  instr_buffer.io.enq.bits  := ifu.io.out
  instr_buffer.io.enq.valid := ifu.io.out.valid
  instr_buffer.io.deq.ready := stall_b
  instr_buffer.io.flush.get := flush

  /* ----- Stage 3 - Instruction Decode (ID) ------- */

  val decode = Module(new Decode)
  decode.io.in       := instr_buffer.io.deq.bits
  decode.io.in.valid := instr_buffer.io.deq.fire

  val rf = Module(new RegFile)
  rf.io.rs1_index := decode.io.out.rs1_index
  rf.io.rs2_index := decode.io.out.rs2_index

  val id_rs1_data         = Wire(UInt(xLen.W))
  val id_rs2_data         = Wire(UInt(xLen.W))
  val id_rs2_data_from_rf = Wire(UInt(xLen.W))
  val id_ex               = Module(new PipelineReg(new DXPacket))
  id_ex.io.in.uop              := decode.io.out
  id_ex.io.in.rs1_data         := id_rs1_data
  id_ex.io.in.rs2_data         := id_rs2_data
  id_ex.io.in.rs2_data_from_rf := id_rs2_data_from_rf
  id_ex.io.en                  := stall_b
  id_ex.io.flush               := flush

  /* ----- Stage 4 - Execution (EX) ---------------- */

  val alu = Module(new ALU)
  alu.io.uop := id_ex.io.out.uop
  alu.io.in1 := id_ex.io.out.rs1_data
  alu.io.in2 := id_ex.io.out.rs2_data

  alu_jmp_packet.valid := MuxLookup(
    alu.io.uop.jmp_op,
    false.B,
    Array(
      s"b$JMP_BR".U   -> alu.io.cmp_out,
      s"b$JMP_JAL".U  -> true.B,
      s"b$JMP_JALR".U -> true.B
    )
  )
  alu_jmp_packet.target := Mux(
    alu.io.uop.jmp_op === s"b$JMP_BR".U,
    id_ex.io.out.uop.pc + SignExt32_64(id_ex.io.out.uop.imm),
    alu.io.adder_out
  )

  val alu_br_out = Wire(UInt(xLen.W))
  alu_br_out := Mux(
    id_ex.io.out.uop.jmp_op === s"b$JMP_JAL".U || id_ex.io.out.uop.jmp_op === s"b$JMP_JALR".U,
    id_ex.io.out.uop.npc,
    alu.io.out
  )

  val is_mem   = id_ex.io.out.uop.fu === s"b$FU_LSU".U
  val is_mdu   = id_ex.io.out.uop.fu === s"b$FU_MDU".U
  val is_csr   = id_ex.io.out.uop.fu === s"b$FU_CSR".U
  val is_store = isStore(id_ex.io.out.uop.lsu_op)
  val is_amo   = isAmo(id_ex.io.out.uop.lsu_op)

  val lsu = Module(new LSU)
  val dmem_proxy = Module(new CachePortProxy()(p.alterPartial({
    case IsITLB => false
    case IsDTLB => true
  })))
  lsu.io.uop      := id_ex.io.out.uop
  lsu.io.is_mem   := is_mem
  lsu.io.is_store := is_store
  lsu.io.is_amo   := is_amo
  lsu.io.addr     := alu_br_out
  lsu.io.wdata    := id_ex.io.out.rs2_data_from_rf

  val mdu = Module(new MDU)
  mdu.io.uop    := id_ex.io.out.uop
  mdu.io.is_mdu := is_mdu
  mdu.io.in1    := id_ex.io.out.rs1_data
  mdu.io.in2    := id_ex.io.out.rs2_data_from_rf

  val csr = Module(new CSR)
  csr.io.uop          := id_ex.io.out.uop
  csr.io.rw.addr      := id_ex.io.out.uop.instr(31, 20)
  csr.io.rw.cmd       := id_ex.io.out.uop.csr_op
  csr.io.rw.wdata     := id_ex.io.out.rs1_data
  sys_jmp_packet      := csr.io.jmp_packet
  prv                 := csr.io.prv
  sv39_en             := csr.io.sv39_en
  satp_ppn            := csr.io.satp_ppn
  sfence_vma          := csr.io.sfence_vma
  csr.io.lsu_addr     := lsu.io.addr
  csr.io.lsu_exc_code := lsu.io.exc_code
  csr.io.mtip         := io.interrupt.mtip

  dmem_proxy.io.in         <> lsu.io.dmem
  dmem_proxy.io.out        <> io.dmem
  dmem_proxy.io.ptw        <> io.dptw
  dmem_proxy.io.prv_mpp    := Mux(csr.io.mprv, csr.io.mpp, prv)
  dmem_proxy.io.sv39_en    := sv39_en
  dmem_proxy.io.satp_ppn   := satp_ppn
  dmem_proxy.io.sfence_vma := sfence_vma

  io.fence_i := csr.io.fence_i

  val ex_wb = Module(new PipelineReg(new XWPacket))
  ex_wb.io.in.uop := id_ex.io.out.uop
  ex_wb.io.in.uop.valid := Mux(
    is_mem,
    lsu.io.valid && (lsu.io.exc_code === 0.U),
    Mux(is_mdu, mdu.io.valid, id_ex.io.out.uop.valid)
  ) && !csr.io.is_int
  ex_wb.io.in.rd_data := MuxLookup(
    id_ex.io.out.uop.fu,
    alu_br_out,
    Array(
      s"b$FU_LSU".U -> lsu.io.rdata,
      s"b$FU_MDU".U -> mdu.io.out,
      s"b$FU_CSR".U -> csr.io.rw.rdata
    )
  )
  ex_wb.io.en    := true.B
  ex_wb.io.flush := false.B

  /* ----- Stage 5 - Write Back (WB) --------------- */

  val commit_uop = ex_wb.io.out.uop
  rf.io.rd_wen   := commit_uop.valid && commit_uop.rd_wen
  rf.io.rd_index := commit_uop.rd_index
  rf.io.rd_data  := ex_wb.io.out.rd_data

  /* ----- Forwarding Unit ------------------------- */

  val need_rs1 = decode.io.out.rs1_src === s"b$RS_RF".U
  val need_rs2 = decode.io.out.rs2_src === s"b$RS_RF".U
  val need_rs2_from_rf =
    isStore(decode.io.out.lsu_op) || isAmo(decode.io.out.lsu_op) || decode.io.out.fu === s"b$FU_MDU".U

  when(
    need_rs1 && id_ex.io.out.uop.rd_wen
      && decode.io.out.rs1_index === id_ex.io.out.uop.rd_index
      && decode.io.out.rs1_index =/= 0.U
  ) {
    id_rs1_data := ex_wb.io.in.rd_data
  }.otherwise {
    id_rs1_data := MuxLookup(
      decode.io.out.rs1_src,
      0.U,
      Array(
        s"b$RS_PC".U  -> ZeroExt32_64(instr_buffer.io.deq.bits.pc),
        s"b$RS_RF".U  -> rf.io.rs1_data,
        s"b$RS_IMM".U -> SignExt32_64(decode.io.out.imm)
      )
    )
  }

  when(
    need_rs2 && id_ex.io.out.uop.rd_wen
      && decode.io.out.rs2_index === id_ex.io.out.uop.rd_index
      && decode.io.out.rs2_index =/= 0.U
  ) {
    id_rs2_data := ex_wb.io.in.rd_data
  }.otherwise {
    id_rs2_data := MuxLookup(
      decode.io.out.rs2_src,
      0.U,
      Array(
        s"b$RS_PC".U  -> ZeroExt32_64(instr_buffer.io.deq.bits.pc),
        s"b$RS_RF".U  -> rf.io.rs2_data,
        s"b$RS_IMM".U -> SignExt32_64(decode.io.out.imm)
      )
    )
  }

  when(
    need_rs2_from_rf && id_ex.io.out.uop.rd_wen
      && decode.io.out.rs2_index === id_ex.io.out.uop.rd_index
      && decode.io.out.rs2_index =/= 0.U
  ) {
    id_rs2_data_from_rf := ex_wb.io.in.rd_data
  }.otherwise {
    id_rs2_data_from_rf := rf.io.rs2_data
  }

  /* ----- Pipeline Control Signals -------------- */

  stall_b := lsu.io.ready && mdu.io.ready
  flush   := alu_jmp_packet.valid || sys_jmp_packet.valid

  /* ----- Performance Counters ------------------ */

  val mcycle   = csr.io.cycle // machine cycle counter
  val minstret = csr.io.instret // machine instructions-retired counter
  csr.io.commit := commit_uop.valid.asUInt

  /* ----- Processor Difftest -------------------- */

  if (enableDifftest) {
    val is_mmio = WireDefault(false.B)
    when(
      commit_uop.valid && RegNext(is_mem) && !RegNext(lsu.io.addr(paddrLen - 1).asBool) && RegNext(prv === PRV.M.U)
    ) {
      is_mmio := true.B
    }

    val is_cycle = WireDefault(false.B)
    when(
      commit_uop.valid && commit_uop.fu === s"b$FU_CSR".U &&
        (commit_uop.instr(31, 20) === "hb00".U || commit_uop.instr(31, 20) === "hc00".U)
    ) {
      is_cycle := true.B
    }

    val diff_ic = Module(new DifftestInstrCommit)
    diff_ic.io.clock   := clock
    diff_ic.io.coreid  := hartID.U
    diff_ic.io.index   := 0.U
    diff_ic.io.pc      := commit_uop.pc
    diff_ic.io.instr   := commit_uop.instr
    diff_ic.io.valid   := commit_uop.valid
    diff_ic.io.special := false.B
    diff_ic.io.skip    := (commit_uop.instr === PUTCH()) || is_mmio || is_cycle
    diff_ic.io.isRVC   := false.B
    diff_ic.io.rfwen   := commit_uop.rd_wen
    diff_ic.io.fpwen   := false.B
    diff_ic.io.wpdest  := commit_uop.rd_index
    diff_ic.io.wdest   := commit_uop.rd_index
    diff_ic.io.robIdx  := 0.U
    diff_ic.io.lqIdx   := 0.U
    diff_ic.io.sqIdx   := 0.U
    diff_ic.io.isLoad  := (commit_uop.lsu_op === s"b$LSU_LD".U) || (commit_uop.lsu_op === s"b$LSU_LDU".U)
    diff_ic.io.isStore := (commit_uop.lsu_op === s"b$LSU_ST".U)
    if (debugInstrCommit) {
      when(commit_uop.valid) {
        printf(
          "%d [COMMIT] pc=%x instr=%x wen=%x wdest=%d prv=%d\n",
          DebugTimer(),
          commit_uop.pc,
          commit_uop.instr,
          commit_uop.rd_wen,
          commit_uop.rd_index,
          prv
        )
      }
    }

    val diff_wb = Module(new DifftestIntWriteback)
    diff_wb.io.clock  := clock
    diff_wb.io.coreid := hartID.U
    diff_wb.io.valid  := commit_uop.valid && commit_uop.rd_wen
    diff_wb.io.dest   := commit_uop.rd_index
    diff_wb.io.data   := ex_wb.io.out.rd_data

    val trap  = (commit_uop.instr === HALT()) && commit_uop.valid
    val rf_a0 = WireInit(0.U(xLen.W))
    BoringUtils.addSink(rf_a0, "rf_a0")

    when(commit_uop.instr === PUTCH() && commit_uop.valid) {
      printf("%c", rf_a0(7, 0))
    }

    val diff_te = Module(new DifftestTrapEvent)
    diff_te.io.clock    := clock
    diff_te.io.coreid   := hartID.U
    diff_te.io.valid    := trap
    diff_te.io.cycleCnt := mcycle
    diff_te.io.instrCnt := minstret
    diff_te.io.hasWFI   := false.B
    diff_te.io.code     := rf_a0(2, 0)
    diff_te.io.pc       := commit_uop.pc
  }
}
