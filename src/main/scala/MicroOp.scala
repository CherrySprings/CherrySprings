import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

class MicroOp(implicit p: Parameters) extends CherrySpringsBundle {
  val valid = Bool()
  val exc   = UInt(EXC_X.length.W)

  val pc    = UInt(xLen.W)
  val npc   = UInt(xLen.W)
  val instr = UInt(32.W)

  val fu      = UInt(FU_X.length.W)
  val alu_op  = UInt(ALU_X.length.W)
  val jmp_op  = UInt(JMP_X.length.W)
  val mdu_op  = UInt(MDU_X.length.W)
  val lsu_op  = UInt(LSU_X.length.W)
  val mem_len = UInt(MEM_X.length.W)
  val csr_op  = UInt(CSR_X.length.W)
  val sys_op  = UInt(SYS_X.length.W)

  val rs1_src = UInt(RS_X.length.W)
  val rs2_src = UInt(RS_X.length.W)

  val rs1_index = UInt(5.W)
  val rs2_index = UInt(5.W)
  val rd_index  = UInt(5.W)
  val rd_wen    = Bool()
  val imm       = UInt(32.W)
  val dw        = Bool()

  def from_decoder(in: UInt): Unit = {
    val imm_type = WireInit(0.U(IMM_X.length.W))
    val entries = Seq(
      valid,
      exc,
      fu,
      alu_op,
      jmp_op,
      mdu_op,
      lsu_op,
      mem_len,
      csr_op,
      sys_op,
      rs1_src,
      rs2_src,
      rd_wen,
      imm_type,
      dw
    )
    var i = 0
    for (entry <- entries.reverse) {
      val entry_width = entry.getWidth
      if (entry_width == 1) {
        entry := in(i)
      } else {
        entry := in(i + entry_width - 1, i)
      }
      i += entry_width
    }

    val imm_i   = Cat(Fill(21, instr(31)), instr(30, 20))
    val imm_s   = Cat(Fill(21, instr(31)), instr(30, 25), instr(11, 7))
    val imm_b   = Cat(Fill(20, instr(31)), instr(7), instr(30, 25), instr(11, 8), 0.U)
    val imm_u   = Cat(instr(31, 12), Fill(12, 0.U))
    val imm_j   = Cat(Fill(12, instr(31)), instr(19, 12), instr(20), instr(30, 21), 0.U)
    val imm_csr = Cat(Fill(27, 0.U), instr(19, 15))

    imm := MuxLookup(
      imm_type,
      0.U(32.W),
      Array(
        s"b$IMM_I".U -> imm_i,
        s"b$IMM_S".U -> imm_s,
        s"b$IMM_B".U -> imm_b,
        s"b$IMM_U".U -> imm_u,
        s"b$IMM_J".U -> imm_j,
        s"b$IMM_Z".U -> imm_csr
      )
    )
  }
}
