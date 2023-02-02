import chisel3.util._
import chisel3.util.experimental.decode._
import Constant._
import freechips.rocketchip.rocket.Instructions._

object HALT {
  def apply() = BitPat("b00000000000000000000000001101011")
}

object PUTCH {
  def apply() = BitPat("b00000000000000000000000001111011")
}

object DecodeTable {
  val decode_default: String = Seq(
    //                v  exc     fu      alu_op    jmp_op    mdu_op      lsu_op       mem_len    csr_op  sys_op      rs1/2_src   rd_wen imm_type dw64
                      N, EXC_II, FU_ALU, ALU_X,    JMP_X,    MDU_X,      LSU_X,       MEM_X,     CSR_X,  SYS_X,      RS_X,     RS_X,   N, IMM_X, Y
  ).reduce(_ + _)

  val decode_table: TruthTable = TruthTable(Map(
    // RV64I
    LUI        -> Seq(Y, EXC_N,  FU_ALU, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_ZERO, RS_IMM,  Y, IMM_U, Y),
    AUIPC      -> Seq(Y, EXC_N,  FU_ALU, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_PC,   RS_IMM,  Y, IMM_U, Y),
    JAL        -> Seq(Y, EXC_N,  FU_JMP, ALU_ADD,  JMP_JAL,  MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_PC,   RS_IMM,  Y, IMM_J, Y),
    JALR       -> Seq(Y, EXC_N,  FU_JMP, ALU_ADD,  JMP_JALR, MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    BEQ        -> Seq(Y, EXC_N,  FU_JMP, ALU_SEQ,  JMP_BR,   MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   N, IMM_B, Y),
    BNE        -> Seq(Y, EXC_N,  FU_JMP, ALU_SNE,  JMP_BR,   MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   N, IMM_B, Y),
    BLT        -> Seq(Y, EXC_N,  FU_JMP, ALU_SLT,  JMP_BR,   MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   N, IMM_B, Y),
    BGE        -> Seq(Y, EXC_N,  FU_JMP, ALU_SGE,  JMP_BR,   MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   N, IMM_B, Y),
    BLTU       -> Seq(Y, EXC_N,  FU_JMP, ALU_SLTU, JMP_BR,   MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   N, IMM_B, Y),
    BGEU       -> Seq(Y, EXC_N,  FU_JMP, ALU_SGEU, JMP_BR,   MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   N, IMM_B, Y),
    LB         -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_LD,      MEM_BYTE,  CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    LH         -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_LD,      MEM_HALF,  CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    LW         -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_LD,      MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    LD         -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_LD,      MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    LBU        -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_LDU,     MEM_BYTE,  CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    LHU        -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_LDU,     MEM_HALF,  CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    LWU        -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_LDU,     MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    SB         -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_ST,      MEM_BYTE,  CSR_N,  SYS_N,      RS_RF,   RS_IMM,  N, IMM_S, Y),
    SH         -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_ST,      MEM_HALF,  CSR_N,  SYS_N,      RS_RF,   RS_IMM,  N, IMM_S, Y),
    SW         -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_ST,      MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_IMM,  N, IMM_S, Y),
    SD         -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_ST,      MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_IMM,  N, IMM_S, Y),
    ADDI       -> Seq(Y, EXC_N,  FU_ALU, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    SLTI       -> Seq(Y, EXC_N,  FU_ALU, ALU_SLT,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    SLTIU      -> Seq(Y, EXC_N,  FU_ALU, ALU_SLTU, JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    XORI       -> Seq(Y, EXC_N,  FU_ALU, ALU_XOR,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    ORI        -> Seq(Y, EXC_N,  FU_ALU, ALU_OR,   JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    ANDI       -> Seq(Y, EXC_N,  FU_ALU, ALU_AND,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    SLLI       -> Seq(Y, EXC_N,  FU_ALU, ALU_SLL,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    SRLI       -> Seq(Y, EXC_N,  FU_ALU, ALU_SRL,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    SRAI       -> Seq(Y, EXC_N,  FU_ALU, ALU_SRA,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, Y),
    ADD        -> Seq(Y, EXC_N,  FU_ALU, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    SUB        -> Seq(Y, EXC_N,  FU_ALU, ALU_SUB,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    SLL        -> Seq(Y, EXC_N,  FU_ALU, ALU_SLL,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    SLT        -> Seq(Y, EXC_N,  FU_ALU, ALU_SLT,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    SLTU       -> Seq(Y, EXC_N,  FU_ALU, ALU_SLTU, JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    XOR        -> Seq(Y, EXC_N,  FU_ALU, ALU_XOR,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    SRL        -> Seq(Y, EXC_N,  FU_ALU, ALU_SRL,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    SRA        -> Seq(Y, EXC_N,  FU_ALU, ALU_SRA,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    OR         -> Seq(Y, EXC_N,  FU_ALU, ALU_OR,   JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    AND        -> Seq(Y, EXC_N,  FU_ALU, ALU_AND,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    ADDIW      -> Seq(Y, EXC_N,  FU_ALU, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, N),
    SLLIW      -> Seq(Y, EXC_N,  FU_ALU, ALU_SLL,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, N),
    SRLIW      -> Seq(Y, EXC_N,  FU_ALU, ALU_SRL,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, N),
    SRAIW      -> Seq(Y, EXC_N,  FU_ALU, ALU_SRA,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_IMM,  Y, IMM_I, N),
    ADDW       -> Seq(Y, EXC_N,  FU_ALU, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, N),
    SUBW       -> Seq(Y, EXC_N,  FU_ALU, ALU_SUB,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, N),
    SLLW       -> Seq(Y, EXC_N,  FU_ALU, ALU_SLL,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, N),
    SRLW       -> Seq(Y, EXC_N,  FU_ALU, ALU_SRL,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, N),
    SRAW       -> Seq(Y, EXC_N,  FU_ALU, ALU_SRA,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, N),
    // SYS
    ECALL      -> Seq(Y, EXC_EC, FU_SYS, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_ZERO, RS_ZERO, N, IMM_X, Y),
    EBREAK     -> Seq(Y, EXC_EB, FU_SYS, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_ZERO, RS_ZERO, N, IMM_X, Y),
    FENCE      -> Seq(Y, EXC_N,  FU_SYS, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_FENCE,  RS_ZERO, RS_ZERO, N, IMM_X, Y),
    FENCE_I    -> Seq(Y, EXC_N,  FU_SYS, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_FENCEI, RS_ZERO, RS_ZERO, N, IMM_X, Y),
    SFENCE_VMA -> Seq(Y, EXC_N,  FU_SYS, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_SFV,    RS_ZERO, RS_ZERO, N, IMM_X, Y),
    MRET       -> Seq(Y, EXC_N,  FU_SYS, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_MRET,   RS_ZERO, RS_ZERO, N, IMM_X, Y),
    SRET       -> Seq(Y, EXC_N,  FU_SYS, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_SRET,   RS_ZERO, RS_ZERO, N, IMM_X, Y),
    WFI        -> Seq(Y, EXC_N,  FU_ALU, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_ZERO, RS_ZERO, N, IMM_X, Y),
    // CSR
    CSRRW      -> Seq(Y, EXC_N,  FU_CSR, ALU_X,    JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_RW, SYS_N,      RS_RF,   RS_X,    Y, IMM_X, Y),
    CSRRS      -> Seq(Y, EXC_N,  FU_CSR, ALU_X,    JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_RS, SYS_N,      RS_RF,   RS_X,    Y, IMM_X, Y),
    CSRRC      -> Seq(Y, EXC_N,  FU_CSR, ALU_X,    JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_RC, SYS_N,      RS_RF,   RS_X,    Y, IMM_X, Y),
    CSRRWI     -> Seq(Y, EXC_N,  FU_CSR, ALU_X,    JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_RW, SYS_N,      RS_IMM,  RS_X,    Y, IMM_Z, Y),
    CSRRSI     -> Seq(Y, EXC_N,  FU_CSR, ALU_X,    JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_RS, SYS_N,      RS_IMM,  RS_X,    Y, IMM_Z, Y),
    CSRRCI     -> Seq(Y, EXC_N,  FU_CSR, ALU_X,    JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_RC, SYS_N,      RS_IMM,  RS_X,    Y, IMM_Z, Y),
    // RV64M 
    MUL        -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_MUL,    LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    MULH       -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_MULH,   LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    MULHSU     -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_MULHSU, LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    MULHU      -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_MULHU,  LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    DIV        -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_DIV,    LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    DIVU       -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_DIVU,   LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    REM        -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_REM,    LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    REMU       -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_REMU,   LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, Y),
    MULW       -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_MUL,    LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, N),
    DIVW       -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_DIV,    LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, N),
    DIVUW      -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_DIVU,   LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, N),
    REMW       -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_REM,    LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, N),
    REMUW      -> Seq(Y, EXC_N,  FU_MDU, ALU_X,    JMP_N,    MDU_REMU,   LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   Y, IMM_X, N),
    // RV64A
    LR_W       -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_LR,      MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    LR_D       -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_LR,      MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    SC_W       -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_SC,      MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    SC_D       -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_SC,      MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOSWAP_W  -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOSWAP, MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOSWAP_D  -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOSWAP, MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOADD_W   -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOADD,  MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOADD_D   -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOADD,  MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOAND_W   -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOAND,  MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOAND_D   -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOAND,  MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOOR_W    -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOOR,   MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOOR_D    -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOOR,   MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOXOR_W   -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOXOR,  MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOXOR_D   -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOXOR,  MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOMAX_W   -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOMAX,  MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOMAX_D   -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOMAX,  MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOMAXU_W  -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOMAXU, MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOMAXU_D  -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOMAXU, MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOMIN_W   -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOMIN,  MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOMIN_D   -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOMIN,  MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOMINU_W  -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOMINU, MEM_WORD,  CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    AMOMINU_D  -> Seq(Y, EXC_N,  FU_LSU, ALU_ADD,  JMP_N,    MDU_X,      LSU_AMOMINU, MEM_DWORD, CSR_N,  SYS_N,      RS_RF,   RS_ZERO, Y, IMM_X, Y),
    // CUSTOM - DEBUG ONLY
    HALT()     -> Seq(Y, EXC_N,  FU_ALU, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   N, IMM_X, Y),
    PUTCH()    -> Seq(Y, EXC_N,  FU_ALU, ALU_ADD,  JMP_N,    MDU_X,      LSU_N,       MEM_X,     CSR_N,  SYS_N,      RS_RF,   RS_RF,   N, IMM_X, Y)
  ).map({ case (k, v) => k -> BitPat(s"b${v.reduce(_ + _)}") }), BitPat(s"b$decode_default"))
}
