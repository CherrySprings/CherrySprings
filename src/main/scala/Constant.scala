import chisel3._

trait Constant {
  val Y = "1"
  val N = "0"

  /*
   * Note:
   * - Some exceptions are generated in LSU (4, 5, 6, 7, 13, 15)
   * - Breakpoint (3) not implemented yet
   */
  val EXC_X   = "???"
  val EXC_N   = "000"
  val EXC_IAM = "001" // Instruction address misaligned (0)
  val EXC_IAF = "010" // Instruction access fault (1)
  val EXC_IPF = "011" // Instruction page fault (12)
  val EXC_II  = "100" // Illegal instruction (2)
  val EXC_EC  = "101" // Environment call from U/S/M-mode (8, 9, 11)
  val EXC_EB  = "111" // Breakpoint (3)

  val FU_X   = "???"
  val FU_ALU = "000"
  val FU_JMP = "001"
  val FU_MDU = "010"
  val FU_LSU = "011"
  val FU_CSR = "100"
  val FU_SYS = "101"

  val ALU_X    = "????"
  val ALU_ADD  = "0000"
  val ALU_SLL  = "0001"
  val ALU_SEQ  = "0010"
  val ALU_SNE  = "0011"
  val ALU_XOR  = "0100"
  val ALU_SRL  = "0101"
  val ALU_OR   = "0110"
  val ALU_AND  = "0111"
  val ALU_SUB  = "1010"
  val ALU_SRA  = "1011"
  val ALU_SLT  = "1100"
  val ALU_SGE  = "1101"
  val ALU_SLTU = "1110"
  val ALU_SGEU = "1111"

  val JMP_X    = "??"
  val JMP_N    = "00"
  val JMP_BR   = "01"
  val JMP_JAL  = "10"
  val JMP_JALR = "11"

  def isJalJalr(x: UInt): Bool = x(1).asBool
  def isBr(x:      UInt): Bool = !x(1).asBool && x(0).asBool

  val MDU_X      = "????"
  val MDU_MUL    = "0000"
  val MDU_MULH   = "0001"
  val MDU_MULHSU = "0010"
  val MDU_MULHU  = "0011"
  val MDU_DIV    = "0100"
  val MDU_DIVU   = "0101"
  val MDU_REM    = "0110"
  val MDU_REMU   = "0111"

  val LSU_X       = "?????"
  val LSU_N       = "00000"
  val LSU_ST      = "00001"
  val LSU_LD      = "00010"
  val LSU_LDU     = "00110"
  val LSU_LR      = "01010"
  val LSU_SC      = "01001"
  val LSU_AMOMIN  = "10000"
  val LSU_AMOMAX  = "10001"
  val LSU_AMOMINU = "10010"
  val LSU_AMOMAXU = "10011"
  val LSU_AMOADD  = "10100"
  val LSU_AMOXOR  = "11000"
  val LSU_AMOOR   = "11001"
  val LSU_AMOAND  = "11010"
  val LSU_AMOSWAP = "11011"

  def isAmo(x:   UInt): Bool = x(4).asBool
  def isStore(x: UInt): Bool = !x(4).asBool && x(0).asBool
  def isLoad(x:  UInt): Bool = !x(4).asBool && x(1).asBool
  def isLdu(x:   UInt): Bool = isLoad(x) && x(2).asBool
  def isLrSc(x:  UInt): Bool = !x(4).asBool && x(3).asBool

  val MEM_X     = "??"
  val MEM_BYTE  = "00"
  val MEM_HALF  = "01"
  val MEM_WORD  = "10"
  val MEM_DWORD = "11"

  val CSR_X  = "??"
  val CSR_N  = "00"
  val CSR_RW = "01"
  val CSR_RS = "10"
  val CSR_RC = "11"

  val SYS_X      = "???"
  val SYS_N      = "000"
  val SYS_MRET   = "001"
  val SYS_SRET   = "010"
  val SYS_FENCE  = "100"
  val SYS_FENCEI = "101"
  val SYS_SFV    = "110" // sfence.vma

  val RS_X    = "??"
  val RS_ZERO = "00"
  val RS_PC   = "01"
  val RS_RF   = "10"
  val RS_IMM  = "11"

  val IMM_X = "???"
  val IMM_I = "000"
  val IMM_S = "001"
  val IMM_B = "010"
  val IMM_U = "011"
  val IMM_J = "100"
  val IMM_Z = "101"
}

object Constant extends Constant {}
