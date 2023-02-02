import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

// Rocket ALU
class ALU(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val uop       = Input(new MicroOp)
    val in1       = Input(UInt(xLen.W))
    val in2       = Input(UInt(xLen.W))
    val out       = Output(UInt(xLen.W))
    val adder_out = Output(UInt(xLen.W))
    val cmp_out   = Output(Bool())
  })

  val fn = io.uop.alu_op

  val is_sub       = fn(3)
  val is_cmp       = fn >= s"b$ALU_SLT".U
  val cmp_unsigned = fn(1)
  val cmp_inverted = fn(0)
  val cmp_eq       = !fn(3)

  // ADD, SUB
  val in2_inv     = Mux(is_sub, (~io.in2).asUInt, io.in2)
  val in1_xor_in2 = io.in1 ^ in2_inv
  io.adder_out := io.in1 + in2_inv + is_sub

  // SLT, SLTU
  val slt = Mux(
    io.in1(xLen - 1) === io.in2(xLen - 1),
    io.adder_out(xLen - 1),
    Mux(cmp_unsigned, io.in2(xLen - 1), io.in1(xLen - 1))
  )
  io.cmp_out := cmp_inverted ^ Mux(cmp_eq, in1_xor_in2 === 0.U, slt)

  // SLL, SRL, SRA
  val (shamt, shin_r) =
    if (xLen == 32) (io.in2(4, 0), io.in1)
    else {
      require(xLen == 64)
      val shin_hi_32 = Fill(32, is_sub && io.in1(31))
      val shin_hi    = Mux(io.uop.dw, io.in1(63, 32), shin_hi_32)
      val shamt      = Cat(io.in2(5) & io.uop.dw.asUInt, io.in2(4, 0))
      (shamt, Cat(shin_hi, io.in1(31, 0)))
    }
  val shin    = Mux(fn === s"b$ALU_SRL".U || fn === s"b$ALU_SRA".U, shin_r, Reverse(shin_r))
  val shout_r = (Cat(is_sub & shin(xLen - 1), shin).asSInt >> shamt)(xLen - 1, 0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(fn === s"b$ALU_SRL".U || fn === s"b$ALU_SRA".U, shout_r, 0.U) |
    Mux(fn === s"b$ALU_SLL".U, shout_l, 0.U)

  // AND, OR, XOR
  val logic = Mux(fn === s"b$ALU_XOR".U || fn === s"b$ALU_OR".U, in1_xor_in2, 0.U) |
    Mux(fn === s"b$ALU_OR".U || fn === s"b$ALU_AND".U, io.in1 & io.in2, 0.U)
  val shift_logic = (is_cmp && slt) | logic | shout
  val out         = Mux(fn === s"b$ALU_ADD".U || fn === s"b$ALU_SUB".U, io.adder_out, shift_logic)

  io.out := out
  if (xLen > 32) {
    require(xLen == 64)
    when(!io.uop.dw) { io.out := Cat(Fill(32, out(31)), out(31, 0)) }
  }
}
