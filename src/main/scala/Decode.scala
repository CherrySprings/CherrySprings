import chisel3._
import chisel3.util.experimental.decode._
import chipsalliance.rocketchip.config._
import Constant._

class Decode(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val in  = Input(new FDPacket)
    val out = Output(new MicroOp)
  })

  val instr = io.in.instr
  val uop   = WireInit(0.U.asTypeOf(new MicroOp))

  uop.pc        := io.in.pc
  uop.npc       := io.in.pc + 4.U
  uop.instr     := instr
  uop.rs1_index := instr(19, 15)
  uop.rs2_index := instr(24, 20)
  uop.rd_index  := instr(11, 7)

  val decode_result = decoder(minimizer = EspressoMinimizer, input = instr, truthTable = DecodeTable.decode_table)

  uop.from_decoder(decode_result)

  when(io.in.valid) {
    when(io.in.page_fault) {
      uop.exc   := s"b$EXC_IPF".U
      uop.valid := false.B
    }.elsewhen(io.in.access_fault) {
      uop.exc   := s"b$EXC_IAF".U
      uop.valid := false.B
    }
  }

  io.out := Mux(io.in.valid, uop, 0.U.asTypeOf(new MicroOp))
}
