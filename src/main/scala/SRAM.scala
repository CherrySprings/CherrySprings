import chisel3._
import chisel3.util._

class SRAM(depth: Int, dw: Int) extends Module {
  val io = IO(new Bundle {
    val en    = Input(Bool())
    val addr  = Input(UInt(log2Up(depth).W))
    val wdata = Input(UInt(dw.W))
    val wen   = Input(Bool())
    val rdata = Output(UInt(dw.W))
  })

  val array = SyncReadMem(depth, UInt(dw.W))

  io.rdata := Mux(RegNext(io.en), array.read(io.addr), 0.U)
  when(io.en && io.wen) {
    array.write(io.addr, io.wdata)
  }
}
