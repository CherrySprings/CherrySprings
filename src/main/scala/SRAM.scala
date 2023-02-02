import chisel3._
import chisel3.util._

class SRAM(size: Int) extends Module {
  val io = IO(new Bundle {
    val addr  = Input(UInt(log2Up(size).W))
    val wdata = Input(UInt(64.W))
    val widx  = Input(UInt(2.W))
    val wen   = Input(Bool())
    val rdata = Output(UInt(256.W))
  })

  val array = for (i <- 0 until 4) yield {
    val sram = SyncReadMem(size, UInt(64.W))
    sram
  }

  io.rdata := Cat(array.map(_.read(io.addr)).reverse)

  when(io.wen) {
    for (i <- 0 until 4) {
      when(io.widx === i.U) {
        array(i).write(io.addr, io.wdata)
      }
    }
  }
}
