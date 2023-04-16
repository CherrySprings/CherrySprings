import chisel3._
import difftest._
import org.chipsalliance.cde.config._

class RegFile(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val rs1_index = Input(UInt(5.W))
    val rs2_index = Input(UInt(5.W))
    val rs1_data  = Output(UInt(xLen.W))
    val rs2_data  = Output(UInt(xLen.W))
    val rd_index  = Input(UInt(5.W))
    val rd_data   = Input(UInt(xLen.W))
    val rd_wen    = Input(Bool())
    val rf_a0     = Output(UInt(8.W)) // only for difftest
    val hartid    = Input(UInt(8.W)) // only for difftest
  })

  def access(x: UInt) = rf(~x)
  def access(i: Int)  = rf(31 - i)

  val rf = RegInit(VecInit(Seq.fill(31)(0.U(xLen.W))))

  when(io.rd_wen && (io.rd_index =/= 0.U)) {
    access(io.rd_index) := io.rd_data;
  }

  io.rs1_data := Mux(io.rs1_index =/= 0.U, access(io.rs1_index), 0.U)
  io.rs2_data := Mux(io.rs2_index =/= 0.U, access(io.rs2_index), 0.U)

  // bypass
  when(io.rd_wen && (io.rd_index =/= 0.U)) {
    when(io.rd_index === io.rs1_index) {
      io.rs1_data := io.rd_data
    }
    when(io.rd_index === io.rs2_index) {
      io.rs2_data := io.rd_data
    }
  }

  io.rf_a0 := 0.U
  if (enableDifftest) {
    val dt_ar = Module(new DifftestArchIntRegState)
    dt_ar.io.clock  := clock
    dt_ar.io.coreid := io.hartid
    dt_ar.io.gpr(0) := 0.U
    for (i <- 1 until 32) {
      dt_ar.io.gpr(i) := Mux(io.rd_wen && (io.rd_index === i.U) && (io.rd_index =/= 0.U), io.rd_data, access(i))
    }
    io.rf_a0 := access(10)(7, 0)
  }
}
