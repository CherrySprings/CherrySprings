import chisel3._
import chisel3.util._
import chisel3.util.random._
import org.chipsalliance.cde.config._

class BTBEntry(implicit p: Parameters) extends CherrySpringsBundle {
  val tag    = UInt((vaddrLen - 2).W)
  val target = UInt((vaddrLen - 2).W)
  val valid  = Bool()
}

class BPU(implicit p: Parameters) extends CherrySpringsModule {
  val pht_default_state = 1.U(2.W)

  val io = IO(new Bundle {
    // from IF stage
    val pc = Input(UInt(xLen.W))

    // from EX stage
    val jmp_packet = Input(new JmpPacket)

    // BPU output, generated in the same clock cycle
    val out = Output(UInt(xLen.W))
  })

  val ghr = RegInit(0.U(ghrLen.W))
  val pht = RegInit(VecInit(Seq.fill(phtSize)(pht_default_state)))
  val btb = RegInit(VecInit(Seq.fill(btbSize)(0.U.asTypeOf(new BTBEntry))))

  // read GHR & PHT
  val pht_ridx  = io.pc(1 + ghrLen, 2) ^ ghr
  val pht_rdata = pht(pht_ridx)
  val pht_taken = (pht_rdata >= 2.U)

  // read BTB
  val btb_rhit  = WireDefault(false.B)
  val btb_rdata = WireDefault(0.U((vaddrLen - 2).W))
  for (i <- 0 until btbSize) {
    when(btb(i).valid && (btb(i).tag === io.pc(vaddrLen - 1, 2))) {
      btb_rhit  := true.B
      btb_rdata := btb(i).target
    }
  }

  // output
  io.out := io.pc + 4.U // default output
  when(pht_taken && btb_rhit) {
    io.out := SignExt39_64(Cat(btb_rdata, 0.U(2.W)))
  }

  // write GHR & PHT
  val pht_widx = io.jmp_packet.bp_pc(1 + ghrLen, 2) ^ ghr
  when(io.jmp_packet.bp_update) {
    ghr := Cat(ghr(ghrLen - 2, 1), io.jmp_packet.bp_taken.asUInt)
    pht(pht_widx) := MuxLookup(
      pht(pht_widx),
      pht_default_state,
      Seq(
        0.U -> Mux(io.jmp_packet.bp_taken, 1.U, 0.U), // strongly not taken
        1.U -> Mux(io.jmp_packet.bp_taken, 2.U, 0.U), // weakly not taken
        2.U -> Mux(io.jmp_packet.bp_taken, 3.U, 1.U), // weakly taken
        3.U -> Mux(io.jmp_packet.bp_taken, 3.U, 2.U) // strongly taken
      )
    )
  }

  // write BTB
  val btb_whit     = WireDefault(false.B)
  val btb_whit_way = WireDefault(0.U(log2Up(btbSize).W))
  for (i <- 0 until btbSize) {
    when(btb(i).valid && (btb(i).tag === io.jmp_packet.bp_pc(vaddrLen - 1, 2))) {
      btb_whit     := true.B
      btb_whit_way := i.U
    }
  }
  val btb_replace_idx = Wire(UInt(log2Up(btbSize).W))
  btb_replace_idx := Mux(btb_whit, btb_whit_way, LFSR(log2Up(btbSize)))
  when(io.jmp_packet.bp_update && io.jmp_packet.bp_taken) {
    btb(btb_replace_idx).valid  := true.B
    btb(btb_replace_idx).tag    := io.jmp_packet.bp_pc(vaddrLen - 1, 2)
    btb(btb_replace_idx).target := io.jmp_packet.target(vaddrLen - 1, 2)
  }

}
