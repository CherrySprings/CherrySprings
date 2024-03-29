import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.rocket._
import org.chipsalliance.cde.config._
import Constant._

object PRV {
  val U = 0
  val S = 1
  val M = 3
}

// Based on RISC-V ISA Volume II: Privileged Architecture, Version 20211203
class CSR(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)

  val io = IO(new Bundle {
    val uop = Input(new MicroOp)
    val rw = new Bundle {
      val addr  = Input(UInt(12.W))
      val cmd   = Input(UInt(CSR_X.length.W))
      val wdata = Input(UInt(xLen.W))
      val rdata = Output(UInt(xLen.W))
      val valid = Output(Bool())
    }
    val prv          = Output(UInt(2.W))
    val mprv         = Output(Bool())
    val mpp          = Output(UInt(2.W))
    val sv39_en      = Output(Bool())
    val satp_asid    = Output(UInt(16.W))
    val satp_ppn     = Output(UInt(44.W))
    val sfence_vma   = Output(Bool())
    val fence_i      = Output(Bool())
    val jmp_packet   = Output(new JmpPacket)
    val lsu_addr     = Input(UInt(xLen.W))
    val lsu_exc_code = Input(UInt(4.W))
    val interrupt    = Input(new ExternalInterrupt)
    val is_int       = Output(Bool())

    val cycle   = Output(UInt(xLen.W))
    val instret = Output(UInt(xLen.W))
    val commit  = Input(UInt(1.W))

    val hartid = Output(UInt(8.W)) // only for difftest
  })

  // privilege mode
  val prv       = RegInit(PRV.M.U)
  val prv_is_m  = (prv === PRV.M.U)
  val prv_is_s  = (prv === PRV.S.U)
  val prv_is_ms = prv_is_m || prv_is_s
  val prv_is_u  = (prv === PRV.U.U)

  val csr_legal = WireDefault(false.B)
  val rdata     = WireDefault(0.U(xLen.W))
  val wdata     = Wire(UInt(xLen.W))
  val wen       = (io.rw.cmd =/= s"b$CSR_N".U) && (io.uop.exc === s"b$EXC_N".U) && csr_legal
  wdata := MuxLookup(
    io.rw.cmd,
    0.U,
    Seq(
      s"b$CSR_RW".U -> io.rw.wdata,
      s"b$CSR_RS".U -> (rdata | io.rw.wdata),
      s"b$CSR_RC".U -> (rdata & ~io.rw.wdata)
    )
  )

  // constant masks
  val s_exc_mask = "h0f7ff".U // exclude env call from M-mode
  val s_int_mask = "h222".U
  val m_int_mask = "haaa".U

  /*
   * Number:      0x300 / 0x100
   * Privilege:   MRW / SRW
   * Name:        mstatus / sstatus
   * Description: Machine / Supervisor status register
   */
  val sstatus     = WireDefault(0.U(xLen.W))
  val status_sie  = RegInit(0.U(1.W))
  val status_spie = RegInit(0.U(1.W))
  val status_ube  = 0.U(1.W)
  val status_spp  = RegInit(0.U(1.W))
  val status_vs   = 0.U(2.W)
  val status_fs   = RegInit(0.U(2.W))
  val status_xs   = 0.U(2.W) // RegInit(0.U(2.W))
  val status_sum  = RegInit(0.U(1.W))
  val status_mxr  = RegInit(0.U(1.W))
  val status_uxl  = log2Up(xLen / 16).U(2.W)
  val status_sd   = status_fs.orR | status_xs.orR
  sstatus := Cat(
    status_sd,
    0.U(29.W),
    status_uxl,
    0.U(12.W),
    status_mxr,
    status_sum,
    0.U(1.W),
    status_xs,
    status_fs,
    0.U(2.W),
    status_vs,
    status_spp,
    0.U(1.W),
    status_ube,
    status_spie,
    0.U(3.W),
    status_sie,
    0.U(1.W)
  )
  val mstatus      = WireDefault(0.U(xLen.W))
  val mstatus_mie  = RegInit(0.U(1.W))
  val mstatus_mpie = RegInit(0.U(1.W))
  val mstatus_mpp  = RegInit(0.U(2.W))
  val mstatus_mprv = RegInit(0.U(1.W))
  val mstatus_tvm  = RegInit(0.U(1.W))
  val mstatus_tw   = RegInit(0.U(1.W))
  val mstatus_tsr  = RegInit(0.U(1.W))
  val mstatus_sxl  = log2Up(xLen / 16).U(2.W)
  val mstatus_sbe  = 0.U(1.W)
  val mstatus_mbe  = 0.U(1.W)
  mstatus := Cat(
    status_sd,
    0.U(25.W),
    mstatus_mbe,
    mstatus_sbe,
    mstatus_sxl,
    status_uxl,
    0.U(9.W),
    mstatus_tsr,
    mstatus_tw,
    mstatus_tvm,
    status_mxr,
    status_sum,
    mstatus_mprv,
    status_xs,
    status_fs,
    mstatus_mpp,
    status_vs,
    status_spp,
    mstatus_mpie,
    status_ube,
    status_spie,
    0.U(1.W),
    mstatus_mie,
    0.U(1.W),
    status_sie,
    0.U(1.W)
  )
  when(io.rw.addr === CSRs.sstatus.U) {
    rdata := sstatus
    when(wen) {
      status_sie  := wdata(1)
      status_spie := wdata(5)
      status_spp  := wdata(8)
      status_fs   := wdata(14, 13)
      // status_xs   := wdata(16, 15)
      status_sum := wdata(18)
      status_mxr := wdata(19)
    }
    csr_legal := prv_is_ms
  }
  when(io.rw.addr === CSRs.mstatus.U) {
    rdata := mstatus
    when(wen) {
      status_sie  := wdata(1)
      status_spie := wdata(5)
      status_spp  := wdata(8)
      status_fs   := wdata(14, 13)
      // status_xs    := wdata(16, 15)
      status_sum   := wdata(18)
      status_mxr   := wdata(19)
      mstatus_mie  := wdata(3)
      mstatus_mpie := wdata(7)
      mstatus_mpp  := wdata(12, 11)
      mstatus_mprv := wdata(17)
      mstatus_tvm  := wdata(20)
      mstatus_tw   := wdata(21)
      mstatus_tsr  := wdata(22)
    }
    csr_legal := prv_is_m
  }
  io.mprv := mstatus_mprv
  io.mpp  := mstatus_mpp

  /*
   * Number:      0x105
   * Privilege:   SRW
   * Name:        stvec
   * Description: Supervisor trap-handler base address
   */
  val stvec = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.stvec.U) {
    rdata := stvec
    when(wen) {
      stvec := wdata
    }
    csr_legal := prv_is_ms
  }

  /*
   * Number:      0x106
   * Privilege:   SRW
   * Name:        scounteren
   * Description: Supervisor counter enable
   */
  val scounteren = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.scounteren.U) {
    rdata := scounteren
    when(wen) {
      scounteren := wdata
    }
    csr_legal := prv_is_ms
  }

  /*
   * Number:      0x140
   * Privilege:   SRW
   * Name:        sscratch
   * Description: Scratch register for supervisor trap handlers
   */
  val sscratch = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.sscratch.U) {
    rdata := sscratch
    when(wen) {
      sscratch := wdata
    }
    csr_legal := prv_is_ms
  }

  /*
   * Number:      0x141
   * Privilege:   SRW
   * Name:        sepc
   * Description: Machine exception program counter
   */
  val sepc = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.sepc.U) {
    rdata := sepc
    when(wen) {
      sepc := wdata
    }
    csr_legal := prv_is_ms
  }

  /*
   * Number:      0x142
   * Privilege:   SRW
   * Name:        scause
   * Description: Supervisor trap cause
   */
  val scause = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.scause.U) {
    rdata := scause
    when(wen) {
      scause := wdata
    }
    csr_legal := prv_is_ms
  }

  /*
   * Number:      0x143
   * Privilege:   SRW
   * Name:        stval
   * Description: Supervisor bad address or instruction
   */
  val stval = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.stval.U) {
    rdata := stval
    when(wen) {
      stval := wdata
    }
    csr_legal := prv_is_ms
  }

  /*
   * Number:      0x180
   * Privilege:   SRW
   * Name:        satp
   * Description: Supervisor address translation and protection
   */
  val satp         = RegInit(0.U(xLen.W))
  val satp_updated = WireDefault(false.B)
  val tvm_en       = prv_is_s && mstatus_tvm.asBool
  // if satp is written with an unsupported MODE, the entire write has no effect
  val satp_wen = (wdata(62, 60) === 0.U)
  io.sv39_en   := satp(63).asBool
  io.satp_asid := satp(59, 44)
  io.satp_ppn  := satp(43, 0)
  when(io.rw.addr === CSRs.satp.U) {
    rdata := satp
    when(wen && satp_wen) {
      satp         := wdata
      io.sv39_en   := wdata(63).asBool // bypass
      io.satp_asid := wdata(59, 44) // bypass
      io.satp_ppn  := wdata(43, 0) // bypass
      satp_updated := prv_is_s // flush pipeline after satp updated if in S mode
    }
    csr_legal := prv_is_ms && !tvm_en
  }

  /*
   * Number:      0xF11
   * Privilege:   MRO
   * Name:        mvendorid
   * Description: Vendor ID
   */
  when(io.rw.addr === CSRs.mvendorid.U) {
    rdata     := 0.U
    csr_legal := prv_is_m
  }

  /*
   * Number:      0xF12
   * Privilege:   MRO
   * Name:        marchid
   * Description: Architecture ID
   */
  when(io.rw.addr === CSRs.marchid.U) {
    rdata     := 0.U
    csr_legal := prv_is_m
  }

  /*
   * Number:      0xF13
   * Privilege:   MRO
   * Name:        mimpid
   * Description: Implementation ID
   */
  when(io.rw.addr === CSRs.mimpid.U) {
    rdata     := 0.U
    csr_legal := prv_is_m
  }

  /*
   * Number:      0xF14
   * Privilege:   MRO (Writable in CherrySprings BootROM)
   * Name:        mhartid
   * Description: Hardware thread ID
   */
  val mhartid          = RegInit(hartID.U(xLen.W))
  val mhartid_writable = RegInit(true.B)
  when(io.rw.addr === CSRs.mhartid.U) {
    rdata := mhartid
    when(wen && mhartid_writable) {
      mhartid          := wdata
      mhartid_writable := false.B
    }
    csr_legal := prv_is_m
  }
  io.hartid := mhartid(7, 0)

  /*
   * Number:      0x301
   * Privilege:   MRW
   * Name:        misa
   * Description: ISA and extensions
   */
  val misa = "h8000000000141101".U(64.W)
  when(io.rw.addr === CSRs.misa.U) {
    rdata     := misa
    csr_legal := prv_is_m
  }

  /*
   * Number:      0x302
   * Privilege:   MRW
   * Name:        medeleg
   * Description: Machine exception delegation register
   */
  val medeleg = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.medeleg.U) {
    rdata := medeleg
    when(wen) {
      medeleg := wdata & s_exc_mask
    }
    csr_legal := prv_is_m
  }

  /*
   * Number:      0x303
   * Privilege:   MRW
   * Name:        mideleg
   * Description: Machine interrupt delegation register
   */
  val mideleg = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.mideleg.U) {
    rdata := mideleg
    when(wen) {
      mideleg := wdata & s_int_mask
    }
    csr_legal := prv_is_m
  }

  /*
   * Number:      0x304 / 0x104
   * Privilege:   MRW / SRW
   * Name:        mie / sie
   * Description: Machine / Supervisor interrupt-enable register
   */
  val mie     = WireDefault(0.U(xLen.W))
  val sie     = WireDefault(0.U(xLen.W))
  val ie_ssie = RegInit(0.U(1.W))
  val ie_msie = RegInit(0.U(1.W))
  val ie_stie = RegInit(0.U(1.W))
  val ie_mtie = RegInit(0.U(1.W))
  val ie_seie = RegInit(0.U(1.W))
  val ie_meie = RegInit(0.U(1.W))
  mie := Cat(
    0.U(52.W),
    ie_meie,
    0.U(1.W),
    ie_seie,
    0.U(1.W),
    ie_mtie,
    0.U(1.W),
    ie_stie,
    0.U(1.W),
    ie_msie,
    0.U(1.W),
    ie_ssie,
    0.U(1.W)
  )
  sie := Cat(
    0.U(54.W),
    ie_seie,
    0.U(3.W),
    ie_stie,
    0.U(3.W),
    ie_ssie,
    0.U(1.W)
  )
  when(io.rw.addr === CSRs.mie.U) {
    rdata := mie
    when(wen) {
      ie_ssie := wdata(1)
      ie_msie := wdata(3)
      ie_stie := wdata(5)
      ie_mtie := wdata(7)
      ie_seie := wdata(9)
      ie_meie := wdata(11)
    }
    csr_legal := prv_is_m
  }
  when(io.rw.addr === CSRs.sie.U) {
    rdata := sie
    when(wen) {
      ie_ssie := wdata(1)
      ie_stie := wdata(5)
      ie_seie := wdata(9)
    }
    csr_legal := prv_is_ms
  }

  /*
   * Number:      0x305
   * Privilege:   MRW
   * Name:        mtvec
   * Description: Machine trap-handler base address
   */
  val mtvec = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.mtvec.U) {
    rdata := mtvec
    when(wen) {
      mtvec := wdata
    }
    csr_legal := prv_is_m
  }

  /*
   * Number:      0x306
   * Privilege:   MRW
   * Name:        mcounteren
   * Description: Machine counter enable
   */
  val mcounteren = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.mcounteren.U) {
    rdata := mcounteren
    when(wen) {
      mcounteren := wdata
    }
    csr_legal := prv_is_m
  }

  /*
   * Number:      0x340
   * Privilege:   MRW
   * Name:        mscratch
   * Description: Scratch register for machine trap handlers
   */
  val mscratch = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.mscratch.U) {
    rdata := mscratch
    when(wen) {
      mscratch := wdata
    }
    csr_legal := prv_is_m
  }

  /*
   * Number:      0x341
   * Privilege:   MRW
   * Name:        mepc
   * Description: Machine exception program counter
   */
  val mepc = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.mepc.U) {
    rdata := mepc
    when(wen) {
      mepc := wdata
    }
    csr_legal := prv_is_m
  }

  /*
   * Number:      0x342
   * Privilege:   MRW
   * Name:        mcause
   * Description: Machine trap cause
   */
  val mcause = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.mcause.U) {
    rdata := mcause
    when(wen) {
      mcause := wdata
    }
    csr_legal := prv_is_m
  }

  /*
   * Number:      0x343
   * Privilege:   MRW
   * Name:        mtval
   * Description: Machine bad address or instruction
   */
  val mtval = RegInit(0.U(xLen.W))
  when(io.rw.addr === CSRs.mtval.U) {
    rdata := mtval
    when(wen) {
      mtval := wdata
    }
    csr_legal := prv_is_m
  }

  /*
   * Number:      0x344 / 0x144
   * Privilege:   MRW / SRW
   * Name:        mip / sip
   * Description: Machine / Supervisor interrupt pending
   */
  val mip       = WireDefault(0.U(xLen.W))
  val sip       = WireDefault(0.U(xLen.W))
  val ip_ssip   = RegInit(0.U(1.W))
  val ip_msip   = io.interrupt.msip.asUInt
  val ip_stip   = RegInit(0.U(1.W))
  val ip_mtip   = io.interrupt.mtip.asUInt
  val ip_seip_r = RegInit(0.U(1.W))
  val ip_seip   = (io.interrupt.seip || ip_seip_r.asBool).asUInt
  val ip_meip   = io.interrupt.meip.asUInt
  mip := Cat(
    0.U(52.W),
    ip_meip,
    0.U(1.W),
    ip_seip,
    0.U(1.W),
    ip_mtip,
    0.U(1.W),
    ip_stip,
    0.U(1.W),
    ip_msip,
    0.U(1.W),
    ip_ssip,
    0.U(1.W)
  )
  sip := Cat(
    0.U(54.W),
    ip_seip,
    0.U(3.W),
    ip_stip,
    0.U(3.W),
    ip_ssip,
    0.U(1.W)
  )
  when(io.rw.addr === CSRs.mip.U) {
    rdata := mip
    when(wen) {
      ip_ssip   := wdata(1)
      ip_stip   := wdata(5)
      ip_seip_r := wdata(9)
    }
    csr_legal := prv_is_m
  }
  when(io.rw.addr === CSRs.sip.U) {
    rdata := sip
    when(wen) {
      ip_ssip := wdata(1)
    }
    csr_legal := prv_is_ms
  }

  /*
   * Number:      0xB00 / 0xC00
   * Privilege:   MRW / URO
   * Name:        mcycle / cycle
   * Description: Machine cycle counter / Cycle counter for RDCYCLE instruction
   */
  val cycle = RegInit(UInt(64.W), 0.U)
  cycle := cycle + 1.U
  when(io.rw.addr === CSRs.mcycle.U) {
    rdata := cycle
    when(wen) {
      cycle := wdata
    }
    csr_legal := prv_is_m
  }
  when(io.rw.addr === CSRs.cycle.U) {
    rdata     := cycle
    csr_legal := prv_is_m || (prv_is_s && mcounteren(0)) || (prv_is_u && mcounteren(0) && scounteren(0))
  }
  io.cycle := cycle

  /*
   * Number:      0xB02 / 0xC02
   * Privilege:   MRW / URO
   * Name:        minstret / instret
   * Description: Machine instructions-retired counter /
   *              Instructions-retired counter for RDINSTRET instruction
   */
  val instret = RegInit(UInt(64.W), 0.U)
  instret := instret + io.commit
  when(io.rw.addr === CSRs.minstret.U) {
    rdata := instret
    when(wen) {
      instret := wdata
    }
    csr_legal := prv_is_m
  }
  when(io.rw.addr === CSRs.instret.U) {
    rdata     := instret
    csr_legal := prv_is_m || (prv_is_s && mcounteren(2)) || (prv_is_u && mcounteren(2) && scounteren(2))
  }
  io.instret := instret

  // CSR module output
  io.rw.rdata := rdata
  io.rw.valid := csr_legal
  io.prv      := prv

  /*
   * An MRET or SRET instruction is used to return from a trap in M-mode or S-mode respectively.
   * When executing an xRET instruction, supposing xPP holds the value y, xIE is set to xPIE; the
   * privilege mode is changed to y; xPIE is set to 1; and xPP is set to the least-privileged
   * supported mode (U if U-mode is implemented, else M). If xPP != M, xRET also sets MPRV = 0.
   */
  val is_mret    = io.uop.sys_op === s"b$SYS_MRET".U
  val mret_legal = prv_is_m
  when(is_mret && mret_legal) {
    prv          := mstatus_mpp
    io.prv       := mstatus_mpp // bypass
    mstatus_mie  := mstatus_mpie
    mstatus_mpie := 1.U
    mstatus_mpp  := PRV.U.U
    when(mstatus_mpp =/= PRV.M.U) {
      mstatus_mprv := 0.U
    }
  }

  val is_sret    = io.uop.sys_op === s"b$SYS_SRET".U
  val sret_legal = prv_is_ms && !mstatus_tsr.asBool
  when(is_sret && sret_legal) {
    prv         := status_spp
    io.prv      := status_spp // bypass
    status_sie  := status_spie
    status_spie := 1.U
    status_spp  := PRV.U.U
    when(status_spp =/= PRV.M.U) {
      mstatus_mprv := 0.U
    }
  }

  /*
   * System instructions
   */
  val is_sfv     = (io.uop.sys_op === s"b$SYS_SFV".U) && io.uop.valid
  val sfv_legal  = prv_is_ms && !(prv_is_s && mstatus_tvm.asBool)
  val is_fence_i = (io.uop.sys_op === s"b$SYS_FENCEI".U) && io.uop.valid
  val is_sys     = io.sfence_vma || is_fence_i
  io.sfence_vma := is_sfv && sfv_legal
  io.fence_i    := is_fence_i

  /*
   * Exception & Interrupt
   */
  val is_exc_from_prev = (io.uop.exc =/= s"b$EXC_N".U)
  val is_exc_from_lsu  = (io.lsu_exc_code =/= 0.U)
  val is_exc_from_csr  = !csr_legal && (io.rw.cmd =/= s"b$CSR_N".U)
  val is_exc_from_sys  = (is_mret && !mret_legal) || (is_sret && !sret_legal) || (is_sfv && !sfv_legal)
  val is_exc           = is_exc_from_prev || is_exc_from_lsu || is_exc_from_csr

  def int2index(x: UInt): UInt = {
    val y = WireDefault(0.U(4.W))
    when(x(11)) {
      y := 11.U // MEI
    }.elsewhen(x(3)) {
      y := 3.U // MSI
    }.elsewhen(x(7)) {
      y := 7.U // MTI
    }.elsewhen(x(9)) {
      y := 9.U // SEI
    }.elsewhen(x(1)) {
      y := 1.U // SSI
    }.elsewhen(x(5)) {
      y := 5.U // STI
    }
    y
  }

  val int_attach     = io.uop.valid && (io.uop.fu === s"b$FU_ALU".U || io.uop.fu === s"b$FU_JMP".U)
  val int_bits       = mip & mie
  val int_bits_mmode = int_bits & (~mideleg)

  val int_index     = WireDefault(0.U(4.W))
  val int_index_tmp = int2index(int_bits)
  when(prv === PRV.M.U) {
    when(mstatus_mie.asBool) {
      int_index := int2index(int_bits_mmode)
    }
  }.otherwise {
    when(((1.U << int_index_tmp) & mideleg) =/= 0.U) { // delegate to S mode
      when(status_sie.asBool || prv === PRV.U.U) {
        int_index := int_index_tmp
      }
    }.otherwise {
      when(mstatus_mie.asBool || prv === PRV.U.U || prv === PRV.S.U) {
        int_index := int_index_tmp
      }
    }
  }
  val is_int = int_attach && (int_index =/= 0.U)

  val cause_exc        = Wire(UInt(4.W))
  val cause_exc_onehot = Wire(UInt(16.W))
  val cause_int        = Wire(UInt(4.W))
  val cause_int_onehot = Wire(UInt(16.W))
  cause_exc := MuxLookup(
    io.uop.exc, // check exception from early stage first
    Mux(
      io.lsu_exc_code =/= 0.U,
      io.lsu_exc_code,
      Mux(is_exc_from_csr || is_exc_from_sys, Causes.illegal_instruction.U, 0.U)
    ),
    Seq(
      s"b$EXC_IAM".U -> Causes.misaligned_fetch.U,
      s"b$EXC_IAF".U -> Causes.fetch_access.U,
      s"b$EXC_II".U  -> Causes.illegal_instruction.U,
      s"b$EXC_EB".U  -> Causes.breakpoint.U,
      s"b$EXC_EC".U  -> Cat("b10".U, prv),
      s"b$EXC_IPF".U -> Causes.fetch_page_fault.U
    )
  )

  cause_exc_onehot := UIntToOH(cause_exc)
  cause_int        := int_index
  cause_int_onehot := UIntToOH(cause_int)
  io.is_int        := is_int

  if (debugInterrupt) {
    when(is_int) {
      printf(cf"${DebugTimer()} [Hart ${hartID}] [Interrupt] ")
      when(int_index === 11.U) {
        printf("MEI")
      }.elsewhen(int_index === 3.U) {
        printf("MSI")
      }.elsewhen(int_index === 7.U) {
        printf("MTI")
      }.elsewhen(int_index === 9.U) {
        printf("SEI")
      }.elsewhen(int_index === 1.U) {
        printf("SSI")
      }.elsewhen(int_index === 5.U) {
        printf("STI")
      }
      printf(cf" (at PC = ${io.uop.pc}%x)\n")
    }
  }

  val tval = WireDefault(0.U(xLen.W))
  when(is_exc_from_lsu) {
    tval := io.lsu_addr
  }
  when(io.uop.exc === s"b$EXC_IPF".U) {
    tval := io.uop.pc
  }

  val trap      = is_exc || is_int
  val trap_to_s = WireDefault(false.B)
  val trap_pc   = WireDefault(0.U(xLen.W))
  when(!prv_is_m) {
    trap_to_s :=
      (is_exc && ((cause_exc_onehot & medeleg) =/= 0.U)) ||
      (is_int && ((cause_int_onehot & mideleg) =/= 0.U))
  }
  when(trap_to_s && trap) {
    stval       := tval
    scause      := Mux(is_exc, cause_exc, Cat(1.U(1.W), 0.U(59.W), cause_int))
    sepc        := io.uop.pc
    status_spie := status_sie
    status_sie  := 0.U
    status_spp  := prv
    prv         := PRV.S.U
    io.prv      := PRV.S.U // bypass
    trap_pc     := Cat(stvec(xLen - 1, 2) + Mux(is_int && (stvec(1, 0) === 1.U), cause_int, 0.U), 0.U(2.W))
  }
  when(!trap_to_s && trap) {
    mtval        := tval
    mcause       := Mux(is_exc, cause_exc, Cat(1.U(1.W), 0.U(59.W), cause_int))
    mepc         := io.uop.pc
    mstatus_mpie := mstatus_mie
    mstatus_mie  := 0.U
    mstatus_mpp  := prv
    prv          := PRV.M.U
    io.prv       := PRV.M.U // bypass
    trap_pc      := Cat(mtvec(xLen - 1, 2) + Mux(is_int && (mtvec(1, 0) === 1.U), cause_int, 0.U), 0.U(2.W))
  }

  // CSR / SYS control flow
  io.jmp_packet        := 0.U.asTypeOf(new JmpPacket)
  io.jmp_packet.valid  := trap || is_sys || satp_updated || (is_mret && mret_legal) || (is_sret && sret_legal)
  io.jmp_packet.target := Mux(trap, trap_pc, Mux(is_sys || satp_updated, io.uop.npc, Mux(is_mret, mepc, sepc)))

  if (enableDifftest) {
    val diff_cs = Module(new DifftestCSRState)
    diff_cs.io.clock          := clock
    diff_cs.io.coreid         := io.hartid
    diff_cs.io.priviledgeMode := prv
    diff_cs.io.mstatus        := mstatus
    diff_cs.io.sstatus        := sstatus
    diff_cs.io.mepc           := mepc
    diff_cs.io.sepc           := sepc
    diff_cs.io.mtval          := mtval
    diff_cs.io.stval          := stval
    diff_cs.io.mtvec          := mtvec
    diff_cs.io.stvec          := stvec
    diff_cs.io.mcause         := mcause
    diff_cs.io.scause         := scause
    diff_cs.io.satp           := satp
    diff_cs.io.mip            := mip
    diff_cs.io.mie            := mie
    diff_cs.io.mscratch       := mscratch
    diff_cs.io.sscratch       := sscratch
    diff_cs.io.mideleg        := mideleg
    diff_cs.io.medeleg        := medeleg

    val diff_ae = Module(new DifftestArchEvent)
    diff_ae.io.clock         := clock
    diff_ae.io.coreid        := io.hartid
    diff_ae.io.intrNO        := RegNext(Mux(is_int, cause_int, 0.U))
    diff_ae.io.cause         := RegNext(Mux(is_exc, cause_exc, 0.U))
    diff_ae.io.exceptionPC   := RegNext(io.uop.pc)
    diff_ae.io.exceptionInst := RegNext(io.uop.instr)
  }
}
