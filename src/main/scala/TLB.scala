import chisel3._
import chisel3.util._
import chisel3.util.random._
import org.chipsalliance.cde.config._
import Constant._

case object IsITLB extends Field[Boolean]
case object IsDTLB extends Field[Boolean]

trait Sv39Parameters extends HasCherrySpringsParameters {
  val pageTableLevels = 3
  val offsetLen       = 12
  val ppn0Len         = 9
  val ppn1Len         = 9
  val ppn2Len         = paddrLen - offsetLen - ppn0Len - ppn1Len // 2
  val ppnLen          = paddrLen - offsetLen // 20
  val vpn2Len         = 9
  val vpn1Len         = 9
  val vpn0Len         = 9
  val vpnLen          = vpn2Len + vpn1Len + vpn0Len
  val asidLen         = 16 // not used now
  val flagLen         = 10

  val tlb4kbEntryLen = flagLen + vpnLen + ppnLen
  val tlb2mbEntryLen = tlb4kbEntryLen - vpn0Len - ppn0Len
  val tlb1gbEntryLen = tlb2mbEntryLen - vpn1Len - ppn1Len
}

class TLB(implicit p: Parameters) extends CherrySpringsModule with Sv39Parameters {
  val io = IO(new Bundle {
    val prv        = Input(UInt(2.W))
    val sfence_vma = Input(Bool())
    // read TLB
    val vaddr  = Input(new Sv39VirtAddr)
    val rpte   = Output(new Sv39PTE)
    val rlevel = Output(UInt(2.W))
    val hit    = Output(Bool())
    // write TLB
    val wen    = Input(Bool())
    val wvaddr = Input(new Sv39VirtAddr)
    val wpte   = Input(new Sv39PTE)
    val wlevel = Input(UInt(2.W))
  })

  // 0: 4KB, 1: 2MB, 2: 1GB, 3: invalid
  assert(io.rlevel =/= 3.U)
  assert(io.wlevel =/= 3.U)

  // TLB sizes
  val tlb4kb_size = 16
  val tlb2mb_size = 4
  val tlb1gb_size = 2

  // random replacement
  val replace_idx = Wire(UInt(log2Up(tlb4kb_size).W))
  replace_idx := LFSR(log2Up(tlb4kb_size))

  /*
   * TLB - 4 KB page
   */
  val array4kb       = RegInit(VecInit(Seq.fill(tlb4kb_size)(0.U.asTypeOf(new TLB4KBEntry))))
  val array4kb_valid = RegInit(VecInit(Seq.fill(tlb4kb_size)(false.B))) // not "valid" in PTE
  val array4kb_rdata = WireDefault(0.U.asTypeOf(new TLB4KBEntry))
  val array4kb_wdata = Wire(new TLB4KBEntry)
  val hit4kb         = WireDefault(false.B)
  // read
  for (i <- 0 until tlb4kb_size) {
    when(array4kb_valid(i) && (array4kb(i).vpn() === io.vaddr.vpn())) {
      hit4kb         := true.B
      array4kb_rdata := array4kb(i)
    }
  }
  // set wdata
  array4kb_wdata.flag := io.wpte.flag
  array4kb_wdata.vpn2 := io.wvaddr.vpn2
  array4kb_wdata.vpn1 := io.wvaddr.vpn1
  array4kb_wdata.vpn0 := io.wvaddr.vpn0
  array4kb_wdata.ppn2 := io.wpte.ppn2
  array4kb_wdata.ppn1 := io.wpte.ppn1
  array4kb_wdata.ppn0 := io.wpte.ppn0
  when(io.wen && (io.wlevel === 0.U)) {
    array4kb(replace_idx)       := array4kb_wdata
    array4kb_valid(replace_idx) := true.B
  }
  when(io.sfence_vma) {
    for (i <- 0 until tlb4kb_size) {
      array4kb_valid(i) := false.B
    }
  }

  /*
   * TLB - 2 MB page
   */
  val array2mb       = RegInit(VecInit(Seq.fill(tlb2mb_size)(0.U.asTypeOf(new TLB2MBEntry))))
  val array2mb_valid = RegInit(VecInit(Seq.fill(tlb2mb_size)(false.B)))
  val array2mb_rdata = Wire(new TLB2MBEntry)
  val array2mb_wdata = Wire(new TLB2MBEntry)
  val hit2mb         = Wire(Bool())
  // read, index by lower bits of vaddr vpn1
  array2mb_rdata := array2mb(io.vaddr.vpn1)
  hit2mb         := array2mb_valid(io.vaddr.vpn1) && (array2mb_rdata.vpn2mb() === io.vaddr.vpn2mb())
  // set wdata
  array2mb_wdata.flag := io.wpte.flag
  array2mb_wdata.vpn2 := io.wvaddr.vpn2
  array2mb_wdata.vpn1 := io.wvaddr.vpn1
  array2mb_wdata.ppn2 := io.wpte.ppn2
  array2mb_wdata.ppn1 := io.wpte.ppn1
  when(io.wen && (io.wlevel === 1.U)) {
    // index by lower bits of wvaddr vpn1
    array2mb(io.wvaddr.vpn1)       := array2mb_wdata
    array2mb_valid(io.wvaddr.vpn1) := true.B
  }
  when(io.sfence_vma) {
    for (i <- 0 until tlb2mb_size) {
      array2mb_valid(i) := false.B
    }
  }

  /*
   * TLB - 1 GB page
   */
  val array1gb       = RegInit(VecInit(Seq.fill(tlb1gb_size)(0.U.asTypeOf(new TLB1GBEntry))))
  val array1gb_valid = RegInit(VecInit(Seq.fill(tlb1gb_size)(false.B)))
  val array1gb_rdata = Wire(new TLB1GBEntry)
  val array1gb_wdata = Wire(new TLB1GBEntry)
  val hit1gb         = Wire(Bool())
  // read, index by lower bits of vaddr vpn2
  array1gb_rdata := array1gb(io.vaddr.vpn2)
  hit1gb         := array1gb_valid(io.vaddr.vpn2) && (array1gb_rdata.vpn1gb() === io.vaddr.vpn1gb())
  // set wdata
  array1gb_wdata.flag := io.wpte.flag
  array1gb_wdata.vpn2 := io.wvaddr.vpn2
  array1gb_wdata.ppn2 := io.wpte.ppn2
  when(io.wen && (io.wlevel === 2.U)) {
    // index by lower bits of wvaddr vpn2
    array1gb(io.wvaddr.vpn2)       := array1gb_wdata
    array1gb_valid(io.wvaddr.vpn2) := true.B
  }
  when(io.sfence_vma) {
    for (i <- 0 until tlb1gb_size) {
      array1gb_valid(i) := false.B
    }
  }

  // TLB read
  io.rpte   := 0.U.asTypeOf(new Sv39PTE)
  io.rlevel := 0.U
  io.hit    := hit4kb || hit2mb || hit1gb
  when(hit4kb) {
    io.rpte.flag := array4kb_rdata.flag
    io.rpte.ppn0 := array4kb_rdata.ppn0
    io.rpte.ppn1 := array4kb_rdata.ppn1
    io.rpte.ppn2 := array4kb_rdata.ppn2
  }.elsewhen(hit2mb) {
    io.rpte.flag := array2mb_rdata.flag
    io.rpte.ppn0 := 0.U
    io.rpte.ppn1 := array2mb_rdata.ppn1
    io.rpte.ppn2 := array2mb_rdata.ppn2
    io.rlevel    := 1.U
  }.elsewhen(hit1gb) {
    io.rpte.flag := array1gb_rdata.flag
    io.rpte.ppn0 := 0.U
    io.rpte.ppn1 := 0.U
    io.rpte.ppn2 := array1gb_rdata.ppn2
    io.rlevel    := 2.U
  }
}
