import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

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
  require(xLen == 64)

  def is_leaf(p: Sv39PTE) = p.flag.r || p.flag.x

  val io = IO(new Bundle {
    val prv        = Input(UInt(2.W))
    val satp_ppn   = Input(UInt(44.W))
    val sfence_vma = Input(Bool())
    val addr_trans = Flipped(new AddrTransPortIO)
    val ptw        = new CachePortIO
  })

  val req = HoldUnless(io.addr_trans.req.bits, io.addr_trans.req.valid)

  val s_tlb :: s_ptw_req :: s_ptw_resp :: Nil = Enum(3)
  val state                                   = RegInit(s_tlb)

  val pt_level_ptw = RegInit(0.U(2.W)) // for page table walk
  val pt_level     = WireDefault(pt_level_ptw)
  assert(pt_level < pageTableLevels.U)

  val pte     = Wire(new Sv39PTE)
  val pte_reg = RegEnable(pte, 0.U.asTypeOf(new Sv39PTE), io.ptw.resp.fire)

  /*
   * TLB - 4 KB page
   */
  val tlb4kb_size    = 16
  val array4kb       = RegInit(VecInit(Seq.fill(tlb4kb_size)(0.U.asTypeOf(new TLB4KBEntry))))
  val array4kb_valid = RegInit(VecInit(Seq.fill(tlb4kb_size)(false.B))) // not "valid" in PTE
  val array4kb_rdata = Wire(new TLB4KBEntry)
  val array4kb_wdata = Wire(new TLB4KBEntry)
  val hit4kb         = Wire(Bool())
  array4kb_rdata := array4kb(req.vaddr.vpn0)
  // check hit or miss
  hit4kb := array4kb_valid(req.vaddr.vpn0) &&
    (array4kb_rdata.vpn === req.vaddr.vpn)
  // set wdata
  array4kb_wdata.flag := pte.flag
  array4kb_wdata.vpn2 := req.vaddr.vpn2
  array4kb_wdata.vpn1 := req.vaddr.vpn1
  array4kb_wdata.vpn0 := req.vaddr.vpn0
  array4kb_wdata.ppn2 := pte.ppn2
  array4kb_wdata.ppn1 := pte.ppn1
  array4kb_wdata.ppn0 := pte.ppn0
  when(io.ptw.resp.fire && is_leaf(pte) && (pt_level_ptw === 0.U)) {
    array4kb(req.vaddr.vpn0)       := array4kb_wdata
    array4kb_valid(req.vaddr.vpn0) := true.B
  }
  when(io.sfence_vma) {
    for (i <- 0 until tlb4kb_size) {
      array4kb_valid(i) := false.B
    }
  }

  /*
   * TLB - 2 MB page
   */
  val tlb2mb_size    = 4
  val array2mb       = RegInit(VecInit(Seq.fill(tlb2mb_size)(0.U.asTypeOf(new TLB2MBEntry))))
  val array2mb_valid = RegInit(VecInit(Seq.fill(tlb2mb_size)(false.B)))
  val array2mb_rdata = Wire(new TLB2MBEntry)
  val array2mb_wdata = Wire(new TLB2MBEntry)
  val hit2mb         = Wire(Bool())
  array2mb_rdata := array2mb(req.vaddr.vpn1)
  // check hit or miss
  hit2mb := array2mb_valid(req.vaddr.vpn1) &&
    (array2mb_rdata.vpn2mb === req.vaddr.vpn2mb)
  // set wdata
  array2mb_wdata.flag := pte.flag
  array2mb_wdata.vpn2 := req.vaddr.vpn2
  array2mb_wdata.vpn1 := req.vaddr.vpn1
  array2mb_wdata.ppn2 := pte.ppn2
  array2mb_wdata.ppn1 := pte.ppn1
  when(io.ptw.resp.fire && is_leaf(pte) && (pt_level_ptw === 1.U)) {
    array2mb(req.vaddr.vpn1)       := array2mb_wdata
    array2mb_valid(req.vaddr.vpn1) := true.B
  }
  when(io.sfence_vma) {
    for (i <- 0 until tlb2mb_size) {
      array2mb_valid(i) := false.B
    }
  }

  pte := io.ptw.resp.bits.rdata.asTypeOf(new Sv39PTE) // default
  when(state === s_tlb) {
    when(hit4kb) {
      pte.flag := array4kb_rdata.flag
      pte.ppn2 := array4kb_rdata.ppn2
      pte.ppn1 := array4kb_rdata.ppn1
      pte.ppn0 := array4kb_rdata.ppn0
    }.elsewhen(hit2mb) {
      pte.flag := array2mb_rdata.flag
      pte.ppn2 := array2mb_rdata.ppn2
      pte.ppn1 := array2mb_rdata.ppn1
      pte.ppn0 := 0.U
    }
  }

  val ptw_complete = !pte.flag.v || (!pte.flag.r && pte.flag.w) || is_leaf(pte) || (pt_level === 0.U)

  switch(state) {
    is(s_tlb) {
      when(io.addr_trans.req.valid && !(hit4kb || hit2mb)) {
        state := s_ptw_req
      }
      pt_level_ptw := (pageTableLevels - 1).U
      when(hit4kb) {
        pt_level := 0.U
      }
      when(hit2mb) {
        pt_level := 1.U
      }
    }
    is(s_ptw_req) {
      when(io.ptw.req.fire) {
        state := s_ptw_resp
      }
    }
    is(s_ptw_resp) {
      when(io.ptw.resp.fire) {
        when(ptw_complete) {
          state := s_tlb
        }.otherwise {
          state        := s_ptw_req
          pt_level_ptw := pt_level_ptw - 1.U
        }
      }
    }
  }

  val l2_addr = Wire(UInt(paddrLen.W))
  val l1_addr = Wire(UInt(paddrLen.W))
  val l0_addr = Wire(UInt(paddrLen.W))

  l2_addr := Cat(io.satp_ppn, req.vaddr.vpn2, 0.U(3.W))
  l1_addr := Cat(pte_reg.ppn, req.vaddr.vpn1, 0.U(3.W))
  l0_addr := Cat(pte_reg.ppn, req.vaddr.vpn0, 0.U(3.W))

  // check page fault for pte
  val page_fault = WireDefault(false.B)
  when(!pte.flag.v || (!pte.flag.r && pte.flag.w)) {
    page_fault := true.B
  }
  when(is_leaf(pte)) {
    when(io.prv === PRV.U.U && !pte.flag.u) {
      page_fault := true.B
    }
    if (p(IsITLB)) {
      when(!pte.flag.x) {
        page_fault := true.B
      }
    }
    if (p(IsDTLB)) {
      when(req.wen && !pte.flag.w) {
        page_fault := true.B
      }
      when(!req.wen && !pte.flag.r) {
        page_fault := true.B
      }
    }
    when(
      (pt_level === 2.U && Cat(pte.ppn1, pte.ppn0) =/= 0.U) ||
        (pt_level === 1.U && pte.ppn0 =/= 0.U)
    ) {
      page_fault := true.B // misaligned superpage
    }
    when(!pte.flag.a) {
      page_fault := true.B
    }
    if (p(IsDTLB)) {
      when(req.wen && !pte.flag.d) {
        page_fault := true.B
      }
    }
  }

  io.addr_trans.resp.valid             := io.addr_trans.resp.bits.tlb_hit || io.addr_trans.resp.bits.ptw_complete
  io.addr_trans.resp.bits.tlb_hit      := hit4kb || hit2mb
  io.addr_trans.resp.bits.ptw_complete := io.ptw.resp.fire && ptw_complete
  io.addr_trans.resp.bits.page_fault   := page_fault
  io.addr_trans.resp.bits.paddr.offset := req.vaddr.offset
  io.addr_trans.resp.bits.paddr.ppn0   := Mux(pt_level > 0.U, req.vaddr.vpn0, pte.ppn0)
  io.addr_trans.resp.bits.paddr.ppn1   := Mux(pt_level > 1.U, req.vaddr.vpn1, pte.ppn1)
  io.addr_trans.resp.bits.paddr.ppn2   := pte_reg.ppn2

  io.ptw.req.bits := 0.U.asTypeOf(new CachePortReq)
  io.ptw.req.bits.addr := MuxLookup(
    pt_level_ptw,
    0.U,
    Array(
      2.U -> l2_addr,
      1.U -> l1_addr,
      0.U -> l0_addr
    )
  )
  io.ptw.req.valid  := (state === s_ptw_req)
  io.ptw.resp.ready := (state === s_ptw_resp)

  if (debugTLB) {
    when(io.addr_trans.req.valid) {
      printf(cf"${DebugTimer()} [TLB] vaddr=${io.addr_trans.req.bits.vaddr} satp_ppn=${io.satp_ppn}\n")
    }
    when(io.ptw.resp.fire) {
      printf(
        "%d [PTW] level=%d pte=%x ppn=%x\n",
        DebugTimer(),
        pt_level,
        io.ptw.resp.bits.rdata,
        io.ptw.resp.bits.rdata(53, 10)
      )
    }
  }
}
