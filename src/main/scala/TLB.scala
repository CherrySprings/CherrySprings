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

  val req_reg = RegInit(0.U.asTypeOf(new AddrTransPortReq))

  when(io.addr_trans.req.fire) {
    req_reg := io.addr_trans.req.bits
  }

  val s_req :: s_resp :: s_ptw_req :: s_ptw_resp :: s_tlb :: Nil = Enum(5)
  val state                                                      = RegInit(s_req)

  val pt_level = RegInit(0.U(2.W))
  assert(pt_level < pageTableLevels.U)

  val pte     = Wire(new Sv39PTE)
  val pte_reg = RegEnable(pte, 0.U.asTypeOf(new Sv39PTE), io.ptw.resp.fire || (state === s_tlb))

  // TLB - 4 KB page
  val tlb4kb_size    = 16
  val array4kb       = Module(new SRAM(tlb4kb_size, tlb4kbEntryLen))
  val array4kb_valid = RegInit(VecInit(Seq.fill(tlb4kb_size)(false.B))) // not "valid" in PTE
  val array4kb_rdata = Wire(new TLB4KBEntry)
  val array4kb_wdata = Wire(new TLB4KBEntry)
  val hit4kb         = Wire(Bool())
  array4kb.io.addr    := HoldUnless(io.addr_trans.req.bits.vaddr.vpn0, io.addr_trans.req.fire) // index by LSB by default
  array4kb_rdata      := array4kb.io.rdata.asTypeOf(new TLB4KBEntry)
  hit4kb              := array4kb_valid(array4kb.io.addr) && (array4kb_rdata.vpn === io.addr_trans.req.bits.vaddr.vpn)
  array4kb.io.wen     := io.ptw.resp.fire && is_leaf(pte) && (pt_level === 0.U)
  array4kb_wdata.flag := pte.flag
  array4kb_wdata.vpn2 := req_reg.vaddr.vpn2
  array4kb_wdata.vpn1 := req_reg.vaddr.vpn1
  array4kb_wdata.vpn0 := req_reg.vaddr.vpn0
  array4kb_wdata.ppn2 := pte.ppn2
  array4kb_wdata.ppn1 := pte.ppn1
  array4kb_wdata.ppn0 := pte.ppn0
  array4kb.io.wdata   := array4kb_wdata.asUInt
  when(array4kb.io.wen) {
    array4kb_valid(array4kb.io.addr) := true.B
  }
  when(io.sfence_vma) {
    for (i <- 0 until tlb4kb_size) {
      array4kb_valid(i) := false.B
    }
  }

  // TLB - 2 MB page
  val tlb2mb_size    = 4
  val array2mb       = Module(new SRAM(tlb2mb_size, tlb2mbEntryLen))
  val array2mb_valid = RegInit(VecInit(Seq.fill(tlb2mb_size)(false.B)))
  val array2mb_rdata = Wire(new TLB2MBEntry)
  val array2mb_wdata = Wire(new TLB2MBEntry)
  val hit2mb         = Wire(Bool())
  array2mb.io.addr    := HoldUnless(io.addr_trans.req.bits.vaddr.vpn1, io.addr_trans.req.fire)
  array2mb_rdata      := array2mb.io.rdata.asTypeOf(new TLB2MBEntry)
  hit2mb              := array2mb_valid(array2mb.io.addr) && (array2mb_rdata.vpn2mb === io.addr_trans.req.bits.vaddr.vpn2mb)
  array2mb.io.wen     := io.ptw.resp.fire && is_leaf(pte) && (pt_level === 1.U)
  array2mb_wdata.flag := pte.flag
  array2mb_wdata.vpn2 := req_reg.vaddr.vpn2
  array2mb_wdata.vpn1 := req_reg.vaddr.vpn1
  array2mb_wdata.ppn2 := pte.ppn2
  array2mb_wdata.ppn1 := pte.ppn1
  array2mb.io.wdata   := array2mb_wdata.asUInt
  when(array2mb.io.wen) {
    array2mb_valid(array2mb.io.addr) := true.B
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

  switch(state) {
    is(s_req) {
      when(io.addr_trans.req.fire) {
        state := s_tlb
      }
    }
    is(s_tlb) {
      state    := Mux(hit4kb || hit2mb, s_resp, s_ptw_req)
      pt_level := Mux(hit4kb, 0.U, Mux(hit2mb, 1.U, (pageTableLevels - 1).U))
    }
    is(s_ptw_req) {
      when(io.ptw.req.fire) {
        state := s_ptw_resp
      }
    }
    is(s_ptw_resp) {
      when(io.ptw.resp.fire) {
        when(!pte.flag.v || (!pte.flag.r && pte.flag.w) || is_leaf(pte) || (pt_level === 0.U)) {
          state := s_resp
        }.otherwise {
          state    := s_ptw_req
          pt_level := pt_level - 1.U
        }
      }
    }
    is(s_resp) {
      when(io.addr_trans.resp.fire) {
        state := s_req
      }
    }
  }

  val l2_addr = Wire(UInt(paddrLen.W))
  val l1_addr = Wire(UInt(paddrLen.W))
  val l0_addr = Wire(UInt(paddrLen.W))

  l2_addr := Cat(io.satp_ppn, req_reg.vaddr.vpn2, 0.U(3.W))
  l1_addr := Cat(pte_reg.ppn, req_reg.vaddr.vpn1, 0.U(3.W))
  l0_addr := Cat(pte_reg.ppn, req_reg.vaddr.vpn0, 0.U(3.W))

  // check page fault for pte_reg
  val page_fault = WireDefault(false.B)
  when(!pte_reg.flag.v || (!pte_reg.flag.r && pte_reg.flag.w)) {
    page_fault := true.B
  }
  when(is_leaf(pte_reg)) {
    when(io.prv === PRV.U.U && !pte_reg.flag.u) {
      page_fault := true.B
    }
    if (p(IsITLB)) {
      when(!pte_reg.flag.x) {
        page_fault := true.B
      }
    }
    if (p(IsDTLB)) {
      when(req_reg.wen && !pte_reg.flag.w) {
        page_fault := true.B
      }
      when(!req_reg.wen && !pte_reg.flag.r) {
        page_fault := true.B
      }
    }
    when(
      (pt_level === 2.U && Cat(pte_reg.ppn1, pte_reg.ppn0) =/= 0.U) ||
        (pt_level === 1.U && pte_reg.ppn0 =/= 0.U)
    ) {
      page_fault := true.B // misaligned superpage
    }
    when(!pte_reg.flag.a) {
      page_fault := true.B
    }
    if (p(IsDTLB)) {
      when(req_reg.wen && !pte_reg.flag.d) {
        page_fault := true.B
      }
    }
  }

  io.addr_trans.req.ready              := (state === s_req)
  io.addr_trans.resp.valid             := (state === s_resp)
  io.addr_trans.resp.bits.page_fault   := page_fault
  io.addr_trans.resp.bits.paddr.offset := req_reg.vaddr.offset
  io.addr_trans.resp.bits.paddr.ppn0   := Mux(pt_level > 0.U, req_reg.vaddr.vpn0, pte_reg.ppn0)
  io.addr_trans.resp.bits.paddr.ppn1   := Mux(pt_level > 1.U, req_reg.vaddr.vpn1, pte_reg.ppn1)
  io.addr_trans.resp.bits.paddr.ppn2   := pte_reg.ppn2

  io.ptw.req.bits := 0.U.asTypeOf(new CachePortReq)
  io.ptw.req.bits.addr := MuxLookup(
    pt_level,
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
    when(io.addr_trans.req.fire) {
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
    when(io.addr_trans.resp.fire) {
      printf(
        cf"${DebugTimer()} [TLB] paddr=${io.addr_trans.resp.bits.paddr}" +
          cf"page_fault=${io.addr_trans.resp.bits.page_fault}\n"
      )
    }
  }
}
