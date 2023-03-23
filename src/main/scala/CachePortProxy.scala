import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

class CachePortProxy(implicit p: Parameters) extends CherrySpringsModule with Sv39Parameters {
  def isLeaf(p: Sv39PTE) = p.flag.r || p.flag.x

  val io = IO(new Bundle {
    val prv        = Input(UInt(2.W))
    val sv39_en    = Input(Bool())
    val satp_ppn   = Input(UInt(44.W))
    val sfence_vma = Input(Bool())
    val in         = Flipped(new CachePortIO)
    val out        = new CachePortIO
    val ptw        = new CachePortIO
  })

  val s_idle :: s_ptw_req :: s_ptw_resp :: s_ptw_complete :: s_access_fault :: Nil = Enum(5)
  val state                                                                        = RegInit(s_idle)

  // input register
  val in_req_bits = HoldUnless(io.in.req.bits, state === s_idle)
  val in_vaddr    = in_req_bits.addr.asTypeOf(new Sv39VirtAddr)

  // access TLB
  val tlb = Module(new TLB)
  tlb.io.prv        := io.prv
  tlb.io.sfence_vma := io.sfence_vma
  tlb.io.vaddr      := io.in.req.bits.addr.asTypeOf(new Sv39VirtAddr)
  val tlb_pte   = tlb.io.rpte
  val tlb_level = tlb.io.rlevel

  // address translation & protection enable
  // need to record state when input fires, in case satp changes when accessing page table
  val atp_en = HoldUnless((io.prv =/= PRV.M.U) && io.sv39_en, state === s_idle)

  // check in port request address range (only check paddr)
  val in_addr       = io.in.req.bits.addr
  val in_addr_clint = (in_addr >= "h02000000".U) && (in_addr <= "h0200FFFF".U)
  val in_addr_plic  = (in_addr >= "h0C000000".U) && (in_addr <= "h0FFFFFFF".U)
  val in_addr_uart  = (in_addr >= "h10000000".U) && (in_addr <= "h1000FFFF".U)
  val access_fault = (in_addr(31) === 0.U) && (io.prv === PRV.M.U || !atp_en) &&
    !(in_addr_clint || in_addr_plic || in_addr_uart)

  // page table walk
  val ptw_level    = RegInit(0.U(2.W))
  val ptw_pte      = io.ptw.resp.bits.rdata.asTypeOf(new Sv39PTE)
  val ptw_pte_reg  = RegEnable(ptw_pte, 0.U.asTypeOf(new Sv39PTE), io.ptw.resp.fire)
  val ptw_complete = !ptw_pte.flag.v || (!ptw_pte.flag.r && ptw_pte.flag.w) || isLeaf(ptw_pte) || (ptw_level === 0.U)
  val page_fault   = WireDefault(false.B)

  switch(state) {
    is(s_idle) {
      when(io.in.req.fire) {
        when(atp_en && !tlb.io.hit) {
          state := s_ptw_req
        }
      }
      when(io.in.req.valid) {
        when(!atp_en && access_fault) {
          state := s_access_fault
        }
      }
      ptw_level := (pageTableLevels - 1).U
    }
    is(s_ptw_req) {
      when(io.ptw.req.fire) {
        state := s_ptw_resp
      }
    }
    is(s_ptw_resp) {
      when(io.ptw.resp.fire) {
        when(ptw_complete) {
          state := s_ptw_complete
        }.otherwise {
          state     := s_ptw_req
          ptw_level := ptw_level - 1.U
        }
      }
    }
    is(s_ptw_complete) { // need one more cycle to avoid comb loop
      when(io.out.req.fire || page_fault) {
        state := s_idle
      }
    }
    is(s_access_fault) {
      when(io.in.resp.fire) {
        state := s_idle
      }
    }
  }

  // ptw address to access page table
  val l2_addr = Wire(UInt(paddrLen.W))
  val l1_addr = Wire(UInt(paddrLen.W))
  val l0_addr = Wire(UInt(paddrLen.W))

  l2_addr := Cat(io.satp_ppn, in_vaddr.vpn2, 0.U(3.W))
  l1_addr := Cat(ptw_pte_reg.ppn, in_vaddr.vpn1, 0.U(3.W))
  l0_addr := Cat(ptw_pte_reg.ppn, in_vaddr.vpn0, 0.U(3.W))

  // ptw memory access port
  io.ptw.req.bits := 0.U.asTypeOf(new CachePortReq)
  io.ptw.req.bits.addr := MuxLookup(
    ptw_level,
    0.U,
    Array(
      2.U -> l2_addr,
      1.U -> l1_addr,
      0.U -> l0_addr
    )
  )
  io.ptw.req.valid  := (state === s_ptw_req)
  io.ptw.resp.ready := (state === s_ptw_resp)

  val prv   = HoldUnless(io.prv, state === s_idle)
  val pte   = Mux(state === s_idle, tlb_pte, ptw_pte_reg)
  val level = Mux(state === s_idle, tlb_level, ptw_level)

  // TLB refill
  tlb.io.wen    := (state === s_ptw_complete) && !page_fault
  tlb.io.wvaddr := in_vaddr
  tlb.io.wpte   := ptw_pte_reg
  tlb.io.wlevel := ptw_level

  // check page fault for pte
  val pf1 = WireDefault(false.B)
  val pf2 = WireDefault(false.B)
  val pf3 = WireDefault(false.B)
  val pf4 = WireDefault(false.B)
  val pf5 = WireDefault(false.B)
  when(!pte.flag.v || (!pte.flag.r && pte.flag.w)) {
    pf1 := true.B
  }
  when(isLeaf(pte)) {
    when(!pte.flag.a) {
      pf2 := true.B
    }
    when(prv === PRV.U.U && !pte.flag.u) {
      pf3 := true.B
    }
    if (p(IsITLB)) {
      when(!pte.flag.x) {
        pf4 := true.B
      }
    }
    if (p(IsDTLB)) {
      when(in_req_bits.wen && (!pte.flag.w || !pte.flag.r || !pte.flag.d)) {
        pf4 := true.B
      }
    }
    when(state === s_ptw_complete) {
      when((ptw_level === 2.U && Cat(pte.ppn1, pte.ppn0) =/= 0.U) || (ptw_level === 1.U && pte.ppn0 =/= 0.U)) {
        pf5 := true.B // misaligned superpage, can only occur during PTW
      }
    }
  }
  page_fault := pf1 || pf2 || pf3 || pf4 || pf5

  // physical address for out req port
  val paddr = Wire(new Sv39PhysAddr)
  paddr.offset := in_vaddr.offset
  paddr.ppn0   := Mux(level > 0.U, in_vaddr.vpn0, pte.ppn0)
  paddr.ppn1   := Mux(level > 1.U, in_vaddr.vpn1, pte.ppn1)
  paddr.ppn2   := pte.ppn2

  // forward in req port to out req port
  io.in.req.ready := (state === s_idle) && (io.out.req.ready || access_fault || (atp_en && (!tlb.io.hit || page_fault)))
  io.out.req.valid := ((state === s_idle) && ((tlb.io.hit && !page_fault) || (!atp_en && !access_fault)) && io.in.req.valid) ||
    (state === s_ptw_complete && !page_fault)
  io.out.req.bits := in_req_bits
  when(atp_en) {
    io.out.req.bits.addr := paddr.asTypeOf(UInt(vaddrLen.W))
  }

  // forward out resp port to in resp port
  val page_fault_reg = BoolStopWatch(
    page_fault && atp_en && ((state === s_idle && tlb.io.hit && io.in.req.fire) || state === s_ptw_complete),
    io.in.resp.fire
  )
  io.in.resp.bits              := io.out.resp.bits
  io.in.resp.bits.page_fault   := page_fault_reg
  io.in.resp.bits.access_fault := (state === s_access_fault)
  io.in.resp.valid             := io.out.resp.valid || io.in.resp.bits.page_fault || io.in.resp.bits.access_fault
  io.out.resp.ready            := io.in.resp.ready

  val debug_name = if (p(IsITLB)) "IPP" else "DPP"
  if (debugPortProxy) {
    when(io.in.req.fire) {
      printf(cf"${DebugTimer()} [$debug_name] [in -req ] ${io.in.req.bits}\n")
    }
    when(io.ptw.req.fire) {
      printf(cf"${DebugTimer()} [$debug_name] [ptw-req ] ${io.ptw.req.bits}\n")
    }
    when(io.out.req.fire) {
      printf(cf"${DebugTimer()} [$debug_name] [out-req ] ${io.out.req.bits}\n")
    }
    when(io.in.resp.fire) {
      printf(cf"${DebugTimer()} [$debug_name] [in -resp] ${io.in.resp.bits}\n")
    }
    when(io.ptw.resp.fire) {
      printf(cf"${DebugTimer()} [$debug_name] [ptw-resp] ${io.ptw.resp.bits}\n")
    }
    when(io.out.resp.fire) {
      printf(cf"${DebugTimer()} [$debug_name] [out-resp] ${io.out.resp.bits}\n")
    }
  }

  if (debugTLB) {
    when(tlb.io.wen) {
      printf(cf"${DebugTimer()} [$debug_name] [tlb-w] vaddr=$in_vaddr pte=$ptw_pte_reg level=$ptw_level\n")
    }
    when(io.in.req.fire && atp_en) {
      printf(
        cf"${DebugTimer()} [$debug_name] [tlb-r] vaddr=${tlb.io.vaddr} pte=$tlb_pte level=$tlb_level hit=${tlb.io.hit} pf=$page_fault\n"
      )
    }
  }
}
