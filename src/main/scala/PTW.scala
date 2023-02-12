import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

case object IsIPTW extends Field[Boolean]
case object IsDPTW extends Field[Boolean]

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
  val asidLen         = 16
  val flagLen         = 8
}

class AddrTransPortReq(implicit p: Parameters) extends CherrySpringsBundle {
  val vaddr = Output(new Sv39VirtAddr)
  val wen   = Output(Bool())
}

class AddrTransPortResp(implicit p: Parameters) extends CherrySpringsBundle {
  val paddr      = Output(new Sv39PhysAddr)
  val page_fault = Output(Bool())
}

class AddrTransPortIO(implicit p: Parameters) extends CherrySpringsBundle {
  val req  = Decoupled(new AddrTransPortReq)
  val resp = Flipped(Decoupled(new AddrTransPortResp))
}

class PTW(implicit p: Parameters) extends CherrySpringsModule with Sv39Parameters {
  require(xLen == 64)

  val is_iptw = p(IsIPTW)
  val is_dptw = p(IsDPTW)

  val io = IO(new Bundle {
    val prv        = Input(UInt(2.W))
    val satp_ppn   = Input(UInt(44.W))
    val addr_trans = Flipped(new AddrTransPortIO)
    val ptw        = new CachePortIO
  })

  val s_req :: s_resp :: s_ptw_req :: s_ptw_resp :: Nil = Enum(4)
  val state                                             = RegInit(s_req)

  val pt_level = RegInit(0.U(2.W))
  val vaddr    = RegInit(0.U.asTypeOf(new Sv39VirtAddr))
  val req_wen  = RegInit(false.B)
  val pte      = Wire(new Sv39PTE)
  val pte_reg  = RegInit(0.U.asTypeOf(new Sv39PTE))
  pte := io.ptw.resp.bits.rdata.asTypeOf(new Sv39PTE)

  val is_leaf    = pte.v && (pte.r || pte.x)
  val page_fault = RegInit(false.B)

  assert(pt_level < pageTableLevels.U)

  switch(state) {
    is(s_req) {
      when(io.addr_trans.req.fire) {
        state := s_ptw_req
      }
    }
    is(s_ptw_req) {
      when(io.ptw.req.fire) {
        state := s_ptw_resp
      }
    }
    is(s_ptw_resp) {
      when(io.ptw.resp.fire) {
        when(!pte.v || is_leaf || (pt_level === 0.U)) {
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

  when(io.addr_trans.req.fire) {
    pt_level   := (pageTableLevels - 1).U
    vaddr      := io.addr_trans.req.bits.vaddr
    req_wen    := io.addr_trans.req.bits.wen
    page_fault := false.B
  }

  when(io.ptw.resp.fire) {
    pte_reg := io.ptw.resp.bits.rdata.asTypeOf(new Sv39PTE)
    when(!pte.v || (!pte.r && pte.w)) {
      page_fault := true.B
    }
    when((pt_level === 0.U) && !pte.r && !pte.x) {
      page_fault := true.B
    }
    when(is_leaf) {
      when(io.prv === PRV.U.U && !pte.u) {
        page_fault := true.B
      }
      if (is_iptw) {
        when(!pte.x) {
          page_fault := true.B
        }
      }
      if (is_dptw) {
        when(req_wen && !pte.w) {
          page_fault := true.B
        }
        when(!req_wen && !pte.r) {
          page_fault := true.B
        }
      }
      // misaligned superpage
      when(
        (pt_level === 2.U && pte.ppn(ppn0Len + ppn1Len - 1, 0) =/= 0.U) ||
          (pt_level === 1.U && pte.ppn(ppn0Len - 1, 0) =/= 0.U)
      ) {
        page_fault := true.B
      }
      when(!pte.a) {
        page_fault := true.B
      }
      if (is_dptw) {
        when(req_wen && !pte.d) {
          page_fault := true.B
        }
      }
    }
  }

  val l2_addr = Wire(UInt(paddrLen.W))
  val l1_addr = Wire(UInt(paddrLen.W))
  val l0_addr = Wire(UInt(paddrLen.W))

  l2_addr := Cat(io.satp_ppn, vaddr.vpn2, 0.U(3.W))
  l1_addr := Cat(pte_reg.ppn, vaddr.vpn1, 0.U(3.W))
  l0_addr := Cat(pte_reg.ppn, vaddr.vpn0, 0.U(3.W))

  io.addr_trans.req.ready              := (state === s_req)
  io.addr_trans.resp.valid             := (state === s_resp)
  io.addr_trans.resp.bits.page_fault   := page_fault
  io.addr_trans.resp.bits.paddr.offset := vaddr.offset
  io.addr_trans.resp.bits.paddr.ppn0   := Mux(pt_level > 0.U, vaddr.vpn0, pte_reg.ppn0())
  io.addr_trans.resp.bits.paddr.ppn1   := Mux(pt_level > 1.U, vaddr.vpn1, pte_reg.ppn1())
  io.addr_trans.resp.bits.paddr.ppn2   := pte_reg.ppn2()

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

  if (debugPTW) {
    when(io.addr_trans.req.fire) {
      printf(cf"${DebugTimer()} [PTW] vaddr=${io.addr_trans.req.bits.vaddr} satp_ppn=${io.satp_ppn}\n")
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
        cf"${DebugTimer()} [PTW] paddr=${io.addr_trans.resp.bits.paddr}" +
          cf"page_fault=${io.addr_trans.resp.bits.page_fault}\n"
      )
    }
  }
}
