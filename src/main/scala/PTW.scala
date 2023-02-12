import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._

case object IsIPTW extends Field[Boolean]
case object IsDPTW extends Field[Boolean]

class AddrTransPortReq(implicit p: Parameters) extends CherrySpringsBundle {
  val vaddr = Output(UInt(vaddrLen.W))
  val wen   = Output(Bool())
}

class AddrTransPortResp(implicit p: Parameters) extends CherrySpringsBundle {
  val paddr      = Output(UInt(paddrLen.W))
  val page_fault = Output(Bool())
}

class AddrTransPortIO(implicit p: Parameters) extends CherrySpringsBundle {
  val req  = Decoupled(new AddrTransPortReq)
  val resp = Flipped(Decoupled(new AddrTransPortResp))
}

class PTW(implicit p: Parameters) extends CherrySpringsModule {
  require(xLen == 64)
  val PageTableLevels = 3
  val is_iptw         = p(IsIPTW)
  val is_dptw         = p(IsDPTW)

  val io = IO(new Bundle {
    val prv        = Input(UInt(2.W))
    val satp_ppn   = Input(UInt(44.W))
    val addr_trans = Flipped(new AddrTransPortIO)
    val ptw        = new CachePortIO
  })

  val s_req :: s_resp :: s_ptw_req :: s_ptw_resp :: Nil = Enum(4)
  val state                                             = RegInit(s_req)

  val pt_level   = RegInit(0.U(2.W))
  val vaddr      = RegInit(0.U(vaddrLen.W))
  val req_wen    = RegInit(false.B)
  val pt_rdata   = RegInit(0.U(xLen.W))
  val pte        = io.ptw.resp.bits.rdata
  val pte_v      = pte(0)
  val pte_r      = pte(1)
  val pte_w      = pte(2)
  val pte_x      = pte(3)
  val pte_u      = pte(4)
  val pte_a      = pte(6)
  val pte_d      = pte(7)
  val is_leaf    = pte_v && (pte_r || pte_x)
  val page_fault = RegInit(false.B)

  assert(pt_level < PageTableLevels.U)

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
        when(!pte_v || is_leaf || (pt_level === 0.U)) {
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
    pt_level   := (PageTableLevels - 1).U
    vaddr      := io.addr_trans.req.bits.vaddr
    req_wen    := io.addr_trans.req.bits.wen
    page_fault := false.B
  }

  when(io.ptw.resp.fire) {
    pt_rdata := io.ptw.resp.bits.rdata
    when(!pte_v || (!pte_r && pte_w)) {
      page_fault := true.B
    }
    when((pt_level === 0.U) && !pte_r && !pte_x) {
      page_fault := true.B
    }
    when(is_leaf) {
      when(io.prv === PRV.U.U && !pte_u) {
        page_fault := true.B
      }
      if (is_iptw) {
        when(!pte_x) {
          page_fault := true.B
        }
      }
      if (is_dptw) {
        when(req_wen && !pte_w) {
          page_fault := true.B
        }
        when(!req_wen && !pte_r) {
          page_fault := true.B
        }
      }
      // misaligned superpage
      when((pt_level === 2.U && pte(27, 10) =/= 0.U) || (pt_level === 1.U && pte(18, 10) =/= 0.U)) {
        page_fault := true.B
      }
      when(!pte_a) {
        page_fault := true.B
      }
      if (is_dptw) {
        when(req_wen && !pte_d) {
          page_fault := true.B
        }
      }
    }
  }

  val l2_addr = Cat(io.satp_ppn, vaddr(38, 30), 0.U(3.W))
  val l1_addr = Cat(pt_rdata(53, 10), vaddr(29, 21), 0.U(3.W))
  val l0_addr = Cat(pt_rdata(53, 10), vaddr(20, 12), 0.U(3.W))

  io.addr_trans.req.ready            := (state === s_req)
  io.addr_trans.resp.valid           := (state === s_resp)
  io.addr_trans.resp.bits.page_fault := page_fault
  io.addr_trans.resp.bits.paddr := MuxLookup(
    pt_level,
    0.U,
    Array(
      2.U -> Cat(pt_rdata(29, 28), vaddr(29, 0)),
      1.U -> Cat(pt_rdata(29, 19), vaddr(20, 0)),
      0.U -> Cat(pt_rdata(29, 10), vaddr(11, 0))
    )
  ) // assume 32-bit paddr

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
      printf("%d [PTW] vaddr=%x satp_ppn=%x\n", DebugTimer(), io.addr_trans.req.bits.vaddr, io.satp_ppn)
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
        "%d [PTW] paddr=%x page_fault=%x\n",
        DebugTimer(),
        io.addr_trans.resp.bits.paddr,
        io.addr_trans.resp.bits.page_fault
      )
    }
  }
}
