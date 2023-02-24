import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

class FDPacket(implicit p: Parameters) extends CherrySpringsBundle {
  val pc         = UInt(xLen.W)
  val instr      = UInt(32.W)
  val valid      = Bool()
  val page_fault = Bool()
}

class DXPacket(implicit p: Parameters) extends CherrySpringsBundle {
  val uop              = new MicroOp
  val rs1_data         = UInt(xLen.W)
  val rs2_data         = UInt(xLen.W)
  val rs2_data_from_rf = UInt(xLen.W)
}

class XWPacket(implicit p: Parameters) extends CherrySpringsBundle {
  val uop     = new MicroOp
  val rd_data = UInt(xLen.W)
}

class PipelineReg[T <: Bundle](packet: T)(implicit p: Parameters) extends CherrySpringsModule {
  val io = IO(new Bundle {
    val in    = Input(packet)
    val out   = Output(packet)
    val en    = Input(Bool())
    val flush = Input(Bool())
  })

  val reg = RegEnable(Mux(io.flush, 0.U.asTypeOf(packet), io.in), 0.U.asTypeOf(packet), io.en)
  io.out := reg
}

class JmpPacket(implicit p: Parameters) extends CherrySpringsBundle {
  val valid  = Bool()
  val target = UInt(xLen.W)
}

class ExternalInterruptIO(implicit p: Parameters) extends CherrySpringsBundle {
  val mtip = Input(Bool())
  val msip = Input(Bool())
  val meip = Input(Bool())
  val seip = Input(Bool())
}

class CachePortReq(implicit p: Parameters) extends CherrySpringsBundle {
  val addr  = Output(UInt(vaddrLen.W))
  val wdata = Output(UInt(xLen.W))
  val wmask = Output(UInt((xLen / 8).W))
  val wen   = Output(Bool())
}

class CachePortResp(implicit p: Parameters) extends CherrySpringsBundle {
  val rdata      = Output(UInt(xLen.W))
  val page_fault = Output(Bool())
}

class CachePortIO(implicit p: Parameters) extends CherrySpringsBundle {
  val req  = Decoupled(new CachePortReq)
  val resp = Flipped(Decoupled(new CachePortResp))
}

class AddrTransPortReq(implicit p: Parameters) extends CherrySpringsBundle {
  val vaddr = Output(new Sv39VirtAddr)
  val wen   = Output(Bool())
}

class AddrTransPortResp(implicit p: Parameters) extends CherrySpringsBundle {
  val paddr        = Output(new Sv39PhysAddr)
  val page_fault   = Output(Bool())
  val tlb_hit      = Output(Bool())
  val ptw_complete = Output(Bool())
}

class AddrTransPortIO(implicit p: Parameters) extends CherrySpringsBundle {
  val req  = Valid(new AddrTransPortReq)
  val resp = Flipped(Valid(new AddrTransPortResp))
}

class Sv39VirtAddr(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val vpn2   = UInt(vpn2Len.W)
  val vpn1   = UInt(vpn1Len.W)
  val vpn0   = UInt(vpn0Len.W)
  val offset = UInt(offsetLen.W)

  def vpn()    = Cat(vpn2, vpn1, vpn0)
  def vpn2mb() = Cat(vpn2, vpn1)
}

class Sv39PhysAddr(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val ppn2   = UInt(ppn2Len.W)
  val ppn1   = UInt(ppn1Len.W)
  val ppn0   = UInt(ppn0Len.W)
  val offset = UInt(offsetLen.W)

  def ppn() = Cat(ppn2, ppn1, ppn0)
}

class Sv39PTEFlag(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val rsw = UInt(2.W)
  val d   = Bool()
  val a   = Bool()
  val g   = Bool()
  val u   = Bool()
  val x   = Bool()
  val w   = Bool()
  val r   = Bool()
  val v   = Bool()
}

class Sv39PTE(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val ppn2 = UInt(ppn2Len.W)
  val ppn1 = UInt(ppn1Len.W)
  val ppn0 = UInt(ppn0Len.W)
  val flag = new Sv39PTEFlag

  def ppn() = Cat(ppn2, ppn1, ppn0)
}

class TLB4KBEntry(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val flag = new Sv39PTEFlag
  val vpn2 = UInt(vpn2Len.W)
  val vpn1 = UInt(vpn1Len.W)
  val vpn0 = UInt(vpn0Len.W)
  val ppn2 = UInt(ppn2Len.W)
  val ppn1 = UInt(ppn1Len.W)
  val ppn0 = UInt(ppn0Len.W)

  def vpn() = Cat(vpn2, vpn1, vpn0)
}

class TLB2MBEntry(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val flag = new Sv39PTEFlag
  val vpn2 = UInt(vpn2Len.W)
  val vpn1 = UInt(vpn1Len.W)
  val ppn2 = UInt(ppn2Len.W)
  val ppn1 = UInt(ppn1Len.W)

  def vpn2mb() = Cat(vpn2, vpn1)
}

class TLB1GBEntry(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val flag = new Sv39PTEFlag
  val vpn  = UInt((vpnLen - vpn0Len - vpn1Len).W)
  val ppn  = UInt((ppnLen - ppn0Len - ppn1Len).W)
}
