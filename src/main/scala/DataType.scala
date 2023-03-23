import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

class FDPacket(implicit p: Parameters) extends CherrySpringsBundle {
  val pc           = UInt(xLen.W)
  val instr        = UInt(32.W)
  val valid        = Bool()
  val page_fault   = Bool()
  val access_fault = Bool()

  override def toPrintable: Printable = {
    cf"v=$valid pc=$pc%x instr=$instr%x pf=$page_fault af=$access_fault"
  }
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

  override def toPrintable: Printable = {
    cf"addr=$addr%x wdata=$wdata%x wmask=$wmask%x wen=$wen"
  }
}

class CachePortResp(implicit p: Parameters) extends CherrySpringsBundle {
  val rdata        = Output(UInt(xLen.W))
  val page_fault   = Output(Bool())
  val access_fault = Output(Bool())

  override def toPrintable: Printable = {
    cf"rdata=$rdata%x pf=$page_fault af=$access_fault"
  }
}

class CachePortIO(implicit p: Parameters) extends CherrySpringsBundle {
  val req  = Decoupled(new CachePortReq)
  val resp = Flipped(Decoupled(new CachePortResp))
}

class Sv39VirtAddr(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val vpn2   = UInt(vpn2Len.W)
  val vpn1   = UInt(vpn1Len.W)
  val vpn0   = UInt(vpn0Len.W)
  val offset = UInt(offsetLen.W)

  def vpn()    = Cat(vpn2, vpn1, vpn0)
  def vpn2mb() = Cat(vpn2, vpn1)
  def vpn1gb() = vpn2

  override def toPrintable: Printable = {
    cf"{vpn2=$vpn2%x vpn1=$vpn1%x vpn0=$vpn0%x offset=$offset%x}"
  }
}

class Sv39PhysAddr(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val ppn2   = UInt(ppn2Len.W)
  val ppn1   = UInt(ppn1Len.W)
  val ppn0   = UInt(ppn0Len.W)
  val offset = UInt(offsetLen.W)

  def ppn() = Cat(ppn2, ppn1, ppn0)

  override def toPrintable: Printable = {
    cf"{ppn2=$ppn2%x ppn1=$ppn1%x ppn0=$ppn0%x offset=$offset%x}"
  }
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

  override def toPrintable: Printable = {
    cf"$rsw%b$d$a$g$u$x$w$r$v"
  }
}

class Sv39PTE(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val ppn2 = UInt(ppn2Len.W)
  val ppn1 = UInt(ppn1Len.W)
  val ppn0 = UInt(ppn0Len.W)
  val flag = new Sv39PTEFlag

  def ppn() = Cat(ppn2, ppn1, ppn0)

  override def toPrintable: Printable = {
    cf"{ppn2=$ppn2%x ppn1=$ppn1%x ppn0=$ppn0%x flag=$flag}"
  }
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
  val vpn2 = UInt(vpn2Len.W)
  val ppn2 = UInt(ppn2Len.W)

  def vpn1gb() = vpn2
}

class CacheEntry(implicit p: Parameters) extends CherrySpringsBundle {
  val tag  = UInt((paddrLen - (5 + log2Up(cacheNumSets))).W)
  val data = UInt(256.W)

  def len() = this.getWidth
}
