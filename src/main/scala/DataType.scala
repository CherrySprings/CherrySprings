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

class Sv39VirtAddr(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val vpn2   = UInt(vpn2Len.W)
  val vpn1   = UInt(vpn1Len.W)
  val vpn0   = UInt(vpn0Len.W)
  val offset = UInt(offsetLen.W)
}

class Sv39PhysAddr(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val ppn2   = UInt(ppn2Len.W)
  val ppn1   = UInt(ppn1Len.W)
  val ppn0   = UInt(ppn0Len.W)
  val offset = UInt(offsetLen.W)
}

class Sv39PTE(implicit p: Parameters) extends CherrySpringsBundle with Sv39Parameters {
  val ppn = UInt(ppnLen.W)
  val rsw = UInt(2.W)
  val d   = Bool()
  val a   = Bool()
  val g   = Bool()
  val u   = Bool()
  val x   = Bool()
  val w   = Bool()
  val r   = Bool()
  val v   = Bool()

  def ppn0() = ppn(ppn0Len - 1, 0)
  def ppn1() = ppn(ppn0Len + ppn1Len - 1, ppn0Len)
  def ppn2() = ppn(ppn0Len + ppn1Len + ppn2Len - 1, ppn0Len + ppn1Len)
}
