import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

class CachePortReq(implicit p: Parameters) extends CherrySpringsBundle {
  val addr  = Output(UInt(xLen.W))
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
