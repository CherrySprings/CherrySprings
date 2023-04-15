import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._
import chisel3.util.RegEnable

class TLTraceBuffer(implicit p: Parameters) extends LazyModule {
  val node = TLIdentityNode()

  lazy val module = new TLTraceBufferModule(this)
}

class TLTraceBufferModule(outer: TLTraceBuffer) extends LazyModuleImp(outer) {
  val (tl_in, _)  = outer.node.in.head
  val (tl_out, _) = outer.node.out.head

  val in_a_scan_bits   = RegEnable(tl_in.a.bits, 0.U.asTypeOf(tl_in.a.bits.cloneType), tl_in.a.fire)
  val in_a_scan_valid  = BoolStopWatch(tl_in.a.fire, tl_out.a.fire, start_high_priority = true)
  val out_b_scan_bits  = RegEnable(tl_out.b.bits, 0.U.asTypeOf(tl_out.b.bits.cloneType), tl_out.b.fire)
  val out_b_scan_valid = BoolStopWatch(tl_out.b.fire, tl_in.b.fire, start_high_priority = true)
  val in_c_scan_bits   = RegEnable(tl_in.c.bits, 0.U.asTypeOf(tl_in.c.bits.cloneType), tl_in.c.fire)
  val in_c_scan_valid  = BoolStopWatch(tl_in.c.fire, tl_out.c.fire, start_high_priority = true)
  val out_d_scan_bits  = RegEnable(tl_out.d.bits, 0.U.asTypeOf(tl_out.d.bits.cloneType), tl_out.d.fire)
  val out_d_scan_valid = BoolStopWatch(tl_out.d.fire, tl_in.d.fire, start_high_priority = true)
  val in_e_scan_bits   = RegEnable(tl_in.e.bits, 0.U.asTypeOf(tl_in.e.bits.cloneType), tl_in.e.fire)
  val in_e_scan_valid  = BoolStopWatch(tl_in.e.fire, tl_out.e.fire, start_high_priority = true)

  tl_out.a.valid := in_a_scan_valid
  tl_out.a.bits  := in_a_scan_bits
  tl_in.a.ready  := !in_a_scan_valid || tl_out.a.fire
  tl_in.b.valid  := out_b_scan_valid
  tl_in.b.bits   := out_b_scan_bits
  tl_out.b.ready := !out_b_scan_valid || tl_in.b.fire
  tl_out.c.valid := in_c_scan_valid
  tl_out.c.bits  := in_c_scan_bits
  tl_in.c.ready  := !in_c_scan_valid || tl_out.c.fire
  tl_in.d.valid  := out_d_scan_valid
  tl_in.d.bits   := out_d_scan_bits
  tl_out.d.ready := !out_d_scan_valid || tl_in.d.fire
  tl_out.e.valid := in_e_scan_valid
  tl_out.e.bits  := in_e_scan_bits
  tl_in.e.ready  := !in_e_scan_valid || tl_out.e.fire
}

object TLTraceBuffer {
  def apply()(implicit p: Parameters): TLNode = {
    val trace_buffer = LazyModule(new TLTraceBuffer)
    trace_buffer.node
  }
}
