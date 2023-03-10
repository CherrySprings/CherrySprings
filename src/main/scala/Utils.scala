import chisel3._
import chisel3.util._

object DebugTimer {
  def apply() = {
    val c = RegInit(0.U(64.W))
    c := c + 1.U
    c
  }
}

object MaskExpand {
  def apply(x: UInt) = Cat(x.asBools.map(Fill(8, _)).reverse)
}

object MaskData {
  def apply(old_data: UInt, new_data: UInt, mask: UInt) = {
    (new_data & mask) | (old_data & (~mask).asUInt)
  }
}

object SignExt32_64 {
  def apply(x: UInt): UInt = Cat(Fill(32, x(31)), x)
}

object ZeroExt32_64 {
  def apply(x: UInt): UInt = Cat(Fill(32, 0.U), x)
}

object SignExt39_64 {
  def apply(x: UInt): UInt = Cat(Fill(25, x(38)), x)
}

object BoolStopWatch {
  def apply(start: Bool, stop: Bool, start_high_priority: Boolean = false) = {
    val r = RegInit(false.B)
    if (start_high_priority) {
      when(stop) { r := false.B }
      when(start) { r := true.B }
    } else {
      when(start) { r := true.B }
      when(stop) { r := false.B }
    }
    r
  }
}

object HoldUnless {
  def apply[T <: Data](x: T, en: Bool): T = {
    Mux(en, x, RegEnable(x, 0.U.asTypeOf(x), en))
  }
}
