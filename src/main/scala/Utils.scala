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

object RegMap {
  def apply(addr: UInt, reg: UInt, wfn: UInt => UInt = (x => x)) = (addr, (reg, wfn))

  def access(
    mapping: Map[UInt, (UInt, UInt => UInt)],
    addr:    UInt,
    rdata:   UInt,
    wdata:   UInt,
    wmask:   UInt,
    wen:     Bool
  ): Unit = {
    mapping.map {
      case (a, (r, wfn)) => {
        when(addr === a) {
          rdata := r
        }
        if (wfn != null) {
          when(addr === a && wen) {
            r := wfn(MaskData(r, wdata, wmask))
          }
        }
      }
    }
  }
}

object MaskedRegMap {
  // Unwritable is null for part of read-only CSR registers
  def Unwritable = null

  // default write function
  def NoSideEffect: UInt => UInt = (x => x)

  // default r/w mask
  def DefaultMask = Fill(64, 1.U)

  def apply(addr: UInt, reg: UInt, wmask: UInt = DefaultMask, wfn: UInt => UInt = (x => x), rmask: UInt = DefaultMask) =
    (addr, (reg, wmask, wfn, rmask))

  def access(
    mapping: Map[UInt, (UInt, UInt, UInt => UInt, UInt)],
    addr:    UInt,
    rdata:   UInt,
    wdata:   UInt,
    wen:     Bool
  ): Unit = {
    mapping.map {
      case (a, (r, wm, wfn, rm)) => {
        when(addr === a) {
          rdata := r & rm
        }
        if (wfn != null) {
          when(addr === a && wen) {
            r := wfn(MaskData(r, wdata, wm))
          }
        }
      }
    }
  }
}
