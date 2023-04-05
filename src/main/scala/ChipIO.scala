import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class ChipTLBundleA(implicit p: Parameters) extends CherrySpringsBundle {
  val opcode  = UInt(3.W)
  val param   = UInt(3.W)
  val size    = UInt(tlSizeBits.W)
  val source  = UInt(tlSourceBits.W)
  val address = UInt(tlAddressBits.W)
  val mask    = UInt((tlDataBits / 8).W)
  val data    = UInt(tlDataBits.W)
  val corrupt = Bool()
}

class ChipTLBundleB(implicit p: Parameters) extends CherrySpringsBundle {
  val opcode  = UInt(3.W)
  val param   = UInt(3.W)
  val size    = UInt(tlSizeBits.W)
  val source  = UInt(tlSourceBits.W)
  val address = UInt(tlAddressBits.W)
  val mask    = UInt((tlDataBits / 8).W)
  val data    = UInt(tlDataBits.W)
  val corrupt = Bool()
}

class ChipTLBundleC(implicit p: Parameters) extends CherrySpringsBundle {
  val opcode  = UInt(3.W)
  val param   = UInt(3.W)
  val size    = UInt(tlSizeBits.W)
  val source  = UInt(tlSourceBits.W)
  val address = UInt(tlAddressBits.W)
  val data    = UInt(tlDataBits.W)
  val corrupt = Bool()
}

class ChipTLBundleD(implicit p: Parameters) extends CherrySpringsBundle {
  val opcode  = UInt(3.W)
  val param   = UInt(2.W)
  val size    = UInt(tlSizeBits.W)
  val source  = UInt(tlSourceBits.W)
  val sink    = UInt(tlSinkBits.W)
  val denied  = Bool()
  val data    = UInt(tlDataBits.W)
  val corrupt = Bool()
}

class ChipTLBundleE(implicit p: Parameters) extends CherrySpringsBundle {
  val sink = UInt(tlSinkBits.W)
}

class ChipTLBundle(implicit p: Parameters) extends CherrySpringsBundle {
  val a = Decoupled(new ChipTLBundleA)
  val b = Flipped(Decoupled(new ChipTLBundleB))
  val c = Decoupled(new ChipTLBundleC)
  val d = Flipped(Decoupled(new ChipTLBundleD))
  val e = Decoupled(new ChipTLBundleE)
}

class TLSource(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name        = s"TLSource",
            sourceId    = IdRange(0, 1 << tlSourceBits),
            requestFifo = true
          )
        )
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val (tl, _) = node.out.head
    val io = IO(new Bundle {
      val tl = Flipped(new ChipTLBundle)
    })

    io.tl <> tl
  }
}

class TLSink(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val device    = new SimpleDevice("TLSink", Seq())
  val beatBytes = 8
  val node = TLManagerNode(
    Seq(
      TLSlavePortParameters.v1(
        managers = Seq(
          TLSlaveParameters.v1(
            address            = Seq(AddressSet(BigInt("00000000", 16), BigInt("ffffffff", 16))),
            resources          = device.reg,
            regionType         = RegionType.UNCACHED,
            supportsAcquireT   = TransferSizes(1, 4 * beatBytes),
            supportsAcquireB   = TransferSizes(1, 4 * beatBytes),
            supportsArithmetic = TransferSizes(1, 4 * beatBytes),
            supportsLogical    = TransferSizes(1, 4 * beatBytes),
            supportsGet        = TransferSizes(1, 4 * beatBytes),
            supportsPutFull    = TransferSizes(1, 4 * beatBytes),
            supportsPutPartial = TransferSizes(1, 4 * beatBytes),
            supportsHint       = TransferSizes(1, 4 * beatBytes)
          )
        ),
        beatBytes = beatBytes,
        endSinkId = 1
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val (tl, _) = node.in.head
    val io = IO(new Bundle {
      val tl = new ChipTLBundle
    })

    io.tl <> tl
  }
}
