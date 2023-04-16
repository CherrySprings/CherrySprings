import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config._
import testchipip._

abstract class SoCAbstract(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val node: Option[TLIdentityNode]
  override lazy val module = new SoCAbstractImp(this)
}

class SoCAbstractImp[+L <: SoCAbstract](l: L) extends LazyModuleImp(l) with HasCherrySpringsParameters {
  val io = IO(new Bundle {
    val in       = if (enableSerdes) Some(Flipped(Decoupled(UInt(tlSerWidth.W)))) else None
    val out      = if (enableSerdes) Some(Decoupled(UInt(tlSerWidth.W))) else None
    val io_clock = if (enableSerdes) Some(Input(Clock())) else None
    val io_reset = if (enableSerdes) Some(Input(Bool())) else None
    val intr     = Input(new ExternalInterrupt)
  })
}

class SoCImp(implicit p: Parameters) extends SoCAbstract {
  val icache  = LazyModule(new ICache)
  val dcache  = LazyModule(new DCache)
  val uncache = LazyModule(new Uncache)
  val xbar    = LazyModule(new TLXbar(policy = TLArbiter.highestIndexFirst))
  val node    = Some(TLIdentityNode())

  // don't modify order of following nodes
  xbar.node := icache.node // 0 (must be the lowest to avoid deadlock)
  xbar.node := TLWidthWidget(8) := uncache.node // 1
  xbar.node := dcache.node // 2 (must be the highest to ensure correct b channel routing for Sesdes)
  node.get  := xbar.node

  override lazy val module = new SoCAbstractImp(this) {
    val core = Module(new Core)

    // interrupt input
    core.io.intr := io.intr

    // instruction cache
    icache.module.io.cache   <> core.io.imem
    icache.module.io.fence_i := core.io.fence_i

    // connect ptw port to data cache
    val xbar = Module(new CachePortXBarNto1(3))
    xbar.io.in(0)          <> core.io.dmem
    xbar.io.in(1)          <> core.io.iptw
    xbar.io.in(2)          <> core.io.dptw
    dcache.module.io.cache <> xbar.io.out

    // uncache
    uncache.module.io.in <> core.io.uncache

    if (debugBus) {
      val (tl, edge) = node.get.out.head
      when(tl.a.fire) {
        printf(cf"${DebugTimer()} [TL] [Hart ${hartID} - a] ${tl.a.bits}\n")
      }
      when(tl.b.fire) {
        printf(cf"${DebugTimer()} [TL] [Hart ${hartID} - b] ${tl.b.bits}\n")
      }
      when(tl.c.fire) {
        printf(cf"${DebugTimer()} [TL] [Hart ${hartID} - c] ${tl.c.bits}\n")
      }
      when(tl.d.fire) {
        printf(cf"${DebugTimer()} [TL] [Hart ${hartID} - d] ${tl.d.bits}\n")
      }
      when(tl.e.fire) {
        printf(cf"${DebugTimer()} [TL] [Hart ${hartID} - e] ${tl.e.bits}\n")
      }
    }
  }
}

class SoC(implicit p: Parameters) extends SoCAbstract {
  val soc_imp = LazyModule(new SoCImp)

  val node = None

  // serdes
  val beatBytes = 32
  val serdes = LazyModule(
    new TLSerdes(
      w = tlSerWidth,
      params = Seq(
        TLSlaveParameters.v1(
          address            = Seq(AddressSet(BigInt("00000000", 16), BigInt("ffffffff", 16))),
          regionType         = RegionType.CACHED,
          supportsAcquireT   = TransferSizes(1, beatBytes),
          supportsAcquireB   = TransferSizes(1, beatBytes),
          supportsArithmetic = TransferSizes(1, beatBytes),
          supportsLogical    = TransferSizes(1, beatBytes),
          supportsGet        = TransferSizes(1, beatBytes),
          supportsPutFull    = TransferSizes(1, beatBytes),
          supportsPutPartial = TransferSizes(1, beatBytes),
          supportsHint       = TransferSizes(1, beatBytes)
        )
      ),
      beatBytes = beatBytes,
      endSinkId = 42 // magic number
    )
  )
  serdes.node := TLTraceBuffer() := soc_imp.node.get

  override lazy val module = new SoCAbstractImp(this) {
    val in_fifo  = Module(new AsyncQueue(UInt(tlSerWidth.W)))
    val out_fifo = Module(new AsyncQueue(UInt(tlSerWidth.W)))
    in_fifo.io.enq        <> io.in.get
    in_fifo.io.enq_clock  := io.io_clock.get
    in_fifo.io.enq_reset  := io.io_reset.get
    in_fifo.io.deq        <> serdes.module.io.ser.head.in
    in_fifo.io.deq_clock  := clock
    in_fifo.io.deq_reset  := reset
    out_fifo.io.enq       <> serdes.module.io.ser.head.out
    out_fifo.io.enq_clock := clock
    out_fifo.io.enq_reset := reset
    out_fifo.io.deq       <> io.out.get
    out_fifo.io.deq_clock := io.io_clock.get
    out_fifo.io.deq_reset := io.io_reset.get

    // interrupt input (sync_dff triggered by on-chip clock & reset)
    val sync_dff = RegInit(VecInit(Seq.fill(3)(0.U.asTypeOf(new ExternalInterrupt))))
    sync_dff(0)            := io.intr
    sync_dff(1)            := sync_dff(0)
    sync_dff(2)            := sync_dff(1)
    soc_imp.module.io.intr := sync_dff(2)
  }
}
