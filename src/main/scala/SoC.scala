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
  val clint_int: IntIdentityNode
  val plic_int:  IntIdentityNode
  val node:      Option[TLIdentityNode]
  override lazy val module = new SoCAbstractImp(this)
}

class SoCAbstractImp[+L <: SoCAbstract](l: L) extends LazyModuleImp(l) with HasCherrySpringsParameters {
  val io = IO(new Bundle {
    val in       = if (enableSerdes) Some(Flipped(Decoupled(UInt(tlSerWidth.W)))) else None
    val out      = if (enableSerdes) Some(Decoupled(UInt(tlSerWidth.W))) else None
    val io_clock = if (enableSerdes) Some(Input(Clock())) else None
    val io_reset = if (enableSerdes) Some(Input(Bool())) else None
  })
}

class SoCImp(implicit p: Parameters) extends SoCAbstract {
  val icache  = LazyModule(new ICache)
  val dcache  = LazyModule(new DCache)
  val uncache = LazyModule(new Uncache)
  val xbar    = LazyModule(new TLXbar(policy = TLArbiter.highestIndexFirst))
  val node    = Some(TLIdentityNode())

  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val plic_int_sink  = IntSinkNode(IntSinkPortSimple(2, 1))
  val clint_int      = IntIdentityNode()
  val plic_int       = IntIdentityNode()
  clint_int_sink := clint_int
  plic_int_sink :*= plic_int

  // don't modify order of following nodes
  xbar.node := icache.node // 0
  xbar.node := dcache.node // 1
  xbar.node := uncache.node // 2
  node.get  := xbar.node

  override lazy val module = new SoCAbstractImp(this) {
    val core = Module(new Core)

    // interrupt input
    core.io.interrupt.msip := clint_int_sink.in.head._1(0)
    core.io.interrupt.mtip := clint_int_sink.in.head._1(1)
    core.io.interrupt.meip := plic_int.in.head._1(0)
    core.io.interrupt.seip := plic_int.in.last._1(0)

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
  }
}

class SoC(implicit p: Parameters) extends SoCAbstract {
  val soc_imp = LazyModule(new SoCImp)

  val node = None

  // interrupt
  val clint_int = IntIdentityNode()
  val plic_int  = IntIdentityNode()
  soc_imp.clint_int := clint_int
  soc_imp.plic_int :*= plic_int

  // serdes
  val beatBytes = 8
  val serdes = LazyModule(
    new TLSerdes(
      w = tlSerWidth,
      params = Seq(
        TLSlaveParameters.v1(
          address            = Seq(AddressSet(BigInt("00000000", 16), BigInt("ffffffff", 16))),
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
  serdes.node := TLBuffer() := soc_imp.node.get

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
  }
}
