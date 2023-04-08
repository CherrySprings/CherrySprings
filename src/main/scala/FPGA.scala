import chisel3._
import chisel3.util._
import difftest._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import java.nio._
import java.nio.file._
import testchipip._
import huancun._

abstract class FPGAAbstract(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val clint_int: Seq[IntIdentityNode]
  val plic_int:  Seq[IntIdentityNode]
  val node:      Option[Seq[TLIdentityNode]]
  override lazy val module = new FPGAAbstractImp(this)
}

class FPGAAbstractImp[+L <: FPGAAbstract](l: L) extends LazyModuleImp(l) with HasCherrySpringsParameters {
  val io = IO(new Bundle {
    val uart     = new UARTIO
    val in       = if (enableSerdes) Some(Vec(numHarts, Flipped(Decoupled(UInt(tlSerWidth.W))))) else None
    val out      = if (enableSerdes) Some(Vec(numHarts, Decoupled(UInt(tlSerWidth.W)))) else None
    val io_clock = if (enableSerdes) Some(Input(Clock())) else None
    val io_reset = if (enableSerdes) Some(Input(Bool())) else None
  })
}

class FPGAImp(implicit p: Parameters) extends FPGAAbstract {
  val xbar              = LazyModule(new TLXbar(policy = TLArbiter.highestIndexFirst))
  val pbar              = LazyModule(new TLXbar)
  val hart_id_allocator = LazyModule(new HartIDAllocator)
  val clint             = LazyModule(new CLINT(CLINTParams(), 8))
  val plic              = LazyModule(new TLPLIC(PLICParams(), 8))
  val uart              = LazyModule(new UART)
  val mem               = LazyModule(new TLVirtualRam256)
  val l2cache           = LazyModule(new HuanCun)
  val node = Some(for (i <- 0 until numHarts) yield {
    val node = TLIdentityNode()
    node
  })

  // BootROM
  lazy val boot_rom_contents = {
    val data = Files.readAllBytes(Paths.get(bootROMImage))
    val rom  = ByteBuffer.wrap(data)
    rom.array()
  }
  val rom = LazyModule(
    new TLROM(
      base            = 0x10000,
      size            = 0x10000,
      contentsDelayed = boot_rom_contents,
      beatBytes       = 8
    )
  )

  // don't modify order of following nodes
  mem.node  := TLFragmenter(32, 64) := TLCacheCork()       := l2cache.node      := xbar.node
  pbar.node := TLFIFOFixer()        := TLFragmenter(8, 32) := TLWidthWidget(32) := xbar.node

  rom.node               := pbar.node
  hart_id_allocator.node := pbar.node
  clint.node             := pbar.node
  plic.node              := pbar.node
  uart.node              := TLFragmenter(4, 8) := TLWidthWidget(8) := pbar.node

  for (i <- 0 until numHarts) {
    xbar.node := TLBuffer() := node.get(i)
  }

  class IntSourceNodeToModule(val num: Int)(implicit p: Parameters) extends LazyModule {
    val sourceNode = IntSourceNode(IntSourcePortSimple(num, ports = 1, sources = 1))
    lazy val module = new LazyModuleImp(this) {
      val in = IO(Input(Vec(num, Bool())))
      in.zip(sourceNode.out.head._1).foreach { case (i, s) => s := i }
    }
  }

  val plicSource = LazyModule(new IntSourceNodeToModule(2))
  plic.intnode := plicSource.sourceNode

  // interrupt sources
  val clint_int = for (i <- 0 until numHarts) yield { val n = IntIdentityNode(); n }
  val plic_int  = for (i <- 0 until numHarts) yield { val n = IntIdentityNode(); n }
  for (i <- 0 until numHarts) {
    clint_int(i) := clint.intnode
    plic_int(i) :*= plic.intnode
  }

  override lazy val module = new FPGAAbstractImp(this) {
    // sync external interrupts
    val ext_intrs = Wire(UInt(2.W))
    ext_intrs := Cat(uart.module.io.intr, 0.U)
    require(plicSource.module.in.length == ext_intrs.getWidth)
    for ((plic_in, interrupt) <- plicSource.module.in.zip(ext_intrs.asBools)) {
      val ext_intr_sync = RegInit(0.U(3.W))
      ext_intr_sync := Cat(ext_intr_sync(1, 0), interrupt)
      plic_in       := ext_intr_sync(2)
    }

    // UART
    io.uart <> uart.module.io.uart

    // CLINT
    val cnt  = RegInit(fpgaTimerFreq.U)
    val tick = (cnt === 0.U)
    cnt                     := Mux(tick, fpgaTimerFreq.U, cnt - 1.U)
    clint.module.io.rtcTick := tick
  }
}

class FPGA(implicit p: Parameters) extends FPGAAbstract {
  val fpga_imp = LazyModule(new FPGAImp)

  val node = None

  // interrupt
  val clint_int = for (i <- 0 until numHarts) yield { val n = IntIdentityNode(); n }
  val plic_int  = for (i <- 0 until numHarts) yield { val n = IntIdentityNode(); n }
  for (i <- 0 until numHarts) {
    clint_int(i) := fpga_imp.clint_int(i)
    plic_int(i) :*= fpga_imp.plic_int(i)
  }

  // desser
  val desser = for (i <- 0 until numHarts) yield {
    val desser = LazyModule(
      new TLDesser(
        w = tlSerWidth,
        params = Seq(
          TLMasterParameters.v1(
            name     = "tl-desser",
            sourceId = IdRange(0, 1 << tlSourceBits)
          )
        )
      )
    )
    desser
  }
  for (i <- 0 until numHarts) {
    fpga_imp.node.get(i) := desser(i).node
  }

  override lazy val module = new FPGAAbstractImp(this) {
    val mergeType     = desser(0).module.mergeTypes(0)
    val wordsPerBeat  = (mergeType.getWidth - 1) / tlSerWidth + 1
    val beatsPerBlock = 4
    val qDepth        = (wordsPerBeat * beatsPerBlock) << tlSourceBits

    for (i <- 0 until numHarts) {
      val in_fifo  = Module(new AsyncQueue(UInt(tlSerWidth.W)))
      val out_fifo = Module(new AsyncQueue(UInt(tlSerWidth.W)))
      in_fifo.io.enq                  <> io.in.get(i)
      in_fifo.io.enq_clock            := io.io_clock.get
      in_fifo.io.enq_reset            := io.io_reset.get
      desser(i).module.io.ser.head.in <> Queue(in_fifo.io.deq, qDepth)
      in_fifo.io.deq_clock            := clock
      in_fifo.io.deq_reset            := reset
      out_fifo.io.enq                 <> Queue(desser(i).module.io.ser.head.out, qDepth)
      out_fifo.io.enq_clock           := clock
      out_fifo.io.enq_reset           := reset
      out_fifo.io.deq                 <> io.out.get(i)
      out_fifo.io.deq_clock           := io.io_clock.get
      out_fifo.io.deq_reset           := io.io_reset.get
    }

    // UART
    io.uart <> fpga_imp.module.io.uart
  }
}
