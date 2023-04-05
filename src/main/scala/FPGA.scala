import chisel3._
import chisel3.util._
import difftest._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.interrupts._
import java.nio._
import java.nio.file._
import testchipip._

abstract class FPGAAbstract(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val clint_int: IntIdentityNode
  val plic_int:  IntIdentityNode
  val node:      Option[TLIdentityNode]
  override lazy val module = new FPGAAbstractImp(this)
}

class FPGAAbstractImp[+L <: FPGAAbstract](l: L) extends LazyModuleImp(l) with HasCherrySpringsParameters {
  val io = IO(new Bundle {
    val uart = new UARTIO
    val in   = if (enableSerdes) Some(Flipped(Decoupled(UInt(tlSerWidth.W)))) else None
    val out  = if (enableSerdes) Some(Decoupled(UInt(tlSerWidth.W))) else None
  })
}

class FPGAImp(implicit p: Parameters) extends FPGAAbstract {
  val xbar              = LazyModule(new TLXbar(policy = TLArbiter.highestIndexFirst))
  val hart_id_allocator = LazyModule(new HartIDAllocator)
  val clint             = LazyModule(new CLINT(CLINTParams(), 8))
  val plic              = LazyModule(new TLPLIC(PLICParams(), 8))
  val uart              = LazyModule(new UART)
  val mem               = LazyModule(new TLVirtualRam)
  val node              = Some(TLIdentityNode())

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
  mem.node               := TLDelayer(0.1)   := xbar.node
  rom.node               := xbar.node
  hart_id_allocator.node := xbar.node
  clint.node             := xbar.node
  plic.node              := xbar.node
  uart.node              := TLWidthWidget(8) := xbar.node
  xbar.node              := TLBuffer()       := TLFIFOFixer() := TLFragmenter(8, 32) := node.get

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
  val clint_int = IntIdentityNode()
  val plic_int  = IntIdentityNode()
  clint_int := clint.intnode
  plic_int :*= plic.intnode

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
  val clint_int = IntIdentityNode()
  val plic_int  = IntIdentityNode()
  clint_int := fpga_imp.clint_int
  plic_int :*= fpga_imp.plic_int

  // desser
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
  fpga_imp.node.get := desser.node

  override lazy val module = new FPGAAbstractImp(this) {
    val mergeType     = desser.module.mergeTypes(0)
    val wordsPerBeat  = (mergeType.getWidth - 1) / tlSerWidth + 1
    val beatsPerBlock = 4
    val qDepth        = (wordsPerBeat * beatsPerBlock) << tlSourceBits
    desser.module.io.ser.head.in <> Queue(io.in.get, qDepth)
    io.out.get                   <> Queue(desser.module.io.ser.head.out, qDepth)

    // UART
    io.uart <> fpga_imp.module.io.uart
  }
}
