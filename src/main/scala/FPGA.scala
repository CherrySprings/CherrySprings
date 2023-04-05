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

class FPGA(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val xbar              = LazyModule(new TLXbar(policy = TLArbiter.highestIndexFirst))
  val hart_id_allocator = LazyModule(new HartIDAllocator)
  val clint             = LazyModule(new CLINT(CLINTParams(), 8))
  val plic              = LazyModule(new TLPLIC(PLICParams(), 8))
  val uart              = LazyModule(new UART)
  val mem               = LazyModule(new TLVirtualRam)
  val source            = LazyModule(new TLSource)

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
  mem.node               := TLDelayer(0.1)      := xbar.node
  rom.node               := TLFragmenter(8, 32) := xbar.node
  hart_id_allocator.node := xbar.node
  clint.node             := xbar.node
  plic.node              := xbar.node
  uart.node              := TLWidthWidget(8)    := xbar.node

  xbar.node := (TLBuffer(abcde = BufferParams(depth = 2, flow = false, pipe = false))
    := TLFIFOFixer(TLFIFOFixer.all)
    := TLAtomicAutomata()
    := TLHintHandler()
    := source.node)

  class IntSourceNodeToModule(val num: Int)(implicit p: Parameters) extends LazyModule {
    val sourceNode = IntSourceNode(IntSourcePortSimple(num, ports = 1, sources = 1))
    lazy val module = new LazyModuleImp(this) {
      val in = IO(Input(Vec(num, Bool())))
      in.zip(sourceNode.out.head._1).foreach { case (i, s) => s := i }
    }
  }

  val plicSource = LazyModule(new IntSourceNodeToModule(2))
  plic.intnode := plicSource.sourceNode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val uart = new UARTIO
      val tl   = Flipped(new ChipTLBundle)
    })

    io.tl <> source.module.io.tl

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
    uart.module.io.uart <> io.uart

    // CLINT
    val cnt  = RegInit(fpgaTimerFreq.U)
    val tick = (cnt === 0.U)
    cnt                     := Mux(tick, fpgaTimerFreq.U, cnt - 1.U)
    clint.module.io.rtcTick := tick
  }
}
