import chisel3._
import chisel3.util._
import difftest._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import sifive.blocks.devices.chiplink._
import java.nio._
import java.nio.file._

class SimTop(implicit p: Parameters) extends LazyModule with BindingScope with HasCherrySpringsParameters {
  lazy val dts = DTS(bindingTree)

  val soc               = LazyModule(new SoC)
  val xbar              = LazyModule(new TLXbar(policy = TLArbiter.highestIndexFirst))
  val pbar              = LazyModule(new TLXbar())
  val hart_id_allocator = LazyModule(new HartIDAllocator)
  val clint             = LazyModule(new CLINT(CLINTParams(), 8))
  val plic              = LazyModule(new TLPLIC(PLICParams(), 8))
  val uart              = LazyModule(new UART)
  val mem               = LazyModule(new TLVirtualRam)

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

  // ChipLink
  val chiplink = LazyModule(
    new ChipLink(
      ChipLinkParams(
        TLUH   = List(ChipLinkParam.mmio),
        TLC    = List(ChipLinkParam.mem),
        syncTX = true
      )
    )
  )
  val chiplink_sink = chiplink.ioNode.makeSink

  val dummy = LazyModule(new TLDummyClient)
  chiplink.node := dummy.node

  val err = LazyModule(
    new TLError(
      DevNullParams(Seq(AddressSet(0x70000000L, 0x1000L - 1)), 64, 64, region = RegionType.TRACKED),
      beatBytes = 8
    )
  )
  xbar.node := (TLBuffer(abcde = BufferParams(depth = 2, flow = false, pipe = false))
    := TLFIFOFixer(TLFIFOFixer.all)
    := TLAtomicAutomata()
    := TLHintHandler()
    := TLWidthWidget(4)
    := chiplink.node)
  err.node := xbar.node

  // don't modify order of following nodes
  mem.node  := xbar.node
  pbar.node := TLFIFOFixer(TLFIFOFixer.all) := TLFragmenter(8, 32) := TLFIFOFixer(TLFIFOFixer.all) := xbar.node

  rom.node               := pbar.node
  hart_id_allocator.node := pbar.node
  clint.node             := pbar.node
  plic.node              := pbar.node
  uart.node              := pbar.node

  soc.clint_int_sink := clint.intnode
  soc.plic_int_sink :*= plic.intnode

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
    ElaborationArtefacts.add("dts", dts)

    chiplink_sink.bundle.b2c <> soc.module.chiplink_io.b2c
    chiplink_sink.bundle.c2b <> soc.module.chiplink_io.c2b

    // sync external interrupts
    val ext_intrs = Wire(UInt(2.W))
    ext_intrs := Cat(uart.module.io.intr, 0.U)
    require(plicSource.module.in.length == ext_intrs.getWidth)
    for ((plic_in, interrupt) <- plicSource.module.in.zip(ext_intrs.asBools)) {
      val ext_intr_sync = RegInit(0.U(3.W))
      ext_intr_sync := Cat(ext_intr_sync(1, 0), interrupt)
      plic_in       := ext_intr_sync(2)
    }

    val io = IO(new Bundle {
      val logCtrl  = new LogCtrlIO
      val perfInfo = new PerfInfoIO
      val uart     = new UARTIO
    })

    uart.module.io.uart <> io.uart

    // CLINT
    val cnt  = RegInit(fpgaTimerFreq.U)
    val tick = (cnt === 0.U)
    cnt                     := Mux(tick, fpgaTimerFreq.U, cnt - 1.U)
    clint.module.io.rtcTick := tick
  }
}
