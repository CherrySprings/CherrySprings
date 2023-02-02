import chisel3._
import chisel3.util._
import Constant._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class VirtualRam(implicit p: Parameters) extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val en    = Input(Bool())
    val addr  = Input(UInt(64.W))
    val rdata = Output(UInt(64.W))
    val wdata = Input(UInt(64.W))
    val wmask = Input(UInt(8.W))
    val wen   = Input(Bool())
  })

  addResource("/vsrc/VirtualRam.v")
}

class TLVirtualRam(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val device    = new SimpleDevice("virtual-ram", Seq("virtual-ram-0"))
  val beatBytes = 8
  val node = TLManagerNode(
    Seq(
      TLSlavePortParameters.v1(
        managers = Seq(
          TLSlaveParameters.v1(
            address            = Seq(AddressSet(BigInt("080000000", 16), BigInt("7fffffff", 16))),
            resources          = device.reg,
            regionType         = RegionType.UNCACHED,
            executable         = true,
            supportsGet        = TransferSizes(1, beatBytes * 4),
            supportsPutFull    = TransferSizes(1, beatBytes * 4),
            supportsPutPartial = TransferSizes(1, beatBytes * 4)
          )
        ),
        beatBytes = beatBytes
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.in.head

    val s_req :: s_resp :: Nil = Enum(2)
    val state                  = RegInit(s_req)

    val count     = RegInit(0.U(3.W))
    val count_max = RegInit(0.U(3.W))

    switch(state) {
      is(s_req) {
        when(tl.a.fire) {
          state := s_resp
          count := 0.U
          count_max := MuxLookup(
            tl.a.bits.size,
            0.U,
            Array(
              3.U -> 0.U,
              4.U -> 1.U,
              5.U -> 3.U
            )
          )
        }
      }
      is(s_resp) {
        when(tl.d.fire) {
          count := count + 1.U
          state := Mux(count =/= count_max, s_resp, s_req)
        }
      }
    }

    val req    = RegInit(0.U.asTypeOf(tl.a.bits))
    val is_get = (req.opcode === TLMessages.Get)
    val is_put = (req.opcode === TLMessages.PutFullData) || (req.opcode === TLMessages.PutPartialData)
    when(tl.a.fire) {
      req := tl.a.bits
    }
    val vr = Module(new VirtualRam)
    vr.io.clk   := clock
    vr.io.en    := (is_get || is_put) && tl.d.fire
    vr.io.addr  := req.address(30, 3) + count
    vr.io.wdata := req.data
    vr.io.wmask := req.mask
    vr.io.wen   := is_put && tl.d.fire

    tl.a.ready := (state === s_req)
    tl.d.valid := (state === s_resp)
    tl.d.bits  := Mux(is_put, edge.AccessAck(req), edge.AccessAck(req, vr.io.rdata))

    if (debugVirtRam) {
      when(tl.a.fire) {
        printf(
          "%d [VRAM-A] addr=%x opcode=%x data=%x mask=%x\n",
          DebugTimer(),
          tl.a.bits.address,
          tl.a.bits.opcode,
          tl.a.bits.data,
          tl.a.bits.mask
        )
      }
      when(tl.d.fire) {
        printf(
          "%d [VRAM-D] data=%x\n",
          DebugTimer(),
          tl.d.bits.data
        )
      }
    }
  }
}
