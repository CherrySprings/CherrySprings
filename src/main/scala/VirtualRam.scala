import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._
import Constant._

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

    val req    = HoldUnless(tl.a.bits, tl.a.fire)
    val is_get = (req.opcode === TLMessages.Get)
    val is_put = (req.opcode === TLMessages.PutFullData) || (req.opcode === TLMessages.PutPartialData)

    val put_count = RegInit(0.U(2.W))
    val get_count = RegInit(0.U(2.W))
    val count_max = MuxLookup(req.size, 0.U, Seq(3.U -> 0.U, 4.U -> 1.U, 5.U -> 3.U))

    switch(state) {
      is(s_req) {
        when(tl.a.fire) {
          when(is_get) {
            state     := s_resp
            get_count := 0.U
          }.otherwise {
            put_count := put_count + 1.U
            when(put_count === count_max) {
              state := s_resp
            }
          }
        }
      }
      is(s_resp) {
        when(tl.d.fire) {
          when(is_get) {
            get_count := get_count + 1.U
            when(get_count === count_max) {
              state := s_req
            }
          }.otherwise {
            state     := s_req
            put_count := 0.U
          }
        }
      }
    }

    val vr = Module(new VirtualRam)
    vr.io.clk   := clock
    vr.io.en    := (is_get && tl.d.fire) || (is_put && tl.a.fire)
    vr.io.addr  := req.address(30, 3) + Mux(is_get, get_count, put_count)
    vr.io.wdata := req.data
    vr.io.wmask := req.mask
    vr.io.wen   := is_put && tl.a.fire

    tl.a.ready := (state === s_req)
    tl.d.valid := (state === s_resp)
    tl.d.bits  := Mux(is_put, edge.AccessAck(req), edge.AccessAck(req, vr.io.rdata))
  }
}

class VirtualRam256(implicit p: Parameters) extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val en    = Input(Bool())
    val addr  = Input(UInt(64.W))
    val rdata = Output(UInt(256.W))
    val wdata = Input(UInt(256.W))
    val wmask = Input(UInt(32.W))
    val wen   = Input(Bool())
  })

  addResource("/vsrc/VirtualRam256.v")
}

class TLVirtualRam256(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val device    = new SimpleDevice("virtual-ram-256", Seq())
  val beatBytes = 32
  val node = TLManagerNode(
    Seq(
      TLSlavePortParameters.v1(
        managers = Seq(
          TLSlaveParameters.v1(
            address            = Seq(AddressSet(BigInt("080000000", 16), BigInt("7fffffff", 16))),
            resources          = device.reg,
            regionType         = RegionType.UNCACHED,
            executable         = true,
            supportsGet        = TransferSizes(beatBytes),
            supportsPutFull    = TransferSizes(beatBytes),
            supportsPutPartial = TransferSizes(beatBytes),
            fifoId             = Some(0)
          )
        ),
        beatBytes = beatBytes
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.in.head

    val s_idle :: s_access_ack :: Nil = Enum(2)
    val state                         = RegInit(s_idle)

    val req    = HoldUnless(tl.a.bits, tl.a.fire)
    val is_get = (req.opcode === TLMessages.Get)
    val is_put = (req.opcode === TLMessages.PutFullData) || (req.opcode === TLMessages.PutPartialData)

    switch(state) {
      is(s_idle) {
        when(tl.a.fire) {
          state := s_access_ack
        }
      }
      is(s_access_ack) {
        when(tl.d.fire) {
          state := s_idle
        }
      }
    }

    val vr = Module(new VirtualRam256)
    vr.io.clk   := clock
    vr.io.en    := (is_get && tl.d.fire) || (is_put && tl.a.fire)
    vr.io.addr  := req.address(30, 5)
    vr.io.wdata := req.data
    vr.io.wmask := req.mask
    vr.io.wen   := is_put && tl.a.fire

    tl.a.ready := (state === s_idle)
    tl.d.valid := (state === s_access_ack)
    tl.d.bits  := Mux(is_put, edge.AccessAck(req), edge.AccessAck(req, vr.io.rdata))
  }
}
