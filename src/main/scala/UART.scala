import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import difftest._

class UART(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val device    = new SimpleDevice("uart", Seq("uart-0"))
  val beatBytes = 8
  val node = TLManagerNode(
    Seq(
      TLSlavePortParameters.v1(
        managers = Seq(
          TLSlaveParameters.v1(
            address            = Seq(AddressSet(BigInt("10000000", 16), BigInt("ffff", 16))),
            resources          = device.reg,
            regionType         = RegionType.UNCACHED,
            supportsGet        = TransferSizes(1, beatBytes),
            supportsPutFull    = TransferSizes(1, beatBytes),
            supportsPutPartial = TransferSizes(1, beatBytes)
          )
        ),
        beatBytes = beatBytes
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val io         = IO(new UARTIO)
    val (tl, edge) = node.in.head

    val s_req :: s_resp :: Nil = Enum(2)
    val state                  = RegInit(s_req)

    switch(state) {
      is(s_req) {
        when(tl.a.fire) {
          state := s_resp
        }
      }
      is(s_resp) {
        when(tl.d.fire) {
          state := s_req
        }
      }
    }

    val req      = tl.a.bits
    val is_get   = (req.opcode === TLMessages.Get)
    val is_put   = (req.opcode === TLMessages.PutFullData) || (req.opcode === TLMessages.PutPartialData)
    val req_r    = RegInit(0.U.asTypeOf(tl.a.bits))
    val is_get_r = RegInit(false.B)

    when(tl.a.fire) {
      req_r    := tl.a.bits
      is_get_r := is_get
    }

    val rdata       = RegInit(0.U(8.W))
    val valid_inout = tl.a.fire && (req.address(15, 0) === "h0000".U)

    // output is valid when a channel fire
    io.out.valid := valid_inout && is_put
    io.out.ch    := req.data

    // input is received when a channel fire
    io.in.valid := valid_inout && is_get
    rdata       := io.in.ch

    tl.a.ready := (state === s_req)
    tl.d.valid := (state === s_resp)
    tl.d.bits  := Mux(is_get_r, edge.AccessAck(req_r, rdata), edge.AccessAck(req_r))
  }
}
