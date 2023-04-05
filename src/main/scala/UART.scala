import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import difftest._

class UART(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  val device    = new SimpleDevice("uartlite", Seq("uartlite-0"))
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

  val RxFifoDepth = 4
  val TxFifoDepth = 4

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val uart = new UARTIO
      val intr = Output(Bool())
    })
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
    val is_put_r = RegInit(false.B)
    val index_r  = req_r.address(3, 2)

    when(tl.a.fire) {
      req_r    := tl.a.bits
      is_get_r := is_get
      is_put_r := is_put
    }

    val rx = Module(new Queue(UInt(8.W), entries = RxFifoDepth, hasFlush = true))
    val tx = Module(new Queue(UInt(8.W), entries = TxFifoDepth, hasFlush = true))

    val intr_en       = RegInit(false.B)
    val tx_just_empty = RegInit(false.B)

    when(tl.a.fire && is_get) {
      tx_just_empty := false.B
    }.elsewhen(!tx.io.deq.valid && RegNext(tx.io.deq.valid)) {
      tx_just_empty := true.B
    }

    val rdata = WireDefault(0.U(32.W))

    // 0x0, read-only
    val rx_fifo = Wire(UInt(32.W))
    rx.io.deq.ready := false.B
    rx_fifo         := rx.io.deq.bits
    when(index_r === 0.U) {
      rdata := rx_fifo
      when(tl.d.fire) {
        rx.io.deq.ready := true.B
      }
    }

    // 0x4, write-only
    val tx_fifo = WireDefault(0.U(32.W))
    tx.io.enq.bits  := tx_fifo
    tx.io.enq.valid := false.B
    when(index_r === 1.U) {
      tx_fifo := req_r.data
      when(tl.d.fire && is_put_r) {
        tx.io.enq.valid := true.B
      }
    }

    // 0x8, read-only
    val status                    = Wire(UInt(32.W))
    val status_rx_fifo_valid_data = Wire(Bool())
    val status_rx_fifo_full       = Wire(Bool())
    val status_tx_fifo_empty      = Wire(Bool())
    val status_tx_fifo_full       = Wire(Bool())
    status_rx_fifo_valid_data := rx.io.deq.valid
    status_rx_fifo_full       := (rx.io.count === RxFifoDepth.U)
    status_tx_fifo_empty      := (tx.io.count === 0.U)
    status_tx_fifo_full       := (tx.io.count === TxFifoDepth.U)
    status := Cat(
      0.U(27.W),
      intr_en.asUInt,
      status_tx_fifo_full,
      status_tx_fifo_empty,
      status_rx_fifo_full,
      status_rx_fifo_valid_data
    )
    when(index_r === 2.U) {
      rdata := status
    }

    // 0xC, write-only
    rx.io.flush.get := false.B
    tx.io.flush.get := false.B
    when(index_r === 3.U) {
      when(tl.d.fire && is_put_r) {
        // reset tx fifo
        when(req_r.data(0)) {
          tx.io.flush.get := true.B
        }
        // reset rx fifo
        when(req_r.data(1)) {
          rx.io.flush.get := true.B
        }
        // enable interrupt
        when(req_r.data(4)) {
          intr_en := true.B
        }
      }
    }

    // interrupt
    io.intr := false.B
    when(intr_en) {
      io.intr := status_rx_fifo_valid_data || tx_just_empty
    }

    // output
    tx.io.deq.ready   := true.B
    io.uart.out.valid := tx.io.deq.valid
    io.uart.out.ch    := tx.io.deq.bits
    when(io.uart.out.valid) {
      printf("%c", io.uart.out.ch)
    }

    // input
    rx.io.enq.valid  := (io.uart.in.ch =/= "hff".U)
    rx.io.enq.bits   := io.uart.in.ch
    io.uart.in.valid := rx.io.enq.ready

    tl.a.ready := (state === s_req)
    tl.d.valid := (state === s_resp)
    tl.d.bits  := Mux(is_get_r, edge.AccessAck(req_r, rdata), edge.AccessAck(req_r))
  }
}
