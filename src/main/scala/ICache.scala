import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class ICache(source: Int, size: Int)(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  require(xLen == 64)

  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name            = s"InstructionCache$source",
            sourceId        = IdRange(source, source + 1),
            supportsProbe   = TransferSizes(xLen),
            supportsGet     = TransferSizes(xLen),
            supportsPutFull = TransferSizes(xLen)
          )
        )
      )
    )
  )

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val cache   = Flipped(new CachePortIO)
      val fence_i = Input(Bool())
    })
    val (tl, edge) = node.out.head

    def getOffset(x: UInt) = x(4, 3)
    def getIndex(x:  UInt) = x(4 + log2Up(size), 5)
    def getTag(x:    UInt) = x(paddrLen - 1, 5 + log2Up(size))

    val req  = io.cache.req
    val resp = io.cache.resp

    // Directed-mapped, #size sets, 4x64-bit cacheline, read-only
    val array        = Module(new SRAM(depth = size, dw = 256))
    val valid        = RegInit(VecInit(Seq.fill(size)(false.B)))
    val tag          = RegInit(VecInit(Seq.fill(size)(0.U((32 - 5 - log2Up(size)).W))))
    val hit          = valid(getIndex(req.bits.addr)) && (getTag(req.bits.addr) === tag(getIndex(req.bits.addr)))
    val hit_r        = RegEnable(hit, false.B, req.fire)
    val addr_r       = RegEnable(req.bits.addr, 0.U, req.fire)
    val refill_count = RegInit(0.U(2.W)) // saturation counter (0 -> 1 -> 2 -> 3 -> 0)

    when(io.fence_i) {
      for (i <- 0 until size) {
        valid(i) := false.B
      }
    }

    val s_check :: s_req :: s_resp :: s_ok :: Nil = Enum(4)
    val state                                     = RegInit(s_check)

    when(tl.d.fire) {
      refill_count := refill_count + 1.U
    }

    switch(state) {
      is(s_check) {
        when(req.fire) {
          state := Mux(hit, s_ok, s_req)
        }
      }
      is(s_req) {
        when(tl.a.fire) {
          state := s_resp
        }
      }
      is(s_resp) {
        when(tl.d.fire && refill_count === 3.U) {
          state := s_ok
        }
      }
      is(s_ok) {
        when(resp.fire) {
          state := s_check
        }
      }
    }

    val (_, get_bits) = edge.Get(source.U, Cat(addr_r(paddrLen - 1, 5), Fill(5, 0.U)), 5.U)

    // cache read
    val rdata = HoldUnless(
      MuxLookup(
        getOffset(addr_r),
        0.U(64.W),
        Array(
          0.U -> array.io.rdata(63, 0),
          1.U -> array.io.rdata(127, 64),
          2.U -> array.io.rdata(191, 128),
          3.U -> array.io.rdata(255, 192)
        )
      ),
      RegNext(req.fire) || RegNext(tl.d.fire)
    )

    // cache write
    val wdata0 = RegEnable(tl.d.bits.data, 0.U(64.W), tl.d.fire && (refill_count === 0.U))
    val wdata1 = RegEnable(tl.d.bits.data, 0.U(64.W), tl.d.fire && (refill_count === 1.U))
    val wdata2 = RegEnable(tl.d.bits.data, 0.U(64.W), tl.d.fire && (refill_count === 2.U))

    array.io.addr  := Mux(state === s_check, getIndex(io.cache.req.bits.addr), getIndex(addr_r))
    array.io.wen   := tl.d.fire && (refill_count === 3.U)
    array.io.wdata := Cat(tl.d.bits.data, wdata2, wdata1, wdata0)

    when(resp.fire) {
      valid(getIndex(addr_r)) := true.B
      tag(getIndex(addr_r))   := getTag(addr_r)
    }

    req.ready            := state === s_check
    tl.a.valid           := state === s_req
    tl.a.bits            := get_bits
    tl.d.ready           := state === s_resp
    resp.valid           := state === s_ok
    resp.bits.rdata      := rdata
    resp.bits.page_fault := false.B
  }
}
