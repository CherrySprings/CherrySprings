import chisel3._
import chisel3.util._
import chisel3.util.random._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._

class ICache(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  require(xLen == 64)

  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name     = s"icache-${hartID}",
            sourceId = IdRange(0, sourceRange)
          )
        )
      )
    )
  )

  lazy val module = new ICacheModule(this)
}

class ICacheModule(outer: ICache) extends LazyModuleImp(outer) with HasCherrySpringsParameters {
  val io = IO(new Bundle {
    val cache   = Flipped(new CachePortIO)
    val fence_i = Input(Bool())
  })
  val (tl, edge) = outer.node.out.head

  /*
   * Instruction cache (PIPT) organization
   * #Sets         = cacheNumSets
   * Associativity = 1
   * Cacheline     = 4 x 64 bits
   */

  val tagWidth = paddrLen - (5 + log2Up(cacheNumSets))
  def getOffset(x: UInt) = x(4, 3)
  def getIndex(x:  UInt) = x(4 + log2Up(cacheNumSets), 5)
  def getTag(x:    UInt) = x(paddrLen - 1, 5 + log2Up(cacheNumSets))

  val req   = io.cache.req
  val resp  = io.cache.resp
  val req_r = RegInit(0.U.asTypeOf(new CachePortReq))

  val array = Module(new SRAM(depth = cacheNumSets, dw = (new CacheEntry).len()))
  val valid = RegInit(VecInit(Seq.fill(cacheNumSets)(false.B)))

  // pipeline control signals
  val s1_valid = Wire(Bool())
  val s2_ready = Wire(Bool())
  val fire     = s1_valid & s2_ready

  // default input
  array.io.en    := fire || tl.d.fire
  array.io.addr  := 0.U
  array.io.wdata := 0.U
  array.io.wen   := false.B

  // array output in stage 2
  val array_out = Wire(new CacheEntry)
  val array_hit = Wire(Bool())
  array_out := HoldUnless(array.io.rdata, RegNext(fire)).asTypeOf(new CacheEntry)
  array_hit := valid(getIndex(req_r.addr)) && (getTag(req_r.addr) === array_out.tag)

  // when pipeline fire, read data and tag in array
  when(fire) {
    array.io.addr := getIndex(req.bits.addr)
    req_r         := req.bits
  }

  // stage 2 FSM
  val s_check :: s_req :: s_resp :: s_ok :: s_init :: Nil = Enum(5)
  val state                                               = RegInit(s_init)

  switch(state) {
    is(s_check) {
      state := Mux(resp.fire && !req.fire, s_init, Mux(array_hit, s_check, s_req))
    }
    is(s_req) {
      when(tl.a.fire) {
        state := s_resp
      }
    }
    is(s_resp) {
      when(tl.d.fire) {
        state := s_ok
      }
    }
    is(s_ok) {
      when(resp.fire) {
        state := Mux(!req.fire, s_init, s_check)
      }
    }
    is(s_init) {
      when(req.fire) {
        state := s_check
      }
    }
  }

  // cache read
  val array_data = HoldUnless(
    MuxLookup(
      getOffset(req_r.addr),
      0.U(64.W),
      Seq(
        0.U -> array_out.data(63, 0),
        1.U -> array_out.data(127, 64),
        2.U -> array_out.data(191, 128),
        3.U -> array_out.data(255, 192)
      )
    ),
    RegNext(req.fire)
  )

  // cache write
  val wdata = RegEnable(tl.d.bits.data, 0.U(256.W), tl.d.fire)

  when(tl.d.fire) {
    array.io.addr               := getIndex(req_r.addr)
    array.io.wdata              := Cat(getTag(req_r.addr), tl.d.bits.data)
    array.io.wen                := true.B
    valid(getIndex(req_r.addr)) := true.B
  }

  val refill_data = MuxLookup(
    getOffset(req_r.addr),
    0.U(64.W),
    Seq(
      0.U -> wdata(63, 0),
      1.U -> wdata(127, 64),
      2.U -> wdata(191, 128),
      3.U -> wdata(255, 192)
    )
  )

  // pipeline control
  s2_ready := ((state === s_check && array_hit) || (state === s_ok) || (state === s_init)) && resp.ready
  s1_valid := req.valid

  // send TL read request
  val source        = Counter(tl.a.fire, sourceRange)._1 // source id
  val (_, get_bits) = edge.Get(source, Cat(req_r.addr(paddrLen - 1, 5), Fill(5, 0.U)), 5.U) // 4x64 bits

  tl.a.valid := (state === s_req)
  tl.a.bits  := get_bits
  tl.d.ready := (state === s_resp)

  // cache port handshake
  req.ready       := s2_ready
  resp.valid      := (state === s_check && array_hit) || (state === s_ok)
  resp.bits       := 0.U.asTypeOf(new CachePortResp)
  resp.bits.rdata := Mux(state === s_ok, refill_data, array_data)

  // clear I cache for fence.i instruction
  when(io.fence_i) {
    for (i <- 0 until cacheNumSets) {
      valid(i) := false.B
    }
  }

  if (debugICache) {
    when(req.fire) {
      printf(cf"${DebugTimer()} [ICache] [req ] ${req.bits}\n")
    }
    when(resp.fire) {
      printf(cf"${DebugTimer()} [ICache] [resp] ${resp.bits}\n")
    }
  }
}
