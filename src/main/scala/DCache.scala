import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._

class DCache(implicit p: Parameters) extends LazyModule with HasCherrySpringsParameters {
  require(xLen == 64)

  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name          = s"dcache-${hartID}",
            sourceId      = IdRange(0, sourceRange),
            supportsProbe = TransferSizes(32)
          )
        )
      )
    )
  )

  lazy val module = new DCacheModule(this)
}

class DCacheModule(outer: DCache) extends LazyModuleImp(outer) with HasCherrySpringsParameters {
  val io = IO(new Bundle {
    val cache      = Flipped(new CachePortIO)
    val fence_i    = Input(Bool())
    val fence_i_ok = Output(Bool())
  })
  val (tl, edge) = outer.node.out.head

  /*
   * Data cache (PIPT) organization
   * #Sets         = cacheNumSets
   * Associativity = 1
   * Cacheline     = 4 x 64 bits
   * Write policy  = write allocate + write back
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
  val dirty = RegInit(VecInit(Seq.fill(cacheNumSets)(false.B)))

  // FSM
  val s_init :: s_check :: s_release :: s_release_ack :: s_acquire :: s_grant :: s_grant_ack :: s_ok :: Nil = Enum(8)
  val state                                                                                                 = RegInit(s_init)

  // FSM for fence.i
  val s_fi_init :: s_fi_next :: s_fi_check :: s_fi_release :: s_fi_release_ack :: s_fi_ok :: Nil = Enum(6)
  val state_fi                                                                                   = RegInit(s_fi_init)

  // write data & mask expanded to 256 bits from input request
  val req_wdata_256 = Wire(UInt(256.W))
  val req_wmask_256 = Wire(UInt(256.W))
  req_wdata_256 := req_r.wdata << (getOffset(req_r.addr) << 6)
  req_wmask_256 := MaskExpand(req_r.wmask) << (getOffset(req_r.addr) << 6)

  // default input
  val array_wdata = Wire(new CacheEntry)
  array_wdata.tag  := getTag(req_r.addr)
  array_wdata.data := 0.U
  array.io.addr    := Mux(state === s_init, getIndex(req.bits.addr), getIndex(req_r.addr))
  array.io.wdata   := array_wdata.asUInt
  array.io.wen     := false.B

  // array output
  val array_out   = Wire(new CacheEntry)
  val array_hit   = Wire(Bool())
  val array_dirty = Wire(Bool())
  array_out   := array.io.rdata.asTypeOf(new CacheEntry)
  array_hit   := valid(getIndex(req_r.addr)) && (getTag(req_r.addr) === array_out.tag)
  array_dirty := valid(getIndex(req_r.addr)) && dirty(getIndex(req_r.addr))

  // write to cache when hit & wen
  when(state === s_check) {
    array_wdata.data := MaskData(array_out.data, req_wdata_256, req_wmask_256)
    array.io.wen     := req_r.wen && array_hit
    when(array.io.wen) {
      dirty(getIndex(req_r.addr)) := true.B
    }
  }

  // when input request fire
  when(req.fire) {
    req_r := req.bits
  }

  val tl_d_bits_r = RegInit(0.U.asTypeOf(tl.d.bits))
  when(tl.d.fire) {
    tl_d_bits_r := tl.d.bits
  }

  switch(state) {
    is(s_init) {
      when(req.fire) {
        state := s_check
      }
    }
    is(s_check) {
      state := Mux(resp.fire, s_init, Mux(array_hit, s_check, Mux(array_dirty, s_release, s_acquire)))
    }
    is(s_release) {
      when(tl.c.fire) {
        state := s_release_ack
      }
    }
    is(s_release_ack) {
      when(tl.d.fire) {
        state := s_acquire
      }
    }
    is(s_acquire) {
      when(tl.a.fire) {
        state := s_grant
      }
    }
    is(s_grant) {
      when(tl.d.fire) {
        state := s_grant_ack
      }
    }
    is(s_grant_ack) {
      when(tl.e.fire) {
        state := s_ok
      }
    }
    is(s_ok) {
      when(resp.fire) {
        state := s_init
      }
    }
  }

  // cache read
  val array_hit_data = HoldUnless(
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

  val refill_data_256 = Mux(
    req_r.wen,
    MaskData(tl.d.bits.data, req_wdata_256, req_wmask_256),
    tl.d.bits.data
  )

  when((state === s_grant) && tl.d.fire) {
    array_wdata.data            := refill_data_256
    array.io.wen                := true.B
    valid(getIndex(req_r.addr)) := true.B
    dirty(getIndex(req_r.addr)) := req_r.wen
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

  // dirty addr & data to be written back to memory
  val dirty_addr = Wire(UInt(paddrLen.W))
  val dirty_data = Wire(UInt(256.W))
  dirty_addr := Cat(array_out.tag, RegNext(array.io.addr), 0.U(5.W))
  dirty_data := array_out.data

  // fence.i
  io.fence_i_ok := (state_fi === s_fi_ok)

  val fi_check_addr = RegInit(0.U(log2Up(cacheNumSets).W))
  when(io.fence_i) {
    array.io.addr := fi_check_addr
  }

  switch(state_fi) {
    is(s_fi_init) {
      when(io.fence_i) {
        state_fi := s_fi_check
      }
    }
    is(s_fi_check) {
      state_fi := Mux(valid(fi_check_addr) && dirty(fi_check_addr), s_fi_release, s_fi_next)
    }
    is(s_fi_release) {
      when(tl.c.fire) {
        state_fi := s_fi_release_ack
      }
    }
    is(s_fi_release_ack) {
      when(tl.d.fire) {
        state_fi := s_fi_next
      }
    }
    is(s_fi_next) {
      fi_check_addr := fi_check_addr + 1.U
      state_fi      := Mux(fi_check_addr === (cacheNumSets - 1).U, s_fi_ok, s_fi_check)
    }
    is(s_fi_ok) {
      for (i <- 0 until cacheNumSets) {
        dirty(i) := false.B
        valid(i) := false.B
      }
      state_fi := s_fi_init
    }
  }

  val req_addr_aligned = Cat(req_r.addr(paddrLen - 1, 5), Fill(5, 0.U))

  // send TL burst read request
  val source            = Counter(tl.a.fire, sourceRange)._1
  val (_, acquire_bits) = edge.AcquireBlock(source, req_addr_aligned, 5.U, TLPermissions.NtoT)
  val (_, release_bits) = edge.Release(source, dirty_addr, 5.U, TLPermissions.TtoN, dirty_data)
  val grant_ack_bits    = edge.GrantAck(tl_d_bits_r)

  tl.a.bits  := acquire_bits
  tl.a.valid := (state === s_acquire)
  tl.c.bits  := release_bits
  tl.c.valid := (state === s_release) || (state_fi === s_fi_release)
  tl.d.ready := (state === s_release_ack) || (state === s_grant) || (state_fi === s_fi_release_ack)
  tl.e.bits  := grant_ack_bits
  tl.e.valid := (state === s_grant_ack)

  // cache port handshake
  req.ready       := (state === s_init) && !io.fence_i && (state_fi === s_fi_init)
  resp.valid      := (state === s_check && array_hit) || (state === s_ok)
  resp.bits       := 0.U.asTypeOf(new CachePortResp)
  resp.bits.rdata := Mux(req_r.wen, 0.U, Mux(state === s_ok, refill_data, array_hit_data))

  if (debugDCache) {
    when(req.fire) {
      printf(cf"${DebugTimer()} [DCache] [in -req ] ${req.bits}\n")
    }
    when(resp.fire) {
      printf(cf"${DebugTimer()} [DCache] [in -resp] ${resp.bits}\n")
    }
    when(tl.a.fire) {
      printf(
        cf"${DebugTimer()} [DCache] [out-req ] addr=${tl.a.bits.address}%x " +
          cf"wdata=${tl.a.bits.data}%x wmask=${tl.a.bits.mask}%x wen=${!(state === s_acquire)}\n"
      )
    }
    when(tl.d.fire) {
      printf(cf"${DebugTimer()} [DCache] [out-resp] rdata=${tl.d.bits.data}\n")
    }
  }
}
