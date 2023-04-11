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
    val cache = Flipped(new CachePortIO)
  })

  val (tl, edge) = outer.node.out.head
  val req        = io.cache.req
  val resp       = io.cache.resp

  // latch input channels
  val tl_b_bits_r = RegEnable(tl.b.bits, 0.U.asTypeOf(tl.b.bits.cloneType), tl.b.fire)
  val tl_d_bits_r = RegEnable(tl.d.bits, 0.U.asTypeOf(tl.d.bits.cloneType), tl.d.fire)
  val req_r       = RegEnable(req.bits, 0.U.asTypeOf(req.bits.cloneType), req.fire)

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

  val array = Module(new SRAM(depth = cacheNumSets, dw = (new CacheEntry).len()))
  val valid = RegInit(VecInit(Seq.fill(cacheNumSets)(false.B)))
  val dirty = RegInit(VecInit(Seq.fill(cacheNumSets)(false.B)))

  // FSM
  val s_init :: s_check :: s_release :: s_release_ack :: s_acquire :: s_grant :: s_grant_ack :: s_ok :: Nil = Enum(8)
  val state                                                                                                 = RegInit(s_init)

  // write data & mask expanded to 256 bits from input request
  val req_wdata_256 = Wire(UInt(256.W))
  val req_wmask_256 = Wire(UInt(256.W))
  req_wdata_256 := req_r.wdata << (getOffset(req_r.addr) << 6)
  req_wmask_256 := MaskExpand(req_r.wmask) << (getOffset(req_r.addr) << 6)

  // array access address (requested by LSU)
  val array_addr = Wire(UInt(paddrLen.W))
  array_addr := Mux(req.fire, req.bits.addr, req_r.addr)

  // default input
  val array_wdata = Wire(new CacheEntry)
  array_wdata.tag  := getTag(array_addr)
  array_wdata.data := 0.U
  array.io.en      := req.fire || resp.fire || tl.b.fire
  array.io.addr    := getIndex(array_addr)
  array.io.wdata   := array_wdata.asUInt
  array.io.wen     := false.B

  // array output
  val array_out   = HoldUnless(array.io.rdata, RegNext(req.fire)).asTypeOf(new CacheEntry)
  val array_hit   = valid(getIndex(array_addr)) && (getTag(array_addr) === array_out.tag)
  val array_dirty = valid(getIndex(array_addr)) && dirty(getIndex(array_addr))

  // cache read
  val rdata_256 = Mux(state === s_ok, tl_d_bits_r.data, array_out.data)
  val rdata_64 = MuxLookup(
    getOffset(array_addr),
    0.U(64.W),
    Seq(
      0.U -> rdata_256(63, 0),
      1.U -> rdata_256(127, 64),
      2.U -> rdata_256(191, 128),
      3.U -> rdata_256(255, 192)
    )
  )

  // cache write
  when(resp.fire) {
    when(state === s_ok) { // miss & refill (read / write) in last cycle
      array_wdata.data := Mux(
        req_r.wen,
        MaskData(tl_d_bits_r.data, req_wdata_256, req_wmask_256),
        tl_d_bits_r.data
      )
      array.io.wen                := true.B
      valid(getIndex(array_addr)) := true.B
      dirty(getIndex(array_addr)) := req_r.wen
    }.otherwise { // hit & write
      array_wdata.data := MaskData(array_out.data, req_wdata_256, req_wmask_256)
      array.io.wen     := req_r.wen && array_hit
      when(array.io.wen) {
        dirty(getIndex(array_addr)) := true.B
      }
    }
  }

  // probing mode
  val probing = BoolStopWatch(tl.b.fire, tl.c.fire, start_high_priority = true)

  switch(state) {
    is(s_init) {
      when(req.fire) {
        state := s_check
      }
    }
    is(s_check) {
      when(resp.fire) {
        state := s_init
      }.elsewhen(!array_hit) {
        state := Mux(array_dirty, s_release, s_acquire)
      }
    }
    is(s_release) {
      when(tl.c.fire && !probing) {
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

  // probe output
  val probe_addr  = Mux(tl.b.fire, tl.b.bits.address, tl_b_bits_r.address)
  val probe_out   = HoldUnless(array.io.rdata, RegNext(tl.b.fire)).asTypeOf(new CacheEntry)
  val probe_hit   = valid(getIndex(probe_addr)) && (getTag(probe_addr) === probe_out.tag)
  val probe_dirty = valid(getIndex(probe_addr)) && dirty(getIndex(probe_addr))
  when(tl.b.fire) {
    array.io.addr := getIndex(probe_addr)
  }

  // clear cacheline when being probed and hit
  when(probing && tl.c.fire && probe_hit) {
    valid(getIndex(probe_addr)) := false.B
    dirty(getIndex(probe_addr)) := false.B
  }

  // aligned address
  val release_addr_aligned = Cat(array_out.tag, RegNext(array.io.addr), 0.U(5.W))
  val req_addr_aligned     = Cat(req_r.addr(paddrLen - 1, 5), 0.U(5.W))

  // send TL burst read request
  val source              = Counter(tl.a.fire || tl.c.fire, sourceRange)._1
  val (_, acquire_bits)   = edge.AcquireBlock(source, req_addr_aligned, 5.U, TLPermissions.NtoT)
  val probe_ack_bits      = edge.ProbeAck(tl_b_bits_r, TLPermissions.NtoN)
  val probe_ack_data_bits = edge.ProbeAck(tl_b_bits_r, TLPermissions.TtoN, probe_out.data)
  val (_, release_bits)   = edge.Release(source, release_addr_aligned, 5.U, TLPermissions.TtoN, array_out.data)
  val grant_ack_bits      = edge.GrantAck(tl_d_bits_r)

  tl.a.bits  := acquire_bits
  tl.a.valid := (state === s_acquire)
  tl.b.ready := !probing
  tl.c.bits  := Mux(probing, Mux(probe_hit, probe_ack_data_bits, probe_ack_bits), release_bits)
  tl.c.valid := probing || (state === s_release)
  tl.d.ready := (state === s_release_ack) || (state === s_grant)
  tl.e.bits  := grant_ack_bits
  tl.e.valid := (state === s_grant_ack)

  // cache port handshake
  req.ready       := (state === s_init) && !(probing || tl.b.fire)
  resp.valid      := ((state === s_check && array_hit) || (state === s_ok)) && !(probing || tl.b.fire)
  resp.bits       := 0.U.asTypeOf(new CachePortResp)
  resp.bits.rdata := rdata_64

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
