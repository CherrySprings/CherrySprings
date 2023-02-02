import chisel3._

trait AXI4Parameters {
  val AXI4AddrWidth = 32
  val AXI4DataWidth = 64
  val AXI4IdWidth   = 4
  val AXI4UserWidth = 1
}

class AXI4IO extends Bundle with AXI4Parameters {
  val awaddr  = Output(UInt(AXI4AddrWidth.W))
  val awlen   = Output(UInt(8.W))
  val awsize  = Output(UInt(3.W))
  val awburst = Output(UInt(2.W))
  val awprot  = Output(UInt(3.W))
  val awlock  = Output(Bool())
  val awcache = Output(UInt(4.W))
  val awqos   = Output(UInt(4.W))
  val awid    = Output(UInt(AXI4IdWidth.W))
  val awuser  = Output(UInt(AXI4UserWidth.W))
  val awvalid = Output(Bool())
  val awready = Input(Bool())

  val wdata  = Output(UInt(AXI4DataWidth.W))
  val wstrb  = Output(UInt((AXI4DataWidth / 8).W))
  val wlast  = Output(Bool())
  val wuser  = Output(UInt(AXI4UserWidth.W))
  val wvalid = Output(Bool())
  val wready = Input(Bool())

  val bresp  = Input(UInt(2.W))
  val bid    = Input(UInt(AXI4IdWidth.W))
  val buser  = Input(UInt(AXI4UserWidth.W))
  val bvalid = Input(Bool())
  val bready = Output(Bool())

  val araddr  = Output(UInt(AXI4AddrWidth.W))
  val arlen   = Output(UInt(8.W))
  val arsize  = Output(UInt(3.W))
  val arburst = Output(UInt(2.W))
  val arprot  = Output(UInt(3.W))
  val arlock  = Output(Bool())
  val arcache = Output(UInt(4.W))
  val arqos   = Output(UInt(4.W))
  val arid    = Output(UInt(AXI4IdWidth.W))
  val aruser  = Output(UInt(AXI4UserWidth.W))
  val arvalid = Output(Bool())
  val arready = Input(Bool())

  val rresp  = Input(UInt(2.W))
  val rdata  = Input(UInt(AXI4DataWidth.W))
  val rlast  = Input(Bool())
  val rid    = Input(UInt(AXI4IdWidth.W))
  val ruser  = Input(UInt(AXI4UserWidth.W))
  val rvalid = Input(Bool())
  val rready = Output(Bool())
}
