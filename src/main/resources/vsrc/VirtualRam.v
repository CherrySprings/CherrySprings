module VirtualRam (
  input         clk,
  input         en,
  input  [63:0] addr,
  output [63:0] rdata,
  input  [63:0] wdata,
  input  [ 7:0] wmask,
  input         wen
);

  wire [63:0] wmask64;

  assign rdata = ram_read_helper(en, addr);

  assign wmask64 = {{8{wmask[7]}}, {8{wmask[6]}}, {8{wmask[5]}}, {8{wmask[4]}},
                    {8{wmask[3]}}, {8{wmask[2]}}, {8{wmask[1]}}, {8{wmask[0]}}};

  always @(posedge clk) begin
    ram_write_helper(addr, wdata, wmask64, wen & en);
  end

endmodule
