module VirtualRam256 (
  input          clk,
  input          en,
  input  [63:0]  addr,
  output [255:0] rdata,
  input  [255:0] wdata,
  input  [31:0]  wmask,
  input          wen
);

  wire [255:0] wmask256;

  assign rdata = {ram_read_helper(en, {addr, 2'b11}), ram_read_helper(en, {addr, 2'b10}),
                  ram_read_helper(en, {addr, 2'b01}), ram_read_helper(en, {addr, 2'b00})};

  assign wmask256 = {{8{wmask[31]}}, {8{wmask[30]}}, {8{wmask[29]}}, {8{wmask[28]}},
                     {8{wmask[27]}}, {8{wmask[26]}}, {8{wmask[25]}}, {8{wmask[24]}},
                     {8{wmask[23]}}, {8{wmask[22]}}, {8{wmask[21]}}, {8{wmask[20]}},
                     {8{wmask[19]}}, {8{wmask[18]}}, {8{wmask[17]}}, {8{wmask[16]}},
                     {8{wmask[15]}}, {8{wmask[14]}}, {8{wmask[13]}}, {8{wmask[12]}},
                     {8{wmask[11]}}, {8{wmask[10]}}, {8{wmask[ 9]}}, {8{wmask[ 8]}},
                     {8{wmask[ 7]}}, {8{wmask[ 6]}}, {8{wmask[ 5]}}, {8{wmask[ 4]}},
                     {8{wmask[ 3]}}, {8{wmask[ 2]}}, {8{wmask[ 1]}}, {8{wmask[ 0]}}};

  always @(posedge clk) begin
    ram_write_helper({addr, 2'b11}, wdata[255:192], wmask256[255:192], wen & en);
    ram_write_helper({addr, 2'b10}, wdata[191:128], wmask256[191:128], wen & en);
    ram_write_helper({addr, 2'b01}, wdata[127: 64], wmask256[127: 64], wen & en);
    ram_write_helper({addr, 2'b00}, wdata[ 63:  0], wmask256[ 63:  0], wen & en);
  end

endmodule
