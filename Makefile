BUILD_DIR = ./build
SRC = src/main/scala/$(wildcard *.scala)
NUM_CORES ?= 2
TARGET ?= fast-sim
EMU_FLAGS = EMU_TRACE=1 EMU_CXX_EXTRA_FLAGS="-DFIRST_INST_ADDRESS=0x80000000" NUM_CORES=$(NUM_CORES) WITH_CHISELDB=0 WITH_CONSTANTIN=0

verilog: $(SRC)
	@mkdir -p $(BUILD_DIR)
	sbt "run -n $(NUM_CORES) --target $(TARGET) -td $(BUILD_DIR)"
	@mv *.v $(BUILD_DIR)
	@mv *.f $(BUILD_DIR)
	@mv *.graphml $(BUILD_DIR)

emu: verilog
	cd difftest && $(MAKE) $(EMU_FLAGS) emu -j

emu2:
	cd difftest && $(MAKE) $(EMU_FLAGS) emu -j

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: verilog clean
