BUILD_DIR = ./build
SRC = src/main/scala/$(wildcard *.scala)
NUM_CORES ?= 1
EMU_FLAGS = EMU_TRACE=1 EMU_CXX_EXTRA_FLAGS="-DFIRST_INST_ADDRESS=0x80000000" NUM_CORES=$(NUM_CORES) WITH_CHISELDB=0
TARGET ?= fast-sim

verilog: $(SRC)
	@mkdir -p $(BUILD_DIR)
	sbt "run $(TARGET) -td $(BUILD_DIR)"
	@mv *.v $(BUILD_DIR)
	@mv firrtl_black_box_resource_files.f $(BUILD_DIR)
	@mv *.graphml $(BUILD_DIR)

emu: verilog
	cd difftest && $(MAKE) $(EMU_FLAGS) emu -j

emu2:
	cd difftest && $(MAKE) $(EMU_FLAGS) emu -j

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: verilog clean
