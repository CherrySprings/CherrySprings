BUILD_DIR = ./build
SRC = src/main/scala/$(wildcard *.scala)
EMU_FLAGS = EMU_TRACE=1 EMU_CXX_EXTRA_FLAGS="-DFIRST_INST_ADDRESS=0x80000000" WITH_CHISELDB=0

verilog: $(SRC)
	@mkdir -p $(BUILD_DIR)
	sbt "run -td $(BUILD_DIR)"
	@mv *.v $(BUILD_DIR)
	@mv firrtl_black_box_resource_files.f $(BUILD_DIR)

emu: verilog
	cd difftest && $(MAKE) $(EMU_FLAGS) emu -j

emu2:
	cd difftest && $(MAKE) $(EMU_FLAGS) emu -j

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: verilog clean
