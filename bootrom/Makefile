bootrom_img = bootrom.img

CROSS_COMPILE ?= riscv64-unknown-elf-
GCC = $(CROSS_COMPILE)gcc
OBJCOPY = $(CROSS_COMPILE)objcopy

all: $(bootrom_img)

%.img: %.bin
	dd if=$< of=$@ bs=128 count=1

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

%.elf: %.S linker.ld
	$(GCC) -Tlinker.ld $< -nostdlib -static -Wl,--no-gc-sections -o $@
