#define HART_ID_ALLOCATOR 0x00020000
#define DRAM_BASE 0x80000000

.section .text.start, "ax", @progbits
.globl _start
_start:
  li t0, HART_ID_ALLOCATOR
  ld t1, 0(t0)
  csrw mhartid, t1
  li s0, DRAM_BASE
  csrr a0, mhartid
  jr s0
  nop
