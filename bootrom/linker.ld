SECTIONS
{
    ROM_BASE = 0x10000; /* ... but actually position independent */

    . = ROM_BASE;
    .text.start : { *(.text.start) }
}
