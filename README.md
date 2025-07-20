# epyx_fastload
Disassembly of the Epyx Fastload cartridge load routines for the Commodore 64

Disassembled February, 2024 by Dave McMurtrie <dave@commodore.international>

This is *not* a complete disassembly of the Epyx Fastload cartridge. I only
disassembled the loading bits. None of the other code like the machine
language monitor is included here.

This is *not* an exact disassembly of the Epyx Fastload cartridge.
It's a disassembly with code modifications which allow the code to run from
$C000. I included a working d64 disk image of this re-assembled code for
educational purposes more than utilitarian purposes.

In addition to the Epyx Fastload C64 kernal load routines, I also disassembled
the 6502 code that is dynamically uploaded to the 1541 disk drive as part
of the normal operation of the Epyx Fastload cartridge.


