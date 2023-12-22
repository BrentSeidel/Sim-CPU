# Motorola 68000
This is an instruction set simulator.  No effort has been made for timing or
cycle accuracy.  Software that does not depend on instruction timing will
probably work.

This is in the early stages of development.  Currently just trying to
get the 68000 variant working before expanding to other variants.  I don't
expect to cover all variants, especially FPU and MMU instructions.  Expect
lots of churn right now, but some progress towards a finished product.

Currently, instructions are being implemented in alphabetical order.  See
the list below for what instructions and addressing modes have been
implement and have basic tests.

Note that at this point testing is basic sanity checks to see if things
work mostly as expected.  Exhaustive testing has not yet been done.

There is a fairly comprehensive [test program])https://github.com/MicroCoreLabs/Projects/tree/master/MCL68/MC68000_Test_Code),
but this simulator doesn't yet have enough implemented to have a hope
of running that.  Eventually, it should be run to give additional confidence.


The addressing modes implemented and tested are:
| Mode | Tested | Syntax | Description |
|:----:|--------|:------:|-------------|
| 0 | Yes | Dn | Data register direct |
| 1 | Yes | An | Address register direct |
| 2 | Yes | (An) | Address register indirect |
| 3 | Yes | (An)+ | Address register indirect with postincrement |
| 4 | Yes | -(An) | Address register indirect with predecrement |
| 5 | No | d(An) | Address register indirect with displacement |
| 6 | No | d(An, ix) | Address register indirect with index (and others) |
| 7/0 | Yes | (xxx).W | Absolute short |
| 7/1 | Yes | (xxx).L | Absolute long |
| 7/2 | No | d(PC) | Program counter with displacement |
| 7/3 | No | d(PC, ix) | Program counter with index |
| 7/4 | No | #xxx | Immediate or status register |

The following instructions have been at least somewhat implemented:
- Addition Group
    - ABCD
    - ADD
    - ADDA
    - ADDI
    - ADDQ
    - ADDX
- Logical Group
    - AND
    - ANDI
    - ANDI to CCR
- Shift Group
    - ASL (memory and register)
    - ASR (memory and register)
- Branch Group
    - Bcc (not all condition code combinations tested)
    - BRA
    - BSR
    - DBcc (not all condition code combinations tested)
- Bit Operation Group
    - BCHG
    - BCLR
    - BSET
    - BTST
- Miscellaneous Instructions
    - CHK
    - CLR
- Compare Group
   - CMP
   - CMPA
   - CMPI
   - CMPM
- Division
    - DIVS
    - DIVU

An initial cut at memory mapped I/O was added, but the CPU simulation
does not yet have instructions to test it.  An initial cut at exceptions
has also been implemented.  Currently, they just print out a message
indicating which exception has been triggered and have not been added
in all places where they should be.
