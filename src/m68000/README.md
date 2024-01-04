# Motorola 68000
This is an instruction set simulator.  No effort has been made for timing or
cycle accuracy.  Software that does not depend on instruction timing will
probably work.

This is in the early stages of development.  Currently just trying to
get the 68000 variant working before expanding to other variants.  I don't
expect to cover all variants, especially FPU and MMU instructions.  Expect
lots of churn right now, but some progress towards a finished product.

The details of exception handling will probably not be identical to actual
hardware.  The basic 68000 exceptions should be fairly similar, but the
mode detailed stack frames from the 68010 and later will probably not
be implemented identically, if at all.

Currently, instructions are being implemented in alphabetical order.  See
the list below for what instructions and addressing modes have been
implement and have basic tests.

Note that at this point testing is basic sanity checks to see if things
work mostly as expected.  Exhaustive testing has not yet been done.

There is a fairly comprehensive [test program](https://github.com/MicroCoreLabs/Projects/tree/master/MCL68/MC68000_Test_Code),
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
| 5 | Yes | d(An) | Address register indirect with displacement |
| 6 | Yes | d(An, ix) | Address register indirect with index (and others) |
| 7/0 | Yes | (xxx).W | Absolute short |
| 7/1 | Yes | (xxx).L | Absolute long |
| 7/2 | Yes | d(PC) | Program counter with displacement |
| 7/3 | Yes | d(PC, ix) | Program counter with index |
| 7/4 | Yes | #xxx | Immediate or status register |

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
    - EOR
    - EORI
    - EORI to CCR
    - EORI to SR
- Shift Group
    - ASL (memory and register)
    - ASR (memory and register)
    - LSL
    - LSR
- Control Transfer Group
    - Bcc (not all condition code combinations tested)
    - BRA
    - BSR
    - DBcc (not all condition code combinations tested)
    - JMP
    - JSR
- Bit Operation Group
    - BCHG
    - BCLR
    - BSET
    - BTST
- Miscellaneous Instructions
    - CHK
    - CLR
    - EXG
    - EXT
    - ILLEGAL
    - LINK
- Compare Group
    - CMP
    - CMPA
    - CMPI
    - CMPM
- Division
    - DIVS
    - DIVU
- Move Group
    - LEA
    - MOVE
    - MOVE to CCR
    - MOVE from SR
    - MOVE to SR
    - MOVEA

The remaining instructions to implement are (just to give a general idea
of progress) below.  They will get moved to the above list as they are
implemented:
- MOVE USP
- MOVEC
- MOVEM
- MOVEP
- MOVEQ
- MOVES
- MULS
- MULU
- NBCD
- NEG
- NEGX
- NOP
- NOT
- OR
- ORI
- ORI to CCR
- ORI to SR
- PEA
- RESET
- ROL
- ROLX
- ROR
- RORX
- RTD
- RTE
- RTR
- RTS
- SBCD
- STOP
- SUB
- SUBA
- SUBI
- SUBQ
- SUBX
- SWAP
- TAS
- TRAP
- TRAPV
- TST
- UNLNK


An initial cut at memory mapped I/O was added, but the CPU simulation
does not yet have instructions to test it.  An initial cut at exceptions
has also been implemented.  Currently, they just print out a message
indicating which exception has been triggered and have not been added
in all places where they should be.
