# Sim-CPU
CPU Simulations written in Ada

This is spun off of the Pi-Mainframe repository.  Moving simulation into
a separate repository will allow simulators to be developed more independently
as well as used in other applications.

## Implementation
The root of the simulators is the abstract object "simulator" defined in the
BBS.Sim_CPU package.  It defined the external interface that all simulators
must implement.  It is expected to evolve as some actual CPU simulators get
implemented.

## Simulators
The following simulators are implemented.

### Simple Example
This is not really a CPU simulator.  Its main purpose is to blink the lights
in the Pi-Mainframe in interesting patterns.

### Intel 8080
This is an instruction set simulator.  No effort has been made for timing or
cycle accuracy.  Software that does not depend on instruction timing will
probably work.

All instructions have been implemented and have had basic sanity checks.
It is expected that there some bugs still remain.  In particular, I'm not
entirely sure that I have the flags correct.  Since I wrote both the
implementation and test, any misunderstanding would show up in both.  I
would appreciate other eyes to take a look at this (and everything else),
and write issues, if necessary.

I have been able to create a bootable CP/M disk image and get it to run
which provides some confidence that things are working at least close
to properly.  I used the CP/M assembly file from [CP/M Sources](http://www.cpm.z80.de/source.html)
modified slightly to assemble using the assemblers and linker from [ASxxxx](https://github.com/0cjs/ASxxxx),
then added my BIOS.  Using the starting and ending address from the
generated listing file, one can loadcpm utility to read in the generated
Intel Hex file and write it to the boot tracks on a disk image.  The L0Boot
file is the bootstrap.  Note that since the simulated disk controller
can read multiple sectors in one transaction, reading the full two
tracks isn't any more complicated that reading a single sector.  Thus,
the boot sector on the disk isn't used.

### Motorola 68000
This is an instruction set simulator.  No effort has been made for timing or
cycle accuracy.  Software that does not depend on instruction timing will
probably work.

This is in the very early stages of development.  Currently just trying to
get the 68000 variant working before expanding to other variants.  Expect
lots of churn right now, but some progress towards a finished product.

Currently, instructions are being implemented in alphabetical order.  The
various addition instructions are basically finished.  Several of the
simple addressing modes have been tested and are working.

Note that at this point testing is basic sanity checks to see if things
work mostly as expected.  Exhaustive testing has not yet been done.

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
| 7/0 | No | xxx.W | Absolute short |
| 7/1 | No | xxx.L | Absolute long |
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
    - ASL (memory)
    - ASR (memory)
    - ASL (register)
    - ASR (register)
- Branch Group
    - Bcc (not all combinations tested)
    - BRA
- Bit Operation Group
    - BCHG

An initial cut at memory mapped I/O was added, but the CPU simulation
does not yet have instructions to test it.
