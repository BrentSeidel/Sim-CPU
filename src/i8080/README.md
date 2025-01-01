# Intel 8080 Simulator
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
generated listing file, one can use the loadcpm utility to read in the generated
Intel Hex file and write it to the boot tracks on a disk image.  The L0Boot
file is the bootstrap.  Note that since the simulated disk controller
can read multiple sectors in one transaction, reading the full two
tracks isn't any more complicated that reading a single sector.  Thus,
the boot sector on the disk isn't used.

Some other [CP/M stuff](https://www.tramm.li/i8080/index.html) is here,
but I haven't looked at it yet.  Other interesting information is at
[Emulator 101](http://www.emulator101.com), including an 8080 test program,
and some other thing.

*NOTE* There seems to be a bug somewhere in the the CP/M code that computes
track and sector numbers.  This is probably due to an error in the implementation
of one or more instructions.  It is probably a fairly subtle things since
most things seem to work correctly.  I am currently trying to track this
down.

I ran the CPU test that I got from Emulator 101 above and it reports that
the CPU is operational.  There is still some flakeyness running CP/M,
but I'm not sure if it's the fault of the CP/M that I have, the disk
image being not properly formatted, or problems with the simulator.

## Booting CP/M
The bootstrap is L0Boot.a80.  The Intel hex format file is L0Boot.ihx and
it can be loaded into the simulator using the "LOAD" command in the CLI.
If you are not using the CLI (for example the [PI-Mainframe](https://github.com/BrentSeidel/Pi-Mainframe)
project), the hex code for the bootstap is:
| Address | Data |
|:-------:|:----:|
| 0000 | 3E |
| 0001 | C0 |
| 0002 | D3 |
| 0003 | 03 |
| 0004 | AF |
| 0005 | D3 |
| 0006 | 05 |
| 0007 | 3E |
| 0008 | 01 |
| 0009 | D3 |
| 000A | 04 |
| 000B | 3E |
| 000C | E4 |
| 000D | D3 |
| 000E | 07 |
| 000F | AF |
| 0010 | D3 |
| 0011 | 06 |
| 0012 | 3E |
| 0013 | 33 |
| 0014 | D3 |
| 0015 | 08 |
| 0016 | 3E |
| 0017 | 40 |
| 0018 | D3 |
| 0019 | 03 |
| 001A | C3 |
| 001B | FD |
| 001C | F9 |

The entry point is address 0.  This loads the operating system sectors
of whatever drive image is attached to drive 0.  The file cpmboot.img
is a bootable CP/M disk image.  There is nothing else on the image, so
it isn't too useful, but can be used to examine images attached to other
drives.  I may eventually add some stuff to this drive image.

## Variants
There are three variants of the simulator available.

### Basic 8080
This is the basic 8080 simulator.  It executes the 8080 instruction set.
No attempt has been made to make it cycle accurate, so it shouldn't be
used to determine timing.  The other variants are based on this.

Interrupts have not been implemented yet.  They may be at some time.  Also,
all I/O devices are accessed through I/O ports - memory mapped I/O has not
been implemented.  It also may be at some point, if needed.

### 8085
From the simulator point of view, this is effectively the same as the 8080.
Two new instructions (RIM and SIM) exist, but don't do much.

### Z-80
The extra Z-80 instructions have been added.  Many of the undocumented instructions
have also been added.  Note that the unused flag bits do not change to match the
real Z-80 hardware (apparently, they represent some bits of an internal register).

