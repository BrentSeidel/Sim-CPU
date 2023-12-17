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
generated listing file, one can loadcpm utility to read in the generated
Intel Hex file and write it to the boot tracks on a disk image.  The L0Boot
file is the bootstrap.  Note that since the simulated disk controller
can read multiple sectors in one transaction, reading the full two
tracks isn't any more complicated that reading a single sector.  Thus,
the boot sector on the disk isn't used.

Some other [CP/M stuff](https://www.tramm.li/i8080/index.html) is here,
but I haven't looked at it yet.  Other interesting information is at
[Emulator 101](http://www.emulator101.com), including an 8080 test program,
and some other thing.
