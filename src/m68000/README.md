# Motorola 68000
This is an instruction set simulator.  No effort has been made for timing or
cycle accuracy.  Software that does not depend on instruction timing will
probably work.

All the 68000 instructions and addressing modes have been implemented and
tested.  I don't expect to cover all variants, especially FPU and MMU
instructions.  Instructions for later processors may be added at some time.
The 68010 instructions are highly likely, CPU32 probably, others maybe.

The details of exception handling will probably not be identical to actual
hardware.  The basic 68000 exceptions should be fairly similar, but the
more detailed stack frames from the 68010 and later will probably not
be implemented identically, if at all.

The fairly comprehensive [test program](https://github.com/MicroCoreLabs/Projects/tree/master/MCL68/MC68000_Test_Code)
has being run against this simulator it now passes.  File name is
MC68000_test_all_opcodes.S68

