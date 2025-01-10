#  LoadCPM
This is a simple program to load a CP/M image into a disk image for a
simulated 8 inch floppy disk.  It uses the 8080 simulator to load an
Intel Hex file into the simulator's memory and then reads it out and into
the disk image.

A BIOS is provided as part of Sim-CPU.  Assembly language source for CP/M
2.2 is available from [CP/M Sources](http://www.cpm.z80.de/source.html)
(and probably other places).  I had to modify the syntax a bit to get it
to assemble using the assembler and linker from [ASxxxx](https://github.com/0cjs/ASxxxx)

The commands to assemble and link CP/M are:
*  First combine the BIOS and the CP/M assembly source into one file with
 the BIOS at the end.  This file should be named CPM22-BIOS.asm.
*  Assemble the file using the command: as8085 -lo CPM22-BIOS
*  Link using the command: aslink -mi CPM22-BIOS
*  This will produce a number of files.  The ones that are the most important
 for creating a bootable disk are: CPM22-BIOS.ihx (the Intel Hex file) and
 CPM22-BIOS.map (the linker map file).
* Run the loadcpm program.  This will require the name of a disk image
 (I use cpmboot.img), the name of the CP/M source (the Intel Hex file),
 the starting address (find the value of the CBASE symbol in the map file),
 and the ending address (find the value of the CMPEND symbol in the map file).
*  You will probably need to also update the values in the L0Boot.asm file
 if you have CBASE at something other than E400.  Or, LoadCPM can now
 create a boot assembly language file for you based on the entered values.
