--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of SimCPU CLI.
--  SimCPU is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  SimCPU is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
with BBS;
with BBS.Sim_CPU;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
with BBS.Sim_CPU.CPU;
with BBS.Sim_CPU.CPU.Example;
with BBS.Sim_CPU.CPU.i8080;
with BBS.Sim_CPU.CPU.m68000;
with BBS.Sim_CPU.CPU.msc6502;
with BBS.Sim_CPU.io.serial;
with BBS.Sim_CPU.io.serial.telnet;
with BBS.Sim_CPU.io.serial.mux;
with BBS.Sim_CPU.io.disk;
with BBS.Sim_CPU.io.Clock;
with BBS.Lisp.parser.File;
with BBS.Lisp.parser.stdio;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
--
--  This is a collection of utility functions to support testing CPU simulators.
--
package cli is
   --
   --  Vector to hold devices
   --
   package dev_vect is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => BBS.Sim_CPU.io.io_access);
   --
   --  Instantiate disk controller
   --
  package floppy_ctrl is new BBS.Sim_CPU.io.disk(sector_size => 128);
   --
   --  The CPU simulator object and I/O devices
   --
   cpu       : BBS.Sim_CPU.CPU.sim_access;
   cpu_selected : Boolean := False;
   print     : aliased BBS.Sim_CPU.io.serial.print8;
   dev_table : array (BBS.Sim_CPU.io.dev_type) of dev_vect.Vector;
   --
   --  Register dump
   --
   procedure dump_reg(c : BBS.Sim_CPU.CPU.simulator'Class);
   --
   --  Do initialization
   --
   procedure init;
   --
   --  Process argument
   --
   procedure process_args;
   --
   --  Command loop.  The supported commands are:
   --  ATTACH <device> <addr> <bus> [<dev-specific>]
   --    Attaches a specific I/O device to the bus address
   --  BREAK <addr>
   --    Set a breakpoint (currently only one can be active at a time)
   --  CONTINUE
   --    Continue execution
   --  CPU
   --    Select the CPU to simulate
   --  DEP <addr> <value>
   --    Deposit value to a memory location
   --  DISK <cmds>
   --    Commands for disk drives
   --  DUMP <addr>
   --    Display a region of memory
   --  EXIT
   --    EXIT the program
   --  GO <addr>
   --    Start execution at a specified address
   --  LISP
   --    Enter Lisp interpreter
   --  LIST
   --    List attached devices or information about an attached controller
   --  LOAD <filename>
   --    Load data from a file into memory
   --  QUIT
   --    Synonym for EXIT
   --  REG
   --    Display register values
   --  RUN
   --    Execute instructions until halt or breakpoint
   --  STEP
   --    Execute one instruction
   --  TAPE <cmds>
   --    Commands for tape drives
   --  TRACE <level>
   --    Print information for each instruction executed
   --  UNBREAK <addr>
   --    Remove a breakpoint
   --
   procedure cmds;
   --
   --  Disk commands.  This is called to process the DISK command in the CLI.
   --  Subcommands are:
   --    CLOSE - Close the file attached to a drive
   --    GEOM - Set the geometry for the attached drive
   --    OPEN - Attach a file representing a disk image to a drive
   --    READONLY - Set a drive to read-only
   --    READWRITE - Set a drive to read-write
   --
   procedure disk_cmd(s : Ada.Strings.Unbounded.Unbounded_String);
   --
   --  Tape commands.  This is called to process the TAPE command in the CLI.
   --  Subcommands are:
   --    CLOSE - Close the file attached to a drive
   --    OPEN - Attach a file to a drive reader or writer
   --
   procedure tape_cmd(s : Ada.Strings.Unbounded.Unbounded_String);
   --
   --  Printer commands.  This is called to process the PRINT command in the CLI.
   --  Subcommands are:
   --    CLOSE - Close the file attached to the printer
   --    OPEN - Attach a file to a printer
   --
   procedure print_cmd(s : Ada.Strings.Unbounded.Unbounded_String);
   --
   --  Attach an I/O device to a simulation
   --  ATTACH dev addr type user
   --    dev  - Device name
   --    addr - Address of device
   --    type - Type for address (MEM or IO)
   --    user - Additional device specific parameters
   --
   --  Supported devices
   --    CLK, FD, MUX, PTP, TEL
   --
   procedure attach(s : Ada.Strings.Unbounded.Unbounded_String);
   --
   --  List devices
   --
   procedure list(s : Ada.Strings.Unbounded.Unbounded_String);
   --
   --  Memory
   --
   procedure dump_mem(start : BBS.Sim_CPU.addr_bus);
   --
   --  Print info for a floppy disk controller
   --
   procedure floppy_info(dev : in out BBS.Sim_CPU.io.io_access; ctrl : Natural);
   --
   --  Add a device to the device table
   --
   procedure add_device(dev : BBS.Sim_CPU.io.io_access);
   --
   --  Make a device name
   --
   function make_dev_name(dev : BBS.Sim_CPU.io.io_access; i : Natural) return String;
   --
   --  Parse device name
   --
   procedure parse_dev_name(name : Ada.Strings.Unbounded.Unbounded_String;
                            dev : out Ada.Strings.Unbounded.Unbounded_String;
                            unit : out Natural);
   --
   --  Find device by name.  If success is False, the returned value is invalid.
   --
   function find_dev_by_name(name : Ada.Strings.Unbounded.Unbounded_String; success : out Boolean)
                             return BBS.Sim_CPU.io.io_access;
   --
   --  Select the CPU to use
   --
   procedure set_cpu(s : Ada.Strings.Unbounded.Unbounded_String);
private
   --
   --  This needs to be set to True when on a Windows machine when using
   --  the git bash shell because it doesn't seem to handle get_immediate properly.
   --
   gitbash : constant Boolean := False;
   --
   --  Record for disk drive information
   --
   type disk_info is record
      kind : Boolean := False;  --  False is floppy disk, True is hard disk
      ctrl : Natural := 0;      --  Controller number
      drive : Natural := 0;     --  Drive attached to the controller
   end record;
   --
   --  Lisp parsers
   --
   --  Buffer for keyboard and file input to Lisp parser
   --
   stdio_buff : aliased BBS.lisp.parser.stdio.parser_stdio;
   file_buff  : aliased BBS.lisp.parser.file.parser_file;
end cli;
