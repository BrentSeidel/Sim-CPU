with BBS.embed;
with BBS.Sim_CPU;
with BBS.Sim_CPU.i8080;
with BBS.Sim_CPU.serial;
with BBS.Sim_CPU.serial.telnet;
with BBS.Sim_CPU.disk;
with Ada.Strings.Unbounded;
--
--  This is a collection of utility functions to support testing CPU simulators.
--
package test_util is
   --
   --  Instantiate disk controller
   --
  package floppy_ctrl is new BBS.Sim_CPU.disk(sector_size => 128, max_drives => 16);
   --
   --  The CPU simulator object and I/O devices
   --
   cpu   : aliased BBS.Sim_CPU.i8080.i8080;
   con   : aliased BBS.Sim_CPU.serial.con8;
   tel   : aliased BBS.Sim_CPU.serial.telnet.tel_tty;
   print : aliased BBS.Sim_CPU.serial.print8;
   fd    : aliased floppy_ctrl.disk_ctrl;
   --
   --  Register dump
   --
   procedure dump_reg(c : BBS.Sim_CPU.simulator'Class);
   --
   --  Command loop
   --
   procedure cmds;
   --
   --  Memory
   --
   procedure dump_mem(start : BBS.Sim_CPU.word);
   --
   --  For hexidecimal numbers
   --
   procedure nextValue(v : out BBS.embed.uint32;
                       s : in out Ada.Strings.Unbounded.Unbounded_String);
   --
private
   --
   --  This needs to be set to True when on a Windows machine when using
   --  the git bash shell because it doesn't seem to handle get_immediate properly.
   --
   gitbash : constant Boolean := False;
   --
end test_util;
