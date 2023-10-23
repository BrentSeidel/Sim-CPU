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
   --  The CPU simulator object
   --
   cpu   : aliased BBS.Sim_CPU.i8080.i8080;
   con   : aliased BBS.Sim_CPU.serial.con8;
   tel   : aliased BBS.Sim_CPU.serial.telnet.tel_tty;
   print : aliased BBS.Sim_CPU.serial.print8;
   fd    : aliased BBS.Sim_CPU.disk.floppy8;
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
   --  This needs to be set to True when on a Windows machine because it doesn't
   --  seem to handle get_immediate properly.
   --
   windows : constant Boolean := True;
   --
end test_util;
