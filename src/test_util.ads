with BBS.embed;
with BBS.Sim_CPU;
with BBS.Sim_CPU.i8080;
--
--  This is a collection of utility functions to support testing CPU simulators.
--
package test_util is
   --
   --  The CPU simulator object
   --
   cpu : BBS.Sim_CPU.i8080.i8080;
   --
   --  Register dump
   --
   procedure dump_reg(c : BBS.Sim_CPU.simulator'Class);
   --
   --  Command loop
   --
   procedure cmds;
   --
   --  For hexidecimal numbers
   --
   function hexDigit(c : Character) return BBS.embed.uint32;
   --
   function isHex(c : Character) return Boolean is
     ((c >= '0' and c <= '9') or (c >= 'A' and c <= 'F') or (c >= 'a' and c <= 'f'))
       with Global => Null;
   pragma Pure_Function(isHex);
   --
end test_util;
