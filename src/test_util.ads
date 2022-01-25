with BBS.embed;
with BBS.Sim_CPU;
with BBS.Sim_CPU.i8080;
with Ada.Strings.Unbounded;
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
   --  Memory
   --
   procedure dump_mem(start : BBS.Sim_CPU.word);
   --
   --  For hexidecimal numbers
   --
   function hexDigit(c : Character) return BBS.embed.uint32;
   function toHex(s : String) return BBS.embed.uint32;
   procedure nextValue(v : out BBS.embed.uint32;
                       s : in out Ada.Strings.Unbounded.Unbounded_String);
   --
   function isHex(c : Character) return Boolean is
     ((c >= '0' and c <= '9') or (c >= 'A' and c <= 'F') or (c >= 'a' and c <= 'f'))
       with Global => Null;
   pragma Pure_Function(isHex);
   --
end test_util;
