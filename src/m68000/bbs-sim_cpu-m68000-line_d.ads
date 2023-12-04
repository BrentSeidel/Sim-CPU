--
--  Package for decoding Line D (13) instructions - ADD/ADDX
--
package BBS.Sim_CPU.m68000.line_d is
   procedure decode_d(self : in out m68000);
   --
   --  Note that some addressing modes for the ADD instruction are
   --  unusable and have been repurposed for ADDX instructions.  Need
   --  to check for that.
   --
   procedure add_instr(self : in out m68000);
   procedure addx_instr(self : in out m68000);
end;
