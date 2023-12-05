--
--  Package for decoding Line 0 instructions -  - Bit manipulation/MOVEP/Immediate
--
package BBS.Sim_CPU.m68000.line_0 is
   procedure decode_0(self : in out m68000);
   procedure decode_ADDI(self : in out m68000);
   procedure decode_ANDI(self : in out m68000);

end;
