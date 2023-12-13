--
--  Package for decoding Line 4 instructions - Miscellaneous
--
package BBS.Sim_CPU.m68000.line_4 is
   procedure decode_4(self : in out m68000);
   procedure decode_CHK(self : in out m68000);
end;
