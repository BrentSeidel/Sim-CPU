--
--  Package for decoding Line E (14) instructions - Shift/Rotate/Bit Field
--
package BBS.Sim_CPU.m68000.line_e is
   procedure decode_e(self : in out m68000);
   procedure decode_aslr2(self : in out m68000);
   procedure decode_aslr1(self : in out m68000);
end;
