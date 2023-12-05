--
--  Package for decoding Line C (12) instructions - AND/MUL/ABCD/EXG
--
package BBS.Sim_CPU.m68000.line_c is
   procedure decode_c(self : in out m68000);
   procedure decode_abcd(self : in out m68000);
   procedure decode_and(self : in out m68000);

end;
