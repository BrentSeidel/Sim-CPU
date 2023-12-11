--
--  Package for decoding Line 0 instructions -  - Bit manipulation/MOVEP/Immediate
--
package BBS.Sim_CPU.m68000.line_0 is
   procedure decode_0(self : in out m68000);
   procedure decode_ADDI(self : in out m68000);
   procedure decode_ANDI(self : in out m68000);
   procedure decode_BCHG(self : in out m68000);
private
   bit_pos : array (long range 0 .. 31) of long := (
               16#0000_0001#,
               16#0000_0002#,
               16#0000_0004#,
               16#0000_0008#,
               16#0000_0010#,
               16#0000_0020#,
               16#0000_0040#,
               16#0000_0080#,
               16#0000_0100#,
               16#0000_0200#,
               16#0000_0400#,
               16#0000_0800#,
               16#0000_1000#,
               16#0000_2000#,
               16#0000_4000#,
               16#0000_8000#,
               16#0001_0000#,
               16#0002_0000#,
               16#0004_0000#,
               16#0008_0000#,
               16#0010_0000#,
               16#0020_0000#,
               16#0040_0000#,
               16#0080_0000#,
               16#0100_0000#,
               16#0200_0000#,
               16#0400_0000#,
               16#0800_0000#,
               16#1000_0000#,
               16#2000_0000#,
               16#4000_0000#,
               16#8000_0000#);
end;
