--
--  Package for decoding Line 0 instructions -  - Bit manipulation/MOVEP/Immediate
--
package BBS.Sim_CPU.m68000.line_0 is
   procedure decode_0(self : in out m68000);
   procedure decode_ADDI(self : in out m68000);
   procedure decode_ANDI(self : in out m68000);
   procedure decode_BCHG(self : in out m68000);
   procedure decode_BCLR(self : in out m68000);
   procedure decode_BSET(self : in out m68000);
   procedure decode_BTST(self : in out m68000);
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
   --
   type step_addi is record  --  Also used for ANDI instruction
     reg_y  : uint3;
     mode_y : uint3;
     size   : data_size;
     code   : uint4;  --  6 for ADDI instruction, 2 for ANDI
     pre    : prefix;
   end record;
   for step_addi use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      size   at 0 range 6 .. 7;
      code   at 0 range 8 .. 11;
      pre    at 0 range 12 ..15;
   end record;
   type step_bit is record
      reg_y   : uint3;
      mode_y  : uint3;
      code    : uint3;  --  Specifies which bit instruction
      reg_x   : uint3;
      pre     : prefix;
   end record;
   for step_bit use record
      reg_y   at 0 range 0 .. 2;
      mode_y  at 0 range 3 .. 5;
      code    at 0 range 6 .. 8;
      reg_x   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   --
   instr_addi : step_addi  --  Decode ADDI instructions
      with address => instr'Address;
   instr_bit : step_bit  --  Decode test the various bit instructions
      with address => instr'Address;
end;
