--
--  Package for decoding Line E (14) instructions - Shift/Rotate/Bit Field
--
package BBS.Sim_CPU.m68000.line_e is
   procedure decode_e(self : in out m68000);
private
   type step_aslr1 is record
      reg_y   : uint3;
      mode_y  : uint3;
      code1   : uint2;  --  3 for ASL/ASR 1 operand
      dir     : Boolean;
      code2   : uint3;  --  0 for ASL/ASR 1 operand
      pre     : prefix;
   end record;
   for step_aslr1 use record
      reg_y   at 0 range 0 .. 2;
      mode_y  at 0 range 3 .. 5;
      code1   at 0 range 6 .. 7;
      dir     at 0 range 8 .. 8;
      code2   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   type step_aslr2 is record
      reg_y : uint3;
      code  : uint2;  --  0 for ASL/ASR 2 operand, 1 for LSL/LSR
      reg   : Boolean;
      size  : data_size;
      dir   : Boolean;
      count : uint3;
      pre   : prefix;
   end record;
   for step_aslr2 use record
      reg_y at 0 range 0 .. 2;
      code  at 0 range 3 .. 4;  --  0 for ASL/ASR, 1 for LSL/LSR
      reg   at 0 range 5 .. 5;
      size  at 0 range 6 .. 7;
      dir   at 0 range 8 .. 8;
      count at 0 range 9 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   --
   instr_aslr1 : step_aslr1  --  Decode ASL/ASR instructions (1 operand)
      with address => instr'Address;
   instr_aslr2 : step_aslr2  --  Decode ASL/ASR instructions (2 operand)
      with address => instr'Address;

   procedure decode_ASLR2(self : in out m68000)
      with pre => ((instr_aslr2.code) = 0 and (instr_aslr2.size /= data_long_long));
   procedure decode_ASLR1(self : in out m68000)
      with pre => ((instr_aslr1.code2 = 0) and (instr_aslr1.code1 = 3));
   procedure decode_LSLR(self : in out m68000)
      with pre => ((instr_aslr2.code) = 1 and (instr_aslr2.size /= data_long_long));
end;
