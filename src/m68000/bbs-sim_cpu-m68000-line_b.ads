--
--  Package for decoding Line 6 instructions - CMP/EOR
--
package BBS.Sim_CPU.m68000.line_b is
   procedure decode_b(self : in out m68000);
   procedure decode_cmp(self : in out m68000);
   procedure decode_cmpm(self : in out m68000);
private
   type step_cmp is record  --  Also used for AND instruction
      reg_y  : uint3;
      mode_y : uint3;
      opmode : uint3;
      reg_x  : uint3;
      pre    : prefix;
   end record;
   for step_cmp use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      opmode at 0 range 6 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 .. 15;
   end record;
   type step_cmpm is record
      reg_y : uint3;
      code1 : uint3;
      size  : data_size;
      code2 : Boolean;
      reg_x : uint3;
      pre   : prefix;
   end record;
   for step_cmpm use record
      reg_y at 0 range 0 .. 2;
      code1 at 0 range 3 .. 5;
      size  at 0 range 6 .. 7;
      code2 at 0 range 8 .. 8;
      reg_x at 0 range 9 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   --
   instr_cmp : step_cmp  --  Decode CMP instructions
      with address => instr'Address;
   instr_cmpm : step_cmpm  --  Decode CMPM instructions
      with address => instr'Address;
end;
