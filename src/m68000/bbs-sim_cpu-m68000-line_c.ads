--
--  Package for decoding Line C (12) instructions - AND/MUL/ABCD/EXG
--
package BBS.Sim_CPU.m68000.line_c is
   procedure decode_c(self : in out m68000);
   procedure decode_abcd(self : in out m68000);
   procedure decode_and(self : in out m68000);
private
   type step_abcd is record
      reg_y    : uint3;
      reg_mem  : reg_type;
      sub_code : code5;  --  16#10# for ABCD instruction
      reg_x    : uint3;
      pre      : prefix;
   end record;
   for step_abcd use record
      reg_y    at 0 range 0 .. 2;
      reg_mem  at 0 range 3 .. 3;
      sub_code at 0 range 4 .. 8;
      reg_x    at 0 range 9 .. 11;
      pre      at 0 range 12 .. 15;
   end record;
   type step_and is record
      reg_y  : uint3;
      mode_y : uint3;
      opmode : uint3;
      reg_x  : uint3;
      pre    : prefix;
   end record;
   for step_and use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      opmode at 0 range 6 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 .. 15;
   end record;
   --
   instr_abcd : step_abcd  --  Decode ABCD instructions
      with address => instr'Address;
   instr_and : step_and    --  Decode ADD/ADDA/AND instructions
      with address => instr'Address;

end;
