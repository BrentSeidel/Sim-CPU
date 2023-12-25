--
--  Package for decoding Line C (12) instructions - AND/MUL/ABCD/EXG
--
package BBS.Sim_CPU.m68000.line_c is
   procedure decode_c(self : in out m68000);
private
   type step_abcd is record
      reg_y    : uint3;
      reg_mem  : reg_type;
      sub_code : uint5;  --  16#10# for ABCD instruction
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
      opmode : uint3;  --  0, 1, 2, 4, 5, and 6 for AND instruction
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
   type step_exg is record
      reg_y  : uint3;
      opmode : uint5;    --  8, 9, and 17 for EXG instruction
      code1  : Boolean;  --  True for EXG instruction
      reg_x  : uint3;
      pre    : prefix;
   end record;
   for step_exg use record
      reg_y  at 0 range 0 .. 2;
      opmode at 0 range 3 .. 7;
      code1  at 0 range 8 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 .. 15;
   end record;
   --
   instr_abcd : step_abcd  --  Decode ABCD instructions
      with address => instr'Address;
   instr_and : step_and    --  Decode ADD/ADDA/AND instructions
      with address => instr'Address;
   instr_exg : step_exg    --  Decode EXG instruction
      with address => instr'Address;

   procedure decode_abcd(self : in out m68000)
      with pre => (instr_abcd.sub_code = 16);
   procedure decode_and(self : in out m68000)
      with pre => ((instr_and.opmode = 0) or (instr_and.opmode = 1) or
                   (instr_and.opmode = 2) or (instr_and.opmode = 4) or
                   (instr_and.opmode = 5) or (instr_and.opmode = 6));
   procedure decode_exg(self : in out m68000)
      with pre => ((instr_exg.opmode = 8) or (instr_exg.opmode = 9) or
                  (instr_exg.opmode = 17));

end;
