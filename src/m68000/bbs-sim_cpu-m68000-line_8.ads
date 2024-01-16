--
--  Package for decoding Group 8 - OR/DIV/SBCD
--
package BBS.Sim_CPU.m68000.line_8 is
   procedure decode_8(self : in out m68000);
private
   type step_2op is record  --  Two operand instruction
     reg_y  : reg_num;
     mode_y : mode_code;
     code   : uint3;  --  7 for DIVS, 3 for DIVU, 0,1,2,4,5,6 for OR
     reg_x  : reg_num;
     pre    : prefix;
   end record;
   for step_2op use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      code   at 0 range 6 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 ..15;
   end record;
   type step_sbcd is record  --  BCD Subtract
      reg_y   : reg_num;
      reg_mem : reg_type;
      code    : uint5;  --  16#10# for SBCD instruction
      reg_x   : reg_num;
      pre     : prefix;
   end record;
   for step_sbcd use record
      reg_y   at 0 range 0 .. 2;
      reg_mem at 0 range 3 .. 3;
      code    at 0 range 4 .. 8;
      reg_x   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   --
   instr_2op : step_2op  --  Decode 2 operand instructions
      with address => instr'Address;
   instr_sbcd : step_sbcd
      with address => instr'Address;

   procedure decode_DIVS(self : in out m68000)
      with pre => (instr_2op.code = 7);
   procedure decode_DIVU(self : in out m68000)
      with pre => (instr_2op.code = 3);
   procedure decode_OR(self : in out m68000)
      with pre => ((instr_2op.code /= 7) and (instr_2op.code /= 3));
   procedure decode_SBCD(self : in out m68000)
      with pre => (instr_sbcd.code = 16#10#);
end;
