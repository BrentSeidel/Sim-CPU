--
--  Package for decoding Group 8 - OR/DIV/SBCD
--
package BBS.Sim_CPU.m68000.line_8 is
   procedure decode_8(self : in out m68000);
private
   type step_div is record
     reg_y  : reg_num;
     mode_y : mode_code;
     code   : uint3;  --  7 for DIVS, 3 for DIVU
     reg_x  : reg_num;
     pre    : prefix;
   end record;
   for step_div use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      code   at 0 range 6 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 ..15;
   end record;
   --
   instr_div : step_div  --  Decode DIV instructions
      with address => instr'Address;

   procedure decode_DIVS(self : in out m68000)
      with pre => (instr_div.code = 7);
   procedure decode_DIVU(self : in out m68000)
      with pre => (instr_div.code = 3);
end;
