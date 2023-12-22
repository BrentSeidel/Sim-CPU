--
--  Package for decoding Group 8 - OR/DIV/SBCD
--
package BBS.Sim_CPU.m68000.line_8 is
   procedure decode_8(self : in out m68000);
   procedure decode_DIVS(self : in out m68000);
   procedure decode_DIVU(self : in out m68000);
private
   type step_div is record
     reg_y  : uint3;
     mode_y : uint3;
     code   : uint3;  --  7 for DIVS, 3 for DIVU
     reg_x  : uint3;
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
end;
