--
--  Package for decoding Line 1 instructions - Move byte
--
package BBS.Sim_CPU.m68000.line_1 is
   procedure decode_1(self : in out m68000);
private
   --
   type step_move is record
      reg_y  : uint3;
      mode_y : uint3;
      mode_x : uint3;
      reg_x  : uint3;
      pre    : prefix;  --  3 For move word
   end record;
   for step_move use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      mode_x at 0 range 6 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 ..15;
   end record;
   --
   instr_move : step_move  --  Decode MOVE instructions
      with address => instr'Address;

   procedure decode_MOVEB(self : in out m68000)
      with pre => (not ((instr_move.mode_x = 1) or
        ((instr_move.mode_x = 7) and (instr_move.reg_x = 2)) or
        ((instr_move.mode_x = 7) and (instr_move.reg_x = 3)) or
        ((instr_move.mode_x = 7) and (instr_move.reg_x = 4))));
end;
