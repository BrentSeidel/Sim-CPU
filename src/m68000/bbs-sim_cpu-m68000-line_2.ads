--
--  Package for decoding Line 2 instructions - Move long
--
package BBS.Sim_CPU.m68000.line_2 is
   procedure decode_2(self : in out m68000);
private
   --
   type step_move is record
      reg_y  : reg_num;
      mode_y : mode_code;
      mode_x : mode_code;
      reg_x  : reg_num;
      pre    : prefix;  --  2 For move long
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

   procedure decode_MOVEL(self : in out m68000)
      with pre => (not ((instr_move.mode_x = 7) and (instr_move.reg_x = 2)) or
        ((instr_move.mode_x = 7) and (instr_move.reg_x = 3)) or
        ((instr_move.mode_x = 7) and (instr_move.reg_x = 4)));
end;
