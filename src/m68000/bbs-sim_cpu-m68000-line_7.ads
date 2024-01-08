--
--  Package for decoding Line 7 instructions - MOVEQ
--
package BBS.Sim_CPU.m68000.line_7 is
   procedure decode_7(self : in out m68000);
private
   type step_moveq is record  --  For branch instructions
      data : byte;
      code : Boolean;  --  False for MOVEQ
      reg  : reg_num;
      pre  : prefix;
   end record;
   for step_moveq use record
      data at 0 range 0 .. 7;
      code at 0 range 8 .. 8;
      reg  at 0 range 9 .. 11;
      pre  at 0 range 12 .. 15;
   end record;
   --
   instr_moveq : step_moveq --  Decode MOVEQ instructions
      with address => instr'Address;

   procedure decode_MOVEQ(self : in out m68000)
      with pre => (not instr_moveq.code);
end;
