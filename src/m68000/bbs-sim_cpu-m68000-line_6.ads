--
--  Package for decoding Line 6 instructions - Bcc/BSR/BRA
--
package BBS.Sim_CPU.m68000.line_6 is
   procedure decode_6(self : in out m68000);
private
   type step_bcc is record  --  For branch instructions
      disp : byte;
      cond : uint4;
      pre  : prefix;
   end record;
   for step_bcc use record
      disp at 0 range 0 .. 7;
      cond at 0 range 8 .. 11;
      pre  at 0 range 12 .. 15;
   end record;
   --
   instr_bcc : step_bcc  --  Decode conditional branch instructions
      with address => instr'Address;
end;
