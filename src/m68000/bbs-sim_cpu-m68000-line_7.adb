with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_7 is
   --
   --  Package for decoding Line 7 instructions - MOVEQ
   --
   procedure decode_7(self : in out m68000) is
   begin
      if not instr_moveq.code then
         decode_MOVEQ(self);
      else
         Ada.Text_IO.Put_Line("Unrecognized line 7 instruction.");
      end if;
   end;
   --
   procedure decode_MOVEQ(self : in out m68000) is
      value : long := sign_extend(instr_moveq.data);
   begin
      Ada.Text_IO.Put_Line("Decoding MOVEQ instruction.");
      self.set_regl(Data, instr_moveq.reg, value);
      self.psw.overflow := False;
      self.psw.carry := False;
      self.psw.negative := (value and 16#8000_0000#) /= 0;
      self.psw.zero := (value = 0);
   end;
end;

