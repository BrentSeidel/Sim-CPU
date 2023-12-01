package body BBS.Sim_CPU.m68000.line_c is
   --
   --  Package for decoding Line C (12) instructions - AND/MUL/ABCD/EXG
   --
   procedure decode_c(self : in out m68000) is
   begin
      if instr_abcd.sub_code = 16#10# then  -- This is an ABCD instruction
         decode_abcd(self);
      else
        null;
      end if;
   end;
   --
   procedure decode_abcd(self : in out m68000) is
      b1 : byte;
      b2 : byte;
   begin
      if instr_abcd.reg_mem = data then
         b1 := self.get_reg(data, instr_abcd.reg_x);
         b2 := self.get_reg(data, instr_abcd.reg_y);
      else
         self.set_reg(address, instr_abcd.reg_x,
            self.get_reg(address, instr_abcd.reg_x) - 1);
         self.set_reg(address, instr_abcd.reg_y,
            self.get_reg(address, instr_abcd.reg_y) - 1);
         b1 := self.memory(self.get_reg(address, instr_abcd.reg_x));
         b2 := self.memory(self.get_reg(address, instr_abcd.reg_y));
      end if;
      b1 := bcd_to_byte(b1);
      b2 := bcd_to_byte(b2);
      b2 := b1 + b2;
      if self.psw.extend then
         b2 := b2 + 1;
      end if;
      self.psw.zero := (b2 = 0);
      if b2 > 100 then
         self.psw.extend := True;
         self.psw.carry  := True;
         b2 := b2 - 100;
      else
         self.psw.extend := False;
         self.psw.carry  := False;
      end if;
      if instr_abcd.reg_mem = data then
         self.set_reg(data, instr_abcd.reg_x, long(byte_to_bcd(b2)));
      else
         self.memory(self.get_reg(address, instr_abcd.reg_x), byte_to_bcd(b2));
      end if;
   end;
end;
