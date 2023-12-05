package body BBS.Sim_CPU.m68000.line_c is
   --
   --  Package for decoding Line C (12) instructions - AND/MUL/ABCD/EXG
   --
   procedure decode_c(self : in out m68000) is
   begin
      if instr_abcd.sub_code = 16#10# then  -- This is an ABCD instruction
         decode_abcd(self);
      else
          decode_and(self);
      end if;
   end;
   --
   procedure decode_abcd(self : in out m68000) is
      b1 : byte;
      b2 : byte;
   begin
      if instr_abcd.reg_mem = data then
         b1 := self.get_regb(data, instr_abcd.reg_x);
         b2 := self.get_regb(data, instr_abcd.reg_y);
      else
         self.set_regl(address, instr_abcd.reg_x,
            self.get_regl(address, instr_abcd.reg_x) - 1);
         self.set_regl(address, instr_abcd.reg_y,
            self.get_regl(address, instr_abcd.reg_y) - 1);
         b1 := self.memory(self.get_regl(address, instr_abcd.reg_x));
         b2 := self.memory(self.get_regl(address, instr_abcd.reg_y));
      end if;
      b1 := bcd_to_byte(b1);
      b2 := bcd_to_byte(b2);
      b2 := b1 + b2;
      if self.psw.extend then
         b2 := b2 + 1;
      end if;
      if b2 /= 0 then
         self.psw.zero := False;
      end if;
      if b2 > 100 then
         self.psw.extend := True;
         self.psw.carry  := True;
         b2 := b2 - 100;
      else
         self.psw.extend := False;
         self.psw.carry  := False;
      end if;
      if instr_abcd.reg_mem = data then
         self.set_regb(data, instr_abcd.reg_x, byte_to_bcd(b2));
      else
         self.memory(self.get_regl(address, instr_abcd.reg_x), byte_to_bcd(b2));
      end if;
   end;
   --
   procedure decode_and(self : in out m68000) is
      reg_x  : uint3 := instr_add.reg_x;
      reg_y  : uint3 := instr_add.reg_y;
      mode_y : uint3 := instr_add.mode_y;
      opmode : uint3 := instr_add.opmode;
      op1    : long;
      op2    : long;
      sum    : long;
   begin
      case opmode is
        when 0 =>  --  Byte <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              self.post_ea(reg_y, mode_y, data_byte);
              op1 := long(self.get_regb(Data, reg_x));
              op2 := self.get_ea(ea, data_byte);
              sum := op1 and op2;
              self.set_regb(Data, reg_x, byte(sum and 16#FF#));
           end;
        when 1 =>  --  Word <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              self.post_ea(reg_y, mode_y, data_word);
              op1 := long(self.get_regw(Data, reg_x));
              op2 := self.get_ea(ea, data_word);
              sum := op1 and op2;
              self.set_regw(Data, reg_x, word(sum and 16#FFFF#));
           end;
        when 2 =>  --  Long <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              self.post_ea(reg_y, mode_y, data_long);
              op1 := self.get_regl(Data, reg_x);
              op2 := self.get_ea(ea, data_long);
              sum := op1 and op2;
              self.set_regl(Data, reg_x, sum);
           end;
        when 4 =>  --  Byte Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              self.post_ea(reg_y, mode_y, data_byte);
              op1 := long(self.get_regb(Data, reg_x));
              op2 := self.get_ea(ea, data_byte);
              sum := op1 and op2;
              self.set_ea(ea, sum and 16#FF#, data_byte);
           end;
        when 5 =>  --  Word Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              self.post_ea(reg_y, mode_y, data_word);
              op1 := long(self.get_regw(Data, reg_x));
              op2 := self.get_ea(ea, data_word);
              sum := op1 and op2;
              self.set_ea(ea, sum and 16#FFFF#, data_word);
           end;
        when 6 =>  --  Long Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              self.post_ea(reg_y, mode_y, data_long);
              op1 := self.get_regl(Data, reg_x);
              op2 := self.get_ea(ea, data_long);
              sum := op1 and op2;
              self.set_ea(ea, sum, data_long);
           end;
        when others =>  --  Should not happen
           null;
      end case;
      --
      --  Compute condition codes
      --
      case opmode is
         when 0 =>  --  Byte size
            self.psw.zero := (sum and 16#FF#) = 0;
            self.psw.negative := (sum and 16#80#) = 16#80#;
         when 1 | 5 =>  --  Word size
            self.psw.zero := (sum and 16#FFFF#) = 0;
            self.psw.negative := (sum and 16#8000#) = 16#8000#;
         when 2 | 6 =>  --  Long size
            self.psw.zero := (sum and 16#FFFF_FFFF#) = 0;
            self.psw.negative := (sum and 16#8000_0000#) = 16#8000_0000#;
         when others =>  --  Modes 3 & 7 do not affect condition codes
            null;
      end case;
      --
      --  Carry, Extend, and Overflow
      --
      if (opmode /= 3) and (opmode /= 7) then
         self.psw.Carry := False;
         self.psw.Overflow := False;
      end if;
   end;
end;
