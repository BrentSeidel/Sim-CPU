with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_c is
   --
   --  Package for decoding Line C (12) instructions - AND/MUL/ABCD/EXG
   --
   procedure decode_c(self : in out m68000) is
   begin
      if (instr_exg.opmode = 8) or (instr_exg.opmode = 9) or
         (instr_exg.opmode = 17) then  --  This is an EXG instruction
         decode_EXG(self);
      elsif instr_abcd.sub_code = 16#10# then  -- This is an ABCD instruction
         decode_ABCD(self);
      elsif (instr_and.opmode = 0) or (instr_and.opmode = 1) or
            (instr_and.opmode = 2) or (instr_and.opmode = 4) or
            (instr_and.opmode = 5) or (instr_and.opmode = 6) then
         decode_AND(self);
      elsif (instr_and.opmode = 3) or (instr_and.opmode = 7) then
         decode_MUL(self);
      else
         Ada.Text_IO.Put_Line("Unimplemented/Unrecognized line C (12) instruction.");
      end if;
   end;
   --
   procedure decode_ABCD(self : in out m68000) is
      b1 : byte;
      b2 : byte;
   begin
      Ada.Text_IO.Put_Line("Processing ABCD instruction");
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
   procedure decode_AND(self : in out m68000) is
      reg_x  : reg_num := instr_and.reg_x;
      reg_y  : reg_num := instr_and.reg_y;
      mode_y : mode_code := instr_and.mode_y;
      opmode : uint3 := instr_and.opmode;
      op1    : long;
      op2    : long;
      sum    : long;
   begin
      Ada.Text_IO.Put_Line("Processing AND instruction");
      case opmode is
        when 0 =>  --  Byte <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              op1 := long(self.get_regb(Data, reg_x));
              op2 := self.get_ea(ea);
              sum := op1 and op2;
              self.set_regb(Data, reg_x, byte(sum and 16#FF#));
              self.post_ea(ea);
           end;
        when 1 =>  --  Word <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              op1 := long(self.get_regw(Data, reg_x));
              op2 := self.get_ea(ea);
              sum := op1 and op2;
              self.set_regw(Data, reg_x, word(sum and 16#FFFF#));
              self.post_ea(ea);
           end;
        when 2 =>  --  Long <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              op1 := self.get_regl(Data, reg_x);
              op2 := self.get_ea(ea);
              sum := op1 and op2;
              self.set_regl(Data, reg_x, sum);
              self.post_ea(ea);
           end;
        when 4 =>  --  Byte Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              op1 := long(self.get_regb(Data, reg_x));
              op2 := self.get_ea(ea);
              sum := op1 and op2;
              self.set_ea(ea, sum and 16#FF#);
              self.post_ea(ea);
           end;
        when 5 =>  --  Word Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              op1 := long(self.get_regw(Data, reg_x));
              op2 := self.get_ea(ea);
              sum := op1 and op2;
              self.set_ea(ea, sum and 16#FFFF#);
              self.post_ea(ea);
           end;
        when 6 =>  --  Long Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              op1 := self.get_regl(Data, reg_x);
              op2 := self.get_ea(ea);
              sum := op1 and op2;
              self.set_ea(ea, sum);
              self.post_ea(ea);
           end;
        when others =>  --  Should not happen
           Ada.Text_IO.Put_Line("AND unrecognized options");
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
      self.psw.Carry := False;
      self.psw.Overflow := False;
   end;
   --
   procedure decode_EXG(self : in out m68000) is
      mode  : uint5 := instr_exg.opmode;
      reg_x : reg_num := instr_exg.reg_x;
      reg_y : reg_num := instr_exg.reg_y;
      temp  : long;
   begin
      Ada.Text_IO.Put_Line("Processing EXG instruction");
      if mode = 8 then  --  Exchange data registers
         temp := self.get_regl(Data, reg_x);
         self.set_regl(Data, reg_x, self.get_regl(Data, reg_y));
         self.set_regl(Data, reg_y, temp);
      elsif mode = 9 then  --  Exchange address registers
         temp := self.get_regl(Address, reg_x);
         self.set_regl(Address, reg_x, self.get_regl(Address, reg_y));
         self.set_regl(Address, reg_y, temp);
      elsif mode = 17 then  --  Exchange address and data register
         temp := self.get_regl(Data, reg_x);
         self.set_regl(Data, reg_x, self.get_regl(Address, reg_y));
         self.set_regl(Address, reg_y, temp);
      end if;
   end;
   --
   procedure decode_MUL(self : in out m68000) is
      ea    : operand := self.get_ea(instr_and.reg_y, instr_and.mode_y, data_word);
      reg_x : reg_num := instr_and.reg_x;
      op1   : long;
      op2   : long;
   begin
      self.psw.carry := False;
      self.psw.overflow := False;
      if instr_and.opmode = 3 then  --  MULU
         Ada.Text_IO.Put_Line("Processing MULU instructions");
         op1 := self.get_ea(ea);
         op2 := long(self.get_regw(Data, reg_x));
      elsif instr_and.opmode = 7 then  --  MULS
         Ada.Text_IO.Put_Line("Processing MULS instructions");
         op1 := sign_extend(word(self.get_ea(ea) and 16#FFFF#));
         op2 := sign_extend(self.get_regw(Data, reg_x));
      else
         Ada.Text_IO.Put_Line("Unrecognized MUL option");
      end if;
      op1 := op1 * op2;
      self.set_regl(Data, reg_x, op1);
      self.psw.negative := (op1 and 16#8000_0000#) /= 0;
      self.psw.zero := (op1 = 0);
   end;
end;
