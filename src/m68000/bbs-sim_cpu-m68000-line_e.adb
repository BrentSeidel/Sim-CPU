with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_e is
   --
   --  Package for decoding Line E (14) instructions - Shift/Rotate/Bit Field
   --
   procedure decode_e(self : in out m68000) is
   begin
      if (instr_aslr2.code) = 0 and (instr_aslr2.size /= data_long_long) then
         Ada.Text_IO.Put_Line("ASL/ASR with reg_y = " & uint3'Image(instr_aslr2.reg_y));
         decode_aslr2(self);
      elsif (instr_aslr1.code2 = 0) and (instr_aslr1.code1 = 3) then
         Ada.Text_IO.Put_Line("ASL/ASR to EA");
         decode_aslr1(self);
      else
         Ada.Text_IO.Put_Line("Other line E instructions");
      end if;
   end;
   --
   --  One operand arithmatic shift left or right
   --  This shifts a single word (16 bits) in memory one bit left or
   --  right.
   --
   procedure decode_aslr1(self : in out m68000) is
      msb   : Boolean;
      nmsb  : Boolean;
      lsb   : Boolean;
      ea    : operand := self.get_ea(instr_aslr1.reg_y, instr_aslr1.mode_y, data_word);
      value : word;
   begin
      value := word(self.get_ea(ea, data_word));
      if instr_aslr1.dir then  --  Shift left
         msb  := (value and 16#8000#) /= 0;
         nmsb := (value and 16#4000#) /= 0;
         value := value * 2;
         self.psw.carry := msb;
         self.psw.extend := msb;
         self.psw.overflow := (msb /= nmsb);
      else  --  Shift right
         lsb  := (value and 1) /= 0;
         value := value / 2;
         self.psw.carry := lsb;
         self.psw.extend := lsb;
         self.psw.overflow := False;
         if msb then
           value := value or 16#8000#;
         end if;
      end if;
      self.psw.negative := (value and 16#8000#) /= 0;
      self.psw.zero := (value = 0);
      self.set_ea(ea, long(value), data_word);
      self.post_ea(instr_aslr1.reg_y, instr_aslr1.mode_y, data_word);
   end;
   --
   --  Two operand arithmatic shift left or right
   --  This can shift a byte, word, or long word in a data register.
   --  The number of bits to shift may be in a register (0-63) or in the
   --  instruction (1-8).
   --
   procedure decode_aslr2(self : in out m68000) is
      count : byte;
      reg   : uint3 := instr_aslr2.reg_y;
      msb   : Boolean;
      nmsb  : Boolean;
      lsb   : Boolean;
      value : long;
   begin
      if instr_aslr2.reg then
        count := byte(self.get_regb(data, instr_aslr2.count) and 16#3F#);
      else
         count := byte(instr_aslr2.count);
         if count = 0 then
            count := 8;
         end if;
      end if;
      self.psw.overflow := False;
      if count = 0 then  --  Only set flags
         self.psw.carry := False;
         self.psw.overflow := False;
         case instr_aslr2.size is
            when data_byte =>
               self.psw.negative := ((self.get_regb(Data, reg) and 16#80#) /= 0);
               self.psw.zero := (self.get_regb(Data, reg) = 0);
            when data_word =>
               self.psw.negative := ((self.get_regw(Data, reg) and 16#8000#) /= 0);
               self.psw.zero := (self.get_regw(Data, reg) = 0);
            when data_long =>
               self.psw.negative := ((self.get_regl(Data, reg) and 16#8000_0000#) /= 0);
               self.psw.zero := (self.get_regl(Data, reg) = 0);
            when others =>
               null;  -- Should never happen due to earlier test
         end case;
      else  -- Do actual shifting
         value := self.get_regl(data, reg);
         self.psw.overflow := False;
         if instr_aslr2.dir then  -- Shift left
            case instr_aslr2.size is
               when data_byte =>
                  for i in 1 .. (count and 16#0F#) loop
                     msb := (value and 16#80#) /= 0;
                     nmsb := (value and 16#40#) /= 0;
                     value := value * 2;
                     if (msb /= nmsb) then
                        self.psw.overflow := True;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#80#) /= 0;
                  self.set_regb(Data, reg, byte(value and 16#FF#));
               when data_word =>
                  for i in 1 .. (count and 16#1F#) loop
                     msb := (value and 16#8000#) /= 0;
                     nmsb := (value and 16#4000#) /= 0;
                     value := value * 2;
                     if (msb /= nmsb) then
                        self.psw.overflow := True;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#8000#) /= 0;
                  self.set_regw(Data, reg, word(value and 16#FFFF#));
               when data_long =>
                  for i in 1 .. (count and 16#3F#) loop
                     msb := (value and 16#8000_0000#) /= 0;
                     nmsb := (value and 16#4000_0000#) /= 0;
                     value := value * 2;
                     if (msb /= nmsb) then
                        self.psw.overflow := True;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#8000_0000#) /= 0;
                  self.set_regl(Data, reg, value);
               when others =>
                  null;  -- Should never happen due to earlier test
            end case;
            self.psw.carry := msb;
            self.psw.extend := msb;
            self.psw.zero := (value = 0);
         else  --  Shift right
         value := self.get_regl(data, reg);
         self.psw.overflow := False;
            case instr_aslr2.size is
               when data_byte =>
                  value := value and 16#FF#;
                  for i in 1 .. (count and 16#0F#) loop
                     msb := (value and 16#80#) /= 0;
                     lsb := (value and 16#01#) /= 0;
                     value := value / 2;
                     if msb then
                        value := value or 16#80#;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#80#) /= 0;
                  self.set_regb(Data, reg, byte(value and 16#FF#));
               when data_word =>
                  value := value and 16#FFFF#;
                  for i in 1 .. (count and 16#1F#) loop
                     msb := (value and 16#8000#) /= 0;
                     lsb := (value and 16#01#) /= 0;
                     value := value / 2;
                     if msb then
                        value := value or 16#8000#;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#8000#) /= 0;
                  self.set_regw(Data, reg, word(value and 16#FFFF#));
               when data_long =>
                  for i in 1 .. (count and 16#2F#) loop
                     msb := (value and 16#8000_0000#) /= 0;
                     lsb := (value and 16#01#) /= 0;
                     value := value / 2;
                     if msb then
                        value := value or 16#8000_0000#;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#8000_0000#) /= 0;
                  self.set_regl(Data, reg, value);
               when others =>
                  null;  -- Should never happen due to earlier test
            end case;
            self.psw.carry := lsb;
            self.psw.extend := lsb;
            self.psw.zero := (value = 0);
         end if;
      end if;
   end;
end;
