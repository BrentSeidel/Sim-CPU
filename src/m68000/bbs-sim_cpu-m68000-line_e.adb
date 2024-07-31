--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of SimCPU.
--  SimCPU is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  SimCPU is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.--
--
with Ada.Text_IO;
with BBS.Sim_CPU.m68000.exceptions;
package body BBS.Sim_CPU.m68000.line_e is
   --
   --  Package for decoding Line E (14) instructions - Shift/Rotate/Bit Field
   --
   procedure decode_e(self : in out m68000) is
   begin
      if (instr_aslr2.code = 0) and (instr_aslr2.size /= data_long_long) then
         decode_ASLR2(self);
      elsif (instr_aslr2.code = 1) and (instr_aslr2.size /= data_long_long) then
         decode_LSLR2(self);
      elsif (instr_aslr1.code2 = 0) and (instr_aslr1.code1 = 3) then
         decode_ASLR1(self);
      elsif (instr_aslr1.code2 = 1) and (instr_aslr1.code1 = 3) then
         decode_LSLR1(self);
      elsif (instr_aslr2.code = 3) and (instr_aslr2.size /= data_long_long) then
         decode_ROLR2(self);
      elsif (instr_aslr1.code2 = 3) and (instr_aslr1.code1 = 3) then
         decode_ROLR1(self);
      elsif (instr_aslr2.code = 2) and (instr_aslr2.size /= data_long_long) then
         decode_ROXLR2(self);
      elsif (instr_aslr1.code2 = 2) and (instr_aslr1.code1 = 3) then
         decode_ROXLR1(self);
      else
--         Ada.Text_IO.Put_Line("Unrecognized line E instruction " & toHex(instr) &
--            " address " & toHex(self.inst_pc));
--         self.cpu_halt := True;
         BBS.Sim_CPU.m68000.exceptions.process_exception(self,
            BBS.Sim_CPU.m68000.exceptions.ex_4_ill_inst);
      end if;
   end;
   --
   --  One operand arithmatic shift left or right
   --  This shifts a single word (16 bits) in memory one bit left or
   --  right.
   --
   procedure decode_ASLR1(self : in out m68000) is
      ea    : constant operand := self.get_ea(instr_aslr1.reg_y, instr_aslr1.mode_y, data_word);
      msbv  : Boolean;
      nmsb  : Boolean;
      lsbv  : Boolean;
      value : word;
   begin
--      Ada.Text_IO.Put_Line("Processing ASL/ASR instruction (memory)");
      value := word(self.get_ea(ea));
      if instr_aslr1.dir then  --  Shift left
         msbv := msb(value);
         nmsb := (value and 16#4000#) /= 0;
         value := value * 2;
         self.psw.carry := msbv;
         self.psw.extend := msbv;
         self.psw.overflow := (msbv /= nmsb);
      else  --  Shift right
         lsbv := lsb(value);
         msbv := msb(value);
         value := value / 2;
         self.psw.carry := lsbv;
         self.psw.extend := lsbv;
         self.psw.overflow := False;
         if msbv then
           value := value or 16#8000#;
         end if;
      end if;
      self.psw.negative := msb(value);
      self.psw.zero := (value = 0);
      self.set_ea(ea, long(value));
      self.post_ea(ea);
   end;
   --
   --  Two operand arithmatic shift left or right
   --  This can shift a byte, word, or long word in a data register.
   --  The number of bits to shift may be in a register (0-63) or in the
   --  instruction (1-8).
   --
   procedure decode_ASLR2(self : in out m68000) is
      reg   : constant reg_num := instr_aslr2.reg_y;
      count : byte;
      msbv  : Boolean;
      nmsb  : Boolean;
      lsbv  : Boolean;
      value : long;
   begin
--      Ada.Text_IO.Put_Line("Processing ASL/ASR instruction (register)");
      if instr_aslr2.reg then
        count := byte(self.get_regb(data, reg_num(instr_aslr2.count)) and 16#3F#);
      else
         count := byte(instr_aslr2.count);
         if count = 0 then
            count := 8;
         end if;
      end if;
      self.psw.overflow := False;
      if count = 0 then  --  Only set flags
         self.psw.carry := False;
         case instr_aslr2.size is
            when data_byte =>
               self.psw.negative := msb(self.get_regb(Data, reg));
               self.psw.zero := (self.get_regb(Data, reg) = 0);
            when data_word =>
               self.psw.negative := msb(self.get_regw(Data, reg));
               self.psw.zero := (self.get_regw(Data, reg) = 0);
            when data_long =>
               self.psw.negative := msb(self.get_regl(Data, reg));
               self.psw.zero := (self.get_regl(Data, reg) = 0);
            when others =>
               null;  -- Should never happen due to earlier test
         end case;
      else  -- Do actual shifting
         value := self.get_regl(data, reg);
         if instr_aslr2.dir then  -- Shift left
            case instr_aslr2.size is
               when data_byte =>
                  for i in 1 .. (count and 16#0F#) loop
                     msbv := (value and 16#80#) /= 0;
                     nmsb := (value and 16#40#) /= 0;
                     value := value * 2;
                     if (msbv /= nmsb) then
                        self.psw.overflow := True;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#80#) /= 0;
                  self.set_regb(Data, reg, byte(value and 16#FF#));
               when data_word =>
                  for i in 1 .. (count and 16#1F#) loop
                     msbv := (value and 16#8000#) /= 0;
                     nmsb := (value and 16#4000#) /= 0;
                     value := value * 2;
                     if (msbv /= nmsb) then
                        self.psw.overflow := True;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#8000#) /= 0;
                  self.set_regw(Data, reg, word(value and 16#FFFF#));
               when data_long =>
                  for i in 1 .. (count and 16#3F#) loop
                     msbv := (value and 16#8000_0000#) /= 0;
                     nmsb := (value and 16#4000_0000#) /= 0;
                     value := value * 2;
                     if (msbv /= nmsb) then
                        self.psw.overflow := True;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#8000_0000#) /= 0;
                  self.set_regl(Data, reg, value);
               when others =>
                  null;  -- Should never happen due to earlier test
            end case;
            self.psw.carry := msbv;
            self.psw.extend := msbv;
            self.psw.zero := (value = 0);
         else  --  Shift right
            value := self.get_regl(data, reg);
            case instr_aslr2.size is
               when data_byte =>
                  value := value and 16#FF#;
                  for i in 1 .. (count and 16#0F#) loop
                     msbv := (value and 16#80#) /= 0;
                     lsbv := lsb(value);
                     value := value / 2;
                     if msbv then
                        value := value or 16#80#;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#80#) /= 0;
                  self.set_regb(Data, reg, byte(value and 16#FF#));
               when data_word =>
                  value := value and 16#FFFF#;
                  for i in 1 .. (count and 16#1F#) loop
                     msbv := (value and 16#8000#) /= 0;
                     lsbv := lsb(value);
                     value := value / 2;
                     if msbv then
                        value := value or 16#8000#;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#8000#) /= 0;
                  self.set_regw(Data, reg, word(value and 16#FFFF#));
               when data_long =>
                  for i in 1 .. (count and 16#2F#) loop
                     msbv := (value and 16#8000_0000#) /= 0;
                     lsbv := lsb(value);
                     value := value / 2;
                     if msbv then
                        value := value or 16#8000_0000#;
                     end if;
                  end loop;
                  self.psw.negative := (value and 16#8000_0000#) /= 0;
                  self.set_regl(Data, reg, value);
               when others =>
                  null;  -- Should never happen due to earlier test
            end case;
            self.psw.carry := lsbv;
            self.psw.extend := lsbv;
            self.psw.zero := (value = 0);
         end if;
      end if;
   end;
   --
   --  One operand logical shift left or right
   --  This shifts a single word (16 bits) in memory one bit left or
   --  right.
   --
   procedure decode_LSLR1(self : in out m68000) is
      ea    : constant operand := self.get_ea(instr_aslr1.reg_y, instr_aslr1.mode_y, data_word);
      msbv  : Boolean;
      nmsb  : Boolean;
      lsbv  : Boolean;
      value : word;
   begin
--      Ada.Text_IO.Put_Line("Processing LSL/LSR instruction (memory)");
      value := word(self.get_ea(ea));
      if instr_aslr1.dir then  --  Shift left
         msbv := msb(value);
         nmsb := (value and 16#4000#) /= 0;
         value := value * 2;
         self.psw.carry := msbv;
         self.psw.extend := msbv;
      else  --  Shift right
         lsbv := lsb(value);
         msbv := msb(value);
         value := value / 2;
         self.psw.carry := lsbv;
         self.psw.extend := lsbv;
      end if;
      self.psw.overflow := False;
      self.psw.negative := msb(value);
      self.psw.zero := (value = 0);
      self.set_ea(ea, long(value));
      self.post_ea(ea);
   end;
   --
   procedure decode_LSLR2(self : in out m68000) is
      reg   : constant reg_num := instr_aslr2.reg_y;
      dir   : constant Boolean := instr_aslr2.dir;
      size  : constant Data_size := instr_aslr2.size;
      count : byte;
      msbv  : Boolean;
      lsbv  : Boolean;
      value : long;
   begin
--      Ada.Text_IO.Put_Line("Processing LSL/LSR instruction (register)");
      if instr_aslr2.reg then
        count := byte(self.get_regb(data, reg_num(instr_aslr2.count)) and 16#3F#);
      else
         count := byte(instr_aslr2.count);
         if count = 0 then
            count := 8;
         end if;
      end if;
      self.psw.overflow := False;
      if count = 0 then  --  Only set flags
         self.psw.carry := False;
         case instr_aslr2.size is
            when data_byte =>
               self.psw.negative := msb(self.get_regb(Data, reg));
               self.psw.zero := (self.get_regb(Data, reg) = 0);
            when data_word =>
               self.psw.negative := msb(self.get_regw(Data, reg));
               self.psw.zero := (self.get_regw(Data, reg) = 0);
            when data_long =>
               self.psw.negative := msb(self.get_regl(Data, reg));
               self.psw.zero := (self.get_regl(Data, reg) = 0);
            when others =>
               null;  -- Should never happen due to earlier test
         end case;
      else  -- Do actual shifting
         value := self.get_regl(data, reg);
         if instr_aslr2.dir then  -- Shift left
            case instr_aslr2.size is
               when data_byte =>
                  for i in 1 .. (count and 16#0F#) loop
                     msbv := (value and 16#80#) /= 0;
                     value := value * 2;
                  end loop;
                  self.psw.negative := (value and 16#80#) /= 0;
                  self.set_regb(Data, reg, byte(value and 16#FF#));
               when data_word =>
                  for i in 1 .. (count and 16#1F#) loop
                     msbv := (value and 16#8000#) /= 0;
                     value := value * 2;
                  end loop;
                  self.psw.negative := (value and 16#8000#) /= 0;
                  self.set_regw(Data, reg, word(value and 16#FFFF#));
               when data_long =>
                  for i in 1 .. (count and 16#3F#) loop
                     msbv := (value and 16#8000_0000#) /= 0;
                     value := value * 2;
                  end loop;
                  self.psw.negative := (value and 16#8000_0000#) /= 0;
                  self.set_regl(Data, reg, value);
               when others =>
                  null;  -- Should never happen due to earlier test
            end case;
            self.psw.carry := msbv;
            self.psw.extend := msbv;
            self.psw.zero := (value = 0);
         else  --  Shift right
            value := self.get_regl(data, reg);
            case instr_aslr2.size is
               when data_byte =>
                  value := value and 16#FF#;
                  for i in 1 .. (count and 16#0F#) loop
                     lsbv := lsb(value);
                     value := value / 2;
                  end loop;
                  self.psw.negative := (value and 16#80#) /= 0;
                  self.set_regb(Data, reg, byte(value and 16#FF#));
               when data_word =>
                  value := value and 16#FFFF#;
                  for i in 1 .. (count and 16#1F#) loop
                     lsbv := lsb(value);
                     value := value / 2;
                  end loop;
                  self.psw.negative := (value and 16#8000#) /= 0;
                  self.set_regw(Data, reg, word(value and 16#FFFF#));
               when data_long =>
                  for i in 1 .. (count and 16#2F#) loop
                     lsbv := lsb(value);
                     value := value / 2;
                  end loop;
                  self.psw.negative := (value and 16#8000_0000#) /= 0;
                  self.set_regl(Data, reg, value);
               when others =>
                  null;  -- Should never happen due to earlier test
            end case;
            self.psw.carry := lsbv;
            self.psw.extend := lsbv;
            self.psw.zero := (value = 0);
         end if;
      end if;
   end;
   --
   procedure decode_ROLR2(self : in out m68000) is
      reg   : constant reg_num := instr_aslr2.reg_y;
      count : byte;
      msbv  : Boolean;
      lsbv  : Boolean;
   begin
--      Ada.Text_IO.Put_Line("Processing ROL/ROR instruction (register)");
      if instr_aslr2.reg then
         count := byte(self.get_regb(Data, reg_num(instr_aslr2.count)) and 16#3F#);
      else
         count := byte(instr_aslr2.count);
         if count = 0 then
            count := 8;
         end if;
      end if;
      self.psw.overflow := False;
      if count = 0 then  --  Only set flags
         self.psw.carry := False;
         case instr_aslr2.size is
            when data_byte =>
               self.psw.negative := msb(self.get_regb(Data, reg));
               self.psw.zero := (self.get_regb(Data, reg) = 0);
            when data_word =>
               self.psw.negative := msb(self.get_regw(Data, reg));
               self.psw.zero := (self.get_regw(Data, reg) = 0);
            when data_long =>
               self.psw.negative := msb(self.get_regl(Data, reg));
               self.psw.zero := (self.get_regl(Data, reg) = 0);
            when others =>
               null;  -- Should never happen due to earlier test
         end case;
      else  -- Do actual shifting
         if instr_aslr2.dir then  -- Shift left
            case instr_aslr2.size is
               when data_byte =>
                  declare
                     val : byte := self.get_regb(Data, reg);
                  begin
--                     Ada.Text_IO.Put_Line("ROL.B #" & byte'Image(count));
                     for i in 1 .. count loop
                        msbv := msb(val);
                        val := val * 2;
                        if msbv then
                           val := val or 1;
                        end if;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regb(Data, reg, val);
                  end;
               when data_word =>
                  declare
                     val : word := self.get_regw(Data, reg);
                  begin
--                     Ada.Text_IO.Put_Line("ROL.# #" & byte'Image(count));
                     for i in 1 .. count loop
                        msbv := msb(val);
                        val := val * 2;
                        if msbv  then
                           val := val or 1;
                        end if;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regw(Data, reg, val);
                  end;
               when data_long =>
                  declare
                     val : long := self.get_regl(Data, reg);
                  begin
--                     Ada.Text_IO.Put_Line("ROL.L #" & byte'Image(count));
                     for i in 1 .. count loop
                        msbv := msb(val);
                        val := val * 2;
                        if msbv then
                           val := val or 1;
                        end if;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regl(Data, reg, val);
                  end;
               when others =>
                  null;  -- Should never happen due to earlier test
            end case;
            self.psw.carry := msbv;
         else  --  Shift right
            case instr_aslr2.size is
               when data_byte =>
                  declare
                     val : byte := self.get_regb(Data, reg);
                  begin
--                     Ada.Text_IO.Put_Line("ROR.B #" & byte'Image(count));
                     for i in 1 .. count loop
                        lsbv := lsb(val);
                        val := val / 2;
                        if lsbv then
                           val := val or 16#80#;
                        end if;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regb(Data, reg, val);
                  end;
               when data_word =>
                  declare
                     val : word := self.get_regw(Data, reg);
                  begin
--                     Ada.Text_IO.Put_Line("ROR.W #" & byte'Image(count));
                     for i in 1 .. count loop
                        lsbv := lsb(val);
                        val := val / 2;
                        if lsbv then
                           val := val or 16#8000#;
                        end if;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regw(Data, reg, val);
                  end;
               when data_long =>
                  declare
                     val : long := self.get_regl(Data, reg);
                  begin
--                     Ada.Text_IO.Put_Line("ROR.L #" & byte'Image(count));
                     for i in 1 .. count loop
                        lsbv := lsb(val);
                        val := val / 2;
                        if lsbv then
                           val := val or 16#8000_0000#;
                        end if;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regl(Data, reg, val);
                  end;
               when others =>
                  null;  -- Should never happen due to earlier test
            end case;
            self.psw.carry := lsbv;
         end if;
      end if;
   end;
   --
   procedure decode_ROLR1(self : in out m68000) is
      ea    : constant operand := self.get_ea(instr_aslr1.reg_y, instr_aslr1.mode_y, data_word);
      msbv  : Boolean;
      lsbv  : Boolean;
      value : word;
   begin
--      Ada.Text_IO.Put_Line("Processing ROL/ROR instruction (memory)");
      value := word(self.get_ea(ea));
      if instr_aslr1.dir then  --  Shift left
         msbv := msb(value);
         value := value * 2;
         self.psw.carry := msbv;
         if msbv then
           value := value or 1;
         end if;
      else  --  Shift right
         lsbv := lsb(value);
         value := value / 2;
         self.psw.carry := lsbv;
         if lsbv then
           value := value or 16#8000#;
         end if;
      end if;
      self.psw.overflow := False;
      self.psw.negative := msb(value);
      self.psw.zero := (value = 0);
      self.set_ea(ea, long(value));
      self.post_ea(ea);
   end;
   --
   procedure decode_ROXLR2(self : in out m68000) is
      reg   : constant reg_num := instr_aslr2.reg_y;
      count : byte;
      msbv  : Boolean;
      lsbv  : Boolean;
   begin
--      Ada.Text_IO.Put_Line("Processing ROXL/ROXR instruction (register)");
      if instr_aslr2.reg then
        count := byte(self.get_regb(Data, reg_num(instr_aslr2.count)) and 16#3F#);
      else
         count := byte(instr_aslr2.count);
         if count = 0 then
            count := 8;
         end if;
      end if;
      self.psw.overflow := False;
      if count = 0 then  --  Only set flags
         self.psw.carry := False;
         case instr_aslr2.size is
            when data_byte =>
               self.psw.negative := msb(self.get_regb(Data, reg));
               self.psw.zero := (self.get_regb(Data, reg) = 0);
            when data_word =>
               self.psw.negative := msb(self.get_regw(Data, reg));
               self.psw.zero := (self.get_regw(Data, reg) = 0);
            when data_long =>
               self.psw.negative := msb(self.get_regl(Data, reg));
               self.psw.zero := (self.get_regl(Data, reg) = 0);
            when others =>
               null;  -- Should never happen due to earlier test
         end case;
      else  -- Do actual shifting
         if instr_aslr2.dir then  -- Shift left
            case instr_aslr2.size is
               when data_byte =>
                  declare
                     val : byte := self.get_regb(Data, reg);
                  begin
                     for i in 1 .. count loop
                        msbv := msb(val);
                        val := val * 2;
                        if self.psw.extend then
                           val := val or 1;
                        end if;
                        self.psw.extend := msbv;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regb(Data, reg, val);
                  end;
               when data_word =>
                  declare
                     val : word := self.get_regw(Data, reg);
                  begin
                     for i in 1 .. count loop
                        msbv := msb(val);
                        val := val * 2;
                        if self.psw.extend then
                           val := val or 1;
                        end if;
                        self.psw.extend := msbv;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regw(Data, reg, val);
                  end;
               when data_long =>
                  declare
                     val : long := self.get_regl(Data, reg);
                  begin
                     for i in 1 .. count loop
                        msbv := msb(val);
                        val := val * 2;
                        if self.psw.extend then
                           val := val or 1;
                        end if;
                        self.psw.extend := msbv;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regl(Data, reg, val);
                  end;
               when others =>
                  null;  -- Should never happen due to earlier test
            end case;
            self.psw.carry := msbv;
         else  --  Shift right
            case instr_aslr2.size is
               when data_byte =>
                  declare
                     val : byte := self.get_regb(Data, reg);
                  begin
                     for i in 1 .. count loop
                        lsbv := lsb(val);
                        val := val / 2;
                        if self.psw.extend then
                           val := val or 16#80#;
                        end if;
                        self.psw.extend := lsbv;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regb(Data, reg, val);
                  end;
               when data_word =>
                  declare
                     val : word := self.get_regw(Data, reg);
                  begin
                     for i in 1 .. count loop
                        lsbv := lsb(val);
                        val := val / 2;
                        if self.psw.extend then
                           val := val or 16#8000#;
                        end if;
                        self.psw.extend := lsbv;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regw(Data, reg, val);
                  end;
               when data_long =>
                  declare
                     val : long := self.get_regl(Data, reg);
                  begin
                     for i in 1 .. count loop
                        lsbv := lsb(val);
                        val := val / 2;
                        if self.psw.extend then
                           val := val or 16#8000_0000#;
                        end if;
                        self.psw.extend := lsbv;
                     end loop;
                     self.psw.zero := val = 0;
                     self.psw.negative := msb(val);
                     self.set_regl(Data, reg, val);
                  end;
               when others =>
                  null;  -- Should never happen due to earlier test
            end case;
            self.psw.carry := lsbv;
         end if;
      end if;
   end;
   --
   procedure decode_ROXLR1(self : in out m68000) is
      ea    : constant operand := self.get_ea(instr_aslr1.reg_y, instr_aslr1.mode_y, data_word);
      msbv  : Boolean;
      lsbv  : Boolean;
      value : word;
   begin
--      Ada.Text_IO.Put_Line("Processing ROXL/ROXR instruction (memory)");
      value := word(self.get_ea(ea));
      if instr_aslr1.dir then  --  Shift left
         msbv := msb(value);
         value := value * 2;
         self.psw.carry := msbv;
         if self.psw.extend then
           value := value or 1;
         end if;
         self.psw.extend := msbv;
      else  --  Shift right
         lsbv := lsb(value);
         value := value / 2;
         self.psw.carry := lsbv;
         if self.psw.extend then
           value := value or 16#8000#;
         end if;
         self.psw.extend := lsbv;
      end if;
      self.psw.overflow := False;
      self.psw.negative := msb(value);
      self.psw.zero := (value = 0);
      self.set_ea(ea, long(value));
      self.post_ea(ea);
   end;
   --
end;
