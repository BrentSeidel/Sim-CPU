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
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with BBS.Sim_CPU.m68000.exceptions;
package body BBS.Sim_CPU.m68000.line_0 is
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                           target => status_word);
   --
   --  Package for decoding Line 0 instructions -  - Bit manipulation/MOVEP/Immediate
   --
   procedure decode_0(self : in out m68000) is
   begin
      if (instr_imm.code = 2) and (instr_imm.size /= data_long_long) then  --  AND immediate instruction
         decode_ANDI(self);
      elsif (instr_imm.code = 6) and (instr_imm.size /= data_long_long) then  --  Add immediate instruction
         decode_ADDI(self);
      elsif (instr_imm.code = 16#A#) and (instr_imm.size /= data_long_long) then  --  Exclusive OR immediate instruction
         decode_EORI(self);
      elsif (instr_imm.code = 16#C#) and (instr_imm.size /= data_long_long) then  --  Compare immediate instruction
         decode_CMPI(self);
      elsif (instr_imm.code = 0) and (instr_imm.size /= data_long_long) then  --  OR immediate instruction
         decode_ORI(self);
      elsif (instr_imm.code = 4) and (instr_imm.size /= data_long_long) then  --  SUB immediate instruction
         decode_SUBI(self);
      elsif ((instr_movep.code = 1) and ((instr_movep.mode = 4) or
            (instr_movep.mode = 5) or (instr_movep.mode = 6) or (instr_movep.mode = 7))) then
         decode_MOVEP(self);
      elsif instr_bit.code = 4 or (instr_bit.code = 0 and instr_bit.reg_x = 4) then
         decode_BTST(self);
      elsif instr_bit.code = 5 or (instr_bit.code = 1 and instr_bit.reg_x = 4) then
         decode_BCHG(self);
      elsif instr_bit.code = 6 or (instr_bit.code = 2 and instr_bit.reg_x = 4) then
         decode_BCLR(self);
      elsif instr_bit.code = 7 or (instr_bit.code = 3 and instr_bit.reg_x = 4) then
         decode_BSET(self);
      else
         BBS.Sim_CPU.m68000.exceptions.process_exception(self,
            BBS.Sim_CPU.m68000.exceptions.ex_4_ill_inst);
      end if;
   end;
   --
   procedure decode_ADDI(self : in out m68000) is
      reg_y  : constant reg_num := instr_imm.reg_y;
      mode_y : constant mode_code := instr_imm.mode_y;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
      ext1   : constant word := self.get_ext;
      ext2   : word;
   begin
--      Ada.Text_IO.Put_Line("Processing ADDI instruction.");
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_byte);
               op1 : constant byte := byte(ext1 and 16#FF#);
               op2 : constant byte := byte(self.get_ea(ea) and 16#FF#);
               sum : constant byte := op1 + op2;
            begin
               self.set_ea(ea, long(sum));
               self.psw.zero := (sum = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when data_word =>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_word);
               op1 : constant word := ext1;
               op2 : constant word := word(self.get_ea(ea) and 16#FFFF#);
               sum : constant word := op1 + op2;
            begin
               self.set_ea(ea, long(sum));
               self.psw.zero := (sum = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when data_long =>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_long);
               op1 : constant long := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
               op2 : constant long := self.get_ea(ea);
               sum : constant long := op1 + op2;
            begin
               self.set_ea(ea, sum);
               self.psw.zero := (sum = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when others =>  --  Should never happen due to previous conditions
            null;
      end case;
      self.psw.negative := Rmsb;
      self.psw.Carry := (Smsb and Dmsb) or ((not Rmsb) and Dmsb)
                        or (Smsb and (not Rmsb));
      self.psw.Extend := self.psw.Carry;
      self.psw.Overflow := (Smsb and Dmsb and (not Rmsb))
                           or ((not Smsb) and (not Dmsb) and Rmsb);
   end;
   --
   procedure decode_ANDI(self : in out m68000) is
      reg_y  : constant reg_num := instr_imm.reg_y;
      mode_y : constant  mode_code := instr_imm.mode_y;
      ext1   : constant word := self.get_ext;
      ext2   : word;
   begin
--      Ada.Text_IO.Put_Line("Processing ANDI instruction.");
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            if (mode_y = 7) and (reg_y = 4) then  --  ANDI to CCR
               declare
                 psw  : constant word := psw_to_word(self.psw);
                 mask : constant word := ext1 and 16#FF#;
               begin
                  self.psw := word_to_psw((mask and psw) or (psw and 16#FF00#));
               end;
            else
               declare
                  ea  : constant operand := self.get_ea(reg_y, mode_y, data_byte);
                  op1 : constant byte := byte(ext1 and 16#FF#);
                  op2 : constant byte := byte(self.get_ea(ea) and 16#FF#);
                  sum : constant byte := op1 and op2;
               begin
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum = 0);
                  self.psw.negative := msb(sum);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
                  self.post_ea(ea);
               end;
            end if;
         when data_word =>
            if (mode_y = 7) and (reg_y = 4) then  --  ANDI to SR
               declare
                 psw  : constant word := psw_to_word(self.psw);
               begin
                  if self.psw.super then
                     self.psw := word_to_psw(ext1 and psw);
                  else
                     BBS.Sim_CPU.m68000.exceptions.process_exception(self,
                        BBS.Sim_CPU.m68000.exceptions.ex_8_priv_viol);
                  end if;
               end;
            else
               declare
                  ea  : constant operand := self.get_ea(reg_y, mode_y, data_word);
                  op1 : constant word := ext1;
                  op2 : constant word := word(self.get_ea(ea) and 16#FFFF#);
                  sum : constant word := op1 and op2;
               begin
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum = 0);
                  self.psw.negative := msb(sum);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
                  self.post_ea(ea);
               end;
            end if;
         when data_long =>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_long);
               op1 : constant long := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
               op2 : constant long := self.get_ea(ea);
               sum : constant long := op1 and op2;
            begin
               self.set_ea(ea, sum);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
               self.psw.Carry := False;
               self.psw.Overflow := False;
               self.post_ea(ea);
            end;
         when others =>  --  Should never happen due to previous conditions
            null;
      end case;
   end;
   --
   procedure decode_CMPI(self : in out m68000) is
      reg_y  : constant reg_num := instr_imm.reg_y;
      mode_y : constant mode_code := instr_imm.mode_y;
      ext1   : constant word := self.get_ext;
      ext2   : word;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
--      Ada.Text_IO.Put_Line("Processing CMPI instruction.");
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_byte);
               op1 : constant byte := byte(ext1 and 16#FF#);
               op2 : constant byte := byte(self.get_ea(ea));
               sum : constant byte := op2 - op1;
            begin
               self.psw.zero := (sum  = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when data_word =>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_word);
               op1 : constant word := ext1;
               op2 : constant word := word(self.get_ea(ea));
               sum : constant word := op2 - op1;
            begin
               self.psw.zero := (sum = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when data_long =>
            declare
               ea   : constant operand := self.get_ea(reg_y, mode_y, data_long);
               op1  : constant long := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
               op2  : constant long := self.get_ea(ea);
               sum  : constant long := op2 - op1;
            begin
               self.psw.zero := (sum  = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when others =>  --  Should never happen due to previous conditions
            null;
      end case;
      self.psw.negative := Rmsb;
      self.psw.overflow := ((not Smsb) and Dmsb and (not Rmsb)) or
                            (Smsb and (not Dmsb) and Rmsb);
      self.psw.carry := (Smsb and not Dmsb) or (Rmsb and not Dmsb) or (Smsb and Rmsb);
   end;
   --
   procedure decode_EORI(self : in out m68000) is
      reg_y  : constant reg_num := instr_imm.reg_y;
      mode_y : constant mode_code := instr_imm.mode_y;
      ext1   : constant word := self.get_ext;
      ext2   : word;
   begin
--      Ada.Text_IO.Put_Line("Processing EORI instruction.");
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            if (mode_y = 7) and (reg_y = 4) then  --  EORI to CCR
               declare
                 psw  : constant word := psw_to_word(self.psw);
                 mask : constant word := ext1 and 16#FF#;
               begin
                  self.psw := word_to_psw((mask xor psw) or (psw and 16#FF00#));
               end;
            else
               declare
                  ea  : constant operand := self.get_ea(reg_y, mode_y, data_byte);
                  op1 : constant byte := byte(ext1 and 16#FF#);
                  op2 : constant byte := byte(self.get_ea(ea) and 16#FF#);
                  sum : constant byte := op1 xor op2;
               begin
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum = 0);
                  self.psw.negative := msb(sum);
                  self.post_ea(ea);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
               end;
            end if;
         when data_word =>
            if (mode_y = 7) and (reg_y = 4) then  --  EORI to SR
               declare
                 psw  : constant word := psw_to_word(self.psw);
               begin
                  if not self.psw.super then
                     BBS.Sim_CPU.m68000.exceptions.process_exception(self,
                        BBS.Sim_CPU.m68000.exceptions.ex_8_priv_viol);
                  else
                     self.psw := word_to_psw(ext1 xor psw);
                  end if;
               end;
            else
               declare
                  ea  : constant operand := self.get_ea(reg_y, mode_y, data_word);
                  op1 : constant word := ext1;
                  op2 : constant word := word(self.get_ea(ea) and 16#FFFF#);
                  sum : constant word := op1 xor op2;
               begin
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum  = 0);
                  self.psw.negative := msb(sum);
                  self.post_ea(ea);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
               end;
            end if;
         when data_long =>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_long);
               op1 : constant long := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
               op2 : constant long := self.get_ea(ea);
               sum : constant long := op1 xor op2;
            begin
               self.set_ea(ea, sum);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
               self.post_ea(ea);
               self.psw.Carry := False;
               self.psw.Overflow := False;
            end;
         when others =>
            Ada.Text_IO.Put_Line("Invalid size for EORI instruction.");
      end case;
   end;
   --
   procedure decode_ORI(self : in out m68000) is
      reg_y  : constant reg_num := instr_imm.reg_y;
      mode_y : constant mode_code := instr_imm.mode_y;
      ext1   : constant word := self.get_ext;
      ext2   : word;
   begin
--      Ada.Text_IO.Put_Line("Processing ORI instruction.");
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            if (mode_y = 7) and (reg_y = 4) then  --  ORI to CCR
               declare
                 psw  : constant word := psw_to_word(self.psw);
                 mask : constant word := ext1 and 16#FF#;
               begin
                  self.psw := word_to_psw( mask or psw);
               end;
            else
               declare
                  ea  : constant operand := self.get_ea(reg_y, mode_y, data_byte);
                  op1 : constant byte := byte(ext1 and 16#FF#);
                  op2 : constant byte := byte(self.get_ea(ea) and 16#FF#);
                  sum : constant byte := op1 or op2;
               begin
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum = 0);
                  self.psw.negative := msb(sum);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
                  self.post_ea(ea);
               end;
            end if;
         when data_word =>
            if (mode_y = 7) and (reg_y = 4) then  --  EORI to SR
               declare
                 psw  : constant word := psw_to_word(self.psw);
               begin
                  if not self.psw.super then
                     BBS.Sim_CPU.m68000.exceptions.process_exception(self,
                        BBS.Sim_CPU.m68000.exceptions.ex_8_priv_viol);
                  else
                     self.psw := word_to_psw(ext1 or psw);
                  end if;
               end;
            else
               declare
                  ea  : constant operand := self.get_ea(reg_y, mode_y, data_word);
                  op1 : constant word := ext1;
                  op2 : constant word := word(self.get_ea(ea) and 16#FFFF#);
                  sum : constant word := op1 or op2;
               begin
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum and 16#FFFF#) = 0;
                  self.psw.negative := msb(sum);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
                  self.post_ea(ea);
               end;
            end if;
         when data_long =>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_long);
               op1 : constant long := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
               op2 : constant long := self.get_ea(ea);
               sum : constant long := op1 or op2;
            begin
               self.set_ea(ea, sum);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
               self.psw.Carry := False;
               self.psw.Overflow := False;
               self.post_ea(ea);
            end;
         when others =>  --  Should never happen due to previous conditions
            null;
      end case;
   end;
   --
   procedure decode_SUBI(self : in out m68000) is
      reg_y  : constant reg_num := instr_imm.reg_y;
      mode_y : constant mode_code := instr_imm.mode_y;
      ext1   : constant word := self.get_ext;
      ext2   : word;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
--      Ada.Text_IO.Put_Line("Processing SUBI instruction.");
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            declare
               ea   : constant operand := self.get_ea(reg_y, mode_y, data_byte);
               src  : constant byte := byte(ext1 and 16#FF#);
               dest : constant byte := byte(self.get_ea(ea) and 16#FF#);
               diff : constant byte := dest - src;
            begin
               self.set_ea(ea, long(diff));
               self.psw.zero := (diff = 0);
               Rmsb := msb(diff);
               Smsb := msb(src);
               Dmsb := msb(dest);
               self.post_ea(ea);
            end;
         when data_word =>
            declare
               ea   : constant operand := self.get_ea(reg_y, mode_y, data_word);
               src  : constant word := ext1;
               dest : constant word := word(self.get_ea(ea) and 16#FFFF#);
               diff : constant word := dest - src;
            begin
               self.set_ea(ea, long(diff));
               self.psw.zero := (diff = 0);
               Rmsb := msb(diff);
               Smsb := msb(src);
               Dmsb := msb(dest);
               self.post_ea(ea);
            end;
         when data_long =>
            declare
               ea   : constant operand := self.get_ea(reg_y, mode_y, data_long);
               src  : constant long := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
               dest : constant long := self.get_ea(ea);
               diff : constant long := dest - src;
            begin
               self.set_ea(ea, diff);
               self.psw.zero := (diff = 0);
               Rmsb := msb(diff);
               Smsb := msb(src);
               Dmsb := msb(dest);
               self.post_ea(ea);
            end;
         when others =>  --  Should never happen due to previous conditions
            null;
      end case;
      self.psw.negative := Rmsb;
      self.psw.Carry := (Smsb and (not Dmsb)) or (Rmsb and (not Dmsb))
                        or (Smsb and Rmsb);
      self.psw.Extend := self.psw.Carry;
      self.psw.Overflow := ((not Smsb) and Dmsb and (not Rmsb))
                           or (Smsb and (not Dmsb) and Rmsb);
   end;
   --
   --  Move to peripheral
   --
   procedure decode_MOVEP(self : in out m68000) is
      disp : constant long := sign_extend(self.get_ext);
      base : constant long := self.get_regl(Address, instr_movep.reg_y) + disp;
      mode : constant uint3 := instr_movep.mode;
      reg  : constant reg_num := instr_movep.reg_x;
      val  : long;
      temp : byte;
   begin
--      Ada.Text_IO.Put_Line("Processing MOVEP instruction.");
      case mode is
         when 4 =>  --  Transfer word from memory to register
            temp := self.memory(base);
            val := long(temp)*16#100#;
            temp := self.memory(base+2);
            val := val + long(temp);
            self.set_regw(Data, reg, word(val and 16#ffff#));
         when 5 =>  --  Transfer long from memory to register
            temp := self.memory(base);
            val  := long(temp)*16#0100_0000#;
            temp := self.memory(base + 2);
            val  := val + long(temp)*16#0001_0000#;
            temp := self.memory(base + 4);
            val  := val + long(temp)*16#0000_0100#;
            temp := self.memory(base + 6);
            val  := val + long(temp);
            self.set_regl(Data, reg, val);
         when 6 =>  --  Transfer word from register to memory
            val  := self.get_regl(Data, reg) and 16#ffff#;
            self.memory(base, byte((val/16#100#) and 16#ff#));
            self.memory(base + 2, byte(val and 16#ff#));
         when 7 =>  --  Transfer long from register to memory
            val  := self.get_regl(Data, reg);
            self.memory(base, byte((val/16#0100_0000#) and 16#ff#));
            self.memory(base+2, byte((val/16#0001_0000#) and 16#ff#));
            self.memory(base+4, byte((val/16#0000_0100#) and 16#ff#));
            self.memory(base+6, byte(val and 16#ff#));
         when others =>  --  Should never happen due to other conditions
            null;
      end case;
   end;
   --
   --  Bit instructions
   --
   procedure decode_BCHG(self : in out m68000) is
      bit_num : long;
      vall    : long;
   begin
--      Ada.Text_IO.Put_Line("Processing BCHG instruction.");
      if instr_bit.code = 5 then  --  Bit number specified in register
         bit_num := self.get_regl(Data, instr_bit.reg_x);
      else  --  Bit number specified in next word
         bit_num := long(self.get_ext and 16#FF#);
      end if;
      if instr_bit.mode_y = 0 then  --  Destination is a data register
         bit_num := bit_num and 16#1F#;  --  32 bits in a long
         vall := self.get_regl(Data, instr_bit.reg_y);
         self.psw.zero := (vall and bit_pos(bit_num)) = 0;
         vall := vall xor bit_pos(bit_num);
         self.set_regl(Data, instr_bit.reg_y, vall);
      else  --  Destination is other
         declare
            ea   : constant operand := self.get_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
            valb : constant byte := byte(self.get_ea(ea));
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (valb and byte(bit_pos(bit_num))) = 0;
            self.set_ea(ea, long(valb xor byte(bit_pos(bit_num))));
            self.post_ea(ea);
         end;
      end if;
   end;
   --
   procedure decode_BCLR(self : in out m68000) is
      bit_num : long;
      vall    : long;
   begin
--      Ada.Text_IO.Put_Line("Processing BCLR instruction.");
      if instr_bit.code = 6 then  --  Bit number specified in register
         bit_num := self.get_regl(Data, instr_bit.reg_x);
      else  --  Bit number specified in next word
         bit_num := long(self.get_ext and 16#FF#);
      end if;
      if instr_bit.mode_y = 0 then  --  Destination is a data register
         bit_num := bit_num and 16#1F#;  --  32 bits in a long
         vall := self.get_regl(Data, instr_bit.reg_y);
         self.psw.zero := (vall and bit_pos(bit_num)) = 0;
         vall := vall and not bit_pos(bit_num);
         self.set_regl(Data, instr_bit.reg_y, vall);
      else  --  Destination is other
         declare
            ea   : constant operand := self.get_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
            valb : constant byte := byte(self.get_ea(ea));
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (valb and byte(bit_pos(bit_num))) = 0;
            self.set_ea(ea, long(valb and not byte(bit_pos(bit_num))));
            self.post_ea(ea);
         end;
      end if;
   end;
   --
   procedure decode_BSET(self : in out m68000) is
      bit_num : long;
      vall    : long;
   begin
--      Ada.Text_IO.Put_Line("Processing BSET instruction.");
      if instr_bit.code = 7 then  --  Bit number specified in register
         bit_num := self.get_regl(Data, instr_bit.reg_x);
      else  --  Bit number specified in next word
         bit_num := long(self.get_ext and 16#FF#);
      end if;
      if instr_bit.mode_y = 0 then  --  Destination is a data register
         bit_num := bit_num and 16#1F#;  --  32 bits in a long
         vall := self.get_regl(Data, instr_bit.reg_y);
         self.psw.zero := (vall and bit_pos(bit_num)) = 0;
         vall := vall or bit_pos(bit_num);
         self.set_regl(Data, instr_bit.reg_y, vall);
      else  --  Destination is other
         declare
            ea   : constant operand := self.get_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
            valb : constant byte := byte(self.get_ea(ea));
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (valb and byte(bit_pos(bit_num))) = 0;
            self.set_ea(ea, long(valb or byte(bit_pos(bit_num))));
            self.post_ea(ea);
         end;
      end if;
   end;
   --
   procedure decode_BTST(self : in out m68000) is
      bit_num : long;
   begin
--      Ada.Text_IO.Put_Line("Processing BTST instruction.");
      if instr_bit.code = 4 then  --  Bit number specified in register
         bit_num := self.get_regl(Data, instr_bit.reg_x);
      else  --  Bit number specified in next word
         bit_num := long(self.get_ext and 16#FF#);
      end if;
      --
      if instr_bit.mode_y = 0 then  --  Destination is a data register
         declare
            val    : long;
         begin
            bit_num := bit_num and 16#1F#;  --  32 bits in a long
            val := self.get_regl(Data, instr_bit.reg_y);
            self.psw.zero := (val and bit_pos(bit_num)) = 0;
         end;
      else  --  Destination is other
         declare
            ea  : constant operand := self.get_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
            val : constant byte := byte(self.get_ea(ea) and 16#FF#);
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (val and byte(bit_pos(bit_num) and 16#FF#)) = 0;
            self.post_ea(ea);
         end;
      end if;
   end;
end;
