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
with BBS.Sim_CPU.CPU.m68000.exceptions;
package body BBS.Sim_CPU.CPU.m68000.line_c is
   --
   --  Package for decoding Line C (12) instructions - AND/MUL/ABCD/EXG
   --
   procedure decode_c(self : in out m68000) is
   begin
      if ((instr_exg.opmode = 8) or (instr_exg.opmode = 9) or
         (instr_exg.opmode = 17)) and instr_exg.code1 then  --  This is an EXG instruction
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
         BBS.Sim_CPU.CPU.m68000.exceptions.process_exception(self,
            BBS.Sim_CPU.CPU.m68000.exceptions.ex_4_ill_inst);
      end if;
   end;
   --
   procedure decode_ABCD(self : in out m68000) is
      b1 : byte;
      b2 : byte;
      dig1a : byte;
      dig2a : byte;
      dig2b : byte;
      addr1 : long;
      addr2 : long;
   begin
--      Ada.Text_IO.Put_Line("Processing ABCD instruction");
      if instr_abcd.reg_mem = data then
         b1 := self.get_regb(data, instr_abcd.reg_x);
         b2 := self.get_regb(data, instr_abcd.reg_y);
      else
         addr1 := self.get_regl(address, instr_abcd.reg_x) - 1;
         addr2 := self.get_regl(address, instr_abcd.reg_y) - 1;
         self.set_regl(address, instr_abcd.reg_x, addr1);
         self.set_regl(address, instr_abcd.reg_y, addr2);
         b1 := self.memory(addr1);
         b2 := self.memory(addr2);
      end if;
      self.psw.carry := False;
      dig1a := (b1/16) and 15;
      dig2a := (b2/16) and 15;
      dig2b := (b1 and 15) + (b2 and 15);
      if self.psw.extend then
         dig2b := dig2b + 1;
      end if;
      if dig2b > 9 then
         dig2b := dig2b - 10;
         dig2a := dig2a + 1;
      end if;
      dig2a := dig2a + dig1a;
      if dig2a > 9 then
         dig2a := dig2a - 10;
         self.psw.carry := True;
      else
         self.psw.carry := False;
      end if;
      b2 := (dig2a and 15)*16 + dig2b;
      self.psw.extend := self.psw.carry;
      if b2 /= 0 then
         self.psw.zero := False;
      end if;
      if instr_abcd.reg_mem = data then
         self.set_regb(data, instr_abcd.reg_x, b2);
      else
         self.memory(addr1, b2);
      end if;
   end;
   --
   procedure decode_AND(self : in out m68000) is
      reg_x  : constant reg_num := instr_and.reg_x;
      reg_y  : constant reg_num := instr_and.reg_y;
      mode_y : constant mode_code := instr_and.mode_y;
      opmode : constant uint3 := instr_and.opmode;
   begin
      Ada.Text_IO.Put_Line("Processing AND instruction");
      case opmode is
         when 0 =>  --  Byte <ea> + Dn -> Dn
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_byte);
               op1 : constant byte := self.get_regb(Data, reg_x);
               op2 : constant byte := byte(self.get_ea(ea) and 16#FF#);
               sum : constant byte := op1 and op2;
            begin
               self.set_regb(Data, reg_x, sum);
               self.post_ea(ea);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
            end;
         when 1 =>  --  Word <ea> + Dn -> Dn
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_word);
               op1 : constant word := self.get_regw(Data, reg_x);
               op2 : constant word := word(self.get_ea(ea) and 16#FFFF#);
               sum : constant word := op1 and op2;
            begin
               self.set_regw(Data, reg_x, sum);
               self.post_ea(ea);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
            end;
         when 2 =>  --  Long <ea> + Dn -> Dn
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_long);
               op1 : constant long := self.get_regl(Data, reg_x);
               op2 : constant long := self.get_ea(ea);
               sum : constant long := op1 and op2;
            begin
               self.set_regl(Data, reg_x, sum);
               self.post_ea(ea);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
            end;
         when 4 =>  --  Byte Dn + <ea> -> <ea>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_byte);
               op1 : constant byte := self.get_regb(Data, reg_x);
               op2 : constant byte := byte(self.get_ea(ea) and 16#FF#);
               sum : constant byte := op1 and op2;
            begin
               self.set_ea(ea, long(sum and 16#FF#));
               self.post_ea(ea);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
            end;
         when 5 =>  --  Word Dn + <ea> -> <ea>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_word);
               op1 : constant word := self.get_regw(Data, reg_x);
               op2 : constant word := word(self.get_ea(ea) and 16#FFFF#);
               sum : constant word := op1 and op2;
            begin
               self.set_ea(ea, long(sum and 16#FFFF#));
               self.post_ea(ea);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
            end;
         when 6 =>  --  Long Dn + <ea> -> <ea>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_long);
               op1 : constant long := self.get_regl(Data, reg_x);
               op2 : constant long := self.get_ea(ea);
               sum : constant long := op1 and op2;
            begin
               self.set_ea(ea, sum);
               self.post_ea(ea);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
            end;
         when others =>  --  Should not happen due to previous checks
            null;
      end case;
      --
      --  Carry and Overflow
      --
      self.psw.Carry := False;
      self.psw.Overflow := False;
   end;
   --
   procedure decode_EXG(self : in out m68000) is
      mode  : constant uint5 := instr_exg.opmode;
      reg_x : constant reg_num := instr_exg.reg_x;
      reg_y : constant reg_num := instr_exg.reg_y;
      temp  : long;
   begin
--      Ada.Text_IO.Put_Line("Processing EXG instruction");
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
      ea    : constant operand := self.get_ea(instr_and.reg_y, instr_and.mode_y, data_word);
      reg_x : constant reg_num := instr_and.reg_x;
      op1   : long;
      op2   : long;
   begin
      self.psw.carry := False;
      self.psw.overflow := False;
      if instr_and.opmode = 3 then  --  MULU
--         Ada.Text_IO.Put_Line("Processing MULU instructions");
         op1 := self.get_ea(ea);
         op2 := long(self.get_regw(Data, reg_x));
      elsif instr_and.opmode = 7 then  --  MULS
--         Ada.Text_IO.Put_Line("Processing MULS instructions");
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
