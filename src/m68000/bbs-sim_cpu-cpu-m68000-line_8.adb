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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS.Sim_CPU.CPU.m68000.exceptions;
package body BBS.Sim_CPU.CPU.m68000.line_8 is
   function uint32_to_int32 is new Ada.Unchecked_Conversion(source => long,
                                                           target => int32);
   function int32_to_uint32 is new Ada.Unchecked_Conversion(source => int32,
                                                           target => long);
   --
   --  Package for decoding Group 8 - OR/DIV/SBCD
   --
   procedure decode_8(self : in out m68000) is
   begin
      if instr_2op.code = 7 then  --  DIVS instructions
         decode_DIVS(self);
      elsif instr_bcd.code = 16#10# then
         decode_SBCD(self);
      elsif instr_2op.code = 3 then  --  DIVU instruction
         decode_DIVU(self);
      elsif (instr_2op.code = 0) or (instr_2op.code = 1) or (instr_2op.code = 2) or
            (instr_2op.code = 4) or (instr_2op.code = 5) or (instr_2op.code = 6) then
         decode_OR(self);
      else
         BBS.Sim_CPU.CPU.m68000.exceptions.process_exception(self,
            BBS.Sim_CPU.CPU.m68000.exceptions.ex_4_ill_inst);
      end if;
   end;
   --
   procedure decode_DIVS(self : in out m68000) is
      reg_y  : constant reg_num := instr_2op.reg_y;
      mode_y : constant mode_code := instr_2op.mode_y;
      reg_x  : constant reg_num := instr_2op.reg_x;
      ea     : constant operand := self.get_ea(reg_y, mode_y, data_word);
      op1    : constant int32 := uint32_to_int32(self.get_regl(Data, reg_x));
      op2    : constant int32 := uint32_to_int32(sign_extend(word(self.get_ea(ea) and 16#FFFF#)));
      result : int32;
      remain : int32;
      resl   : long;
      reml   : long;
   begin
      self.post_ea(ea);
--      Ada.Text_IO.Put_Line("Processing DIVS instruction.");
      if op2 = 0 then  --  Divide by 0 exception
         BBS.Sim_CPU.CPU.m68000.exceptions.process_exception(self,
               BBS.Sim_CPU.CPU.m68000.exceptions.ex_5_div0);
         return;
      end if;
      result := op1/op2;
      remain := op1 rem op2;
      resl := int32_to_uint32(result);
      reml := int32_to_uint32(remain);
      self.psw.carry := False;
      if (resl /= sign_extend(word(resl and 16#FFFF#))) then
         self.psw.overflow := True;
      else
         self.psw.overflow := False;
         self.psw.zero := ((int32_to_uint32(result) and 16#FFFF#) = 0);
         self.psw.negative := (int32_to_uint32(result) and 16#8000#) /= 0;
         resl := resl and 16#FFFF#;
         resl := resl or ((reml and 16#FFFF#)*16#0001_0000#);
         self.set_regl(data, reg_x, resl);
      end if;
   end;
   --
   procedure decode_DIVU(self : in out m68000) is
      reg_y  : constant reg_num := instr_2op.reg_y;
      mode_y : constant  mode_code := instr_2op.mode_y;
      reg_x  : constant reg_num := instr_2op.reg_x;
      ea     : constant operand := self.get_ea(reg_y, mode_y, data_word);
      op1    : constant long := self.get_regl(Data, reg_x);
      op2    : constant long := long(word(self.get_ea(ea) and 16#FFFF#));
      result : long;
      remain : long;
   begin
      self.post_ea(ea);
--      Ada.Text_IO.Put_Line("Processing DIVU instruction.");
      if op2 = 0 then  --  Divide by 0 exception
         BBS.Sim_CPU.CPU.m68000.exceptions.process_exception(self,
               BBS.Sim_CPU.CPU.m68000.exceptions.ex_5_div0);
         return;
      end if;
      result := op1/op2;
      remain := op1 rem op2;
      self.psw.carry := False;
      if (result > 16#FFFF#) then
         self.psw.overflow := True;
      else
         self.psw.overflow := False;
         self.psw.negative := ((result and 16#8000#) /= 0);
         self.psw.zero := ((result and 16#FFFF#) = 0);
         result := result and 16#FFFF#;
         result := result or ((remain and 16#FFFF#)*16#0001_0000#);
         self.set_regl(data, reg_x, result);
      end if;
   end;
   --
   procedure decode_OR(self : in out m68000) is
      reg_x  : constant reg_num := instr_2op.reg_x;
      reg_y  : constant reg_num := instr_2op.reg_y;
      mode_y : constant mode_code := instr_2op.mode_y;
      opmode : constant uint3 := instr_2op.code;
   begin
--      Ada.Text_IO.Put_Line("Processing OR instruction");
      case opmode is
         when 0 =>  --  Byte <ea> + Dn -> Dn
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_byte);
               op1 : constant byte := self.get_regb(Data, reg_x);
               op2 : constant byte := byte(self.get_ea(ea) and 16#FF#);
               sum : constant byte := op1 or op2;
            begin
               self.set_regb(Data, reg_x, sum);
               self.post_ea(ea);
               self.psw.negative := msb(sum);
               self.psw.zero := (sum = 0);
            end;
         when 1 =>  --  Word <ea> + Dn -> Dn
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_word);
               op1 : constant word := self.get_regw(Data, reg_x);
               op2 : constant word := word(self.get_ea(ea) and 16#FFFF#);
               sum : constant word := op1 or op2;
            begin
               self.set_regw(Data, reg_x, sum);
               self.post_ea(ea);
               self.psw.negative := msb(sum);
               self.psw.zero := (sum = 0);
            end;
         when 2 =>  --  Long <ea> + Dn -> Dn
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_long);
               op1 : constant long := self.get_regl(Data, reg_x);
               op2 : constant long := self.get_ea(ea);
               sum : constant long := op1 or op2;
            begin
               self.set_regl(Data, reg_x, sum);
               self.post_ea(ea);
               self.psw.negative := msb(sum);
               self.psw.zero := (sum = 0);
            end;
         when 4 =>  --  Byte Dn + <ea> -> <ea>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_byte);
               op1 : constant byte := self.get_regb(Data, reg_x);
               op2 : constant byte := byte(self.get_ea(ea) and 16#FF#);
               sum : constant byte := op1 or op2;
            begin
               self.set_ea(ea, long(sum and 16#FF#));
               self.post_ea(ea);
               self.psw.negative := msb(sum);
               self.psw.zero := (sum = 0);
            end;
         when 5 =>  --  Word Dn + <ea> -> <ea>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_word);
               op1 : constant word := self.get_regw(Data, reg_x);
               op2 : constant word := word(self.get_ea(ea) and 16#FFFF#);
               sum : constant word := op1 or op2;
            begin
               self.set_ea(ea, long(sum and 16#FFFF#));
               self.post_ea(ea);
               self.psw.negative := msb(sum);
               self.psw.zero := (sum = 0);
            end;
         when 6 =>  --  Long Dn + <ea> -> <ea>
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_long);
               op1 : constant long := self.get_regl(Data, reg_x);
               op2 : constant long := self.get_ea(ea);
               sum : constant long := op1 or op2;
            begin
               self.set_ea(ea, sum);
               self.post_ea(ea);
               self.psw.negative := msb(sum);
               self.psw.zero := (sum = 0);
            end;
         when others =>  --  Should not happen (DIVS/DIVU instructions)
            Ada.Text_IO.Put_Line("OR unrecognized options");
      end case;
      --
      --  Compute condition codes
      --
      --
      --  Carry, Extend, and Overflow
      --
      self.psw.Carry := False;
      self.psw.Overflow := False;
   end;
   --  dest = dest - src  (in BCD)
   procedure decode_SBCD(self : in out m68000) is
      reg_x : constant reg_num := instr_bcd.reg_x;
      reg_y : constant reg_num := instr_bcd.reg_y;
      dest  : byte;
      src   : byte;
      msds  : byte;  --  Most significant digit of source
      msdrd : byte;  --  Most significant digit of destination/result
      lsdr  : byte;  --  Least significant digit of result
      addr1 : long;
      addr2 : long;
   begin
--      Ada.Text_IO.Put_Line("Processing SBCD instruction");
      if instr_bcd.reg_mem = data then
         dest := self.get_regb(data, reg_x);
         src  := self.get_regb(data, reg_y);
      else
         addr1 := self.get_regl(address, reg_x) - 1;
         addr2 := self.get_regl(address, reg_y) - 1;
         self.set_regl(address, reg_x, addr1);
         self.set_regl(address, reg_y, addr2);
         dest := self.memory(addr1);
         src  := self.memory(addr2);
      end if;
      self.psw.carry := False;
      msds  := (src/16) and 15;   --  MSD of src
      msdrd := (dest/16) and 15;  --  MSD of dest
      lsdr  := (dest and 15) - (src and 15);  --  LSD of result
      if self.psw.extend then
         lsdr := lsdr - 1;
      end if;
      if msb(lsdr) then  --  Check for negative in an unsigned
         lsdr := lsdr + 10;
         msdrd := msdrd - 1;
      end if;
      msdrd := msdrd - msds;  --  MSD of result
      if msb(msdrd) then  --  Check for negative in an unsigned
         msdrd := msdrd + 10;
         self.psw.carry := True;
      end if;
      dest := (msdrd and 15)*16 + lsdr;
      self.psw.extend := self.psw.carry;
      if dest /= 0 then   --  Check if zero flag should be cleared
         self.psw.zero := False;
      end if;
      if instr_bcd.reg_mem = data then
         self.set_regb(data, reg_x, dest);
      else
         self.memory(addr1, dest);
      end if;
   end;
end;
