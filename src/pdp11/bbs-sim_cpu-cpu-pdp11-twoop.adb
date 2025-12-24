--
--  Author: Brent Seidel
--  Date: 15-Dec-2025
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
--  Code for PDP-11 two operand instructions:
--    ADD
--    BIC/BICB
--    BIS/BISB
--    BIT/BITB
--    MOV/MOVB
--    CMP/CMPB
--    SUB
--
with Ada.Text_IO;
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package body BBS.Sim_CPU.CPU.PDP11.twoop is
   --
   --  Move word
   --
   procedure MOV(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_word);
      val     : constant word := self.get_ea(ea_src);
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_word);
      begin
         self.set_ea(ea_dest, val);
         self.post_ea(ea_dest);
      end;
      self.psw.zero := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := False;
   end;
   --
   --  Move byte
   --
   procedure MOVB(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_byte);
      val     : constant word := self.get_ea(ea_src);
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_byte);
      begin
         if ea_dest.mode = 0 then
            ea_dest.size := data_word;
            self.set_ea(ea_dest, sign_extend((byte(val and 16#FF#))));
         else
            self.set_ea(ea_dest, val);
         end if;
         self.post_ea(ea_dest);
      end;
      self.psw.zero := (val = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := False;
   end;
   --
   --  Compare word or byte
   --
   procedure CMP(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_word);
      src     : constant word := self.get_ea(ea_src);
      diff    : uint32;
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_word);
         dest    : constant word := self.get_ea(ea_dest);
      begin
         self.post_ea(ea_dest);
         diff := uint32(src) - uint32(dest);
         self.psw.overflow := ((src and 16#8000#) /= (dest and 16#8000#)) and
           ((dest and 16#8000#) = word(diff and 16#8000#));
      end;
      self.psw.zero := (diff = 0);
      self.psw.negative := (diff and 16#8000#) /= 0;
      --
      --  PDP-11/05 manual conflict with PDP11 processor handbook and standard
      --  practice for setting carry.  Standard practives is used here.
      --
      self.psw.carry := (diff and 16#ffff_0000#) /= 0;
   end;
   --
   procedure CMPB(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_byte);
      src     : constant word := self.get_ea(ea_src);
      diff    : uint32;
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_byte);
         dest    : constant word := self.get_ea(ea_dest);
      begin
         self.post_ea(ea_dest);
         diff := uint32(src) - uint32(dest);
         self.psw.overflow := ((src and 16#80#) /= (dest and 16#80#)) and
           ((dest and 16#80#) = word(diff and 16#80#));
      end;
      self.psw.zero := (diff = 0);
      self.psw.negative := (diff and 16#80#) /= 0;
      --
      --  PDP-11/05 manual conflict with PDP11 processor handbook and standard
      --  practice for setting carry.  Standard practives is used here.
      --
      self.psw.carry := (diff and 16#ffff_ff00#) /= 0;
   end;
   --
   --  Addition and subtraction
   --
   procedure ADD(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_word);
      src     : constant word := self.get_ea(ea_src);
      sum    : uint32;
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_word);
         dest    : constant word := self.get_ea(ea_dest);
      begin
         sum := uint32(dest) + uint32(src);
         Ada.Text_IO.Put_Line("ADD: " & toHex(src) & " + " & toHex(dest) & " = " & toHex(sum));
         self.set_ea(ea_dest, word(sum and 16#FFFF#));
         self.post_ea(ea_dest);
         self.psw.overflow := ((src and 16#8000#) /= (dest and 16#8000#)) and
           ((dest and 16#8000#) = word(sum and 16#8000#));
      end;
      self.psw.zero := (sum = 0);
      self.psw.negative := (sum and 16#8000#) /= 0;
      self.psw.carry := (sum and 16#ffff_0000#) /= 0;
   end;
   --
   procedure SUB(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_word);
      src     : constant word := self.get_ea(ea_src);
      diff    : uint32;
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_word);
         dest    : constant word := self.get_ea(ea_dest);
      begin
         diff := uint32(dest) - uint32(src);
         self.set_ea(ea_dest, word(diff and 16#FFFF#));
         self.post_ea(ea_dest);
         self.psw.overflow := ((src and 16#8000#) /= (dest and 16#8000#)) and
           ((dest and 16#8000#) = word(diff and 16#8000#));
      end;
      self.psw.zero := (diff = 0);
      self.psw.negative := (diff and 16#8000#) /= 0;
      self.psw.carry := (diff and 16#ffff_0000#) /= 0;
   end;
   --
   --  Bit instructions
   --
   --  Bit test
   procedure BIT(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_word);
      src     : constant word := self.get_ea(ea_src);
      result  : word;
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_word);
         dest    : constant word := self.get_ea(ea_dest);
      begin
         result := src and dest;
         self.post_ea(ea_dest);
      end;
      self.psw.zero := (result = 0);
      self.psw.negative := (result and 16#8000#) /= 0;
      self.psw.overflow := False;
   end;
   --
   procedure BITB(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_byte);
      src     : constant word := self.get_ea(ea_src);
      result  : word;
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_byte);
         dest    : constant word := self.get_ea(ea_dest);
      begin
         result := src and dest;
         self.post_ea(ea_dest);
      end;
      self.psw.zero := (result = 0);
      self.psw.negative := (result and 16#80#) /= 0;
      self.psw.overflow := False;
   end;
   --
   -- Bit clear
   procedure BIC(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_word);
      src     : constant word := self.get_ea(ea_src);
      result  : word;
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_word);
         dest    : constant word := self.get_ea(ea_dest);
      begin
         result := (not src) and dest;
         self.set_ea(ea_dest, result);
         self.post_ea(ea_dest);
      end;
      self.psw.zero := (result = 0);
      self.psw.negative := (result and 16#8000#) /= 0;
      self.psw.overflow := False;
   end;
   --
   procedure BICB(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_byte);
      src     : constant word := self.get_ea(ea_src);
      result  : word;
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_byte);
         dest    : constant word := self.get_ea(ea_dest);
      begin
         result := (not src) and dest and 16#FF#;
         self.set_ea(ea_dest, result);
         self.post_ea(ea_dest);
      end;
      self.psw.zero := (result = 0);
      self.psw.negative := (result and 16#80#) /= 0;
      self.psw.overflow := False;
   end;
   --
   -- Bit Set
   procedure BIS(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_word);
      src     : constant word := self.get_ea(ea_src);
      result  : word;
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_word);
         dest    : constant word := self.get_ea(ea_dest);
      begin
         result := src or dest;
         self.set_ea(ea_dest, result);
         self.post_ea(ea_dest);
      end;
      self.psw.zero := (result = 0);
      self.psw.negative := (result and 16#8000#) /= 0;
      self.psw.overflow := False;
   end;
   --
   procedure BISB(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_byte);
      src     : constant word := self.get_ea(ea_src);
      result  : word;
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_byte);
         dest    : constant word := self.get_ea(ea_dest);
      begin
         result := (src or dest) and 16#FF#;
         Ada.Text_IO.Put_Line("BISB: " &  toHex(src) & ", " & toHex(dest) & " = " & toHex(result));
         self.set_ea(ea_dest, result);
         self.post_ea(ea_dest);
      end;
      self.psw.zero := (result = 0);
      self.psw.negative := (result and 16#80#) /= 0;
      self.psw.overflow := False;
   end;
   --
end;
