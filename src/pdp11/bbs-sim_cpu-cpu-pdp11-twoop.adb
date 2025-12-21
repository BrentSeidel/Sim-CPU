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
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_byte);
      begin
         self.set_ea(ea_dest, val);
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
      self.psw.carry := (diff and 16#ffff_ff00#) /= 0;
   end;
   --
end;
