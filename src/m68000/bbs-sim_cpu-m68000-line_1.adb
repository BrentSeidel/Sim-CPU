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
package body BBS.Sim_CPU.m68000.line_1 is
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                           target => status_word);
   --
   --  Package for decoding Line 1 instructions - Move byte
   --
   procedure decode_1(self : in out m68000) is
      mode_x : constant mode_code := instr_move.mode_x;
      reg_x  : constant reg_num := instr_move.reg_x;
   begin
      --
      --  Check to make sure that addressing modes are legal.
      --
      if (mode_x = 1) or  --  Not An.  Note that MOVEA is not legal for byte size
        ((mode_x = 7) and (reg_x = 2)) or  --  Not d(PC)
        ((mode_x = 7) and (reg_x = 3)) or  --  Not d(PC,Xi)
        ((mode_x = 7) and (reg_x = 4)) then  --  Not immediate
         Ada.Text_IO.Put_Line("Invalid destination mode for MOVE.B");
         BBS.Sim_CPU.m68000.exceptions.process_exception(self,
            BBS.Sim_CPU.m68000.exceptions.ex_4_ill_inst);
      else
         decode_MOVEB(self);
      end if;
   end;
   --
   procedure decode_MOVEB(self : in out m68000) is
      ea_src  : constant operand := self.get_ea(instr_move.reg_y, instr_move.mode_y, data_byte);
      ea_dest : constant operand := self.get_ea(instr_move.reg_x, instr_move.mode_x, data_byte);
      val     : constant byte := byte(self.get_ea(ea_src) and 16#FF#);
   begin
--      Ada.Text_IO.Put_Line("Processing MOVE.B instruction");
      self.set_ea(ea_dest, long(val));
      self.post_ea(ea_src);
      self.post_ea(ea_dest);
      self.psw.negative := msb(val);
      self.psw.zero := (val = 0);
      self.psw.overflow := False;
      self.psw.carry := False;
   end;
end;
