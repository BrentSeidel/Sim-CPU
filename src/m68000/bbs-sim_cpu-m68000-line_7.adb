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
package body BBS.Sim_CPU.m68000.line_7 is
   --
   --  Package for decoding Line 7 instructions - MOVEQ
   --
   procedure decode_7(self : in out m68000) is
   begin
      if not instr_moveq.code then
         decode_MOVEQ(self);
      else
         BBS.Sim_CPU.m68000.exceptions.process_exception(self,
            BBS.Sim_CPU.m68000.exceptions.ex_4_ill_inst);
      end if;
   end;
   --
   procedure decode_MOVEQ(self : in out m68000) is
      value : constant long := sign_extend(instr_moveq.data);
   begin
--      Ada.Text_IO.Put_Line("Decoding MOVEQ instruction.");
      self.set_regl(Data, instr_moveq.reg, value);
      self.psw.overflow := False;
      self.psw.carry := False;
      self.psw.negative := (value and 16#8000_0000#) /= 0;
      self.psw.zero := (value = 0);
   end;
end;

