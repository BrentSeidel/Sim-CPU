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
--  Package for decoding Line 3 instructions - Move word
--
package BBS.Sim_CPU.CPU.m68000.line_3 is
   procedure decode_3(self : in out m68000);
private
   --
   procedure decode_MOVEW(self : in out m68000)
      with pre => (not ((instr_move.mode_x = 7) and (instr_move.reg_x = 2)) or
        ((instr_move.mode_x = 7) and (instr_move.reg_x = 3)) or
        ((instr_move.mode_x = 7) and (instr_move.reg_x = 4)));
end;
