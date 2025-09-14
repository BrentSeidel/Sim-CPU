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
--  Package for decoding Group 8 - OR/DIV/SBCD
--
package BBS.Sim_CPU.CPU.m68000.line_8 is
   procedure decode_8(self : in out m68000);
private
   --
   procedure decode_DIVS(self : in out m68000)
      with pre => (instr_2op.code = 7);
   procedure decode_DIVU(self : in out m68000)
      with pre => (instr_2op.code = 3);
   procedure decode_OR(self : in out m68000)
      with pre => ((instr_2op.code /= 7) and (instr_2op.code /= 3));
   procedure decode_SBCD(self : in out m68000)
      with pre => (instr_bcd.code = 16#10#);
end;
