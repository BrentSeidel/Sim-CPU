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
--  Package for decoding Line 9 instructions - SUB/SUBX
--
package BBS.Sim_CPU.CPU.m68000.line_9 is
   procedure decode_9(self : in out m68000);
private
   --
   --  Note that some addressing modes for the SUB instruction are
   --  unusable and have been repurposed for SUBX instructions.  Need
   --  to check for that.
   --
   procedure decode_SUB(self : in out m68000)
      with pre => (not (instr_2op_size.code1 = 0 and instr_2op_size.code2 and instr_2op_size.size /= data_long_long));
   procedure decode_SUBX(self : in out m68000)
      with pre => (instr_2op_size.code1 = 0 and instr_2op_size.code2 and instr_2op_size.size /= data_long_long);
end;
