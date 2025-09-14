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
--  Package for decoding Line C (12) instructions - AND/MUL/ABCD/EXG
--
package BBS.Sim_CPU.CPU.m68000.line_c is
   procedure decode_c(self : in out m68000);
private
   --
   instr_exg : step_exg    --  Decode EXG instruction
      with address => instr'Address;

   procedure decode_ABCD(self : in out m68000)
      with pre => (instr_bcd.code = 16);
   procedure decode_AND(self : in out m68000)
      with pre => ((instr_2op.code = 0) or (instr_2op.code = 1) or
                   (instr_2op.code = 2) or (instr_2op.code = 4) or
                   (instr_2op.code = 5) or (instr_2op.code = 6));
   procedure decode_MUL(self : in out m68000)
      with pre => ((instr_2op.code = 3) or (instr_2op.code = 7));
   procedure decode_EXG(self : in out m68000)
      with pre => ((instr_exg.code = 8) or (instr_exg.code = 9) or
                  (instr_exg.code = 17));

end;
