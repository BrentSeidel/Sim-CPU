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
   procedure decode_ABCD(self : in out m68000)
      with pre => (self.instr.bcd.code = 16);
   procedure decode_AND(self : in out m68000)
      with pre => ((self.instr.op2.code = 0) or (self.instr.op2.code = 1) or
                   (self.instr.op2.code = 2) or (self.instr.op2.code = 4) or
                   (self.instr.op2.code = 5) or (self.instr.op2.code = 6));
   procedure decode_MUL(self : in out m68000)
      with pre => ((self.instr.op2.code = 3) or (self.instr.op2.code = 7));
   procedure decode_EXG(self : in out m68000)
      with pre => ((self.instr.exg.code = 8) or (self.instr.exg.code = 9) or
                  (self.instr.exg.code = 17));

end;
