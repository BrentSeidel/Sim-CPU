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
--  Package for decoding Line 6 instructions - CMP/EOR
--
package BBS.Sim_CPU.CPU.m68000.line_b is
   procedure decode_b(self : in out m68000);
private
   --
   procedure decode_CMP(self : in out m68000)
      with pre => ((self.instr.op2.code = 0) or (self.instr.op2.code = 1) or
         (self.instr.op2.code = 2) or (self.instr.op2.code = 3) or
         (self.instr.op2.code = 7));
   procedure decode_CMPM(self : in out m68000)
      with pre => ((self.instr.cmpm.code1 = 1) and ((self.instr.op2.code = 4) or
         (self.instr.op2.code = 5) or (self.instr.op2.code = 6)));
   procedure decode_EOR(self : in out m68000)
      with pre => ((self.instr.cmpm.code1 /= 1) and ((self.instr.op2.code = 4) or
       (self.instr.op2.code = 5) or (self.instr.op2.code = 6)));
end;
