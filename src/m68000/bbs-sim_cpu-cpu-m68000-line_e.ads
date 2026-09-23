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
--  Package for decoding Line E (14) instructions - Shift/Rotate/Bit Field
--
package BBS.Sim_CPU.CPU.m68000.line_e is
   procedure decode_e(self : in out m68000);
private
   --
   procedure decode_ASLR2(self : in out m68000)
      with pre => ((self.instr.aslr2.code) = 0 and (self.instr.aslr2.size /= data_long_long));
   procedure decode_ASLR1(self : in out m68000)
      with pre => ((self.instr.aslr1.code2 = 0) and (self.instr.aslr1.code1 = 3));
   procedure decode_LSLR1(self : in out m68000)
      with pre => ((self.instr.aslr1.code1 = 3) and (self.instr.aslr1.code2 = 1));
   procedure decode_LSLR2(self : in out m68000)
      with pre => ((self.instr.aslr2.code) = 1 and (self.instr.aslr2.size /= data_long_long));
   procedure decode_ROLR2(self : in out m68000)
      with pre => ((self.instr.aslr2.code = 3) and (self.instr.aslr2.size /= data_long_long));
   procedure decode_ROLR1(self : in out m68000)
      with pre => ((self.instr.aslr1.code2 = 3) and (self.instr.aslr1.code1 = 3));
   procedure decode_ROXLR2(self : in out m68000)
      with pre => ((self.instr.aslr2.code = 2) and (self.instr.aslr2.size /= data_long_long));
   procedure decode_ROXLR1(self : in out m68000)
      with pre => ((self.instr.aslr1.code2 = 2) and (self.instr.aslr1.code1 = 3));
end;
