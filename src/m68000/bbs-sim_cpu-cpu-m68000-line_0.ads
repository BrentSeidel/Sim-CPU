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
--  Package for decoding Line 0 instructions -  - Bit manipulation/MOVEP/Immediate
--
package BBS.Sim_CPU.CPU.m68000.line_0 is
   procedure decode_0(self : in out m68000);
private
   --
   procedure decode_ADDI(self : in out m68000)
      with pre => ((self.instr.op1_size.code = 6) and (self.instr.op1_size.size /= data_long_long));
   procedure decode_ANDI(self : in out m68000)
      with pre => ((self.instr.op1_size.code = 2) and (self.instr.op1_size.size /= data_long_long));
   procedure decode_BCHG(self : in out m68000)
      with pre => ((self.instr.op2.code = 5) or ((self.instr.op2.code = 1) and (self.instr.op2.reg_x = 4)));
   procedure decode_BCLR(self : in out m68000)
      with pre => ((self.instr.op2.code = 6) or ((self.instr.op2.code = 2) and (self.instr.op2.reg_x = 4)));
   procedure decode_BSET(self : in out m68000)
      with pre => ((self.instr.op2.code = 7) or ((self.instr.op2.code = 3) and (self.instr.op2.reg_x = 4)));
   procedure decode_BTST(self : in out m68000)
      with pre => ((self.instr.op2.code = 4) or ((self.instr.op2.code = 0) and (self.instr.op2.reg_x = 4)));
   procedure decode_CMPI(self : in out m68000)
      with pre => ((self.instr.op1_size.code = 16#C#) and (self.instr.op1_size.size /= data_long_long));
   procedure decode_EORI(self : in out m68000)
      with pre => ((self.instr.op1_size.code = 16#A#) and (self.instr.op1_size.size /= data_long_long));
   procedure decode_ORI(self : in out m68000)
      with pre => ((self.instr.op1_size.code = 0) and (self.instr.op1_size.size /= data_long_long));
   procedure decode_MOVEP(self : in out m68000)
      with pre => ((self.instr.movep.code = 1) and ((self.instr.movep.mode = 4) or
                  (self.instr.movep.mode = 5) or (self.instr.movep.mode = 6) or
                  (self.instr.movep.mode = 7)));
   procedure decode_SUBI(self : in out m68000)
      with pre => ((self.instr.op1_size.code = 4) and (self.instr.op1_size.size /= data_long_long));
   --
end;
