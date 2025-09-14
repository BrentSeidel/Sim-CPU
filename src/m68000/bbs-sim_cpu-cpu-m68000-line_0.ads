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
   instr_movep : step_movep
      with address => instr'Address;
   --
   procedure decode_ADDI(self : in out m68000)
      with pre => ((instr_1op_size.code = 6) and (instr_1op_size.size /= data_long_long));
   procedure decode_ANDI(self : in out m68000)
      with pre => ((instr_1op_size.code = 2) and (instr_1op_size.size /= data_long_long));
   procedure decode_BCHG(self : in out m68000)
      with pre => ((instr_2op.code = 5) or ((instr_2op.code = 1) and (instr_2op.reg_x = 4)));
   procedure decode_BCLR(self : in out m68000)
      with pre => ((instr_2op.code = 6) or ((instr_2op.code = 2) and (instr_2op.reg_x = 4)));
   procedure decode_BSET(self : in out m68000)
      with pre => ((instr_2op.code = 7) or ((instr_2op.code = 3) and (instr_2op.reg_x = 4)));
   procedure decode_BTST(self : in out m68000)
      with pre => ((instr_2op.code = 4) or ((instr_2op.code = 0) and (instr_2op.reg_x = 4)));
   procedure decode_CMPI(self : in out m68000)
      with pre => ((instr_1op_size.code = 16#C#) and (instr_1op_size.size /= data_long_long));
   procedure decode_EORI(self : in out m68000)
      with pre => ((instr_1op_size.code = 16#A#) and (instr_1op_size.size /= data_long_long));
   procedure decode_ORI(self : in out m68000)
      with pre => ((instr_1op_size.code = 0) and (instr_1op_size.size /= data_long_long));
   procedure decode_MOVEP(self : in out m68000)
      with pre => ((instr_movep.code = 1) and ((instr_movep.mode = 4) or
                  (instr_movep.mode = 5) or (instr_movep.mode = 6) or
                  (instr_movep.mode = 7)));
   procedure decode_SUBI(self : in out m68000)
      with pre => ((instr_1op_size.code = 4) and (instr_1op_size.size /= data_long_long));
   --
end;
