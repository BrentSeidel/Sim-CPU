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
--  Package for decoding Line 6 instructions - CMP/EOR
--
package BBS.Sim_CPU.m68000.line_b is
   procedure decode_b(self : in out m68000);
private
   type step_cmp is record  --  Also used for AND and EOR instructions
      reg_y  : reg_num;
      mode_y : mode_code;
      opmode : uint3;
      reg_x  : reg_num;
      pre    : prefix;
   end record;
   for step_cmp use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      opmode at 0 range 6 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 .. 15;
   end record;
   type step_cmpm is record
      reg_y : reg_num;
      code1 : uint3;
      size  : data_size;
      code2 : Boolean;
      reg_x : reg_num;
      pre   : prefix;
   end record;
   for step_cmpm use record
      reg_y at 0 range 0 .. 2;
      code1 at 0 range 3 .. 5;
      size  at 0 range 6 .. 7;
      code2 at 0 range 8 .. 8;
      reg_x at 0 range 9 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   --
   instr_cmp : step_cmp  --  Decode CMP instructions
      with address => instr'Address;
   instr_cmpm : step_cmpm  --  Decode CMPM instructions
      with address => instr'Address;

   procedure decode_CMP(self : in out m68000)
      with pre => ((instr_cmp.opmode = 0) or (instr_cmp.opmode = 1) or
         (instr_cmp.opmode = 2) or (instr_cmp.opmode = 3) or
         (instr_cmp.opmode = 7));
   procedure decode_CMPM(self : in out m68000)
      with pre => ((instr_cmpm.code1 = 1) and ((instr_cmp.opmode = 4) or
         (instr_cmp.opmode = 5) or (instr_cmp.opmode = 6)));
   procedure decode_EOR(self : in out m68000)
      with pre => ((instr_cmpm.code1 /= 1) and ((instr_cmp.opmode = 4) or
       (instr_cmp.opmode = 5) or (instr_cmp.opmode = 6)));
end;
