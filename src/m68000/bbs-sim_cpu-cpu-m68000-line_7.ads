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
--  Package for decoding Line 7 instructions - MOVEQ
--
package BBS.Sim_CPU.CPU.m68000.line_7 is
   procedure decode_7(self : in out m68000);
private
   type step_moveq is record  --  For branch instructions
      data : byte;
      code : Boolean;  --  False for MOVEQ
      reg  : reg_num;
      pre  : prefix;
   end record;
   for step_moveq use record
      data at 0 range 0 .. 7;
      code at 0 range 8 .. 8;
      reg  at 0 range 9 .. 11;
      pre  at 0 range 12 .. 15;
   end record;
   --
   instr_moveq : step_moveq --  Decode MOVEQ instructions
      with address => instr'Address;

   procedure decode_MOVEQ(self : in out m68000)
      with pre => (not instr_moveq.code);
end;
