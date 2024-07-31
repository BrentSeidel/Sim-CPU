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
--
--  Package for decoding Line 6 instructions - Bcc/BSR/BRA
--
package BBS.Sim_CPU.m68000.line_6 is
   procedure decode_6(self : in out m68000);
private
   type step_bcc is record  --  For branch instructions
      disp : byte;
      cond : uint4;
      pre  : prefix;
   end record;
   for step_bcc use record
      disp at 0 range 0 .. 7;
      cond at 0 range 8 .. 11;
      pre  at 0 range 12 .. 15;
   end record;
   --
   instr_bcc : step_bcc  --  Decode conditional branch instructions
      with address => instr'Address;
end;
