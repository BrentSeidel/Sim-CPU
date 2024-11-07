--
--  Author: Brent Seidel
--  Date: 5-Nov-2024
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
package BBS.Sim_CPU.i8080.z80 is
   --
   --  Convert the Z-80 flags to a string representation
   --
   function flags(f : status_word) return String;
   --
   --  Perform Z-80 DAA instruction
   --
   function daa(a : byte; f : in out status_word) return byte;
   --
   --  Z-80 Two byte instructions
   --
   procedure prefix_cb(self : in out i8080);
   procedure prefix_dd(self : in out i8080);
   procedure prefix_ed(self : in out i8080);
   procedure prefix_fd(self : in out i8080);
end;
