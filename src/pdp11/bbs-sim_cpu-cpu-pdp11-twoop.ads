--
--  Author: Brent Seidel
--  Date: 15-Dec-2025
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
--  Code for PDP-11 MOV and MOVB instructions
--
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package BBS.Sim_CPU.CPU.PDP11.twoop is
   --
   --  Move word or byte
   --
   procedure MOV(self : in out PDP11);
   procedure MOVB(self : in out PDP11);
   --
   --  Compare word or byte
   --
   procedure CMP(self : in out PDP11);
   procedure CMPB(self : in out PDP11);
end;
