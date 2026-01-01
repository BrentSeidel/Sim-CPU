--
--  Author: Brent Seidel
--  Date: 24-Dec-2025
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
--  Code for PDP-11 instructions with the 4 MSBs set to 0.
--
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package BBS.Sim_CPU.CPU.PDP11.Line_8 is
   --
   --  Decode instruction
   --
   procedure decode(self : in out PDP11);
   --
   --  Routines for instructions
   --
   --  Byte size single operand instructions with word size counterparts
   procedure CLRB(self : in out PDP11);
   procedure COMB(self : in out PDP11);
   procedure INCB(self : in out PDP11);
   procedure DECB(self : in out PDP11);
   procedure NEGB(self : in out PDP11);
   procedure ADCB(self : in out PDP11);
   procedure SBCB(self : in out PDP11);
   procedure TSTB(self : in out PDP11);
   procedure RORB(self : in out PDP11);
   procedure ROLB(self : in out PDP11);
   procedure ASRB(self : in out PDP11);
   procedure ASLB(self : in out PDP11);
   --
   --  Branch instructions
   procedure BPL(self : in out PDP11);
   procedure BMI(self : in out PDP11);
   procedure BHI(self : in out PDP11);
   procedure BLOS(self : in out PDP11);
   procedure BVC(self : in out PDP11);
   procedure BVS(self : in out PDP11);
   procedure BCC(self : in out PDP11);  --  Also BHIS
   procedure BCS(self : in out PDP11);  --  Also BLO
end;
