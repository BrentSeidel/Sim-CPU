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
package BBS.Sim_CPU.CPU.PDP11.Line_0 is
   --
   --  Decode instruction
   --
   procedure decode(self : in out PDP11);
   --
   --  Routines for instructions
   --
   --  Word size single operand instructions with byte counterparts
   procedure CLR(self : in out PDP11);
   procedure COM(self : in out PDP11);
   procedure INC(self : in out PDP11);
   procedure DEC(self : in out PDP11);
   procedure NEG(self : in out PDP11);
   procedure ADC(self : in out PDP11);
   procedure SBC(self : in out PDP11);
   procedure TST(self : in out PDP11);
   procedure ROR(self : in out PDP11);
   procedure ROL(self : in out PDP11);
   procedure ASR(self : in out PDP11);
   procedure ASL(self : in out PDP11);
   --
   --  Misc single operand instructions without byte counterparts
   procedure JMP(self : in out PDP11);
   procedure SWAB(self : in out PDP11);
   --
   --  Branch instructions
   procedure BR(self : in out PDP11);
   procedure BNE(self : in out PDP11);
   procedure BEQ(self : in out PDP11);
   procedure BGE(self : in out PDP11);
   procedure BLT(self : in out PDP11);
   procedure BGT(self : in out PDP11);
   procedure BLE(self : in out PDP11);
   --
   --  Condition codes
   procedure codes(self : in out PDP11);
end;
