--
--  Author: Brent Seidel
--  Date: 8-Jun-2025
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
with Ada.Text_IO;
package body BBS.Sim_CPU.io is
   --
   --  ----------------------------------------------------------------------
   --  I/O device actions
   --
   function getBase(self : in out io_device) return addr_bus is (self.base);
   procedure setBase(self : in out io_device; base : addr_bus) is
   begin
      self.base := base;
   end;
   --
   --  Set the owning CPU
   --
   procedure setOwner(self : in out io_device; owner : BBS.Sim_CPU.CPU.sim_access) is
   begin
      self.host := owner;
   end;
end;
