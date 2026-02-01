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
with BBS.Sim_CPU.CPU;
package BBS.Sim_CPU.io is
   --
   --  The I/O device object for simulated CPUs
   --
   type io_device is abstract tagged limited private;
   type io_access is access all io_device'Class;
   --
   --  Codes for various device types
   --  NL - Null device
   --  TT - Serial interface/terminal
   --  FD - Floppy disk
   --  HD - Hard disk
   --  CL - Clock
   --  PT - Paper tape
   --  MT - Magnetic tape
   --
   type dev_type is (NL, TT, FD, HD, CL, PT, MT);
   --
   --  ----------------------------------------------------------------------
   --  I/O device actions
   --
   procedure write(self : in out io_device; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is abstract;
   function read(self : in out io_device; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus is abstract;
   function getSize(self : in out io_device) return addr_bus is (0);
   function getBase(self : in out io_device) return addr_bus;
   procedure setBase(self : in out io_device; base : addr_bus);
   procedure setOwner(self : in out io_device; owner : BBS.Sim_CPU.CPU.sim_access);
   function name(self : in out io_device) return String is ("Unimplemented");
   function description(self : in out io_device) return String is ("Unimplemented");
   procedure setException(self : in out io_device; except : long) is abstract;
   function dev_class(self : in out io_device) return dev_type is (NL);
   procedure reset(self : in out io_device) is null;
   procedure shutdown(self : in out io_device) is null;
private
   --
   --  These are the basic features that all I/O devices include.
   --
   type io_device is abstract tagged limited record
      base : addr_bus;  --  The base address
      host : BBS.Sim_CPU.CPU.sim_access;
   end record;
end;
