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
   type dev_type is (NL,   --  Null device
                     TT,   --  Terminal/serial port type device
                     FD,   --  Disk type device
                     CL,   --  Clock type device
                     PT,   --  Paper tape type device
                     MT,   --  Magnetic tape type device
                     MM);  --  Memory management type device
   --
   --  ----------------------------------------------------------------------
   --  I/O device actions
   --
   --  Write to a device register
   --
   procedure write(self : in out io_device; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is abstract;
   --
   --  Read from a device register.
   --
   function read(self : in out io_device; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus is abstract;
   --
   --  Get size in bytes of number of addresses used.
   --
   function getSize(self : in out io_device) return addr_bus is (0);
   --
   --  Get base address of device registers.
   --
   function getBase(self : in out io_device) return addr_bus;
   --
   --  Set the base address of the device registers.
   --
   procedure setBase(self : in out io_device; base : addr_bus);
   --
   --  Set the owning CPU simulation.  This is mainly used to allow the device
   --  to send interrupts to the CPU.
   --
   procedure setOwner(self : in out io_device; owner : BBS.Sim_CPU.CPU.sim_access);
   --
   --  Return text for the device name.
   --
   function name(self : in out io_device) return String is ("Unimplemented");
   --
   --  Return a brief text description of the device.
   --
   function description(self : in out io_device) return String is ("Unimplemented");
   --
   --  Set the exception vector for the device.  The semantics of this depend on
   --  both the device and the CPU.
   --
   procedure setException(self : in out io_device; except : long) is abstract;
   --
   --  Return the type/class of the device.  See list of device types above.
   --
   function dev_class(self : in out io_device) return dev_type is (NL);
   --
   --  Send a reset signal to the device.  What the device does is device specific.
   --  If this is not overridden by the device, nothing is done.
   --
   procedure reset(self : in out io_device) is null;
   --
   --  Send a shutdown signal to the device.  What the device does is device specific.
   --  If this is not overridden by the device, nothing is done.
   --
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
