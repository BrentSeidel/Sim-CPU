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
--
--  This package is the basis of the system bus.  This includes both memory,  I/O
--  devices, and anything else that can be addressed by the processor.  It is
--  possible to create memory systems with non-contiguous blocks of memory and
--  introduce various errors into the memory system.  Any address translation or
--  or memory management would be included in one of these objects.
--
limited with BBS.Sim_CPU.CPU;
with BBS.Sim_CPU.io;
package BBS.Sim_CPU.bus is
   --
   --  The memory object.  This object will contain memory,  I/O, and an optional
   --  memory management unit.
   --
   type bus is abstract tagged private;
   type bus_access is access all bus'Class;
   --
   --  Setup the bus object.
   --
   --
   --  Called to attach an I/O device to a bus at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --
   procedure attach_io(self : in out bus; io_dev : BBS.Sim_CPU.io.io_access;
                       base_addr : addr_bus; which_bus : bus_type) is abstract;
   --
   --  Called to attach a CPU to to a bus.  This is intended to be used by a CPU
   --  object when the attach_bus method of a CPU object is called.
   --
   procedure attach_cpu(self : in  out bus; cpu_dev : BBS.Sim_CPU.CPU.sim_access; index : Natural)
   is abstract;
   --
   --  Bus transactions from the processor depend on the address, the processor
   --  mode, and the address type.  An address type of ADDR_IO signifies I/O
   --  addresses for processors that implement them.  These functions may
   --  include address translation.
   --
   function read(self : in out bus; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return data_bus is abstract;
   --
   procedure write(self : in out bus; addr : addr_bus; data: data_bus; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat) is abstract;
   --
   --  Bus transactions from I/O devices are generally direct to memory (DMA) without
   --  address translation.  The I/O device must be given the physical address to use.
   --
   function dmar(self : in out bus; addr : addr_bus; status : out bus_stat) return data_bus is abstract;
   --
   procedure dmaw(self : in out bus; addr : addr_bus; data: data_bus; status : out bus_stat) is abstract;
private
   --
   --  Memory object.
   --
   type bus is abstract tagged record
      null;
   end record;
end;
