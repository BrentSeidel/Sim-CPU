--
--  Author: Brent Seidel
--  Date: 19-Jun-2025
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
--  Bus with 8 bit memory attached.
--
with BBS.Sim_CPU.io;
with BBS.Sim_CPU.CPU;
package BBS.Sim_CPU.bus.mem8 is
   --
   --  8 Bit memory with separate IO space.
   --
   type mem8io(mem_size : addr_bus) is new bus with private;
   type mem8io_access is access all mem8io'Class;
   --
   --  Setup the bus object.
   --
   --
   --  Called to attach an I/O device to a bus at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --
   procedure attach_io(self : in out mem8io; io_dev : BBS.Sim_CPU.io.io_access;
                       base_addr : addr_bus; which_bus : bus_type);
   --
   --  Called to attach a CPU to to a bus.  Index is ignored as this bus only
   --  supports a single CPU.
   --
   procedure attach_cpu(self : in  out mem8io; cpu_dev : BBS.Sim_CPU.CPU.sim_access; index : Natural);
   --
   --  Bus transactions depend on the address, the processor mode, and the address
   --  type.  An address type of ADDR_IO signifies I/O addresses for processors
   --  that implement them.
   --
   overriding
   function read(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return data_bus;
   --
   overriding
   procedure write(self : in out mem8io; addr : addr_bus; data: data_bus; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat);
   --
   --  Bus transactions from I/O devices are generally direct to memory (DMA) without
   --  address translation.  The I/O device must be given the physical address to use.
   --
   overriding
   function dmar(self : in out mem8io; addr : addr_bus; status : out bus_stat) return data_bus;
   --
   overriding
   procedure dmaw(self : in out mem8io; addr : addr_bus; data: data_bus; status : out bus_stat);
   --
private
   --
   --  Types for memory and I/O arrays.
   --
   type mem_array is array (addr_bus range <>) of byte;
   type io_array is array (byte'Range) of BBS.Sim_CPU.io.io_access;
   --
   --  The bus type with arrays for memory and I/O ports as well as a pointer to
   --  the CPU.
   --
   type mem8io(mem_size : addr_bus) is new bus with record
      cpu : BBS.Sim_CPU.CPU.sim_access;
      mem : mem_array(0 .. mem_size) := (others => 0);
      io_ports     : io_array := (others => null);
   end record;
end;
