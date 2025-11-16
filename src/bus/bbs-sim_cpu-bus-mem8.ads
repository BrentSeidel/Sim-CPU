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
with Ada.Containers.Indefinite_Ordered_Maps;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
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
   overriding
   procedure attach_io(self : in out mem8io; io_dev : BBS.Sim_CPU.io.io_access;
                       base_addr : addr_bus; which_bus : bus_type);
   --
   --  Called to attach a CPU to to a bus.  Index is ignored as this bus only
   --  supports a single CPU.
   --
   overriding
   procedure attach_cpu(self : in  out mem8io; cpu_dev : BBS.Sim_CPU.CPU.sim_access; index : Natural);
   --
   --  Bus transactions from the processor depend on the address, the processor
   --  mode, and the address type.  An address type of ADDR_IO signifies I/O
   --  addresses for processors that implement them.  These functions may
   --  include address translation.
   --
   --  These are read logical and write logical.  The logical address may be translated
   --  into a physical address.
   --
   overriding
   function readl(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return data_bus;
   --
   --  Read various sizes in LSB first
   --
   overriding
   function readl8l(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte;
   overriding
   function readl16l(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return word;
   overriding
   function readl32l(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                     addr_kind : addr_type; status : out bus_stat) return data_bus is (0);
   --
   --  Read various sizes in MSB first
   --
   overriding
   function readl8m(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte;
   overriding
   function readl16m(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return word;
   overriding
   function readl32m(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return data_bus is (0);
   --
   overriding
   procedure writel(self : in out mem8io; addr : addr_bus; data: data_bus; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat);
   --
   --  Write various sizes in LSB first
   --
   overriding
   procedure writel8l(self : in out mem8io; addr : addr_bus; data : byte; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   overriding
   procedure writel16l(self : in out mem8io; addr : addr_bus; data : word; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   overriding
   procedure writel32l(self : in out mem8io; addr : addr_bus; data : data_bus; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat) is Null;
   --
   --  Write various sizes in MSB first
   --
   overriding
   procedure writel8m(self : in out mem8io; addr : addr_bus; data : byte; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   overriding
   procedure writel16m(self : in out mem8io; addr : addr_bus; data : word; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   overriding
   procedure writel32m(self : in out mem8io; addr : addr_bus; data : data_bus; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat) is Null;
   --
   --  Bus transactions from I/O devices are generally direct to memory (DMA) without
   --  address translation.  The I/O device must be given the physical address to use.
   --
   --  These are read physical and write physical.  The physical address is used
   --  directly to access memory.
   --
   overriding
   function readp(self : in out mem8io; addr : addr_bus; status : out bus_stat) return data_bus;
   --
   overriding
   procedure writep(self : in out mem8io; addr : addr_bus; data: data_bus; status : out bus_stat);
   --
   --  Memory size and adjustment.  If not overridden, they will return 0 for
   --  sizes and do nothing.
   --
   --  The the size that memory has been configured for.  This should not change
   --  over the lifetime of the object.
   --
   function mem_size(self : in out mem8io) return addr_bus;
   --
   --  For debugging (or maybe other) purposes, the maximum address can be set.
   --  If greater than the configured size, this is ignored.  Accessing memory
   --  beyond the maximum address will return a BUS_NONE status.
   --
   procedure set_max_addr(self : in out mem8io; size : addr_bus);
   --
   --  Return this maximum address.  This should always be less than or equal to
   --  the configured size.
   --
   function get_max_addr(self : in out mem8io) return addr_bus;
   --  ------------------------------------------------------------------------
   --
   --  8 Bit memory with memory mapped IO space.
   --
   type mem8mem(mem_size : addr_bus) is new bus with private;
   type mem8mem_access is access all mem8mem'Class;
   --
   --  Setup the bus object.
   --
   --
   --  Called to attach an I/O device to a bus at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --
   overriding
   procedure attach_io(self : in out mem8mem; io_dev : BBS.Sim_CPU.io.io_access;
                       base_addr : addr_bus; which_bus : bus_type);
   --
   --  Called to attach a CPU to to a bus.  Index is ignored as this bus only
   --  supports a single CPU.
   --
   overriding
   procedure attach_cpu(self : in  out mem8mem; cpu_dev : BBS.Sim_CPU.CPU.sim_access; index : Natural);
   --
   --  Bus transactions from the processor depend on the address, the processor
   --  mode, and the address type.  An address type of ADDR_IO signifies I/O
   --  addresses for processors that implement them.  These functions may
   --  include address translation.
   --
   --  These are read logical and write logical.  The logical address may be translated
   --  into a physical address.
   --
   overriding
   function readl(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return data_bus;
   --
   --  Read various sizes in LSB first
   --
   overriding
   function readl8l(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte;
   overriding
   function readl16l(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return word;
   overriding
   function readl32l(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                     addr_kind : addr_type; status : out bus_stat) return data_bus;
   --
   --  Read various sizes in MSB first
   --
   overriding
   function readl8m(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte;
   overriding
   function readl16m(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return word;
   overriding
   function readl32m(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return data_bus;
   --
   overriding
   procedure writel(self : in out mem8mem; addr : addr_bus; data: data_bus; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat);
   --
   --  Write various sizes in LSB first
   --
   overriding
   procedure writel8l(self : in out mem8mem; addr : addr_bus; data : byte; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   overriding
   procedure writel16l(self : in out mem8mem; addr : addr_bus; data : word; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   overriding
   procedure writel32l(self : in out mem8mem; addr : addr_bus; data : data_bus; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   --
   --  Write various sizes in MSB first
   --
   overriding
   procedure writel8m(self : in out mem8mem; addr : addr_bus; data : byte; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   overriding
   procedure writel16m(self : in out mem8mem; addr : addr_bus; data : word; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   overriding
   procedure writel32m(self : in out mem8mem; addr : addr_bus; data : data_bus; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   --
   --  Bus transactions from I/O devices are generally direct to memory (DMA) without
   --  address translation.  The I/O device must be given the physical address to use.
   --
   --  These are read physical and write physical.  The physical address is used
   --  directly to access memory.
   --
   overriding
   function readp(self : in out mem8mem; addr : addr_bus; status : out bus_stat) return data_bus;
   --
   overriding
   procedure writep(self : in out mem8mem; addr : addr_bus; data: data_bus; status : out bus_stat);
   --
   --  Memory size and adjustment.  If not overridden, they will return 0 for
   --  sizes and do nothing.
   --
   --  The the size that memory has been configured for.  This should not change
   --  over the lifetime of the object.
   --
   function mem_size(self : in out mem8mem) return addr_bus;
   --
   --  For debugging (or maybe other) purposes, the maximum address can be set.
   --  If greater than the configured size, this is ignored.  Accessing memory
   --  beyond the maximum address will return a BUS_NONE status.
   --
   procedure set_max_addr(self : in out mem8mem; size : addr_bus);
   --
   --  Return this maximum address.  This should always be less than or equal to
   --  the configured size.
   --
   function get_max_addr(self : in out mem8mem) return addr_bus;
   --  ========================================================================
private
   --
   --  For memory mapped I/O devices
   --
   package io_map_type is new Ada.Containers.Indefinite_Ordered_maps
         (key_type => addr_bus, element_type => BBS.Sim_CPU.io.io_access);
   --
   --  Types for memory and I/O arrays.
   --
   type mem_array is array (addr_bus range <>) of byte;
   type io_array is array (byte'Range) of BBS.Sim_CPU.io.io_access;
   --  ------------------------------------------------------------------------
   --
   --  The bus type with arrays for memory and I/O ports as well as a pointer to
   --  the CPU.
   --
   type mem8io(mem_size : addr_bus) is new bus with record
      cpu      : BBS.Sim_CPU.CPU.sim_access;
      mem      : mem_array(0 .. mem_size) := (others => 0);
      max_size : addr_bus := mem_size;
      io_ports : io_array := (others => null);
   end record;
   --  ------------------------------------------------------------------------
   --
   --  The bus type with arrays for memory and I/O ports as well as a pointer to
   --  the CPU.
   --
   type mem8mem(mem_size : addr_bus) is new bus with record
      cpu      : BBS.Sim_CPU.CPU.sim_access;
      mem      : mem_array(0 .. mem_size) := (others => 0);
      max_size : addr_bus := mem_size;
      io_ports : io_map_type.Map;
   end record;
end;
