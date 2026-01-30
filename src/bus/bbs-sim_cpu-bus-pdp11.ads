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
--  Bus specifically for PDP-11 type processors.  This would be unibus or Q-Bus.
--
with Ada.Containers.Indefinite_Ordered_Maps;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
with BBS.Sim_CPU.CPU;
package BBS.Sim_CPU.bus.pdp11 is
   --
   --  There are basically three types of addressing.  Note that Unibus uses 18
   --  bits for addressing.
   --  No memory management
   --  - The top 4Kwords of the 16 bit CPU address space is relocated to the top
   --    8kWords of the 18 bit Unibus address space.
   --  - All systems start in this mode.
   --  Memory management with 18 bit addressing
   --  - Maps 16 bit CPU address space to 18 bit Unibus address space.
   --  Memory management with 22 bit addressing
   --  - Maps 16 bit CPU address space to 22 bit memory address space.
   --  - Also maps 18 bit Unibus address space to 22 bit memory address spcae.
   --
   --  Some memoy management also includes separate I (instruction) and D (data)
   --  space.  Some processors add a supervisor mode in addition to kernel and user.
   --
   type mmu_type is (none, mmu18, mmu22);
   --
   --  Define the unibus object.
   --
   type unibus(mem_size : addr_bus) is new bus with private;
   type unibus_access is access all unibus'Class;
   --
   --  Setup the bus object.
   --
   --
   --  Called to attach an I/O device to a bus at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --
   overriding
   procedure attach_io(self : in out unibus; io_dev : BBS.Sim_CPU.io.io_access;
                       base_addr : addr_bus; which_bus : bus_type);
   --
   --  Called to attach a CPU to to a bus.  Index is ignored as this bus only
   --  supports a single CPU.
   --
   overriding
   procedure attach_cpu(self : in  out unibus; cpu_dev : BBS.Sim_CPU.CPU.sim_access; index : Natural);
   --
   --  Return bus size
   --
   overriding
   function size(self : in out unibus) return bus_size is (bits16);
   --
   --  Bus transactions from the processor depend on the address, the processor
   --  mode, and the address type.  An address type of ADDR_IO signifies I/O
   --  addresses for processors that implement them.  These functions may
   --  include address translation.
   --
   --  These are read logical and write logical.  The logical address may be translated
   --  into a physical address.
   --
   --
   --  Read various sizes in LSB first
   --
   overriding
   function readl8l(self : in out unibus; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte;
   overriding
   function readl16l(self : in out unibus; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return word;
   --
   --  Write various sizes in LSB first
   --
   overriding
   procedure writel8l(self : in out unibus; addr : addr_bus; data : byte; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   overriding
   procedure writel16l(self : in out unibus; addr : addr_bus; data : word; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat);
   --
   --  Bus transactions from I/O devices are generally direct to memory (DMA) without
   --  address translation.  The I/O device must be given the physical address to use.
   --
   --  These are read physical and write physical.  The physical address is used
   --  directly to access memory.
   --
   overriding
   function readp(self : in out unibus; addr : addr_bus; status : out bus_stat) return data_bus;
   --
   overriding
   procedure writep(self : in out unibus; addr : addr_bus; data: data_bus; status : out bus_stat);
   --
   --  Memory size and adjustment.  If not overridden, they will return 0 for
   --  sizes and do nothing.
   --
   --  The the size that memory has been configured for.  This should not change
   --  over the lifetime of the object.
   --
   function mem_size(self : in out unibus) return addr_bus;
   --
   --  For debugging (or maybe other) purposes, the maximum address can be set.
   --  If greater than the configured size, this is ignored.  Accessing memory
   --  beyond the maximum address will return a BUS_NONE status.
   --
   procedure set_max_addr(self : in out unibus; size : addr_bus);
   --
   --  Return this maximum address.  This should always be less than or equal to
   --  the configured size.
   --
   function get_max_addr(self : in out unibus) return addr_bus;
   --  ========================================================================
private
   --
   --  For memory mapped I/O devices
   --
   package io_map_type is new Ada.Containers.Indefinite_Ordered_maps
         (key_type => addr_bus, element_type => BBS.Sim_CPU.io.io_access);
   --
   --  Type for memory array.
   --
   type mem_array is array (addr_bus range <>) of byte;
   --  ------------------------------------------------------------------------
   --
   --  The bus type with arrays for memory and I/O ports as well as a pointer to
   --  the CPU.
   --
   type unibus(mem_size : addr_bus) is new bus with record
      cpu      : BBS.Sim_CPU.CPU.sim_access;
      mem      : mem_array(0 .. mem_size) := (others => 0);
      max_size : addr_bus := mem_size;
      io_ports : io_map_type.Map;
      mmu_mode : mmu_type := none;
   end record;
   --
   --  Perform address translation for logical reads
   --
   function translate(self : in out unibus; addr : addr_bus) return addr_bus;
   --
   --  Constants for I/O page
   --
   base_io_start : constant addr_bus := 8#176_000#;  --  Start of I/O page in unmapped CPU address
   base_io_end   : constant addr_bus := 8#177_777#;  --  End of I/O page in unmapped CPU address
   ub_io_start   : constant addr_bus := 8#776_000#;  --  Start of Unibus I/O page
   ub_io_end     : constant addr_bus := 8#777_777#;  --  End of Unibus I/O page (and max Unibus address)
   bad_addr      : constant addr_bus := 16#FFFF_FFFF#;  --  Out of range address to indicate errors
end;
