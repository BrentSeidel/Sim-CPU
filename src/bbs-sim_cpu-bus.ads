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
   type bus is tagged private;
   type bus_access is access all bus'Class;
   --
   --  Setup the bus object.
   --
   --
   --  Called to attach an I/O device to a bus at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --
   procedure attach_io(self : in out bus; io_dev : BBS.Sim_CPU.io.io_access;
                       base_addr : addr_bus; which_bus : bus_type) is Null;
   --
   --  Called to attach a CPU to to a bus.  This is intended to be used by a CPU
   --  object when the attach_bus method of a CPU object is called.
   --
   procedure attach_cpu(self : in  out bus; cpu_dev : BBS.Sim_CPU.CPU.sim_access; index : Natural) is null;
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   procedure deposit(self : in out bus) is Null;
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   procedure examine(self : in out bus) is Null;
   --
   --  Return bus size
   --
   function size(self : in out bus) return bus_size is (bits0);
   --
   --  Bus transactions from the processor depend on the address, the processor
   --  mode, and the address type.  An address type of ADDR_IO signifies I/O
   --  addresses for processors that implement them.  These functions may
   --  include address translation.
   --
   --  These are read logical and write logical.  The logical address may be translated
   --  into a physical address.
   --
   function readl(self : in out bus; addr : addr_bus; mode : proc_mode;
                  addr_kind : addr_type; status : out bus_stat) return data_bus is (0);
   --
   --  Read various sizes in LSB first
   --
   function readl8l(self : in out bus; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte is (0);
   function readl16l(self : in out bus; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return word is (0);
   function readl32l(self : in out bus; addr : addr_bus; mode : proc_mode;
                     addr_kind : addr_type; status : out bus_stat) return data_bus is (0);
   --
   --  Read various sizes in MSB first
   --
   function readl8m(self : in out bus; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte is (0);
   function readl16m(self : in out bus; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return word is (0);
   function readl32m(self : in out bus; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return data_bus is (0);
   --
   procedure writel(self : in out bus; addr : addr_bus; data: data_bus; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat) is Null;
   --
   --  Write various sizes in LSB first
   --
   procedure writel8l(self : in out bus; addr : addr_bus; data : byte; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat) is Null;
   procedure writel16l(self : in out bus; addr : addr_bus; data : word; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat) is Null;
   procedure writel32l(self : in out bus; addr : addr_bus; data : data_bus; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat) is Null;
   --
   --  Write various sizes in MSB first
   --
   procedure writel8m(self : in out bus; addr : addr_bus; data : byte; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat) is Null;
   procedure writel16m(self : in out bus; addr : addr_bus; data : word; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat) is Null;
   procedure writel32m(self : in out bus; addr : addr_bus; data : data_bus; mode : proc_mode;
                    addr_kind : addr_type; status : out bus_stat) is Null;
   --
   --  Bus transactions from I/O devices are generally direct to memory (DMA) without
   --  address translation.  The I/O device must be given the physical address to use.
   --
   --  These are read physical and write physical.  The physical address is used
   --  directly to access memory.
   --
   function readp(self : in out bus; addr : addr_bus; status : out bus_stat) return data_bus is (0);
   --
   procedure writep(self : in out bus; addr : addr_bus; data: data_bus; status : out bus_stat) is Null;
   --
   --  Memory size and adjustment.  If not overridden, they will return 0 for
   --  sizes and do nothing.
   --
   --  The the size that memory has been configured for.  This should not change
   --  over the lifetime of the object.
   --
   function mem_size(self : in out bus) return addr_bus is (0);
   --
   --  For debugging (or maybe other) purposes, the maximum address can be set.
   --  If greater than the configured size, this is ignored.  Accessing memory
   --  beyond the maximum address will return a BUS_NONE status.
   --
   procedure set_max_addr(self : in out bus; size : addr_bus) is null;
   --
   --  Return this maximum address.  This should always be less than or equal to
   --  the configured size.
   --
   function get_max_addr(self : in out bus) return addr_bus is (0);
   --
   --  Simulator switches and lights
   --
   function get_lr_data(self : in out bus) return data_bus;
   function get_lr_addr(self : in out bus) return addr_bus;
   function get_lr_ctrl(self : in out bus) return ctrl_mode;
   function get_sr_ad(self : in out bus) return ad_bus;
   function get_sr_ctrl(self : in out bus) return ctrl_mode;

   procedure set_lr_data(self : in out bus; value : data_bus);
   procedure set_lr_addr(self : in out bus; value : addr_bus);
   procedure set_lr_ctrl(self : in out bus; value : ctrl_mode);
   procedure set_sr_ad(self : in out bus; value : ad_bus);
   procedure set_sr_ctrl(self : in out bus; value : ctrl_mode);
private
   --
   --  Bus object.
   --
   type bus is tagged record
      lr_addr : addr_bus;   --  LED register for address
      lr_data : data_bus;   --  LED register for data
      sr_ad   : ad_bus;     --  Switch register for address/data
      lr_ctl  : ctrl_mode;  --  LED registers for control/mode
      sr_ctl  : ctrl_mode;  --  Switch register for control/mode
      addr    : addr_bus;   --  Address for deposit/examine
      trace   : Natural;    --  Trace level
   end record;
end;
