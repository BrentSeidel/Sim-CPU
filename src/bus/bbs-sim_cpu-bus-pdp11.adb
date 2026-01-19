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
with Ada.Text_IO;
with BBS.Sim_CPU.CPU.pdp11;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package body BBS.Sim_CPU.bus.pdp11 is
   --
   --  Perform address translation for logical reads
   --
   function translate(self : in out unibus; addr : addr_bus) return addr_bus is
   begin
      if self.mmu_mode = none then
         if addr < base_io_start then
--            Ada.Text_IO.Put_Line("UNIBUS: No translation for address " & toOct(addr));
            return addr;
         elsif addr <= base_io_end then
--            Ada.Text_IO.Put_Line("UNIBUS: Translating 16 bit I/O address " & toOct(addr)
--                                   & " to " & toOct(addr + (ub_io_start - base_io_start)));
            return addr + (ub_io_start - base_io_start);
         else
            Ada.Text_IO.Put_Line("MMU: Address out of range for 16 bit mode.");
            return bad_addr;
         end if;
      else
         Ada.Text_IO.Put_Line("MMU: Modes other than none are not yet supported");
         return bad_addr;
      end if;
   end;
   --
   --  Bus transactions depend on the address, the processor mode, and the address
   --  type.
   --
   --  Setup the bus object.
   --
   --  Called to attach an I/O device to the unibus at a specific address.
   --
   procedure attach_io(self : in out unibus; io_dev : BBS.Sim_CPU.io.io_access;
                       base_addr : addr_bus; which_bus : bus_type) is
      size : addr_bus := io_dev.all.getSize;
      valid : Boolean := True;
   begin
      Ada.Text_IO.Put_Line("BUS: Attaching I/O device");
      if which_bus = BUS_MEMORY then
         --
         --  Check for unibus I/O address space
         --
         if (base_addr < ub_io_start) or (base_addr > ub_io_end) then
            Ada.Text_IO.Put_Line("BUS: I/O address given outside of Unibus I/O page.");
            valid := False;
         end if;
         --
         --  Check for port conflicts
         --
         for i in base_addr .. base_addr + size - 1 loop
           if self.io_ports.contains(i) then
               valid := False;
               Ada.Text_IO.Put_Line("BUS: Port conflict detected attching " & io_dev.name & " to port " & toHex(i));
           end if;
           exit when not valid;
         end loop;
         --
         --  If no conflict, attach the port
         --
         if valid then
            for i in base_addr .. base_addr + size - 1 loop
               self.io_ports.include(i, io_dev);
               Ada.Text_IO.Put_Line("BUS: Attaching " & io_dev.name &
                  " to memory location " & toHex(i));
            end loop;
            io_dev.setBase(base_addr);
         end if;
      elsif which_bus = BUS_IO then
         Ada.Text_IO.Put_Line("BUS: I/O Space not supported by this bus");
      else
         Ada.Text_IO.Put_Line("BUS: Unknown I/O bus type");
      end if;
   end;
   --
   --  Called to attach a CPU to to a bus.
   --
   procedure attach_cpu(self : in  out unibus; cpu_dev : BBS.Sim_CPU.CPU.sim_access; index : Natural) is
   begin
      if not (cpu_dev.all in BBS.Sim_CPU.CPU.pdp11.PDP11'Class) then
         Ada.Text_IO.Put_Line("WARNING: Attaching a non-PDP-11 processor to a Unibus.  Some things may not work properly.");
      end if;
      self.cpu := cpu_dev;
   end;
   --
   --  Bus transactions from the processor depend on the address, the processor
   --  mode, and the address type.  An address type of ADDR_IO signifies I/O
   --  addresses for processors that implement them.  These functions may
   --  include address translation.
   --
   --  Read a byte from logical memory LSB first (this is identical to MSB first
   --  for a single byte read/write).
   --
   function readl8l(self : in out unibus; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte is
      tdata : byte;
      taddr : addr_bus := self.translate(addr);
   begin
      self.lr_addr := taddr;
      self.lr_ctl.atype := addr_kind;
      self.lr_ctl.mode := mode;
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Check for Unibus I/O page first, then either read I/O or memory.
         --
         status := BUS_SUCC;
         if taddr >= ub_io_start then
            if self.io_ports.contains(taddr) then
--               Ada.Text_IO.Put_Line("BUSB: Reading from I/O device " & self.io_ports(taddr).all.name);
               tdata := byte(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
               self.lr_data := data_bus(tdata);
               return tdata;
            else
               status := BUS_NONE;
               return 0;
            end if;
         end if;
         if (taddr > self.max_size) then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         tdata := self.mem(taddr);
         self.lr_data := data_bus(tdata);
         return tdata;
      elsif addr_kind = ADDR_IO then
         status := BUS_NONE;
         return 0;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
         return 0;
      end if;
      status := BUS_NONE;
      return 0;
   end;
   --
   function readl16l(self : in out unibus; addr : addr_bus; mode : proc_mode;
                     addr_kind : addr_type; status : out bus_stat) return word is
      tdata : word;
      taddr : addr_bus := self.translate(addr);
   begin
      self.lr_addr := taddr;
      self.lr_ctl.atype := addr_kind;
      self.lr_ctl.mode := mode;
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Check for Unibus I/O page first, then either read I/O or memory.
         --
         status := BUS_SUCC;
         if taddr >= ub_io_start then
            --
            --  Note that the CPU registers are located in the Unibus I/O page and
            --  are one address apart.  When these are implemented, the odd address
            --  check will need to be adjusted not to check these particular addresses.
            --
            if (taddr and 1) = 1 then  --  Check for memory odd address
               status := BUS_ALIGN;
               return 0;
            end if;
            if self.io_ports.contains(taddr) then
--               Ada.Text_IO.Put_Line("BUSW: Reading from I/O device " & self.io_ports(taddr).all.name);
               tdata := word(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
            else
               status := BUS_NONE;
               return 0;
            end if;
            taddr := taddr + 1;
            if self.io_ports.contains(taddr) then
--               Ada.Text_IO.Put_Line("BUSW: Reading from I/O device " & self.io_ports(taddr).all.name);
               tdata := tdata + word(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#) * 16#100#;
            else
               status := BUS_NONE;
               return 0;
            end if;
         else
            if taddr > self.max_size - 1 then
               status := BUS_NONE;
               return 0;
            end if;
            if (taddr and 1) = 1 then  --  Check for memory odd address
               status := BUS_ALIGN;
               return 0;
            end if;
            tdata := word(self.mem(taddr));
            taddr := taddr + 1;
            tdata := tdata + word(self.mem(taddr)) * 16#100#;
         end if;
         self.lr_data := data_bus(tdata);
         return tdata;
      elsif addr_kind = ADDR_IO then
         status := BUS_NONE;
         return 0;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
         return 0;
      end if;
      status := BUS_NONE;
      return 0;
   end;
   --
   --  Write a byte to logical memory LSB first (this is identical to MSB first
   --  for a single byte read/write).
   --
   procedure writel8l(self : in out unibus; addr : addr_bus; data: byte; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat) is
      taddr : addr_bus := self.translate(addr);
   begin
      self.lr_addr := taddr;
      self.lr_ctl.atype := addr_kind;
      self.lr_ctl.mode := mode;
      self.lr_data := data_bus(data);
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Check for Unibus I/O page first, then either write I/O or memory.
         --
         status := BUS_SUCC;
         if taddr >= ub_io_start then
            if self.io_ports.contains(taddr) then
--               Ada.Text_IO.Put_Line("BUSB: Writing to I/O device " & self.io_ports(taddr).all.name);
               self.io_ports(taddr).all.write(taddr, data_bus(data));
            else
               status := BUS_NONE;
            end if;
            return;
         end if;
         if addr > self.max_size then
            status := BUS_NONE;
            return;
         end if;
         self.mem(taddr) := data and 16#FF#;
      elsif addr_kind = ADDR_IO then
         Ada.Text_IO.Put_Line("BUS: I/O Space not supported by this bus");
         status := BUS_NONE;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
      end if;
   end;
   --
   --  Write a word to logical memory LSB first.
   --
   procedure writel16l(self : in out unibus; addr : addr_bus; data: word; mode : proc_mode;
                       addr_kind : addr_type; status : out bus_stat) is
      taddr : addr_bus := self.translate(addr);
      tdata : byte;
   begin
      self.lr_addr := taddr;
      self.lr_ctl.atype := addr_kind;
      self.lr_ctl.mode := mode;
      self.lr_data := data_bus(data);
      tdata := byte(data and 16#FF#);
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Check for Unibus I/O page first, then either write I/O or memory.
         --
         status := BUS_SUCC;
         if taddr >= ub_io_start then
            --
            --  Note that the CPU registers are located in the Unibus I/O page and
            --  are one address apart.  When these are implemented, the odd address
            --  check will need to be adjusted not to check these particular addresses.
            --
            if (taddr and 1) = 1 then  --  Check for memory odd address
               status := BUS_ALIGN;
               return;
            end if;
            if self.io_ports.contains(taddr) then
--               Ada.Text_IO.Put_Line("BUSW: Writing to I/O device " & self.io_ports(taddr).all.name);
               self.io_ports(taddr).all.write(addr, data_bus(tdata));
            else
               status := BUS_NONE;
               return;
            end if;
            taddr := taddr + 1;
            tdata := byte(data/16#100#);
            if self.io_ports.contains(taddr) then
--               Ada.Text_IO.Put_Line("BUSW: Writing to I/O device " & self.io_ports(taddr).all.name);
               self.io_ports(taddr).all.write(addr, data_bus(tdata));
            else
               status := BUS_NONE;
               return;
            end if;
            return;
         end if;
         if taddr > self.max_size - 1 then
            status := BUS_NONE;
            return;
         end if;
         if (taddr and 1) = 1 then  --  Check for memory odd address
            status := BUS_ALIGN;
            return;
         end if;
         self.mem(taddr) := tdata;
         taddr := taddr + 1;
         tdata := byte(data/16#100#);
         self.mem(taddr) := tdata;
      elsif addr_kind = ADDR_IO then
         Ada.Text_IO.Put_Line("BUS: I/O Space not supported by this bus");
         status := BUS_NONE;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
      end if;
   end;
   --
   --  Bus transactions from I/O devices are generally direct to memory (DMA) without
   --  address translation.  The I/O device must be given the physical address to use.
   --
   --  These are read physical and write physical.  The physical address is used
   --  directly to access memory.
   --
   overriding
   function readp(self : in out unibus; addr : addr_bus; status : out bus_stat) return data_bus is
   begin
      if addr > self.max_size then
         status := BUS_NONE;
         return 0;
      end if;
      status := BUS_SUCC;
      return data_bus(self.mem(addr));
   end;
   --
   overriding
   procedure writep(self : in out unibus; addr : addr_bus; data: data_bus; status : out bus_stat) is
   begin
      if addr > self.max_size then
         status := BUS_NONE;
         return;
      end if;
      status := BUS_SUCC;
      self.mem(addr) := byte(data and 16#ff#);
   end;
   --
   --  Memory size and adjustment.  If not overridden, they will return 0 for
   --  sizes and do nothing.
   --
   --  The the size that memory has been configured for.  This should not change
   --  over the lifetime of the object.
   --
   function mem_size(self : in out unibus) return addr_bus is
   begin
      return self.mem_size;
   end;
   --
   --  For debugging (or maybe other) purposes, the maximum address can be set.
   --  If greater than the configured size, this is ignored.  Accessing memory
   --  beyond the maximum address will return a BUS_NONE status.
   --
   procedure set_max_addr(self : in out unibus; size : addr_bus) is
   begin
      if size < self.mem_size then
         self.max_size := size;
      else
         self.max_size := self.mem_size;
      end if;
   end;
   --
   --  Return this maximum address.  This should always be less than or equal to
   --  the configured size.
   --
   function get_max_addr(self : in out unibus) return addr_bus is
   begin
      return self.max_size;
   end;
   --
end;
