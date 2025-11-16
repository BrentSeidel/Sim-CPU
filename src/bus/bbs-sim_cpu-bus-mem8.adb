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
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package body BBS.Sim_CPU.bus.mem8 is
   --
   --  Bus transactions depend on the address, the processor mode, and the address
   --  type.  An address type of ADDR_IO signifies I/O addresses for processors
   --  that implement them.
   --
   --
   --  Setup the bus object.
   --
   --
   --  Called to attach an I/O device to a bus at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --
   procedure attach_io(self : in out mem8io; io_dev : BBS.Sim_CPU.io.io_access;
                       base_addr : addr_bus; which_bus : bus_type) is
      size : addr_bus := io_dev.all.getSize;
      valid : Boolean := True;
   begin
      Ada.Text_IO.Put_Line("BUS: Attaching I/O device");
      if which_bus = BUS_IO then
         --
         --  Check for port conflicts
         --
         for i in uint8(base_addr) .. uint8(base_addr + size - 1) loop
            if self.io_ports(i) /= null then
               valid := False;
               Ada.Text_IO.Put_Line("BUS: Port conflict detected attaching device to port " & toHex(i));
            end if;
            exit when not valid;
         end loop;
         if valid then
            for i in uint8(base_addr) .. uint8(base_addr + size - 1) loop
               self.io_ports(i) := io_dev;
               Ada.Text_IO.Put_Line("BUS: Attaching " & io_dev.name & " to I/O port " & toHex(i));
            end loop;
            io_dev.setBase(base_addr);
         end if;
      elsif which_bus = BUS_MEMORY then
         Ada.Text_IO.Put_Line("BUS: Memory mapped I/O not yet implemented");
      else
         Ada.Text_IO.Put_Line("BUS: Unknown I/O bus type");
      end if;
   end;
   --
   --  Called to attach a CPU to to a bus.
   --
   procedure attach_cpu(self : in  out mem8io; cpu_dev : BBS.Sim_CPU.CPU.sim_access; index : Natural) is
   begin
      self.cpu := cpu_dev;
   end;
   --
   --  Bus transactions from the processor depend on the address, the processor
   --  mode, and the address type.  An address type of ADDR_IO signifies I/O
   --  addresses for processors that implement them.  These functions may
   --  include address translation.
   --
   --  Read from logical memory
   --
   function readl(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return data_bus is
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         return data_bus(self.mem(addr));
      elsif addr_kind = ADDR_IO then
         if self.io_ports(byte(addr and 16#ff#)) /= null then
            status := BUS_SUCC;
            return self.io_ports(byte(addr and 16#ff#)).all.read(addr);
         end if;
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
   --  Read a byte from logical memory LSB first (this is identical to MSB first
   --  for a single byte read/write).
   --
   function readl8l(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte is
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         return self.mem(addr);
      elsif addr_kind = ADDR_IO then
         if self.io_ports(byte(addr and 16#ff#)) /= null then
            status := BUS_SUCC;
            return byte(self.io_ports(byte(addr and 16#ff#)).all.read(addr) and 16#FF#);
         end if;
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
   --  Read a word from memory LSB first
   --
   function readl16l(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                     addr_kind : addr_type; status : out bus_stat) return word is
      tdata : word;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size - 1 then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         --
         --  Read memory.  Checks for memory mapped I/O.  Checks for shared memory,
         --  memory management, or other special stuff can be added here.
         --
         tdata := word(self.mem(addr));
         tdata := tdata + word(self.mem(addr + 1)) * 16#100#;
         return tdata;
      elsif addr_kind = ADDR_IO then
         if self.io_ports(byte(addr and 16#ff#)) /= null then
            status := BUS_SUCC;
            tdata := word(self.io_ports(byte(addr and 16#ff#)).all.read(addr) and 16#FF#);
         else
            status := BUS_NONE;
            return 0;
         end if;
         if self.io_ports(byte(addr+1 and 16#ff#)) /= null then
            status := BUS_SUCC;
            tdata := tdata + word(self.io_ports(byte(addr+1 and 16#ff#)).all.read(addr+1) and 16#FF#)*16#100#;
         end if;
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
   --  Read a byte from logical memory MSB first (this is identical to LSB first
   --  for a single byte read/write).
   --
   function readl8m(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte is
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         return self.mem(addr);
      elsif addr_kind = ADDR_IO then
         if self.io_ports(byte(addr and 16#ff#)) /= null then
            status := BUS_SUCC;
            return byte(self.io_ports(byte(addr and 16#ff#)).all.read(addr) and 16#FF#);
         end if;
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
   --  Read a word from memory MSB first
   --
   function readl16m(self : in out mem8io; addr : addr_bus; mode : proc_mode;
                     addr_kind : addr_type; status : out bus_stat) return word is
      tdata : word;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size - 1 then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         --
         --  Read memory.  Checks for memory mapped I/O.  Checks for shared memory,
         --  memory management, or other special stuff can be added here.
         --
         tdata := word(self.mem(addr)) * 16#100#;
         tdata := tdata + word(self.mem(addr + 1));
         return tdata;
      elsif addr_kind = ADDR_IO then
         if self.io_ports(byte(addr and 16#ff#)) /= null then
            status := BUS_SUCC;
            tdata := word(self.io_ports(byte(addr and 16#ff#)).all.read(addr) and 16#FF#)*16#100#;
         else
            status := BUS_NONE;
            return 0;
         end if;
         if self.io_ports(byte(addr+1 and 16#ff#)) /= null then
            status := BUS_SUCC;
            tdata := tdata + word(self.io_ports(byte(addr+1 and 16#ff#)).all.read(addr+1) and 16#FF#);
         end if;
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
   --  Write to logical memory
   --
   procedure writel(self : in out mem8io; addr : addr_bus; data: data_bus; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat) is
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         self.mem(addr) := byte(data and 16#ff#);
      elsif addr_kind = ADDR_IO then
         if self.io_ports(byte(addr and 16#ff#)) /= null then
            self.io_ports(byte(addr and 16#ff#)).all.write(addr, data);
            status := BUS_SUCC;
         else
            status := BUS_NONE;
         end if;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
      end if;
   end;
   --
   --  Write a byte to logical memory LSB first (this is identical to MSB first
   --  for a single byte read/write).
   --
   procedure writel8l(self : in out mem8io; addr : addr_bus; data: byte; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat) is
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         self.mem(addr) := data;
      elsif addr_kind = ADDR_IO then
         if self.io_ports(byte(addr and 16#ff#)) /= null then
            self.io_ports(byte(addr and 16#ff#)).all.write(addr, data_bus(data));
            status := BUS_SUCC;
         else
            status := BUS_NONE;
         end if;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
      end if;
   end;
   --
   --  Write a word to logical memory LSB first.
   --
   procedure writel16l(self : in out mem8io; addr : addr_bus; data: word; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat) is
      taddr : byte;
      tdata : byte;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         self.mem(addr) := byte(data and 16#FF#);
         self.mem(addr + 1) := byte(data/16#100#);
      elsif addr_kind = ADDR_IO then
         taddr := byte(addr and 16#ff#);
         tdata :=  byte(data and 16#FF#);
         if self.io_ports(taddr) /= null then
            self.io_ports(taddr).all.write(addr_bus(taddr), data_bus(tdata));
            status := BUS_SUCC;
         else
            status := BUS_NONE;
         end if;
         taddr := taddr + 1;
         tdata :=  byte(data/16#100#);
         if self.io_ports(taddr) /= null then
            self.io_ports(taddr).all.write(addr_bus(taddr), data_bus(tdata));
            status := BUS_SUCC;
         else
            status := BUS_NONE;
         end if;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
      end if;
   end;
   --
   --  Write a byte to logical memory MSB first (this is identical to LSB first
   --  for a single byte read/write).
   --
   procedure writel8m(self : in out mem8io; addr : addr_bus; data: byte; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat) is
      taddr : byte;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         self.mem(addr) := data;
      elsif addr_kind = ADDR_IO then
         taddr := byte(addr and 16#ff#);
         if self.io_ports(taddr) /= null then
            self.io_ports(taddr).all.write(addr_bus(taddr), data_bus(data));
            status := BUS_SUCC;
         else
            status := BUS_NONE;
         end if;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
      end if;
   end;
   --
   --  Write a word to logical memory MSB first.
   --
   procedure writel16m(self : in out mem8io; addr : addr_bus; data: word; mode : proc_mode;
                       addr_kind : addr_type; status : out bus_stat) is
      taddr : byte;
      tdata : byte;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         self.mem(addr) := byte(data/16#100#);
         self.mem(addr + 1) := byte(data and 16#FF#);
      elsif addr_kind = ADDR_IO then
         taddr := byte(addr and 16#ff#);
         tdata :=  byte(data and 16#FF#);
         if self.io_ports(taddr) /= null then
            self.io_ports(taddr).all.write(addr_bus(taddr), data_bus(tdata));
            status := BUS_SUCC;
         else
            status := BUS_NONE;
         end if;
         taddr := taddr + 1;
         tdata :=  byte(data/16#100#);
         if self.io_ports(taddr) /= null then
            self.io_ports(taddr).all.write(addr_bus(taddr), data_bus(tdata));
            status := BUS_SUCC;
         else
            status := BUS_NONE;
         end if;
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
   function readp(self : in out mem8io; addr : addr_bus; status : out bus_stat) return data_bus is
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
   procedure writep(self : in out mem8io; addr : addr_bus; data: data_bus; status : out bus_stat) is
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
   function mem_size(self : in out mem8io) return addr_bus is
   begin
      return self.mem_size;
   end;
   --
   --  For debugging (or maybe other) purposes, the maximum address can be set.
   --  If greater than the configured size, this is ignored.  Accessing memory
   --  beyond the maximum address will return a BUS_NONE status.
   --
   procedure set_max_addr(self : in out mem8io; size : addr_bus) is
   begin
      if size < self.mem_size then
         self.max_size := size;
      end if;
   end;
   --
   --  Return this maximum address.  This should always be less than or equal to
   --  the configured size.
   --
   function get_max_addr(self : in out mem8io) return addr_bus is
   begin
      return self.max_size;
   end;
   --  ------------------------------------------------------------------------
   --
   --  Bus transactions depend on the address, the processor mode, and the address
   --  type.  An address type of ADDR_IO signifies I/O addresses for processors
   --  that implement them.
   --
   --
   --  Setup the bus object.
   --
   --
   --  Called to attach an I/O device to a bus at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --
   procedure attach_io(self : in out mem8mem; io_dev : BBS.Sim_CPU.io.io_access;
                       base_addr : addr_bus; which_bus : bus_type) is
      size : addr_bus := io_dev.all.getSize;
      valid : Boolean := True;
   begin
      Ada.Text_IO.Put_Line("BUS: Attaching I/O device");
      if which_bus = BUS_MEMORY then
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
   procedure attach_cpu(self : in  out mem8mem; cpu_dev : BBS.Sim_CPU.CPU.sim_access; index : Natural) is
   begin
      self.cpu := cpu_dev;
   end;
   --
   --  Bus transactions from the processor depend on the address, the processor
   --  mode, and the address type.  An address type of ADDR_IO signifies I/O
   --  addresses for processors that implement them.  These functions may
   --  include address translation.
   --
   --  Read logical memory
   --
   function readl(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return data_bus is
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         --
         --  Read memory.  Checks for memory mapped I/O.  Checks for shared memory,
         --  memory management, or other special stuff can be added here.
         --
         if self.io_ports.contains(addr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(addr).all.name);
            return (self.io_ports(addr).all.read(addr_bus(addr)) and 16#FF#);
         end if;
         return data_bus(self.mem(addr));
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
   --  Read a byte from logical memory LSB first (this is identical to MSB first
   --  for a single byte read/write).
   --
   function readl8l(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte is
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         --
         --  Read memory.  Checks for memory mapped I/O.  Checks for shared memory,
         --  memory management, or other special stuff can be added here.
         --
         if self.io_ports.contains(addr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(addr).all.name);
            return byte(self.io_ports(addr).all.read(addr_bus(addr)) and 16#FF#);
         end if;
         return self.mem(addr);
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
   function readl16l(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                     addr_kind : addr_type; status : out bus_stat) return word is
      tdata : word;
      taddr : addr_bus := addr;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size - 1 then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         --
         --  Read memory.  Checks for memory mapped I/O.  Checks for shared memory,
         --  memory management, or other special stuff can be added here.
         --
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := word(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
         else
            tdata := word(self.mem(taddr));
         end if;
         taddr := taddr + 1;
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := tdata + word(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#) * 16#100#;
         else
            tdata := tdata + word(self.mem(taddr)) * 16#100#;
         end if;
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
   --  Read a long from logical memory LSB first.
   --
   function readl32l(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                     addr_kind : addr_type; status : out bus_stat) return long is
      tdata : long;
      taddr : addr_bus := addr;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size - 1 then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         --
         --  Read memory.  Checks for memory mapped I/O.  Checks for shared memory,
         --  memory management, or other special stuff can be added here.
         --
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := long(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
         else
            tdata := long(self.mem(taddr));
         end if;
         taddr := taddr + 1;
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := tdata + long(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
         else
            tdata := tdata + long(self.mem(taddr))*16#100#;
         end if;
         taddr := taddr + 1;
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := tdata + long(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
         else
            tdata := tdata + long(self.mem(taddr))*16#1_0000#;
         end if;
         taddr := taddr + 1;
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := tdata + long(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
         else
            tdata := tdata + long(self.mem(taddr))*16#100_0000#;
         end if;
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
   --  Read a byte from logical memory MSB first (this is identical to LSB first
   --  for a single byte read/write).
   --
   function readl8m(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; status : out bus_stat) return byte is
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         --
         --  Read memory.  Checks for memory mapped I/O.  Checks for shared memory,
         --  memory management, or other special stuff can be added here.
         --
         if self.io_ports.contains(addr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(addr).all.name);
            return byte(self.io_ports(addr).all.read(addr_bus(addr)) and 16#FF#);
         end if;
         return self.mem(addr);
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
   --  Read a word from logical memory MSB first.
   --
   function readl16m(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                     addr_kind : addr_type; status : out bus_stat) return word is
      tdata : word;
      taddr : addr_bus := addr;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size - 1 then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         --
         --  Read memory.  Checks for memory mapped I/O.  Checks for shared memory,
         --  memory management, or other special stuff can be added here.
         --
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := word(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
         else
            tdata := word(self.mem(taddr));
         end if;
         tdata := tdata*16#100#;
         taddr := taddr + 1;
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := tdata + word(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#) * 16#100#;
         else
            tdata := tdata + word(self.mem(taddr));
         end if;
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
   --  Read a long from logical memory MSB first.
   --
   function readl32m(self : in out mem8mem; addr : addr_bus; mode : proc_mode;
                     addr_kind : addr_type; status : out bus_stat) return long is
      tdata : long;
      taddr : addr_bus := addr;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size - 1 then
            status := BUS_NONE;
            return 0;
         end if;
         status := BUS_SUCC;
         --
         --  Read memory.  Checks for memory mapped I/O.  Checks for shared memory,
         --  memory management, or other special stuff can be added here.
         --
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := long(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
         else
            tdata := long(self.mem(taddr));
         end if;
         tdata := tdata*16#100#;
         taddr := taddr + 1;
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := tdata + long(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
         else
            tdata := tdata + long(self.mem(taddr));
         end if;
         tdata := tdata*16#100#;
         taddr := taddr + 1;
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := tdata + long(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
         else
            tdata := tdata + long(self.mem(taddr));
         end if;
         tdata := tdata*16#100#;
         taddr := taddr + 1;
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Reading from I/O device " & self.io_ports(taddr).all.name);
            tdata := tdata + long(self.io_ports(taddr).all.read(addr_bus(taddr)) and 16#FF#);
         else
            tdata := tdata + long(self.mem(taddr));
         end if;
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
   --  Write logical memory
   --
   procedure writel(self : in out mem8mem; addr : addr_bus; data: data_bus; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat) is
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         --
         --  Set memory.  Checks for memory mapped I/O.  Checks for shared memory
         --  or other special stuff can be added here.
         --
         if self.io_ports.contains(addr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(addr).all.name);
            self.io_ports(addr).all.write(addr, data_bus(data));
         else
            self.mem(addr) := byte(data and 16#FF#);
         end if;
      elsif addr_kind = ADDR_IO then
         Ada.Text_IO.Put_Line("BUS: I/O Space not supported by this bus");
         status := BUS_NONE;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
      end if;
   end;
   --
   --  Write a byte to logical memory LSB first (this is identical to MSB first
   --  for a single byte read/write).
   --
   procedure writel8l(self : in out mem8mem; addr : addr_bus; data: byte; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat) is
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         --
         --  Set memory.  Checks for memory mapped I/O.  Checks for shared memory
         --  or other special stuff can be added here.
         --
         if self.io_ports.contains(addr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(addr).all.name);
            self.io_ports(addr).all.write(addr, data_bus(data));
         else
            self.mem(addr) := data and 16#FF#;
         end if;
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
   procedure writel16l(self : in out mem8mem; addr : addr_bus; data: word; mode : proc_mode;
                       addr_kind : addr_type; status : out bus_stat) is
      taddr : addr_bus := addr;
      tdata : byte;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if taddr > self.max_size - 1 then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         --
         --  Set memory.  Checks for memory mapped I/O.  Checks for shared memory
         --  or other special stuff can be added here.
         --
         tdata := byte(data and 16#FF#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
         taddr := taddr + 1;
         tdata := byte(data/16#100#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
      elsif addr_kind = ADDR_IO then
         Ada.Text_IO.Put_Line("BUS: I/O Space not supported by this bus");
         status := BUS_NONE;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
      end if;
   end;
   --
   --  Write a long to logical memory LSB first.
   --
   procedure writel32l(self : in out mem8mem; addr : addr_bus; data: long; mode : proc_mode;
                       addr_kind : addr_type; status : out bus_stat) is
      taddr : addr_bus := addr;
      tdata : byte;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if taddr > self.max_size - 1 then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         --
         --  Set memory.  Checks for memory mapped I/O.  Checks for shared memory
         --  or other special stuff can be added here.
         --
         tdata := byte(data and 16#FF#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
         taddr := taddr + 1;
         tdata := byte(data/16#100# and 16#FF#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
         taddr := taddr + 1;
         tdata := byte(data/16#1_0000# and 16#FF#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
         taddr := taddr + 1;
         tdata := byte(data/16#100_0000#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
      elsif addr_kind = ADDR_IO then
         Ada.Text_IO.Put_Line("BUS: I/O Space not supported by this bus");
         status := BUS_NONE;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
      end if;
   end;
   --
   --  Write a byte to logical memory MSB first (this is identical to LSB first
   --  for a single byte read/write).
   --
   procedure writel8m(self : in out mem8mem; addr : addr_bus; data: byte; mode : proc_mode;
                   addr_kind : addr_type; status : out bus_stat) is
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if addr > self.max_size then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         --
         --  Set memory.  Checks for memory mapped I/O.  Checks for shared memory
         --  or other special stuff can be added here.
         --
         if self.io_ports.contains(addr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(addr).all.name);
            self.io_ports(addr).all.write(addr, data_bus(data));
         else
            self.mem(addr) := data and 16#FF#;
         end if;
      elsif addr_kind = ADDR_IO then
         Ada.Text_IO.Put_Line("BUS: I/O Space not supported by this bus");
         status := BUS_NONE;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
      end if;
   end;
   --
   --  Write a word to logical memory MSB first.
   --
   procedure writel16m(self : in out mem8mem; addr : addr_bus; data: word; mode : proc_mode;
                       addr_kind : addr_type; status : out bus_stat) is
      taddr : addr_bus := addr;
      tdata : byte;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if taddr > self.max_size - 1 then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         --
         --  Set memory.  Checks for memory mapped I/O.  Checks for shared memory
         --  or other special stuff can be added here.
         --
         tdata := byte(data/16#100#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
         taddr := taddr + 1;
         tdata := byte(data and 16#FF#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
      elsif addr_kind = ADDR_IO then
         Ada.Text_IO.Put_Line("BUS: I/O Space not supported by this bus");
         status := BUS_NONE;
      elsif addr_kind = ADDR_NONE then
         status := BUS_NONE;
      end if;
   end;
   --
   --  Write a long to logical memory MSB first.
   --
   procedure writel32m(self : in out mem8mem; addr : addr_bus; data: long; mode : proc_mode;
                       addr_kind : addr_type; status : out bus_stat) is
      taddr : addr_bus := addr;
      tdata : byte;
   begin
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Address translation goes here.
         --
         if taddr > self.max_size - 1 then
            status := BUS_NONE;
            return;
         end if;
         status := BUS_SUCC;
         --
         --  Set memory.  Checks for memory mapped I/O.  Checks for shared memory
         --  or other special stuff can be added here.
         --
         tdata := byte(data/16#100_0000#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
         taddr := taddr + 1;
         tdata := byte(data/16#1_0000# and 16#FF#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
         taddr := taddr + 1;
         tdata := byte(data/16#100# and 16#FF#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
         taddr := taddr + 1;
         tdata := byte(data and 16#FF#);
         if self.io_ports.contains(taddr) then
            Ada.Text_IO.Put_Line("BUS: Writing to I/O device " & self.io_ports(taddr).all.name);
            self.io_ports(taddr).all.write(addr, data_bus(tdata));
         else
            self.mem(taddr) := tdata;
         end if;
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
   function readp(self : in out mem8mem; addr : addr_bus; status : out bus_stat) return data_bus is
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
   procedure writep(self : in out mem8mem; addr : addr_bus; data: data_bus; status : out bus_stat) is
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
   function mem_size(self : in out mem8mem) return addr_bus is
   begin
      return self.mem_size;
   end;
   --
   --  For debugging (or maybe other) purposes, the maximum address can be set.
   --  If greater than the configured size, this is ignored.  Accessing memory
   --  beyond the maximum address will return a BUS_NONE status.
   --
   procedure set_max_addr(self : in out mem8mem; size : addr_bus) is
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
   function get_max_addr(self : in out mem8mem) return addr_bus is
   begin
      return self.max_size;
   end;
   --
end;
