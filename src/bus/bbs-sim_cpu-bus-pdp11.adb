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
use type BBS.Sim_CPU.CPU.pdp11.variants_pdp11;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
use type BBS.Sim_CPU.io.dev_type;
package body BBS.Sim_CPU.bus.pdp11 is
   --
   --  Perform address translation for logical reads
   --
   function translate(self : in out unibus; addr : addr_bus; mode : proc_mode;
                      addr_kind : addr_type; rw : Boolean) return addr_bus is
   begin
      --
      --  Check if MMU.  If not, just relocate the upper 8k.
      --
      if self.has_mmu then
         return self.mmu.translate(addr, mode, addr_kind, rw);
      else
         if addr < base_io_start then
            return addr;
         elsif addr <= base_io_end then
            return addr + (ub_io_start - base_io_start);
         else
            Ada.Text_IO.Put_Line("MMU: Address out of range for 16 bit mode.");
            return bad_addr;
         end if;
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
      --
      --  Check if adding a memory management unit.  This should be a KT11 device.
      --  It is handled differently from other I/O devices.
      --
      if io_dev.dev_class = BBS.Sim_CPU.io.MM then
         Ada.Text_IO.Put_Line("BUS: Attaching MMU " & io_dev.name);
         self.mmu := BBS.Sim_CPU.io.kt11.kt11_access(io_dev);
         self.has_mmu := True;
         self.mmu.reset;
         self.mmu.setOwner(BBS.Sim_CPU.cpu.sim_access(self.cpu));
         self.devices.append(io_dev);
         return;
      end if;
      Ada.Text_IO.Put_Line("BUS: Attaching I/O device " & io_dev.name);
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
               Ada.Text_IO.Put_Line("BUS: Address conflict detected attching " & io_dev.name &
                                      " to location " & toOct(i) & " (" & toHex(i) & ")");
           end if;
           exit when not valid;
         end loop;
         --
         --  If no conflict, attach the port
         --
         if valid then
            for i in base_addr .. base_addr + size - 1 loop
               self.io_ports.include(i, io_dev);
               if self.cpu.all.trace.bus then
                  Ada.Text_IO.Put_Line("BUS: Attaching " & io_dev.name &
                                         " to memory location " & toOct(i) & " (" & toHex(i) & ")");
                  end if;
            end loop;
            io_dev.setBase(base_addr);
            self.devices.append(io_dev);
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
      self.cpu := BBS.Sim_CPU.CPU.pdp11.PDP11_access(cpu_dev);
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
      tdata  : byte;
      taddr  : addr_bus := self.translate(addr, mode, addr_kind, False);
      config : constant BBS.Sim_CPU.CPU.PDP11.features := self.cpu.all.get_features;
   begin
      self.lr_addr := taddr;
      self.lr_ctl.atype := addr_kind;
      self.lr_ctl.mode := mode;
      if taddr = bad_addr then
         status := BUS_MMU;
         return 0;
      end if;
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Check for Unibus I/O page first, then either read I/O or memory.
         --
         status := BUS_SUCC;
         if taddr >= ub_io_start then
            if self.has_mmu then
               if ((taddr >= mmu0_start) and (taddr <= mmu0_end)) or
                 ((taddr >= mmu1_start) and (taddr <= mmu1_end)) then
                  tdata := byte(self.mmu.read(taddr, bits8, status) and 16#FF#);
                  self.lr_data := data_bus(tdata);
                  return tdata;
               end if;
            end if;
            if self.io_ports.contains(taddr) then
               tdata := byte(self.io_ports(taddr).all.read(taddr, bits8, status) and 16#FF#);
               self.lr_data := data_bus(tdata);
               return tdata;
            elsif taddr = io_csr then
               return byte(self.sr_ad and 16#FF#);
            elsif taddr = io_csr + 1 then
               return byte((self.sr_ad/16#100#) and 16#FF#);
            elsif taddr = io_ps then
               return byte(self.cpu.all.read_reg(reg_psw) and 16#FF#);
            elsif taddr = io_ps + 1 then
               if (self.cpu.all.variant = var_1110) then
                  status := BUS_ALIGN;
               end if;
               return byte((self.cpu.all.read_reg(reg_psw)/16#100#) and 16#FF#);
            elsif (taddr = io_r0) and config.reg_bus then
               return byte((self.cpu.all.read_reg(reg_r0)/16#100#) and 16#FF#);
            elsif (taddr = io_r1) and config.reg_bus then
               return byte((self.cpu.all.read_reg(reg_r1)/16#100#) and 16#FF#);
            elsif (taddr = io_r2) and config.reg_bus then
               return byte((self.cpu.all.read_reg(reg_r2)/16#100#) and 16#FF#);
            elsif (taddr = io_r3) and config.reg_bus then
               return byte((self.cpu.all.read_reg(reg_r3)/16#100#) and 16#FF#);
            elsif (taddr = io_r4) and config.reg_bus then
               return byte((self.cpu.all.read_reg(reg_r4)/16#100#) and 16#FF#);
            elsif (taddr = io_r5) and config.reg_bus then
               return byte((self.cpu.all.read_reg(reg_r5)/16#100#) and 16#FF#);
            elsif (taddr = io_sp) and config.reg_bus then
               return byte((self.cpu.all.read_reg(reg_ksp)/16#100#) and 16#FF#);
            elsif (taddr = io_pc) and config.reg_bus then
               return byte((self.cpu.all.read_reg(reg_pc)/16#100#) and 16#FF#);
            else
               Ada.Text_IO.Put_Line("BUSB: Reading unassigned I/O address " & toOct(taddr));
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
      tdata  : word;
      taddr  : addr_bus := self.translate(addr, mode, addr_kind, False);
      config : constant BBS.Sim_CPU.CPU.PDP11.features := self.cpu.all.get_features;
   begin
      self.lr_addr := taddr;
      self.lr_ctl.atype := addr_kind;
      self.lr_ctl.mode := mode;
      if taddr = bad_addr then
         status := BUS_MMU;
         return 0;
      end if;
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Check for Unibus I/O page first, then either read I/O or memory.
         --
         status := BUS_SUCC;
         if taddr >= ub_io_start then
            --
            --  Note that the CPU registers are located in the Unibus I/O page and
            --  are one address apart.  Check for these before checking for odd addresses
            --
            if (taddr = io_r0) and config.reg_bus then
               return word(self.cpu.all.read_reg(reg_r0) and 16#FFFF#);
            elsif (taddr = io_r1) and config.reg_bus then
               return word(self.cpu.all.read_reg(reg_r1) and 16#FFFF#);
            elsif (taddr = io_r2) and config.reg_bus then
               return word(self.cpu.all.read_reg(reg_r2) and 16#FFFF#);
            elsif (taddr = io_r3) and config.reg_bus then
               return word(self.cpu.all.read_reg(reg_r3) and 16#FFFF#);
            elsif (taddr = io_r4) and config.reg_bus then
               return word(self.cpu.all.read_reg(reg_r4) and 16#FFFF#);
            elsif (taddr = io_r5) and config.reg_bus then
               return word(self.cpu.all.read_reg(reg_r5) and 16#FFFF#);
            elsif (taddr = io_sp) and config.reg_bus then
               return word(self.cpu.all.read_reg(reg_ksp) and 16#FFFF#);
            elsif (taddr = io_pc) and config.reg_bus then
               return word(self.cpu.all.read_reg(reg_pc) and 16#FFFF#);
            end if;
            if (taddr and 1) = 1 then  --  Check for memory odd address
               status := BUS_ALIGN;
               return 0;
            end if;
            if self.has_mmu then
               if ((taddr >= mmu0_start) and (taddr <= mmu0_end)) or
                 ((taddr >= mmu1_start) and (taddr <= mmu1_end)) then
                  tdata := word(self.mmu.read(taddr, bits16, status) and 16#FFFF#);
                  self.lr_data := data_bus(tdata);
                  return tdata;
               end if;
            end if;
            if self.io_ports.contains(taddr) then
               tdata := word(self.io_ports(taddr).all.read(taddr, bits16, status) and 16#FFFF#);
            elsif taddr = io_csr then
               return word(self.sr_ad and 16#FFFF#);
            elsif taddr = io_ps then
               if self.cpu.all.trace.bus then
                  Ada.Text_IO.Put_line("BUSW: Reading " & toOct(word(self.cpu.all.read_reg(reg_psw) and 16#FFFF#)) & " from PSW.");
               end if;
               return word(self.cpu.all.read_reg(reg_psw) and 16#FFFF#);
            else
               Ada.Text_IO.Put_Line("BUSW: Reading unassigned I/O address " & toOct(taddr));
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
      taddr  : addr_bus := self.translate(addr, mode, addr_kind, True);
      config : constant BBS.Sim_CPU.CPU.PDP11.features := self.cpu.all.get_features;
   begin
      self.lr_addr := taddr;
      self.lr_ctl.atype := addr_kind;
      self.lr_ctl.mode := mode;
      self.lr_data := data_bus(data);
      if taddr = bad_addr then
         status := BUS_MMU;
         return;
      end if;
      if (addr_kind = ADDR_INTR) or (addr_kind = ADDR_DATA) or (addr_kind = ADDR_INST) then
         --
         --  Check for Unibus I/O page first, then either write I/O or memory.
         --
         status := BUS_SUCC;
         if taddr >= ub_io_start then
            if self.has_mmu then
               if ((taddr >= mmu0_start) and (taddr <= mmu0_end)) or
                 ((taddr >= mmu1_start) and (taddr <= mmu1_end)) then
                  self.mmu.write(taddr, data_bus(data), bits8, status);
                  return;
               end if;
            end if;
            if self.io_ports.contains(taddr) then
               self.io_ports(taddr).all.write(taddr, data_bus(data), bits8, status);
            elsif taddr = io_csr then
               null;  --  TODO: Optionally interface with LSB of hardware switch register.
            elsif taddr = io_csr + 1 then
               null;  --  TODO: Optionally interface with MSB of hardware switch register.
            elsif taddr = io_ps then  --  Processor status register LSB
               if self.cpu.all.trace.bus then
                  Ada.Text_IO.Put("BUSB: Writing " & toOct(byte(data and 16#FF#)) & " to PSW LSB.  Result value is ");
               end if;
               if not config.set_PSW_T then
                  self.cpu.all.set_reg(reg_psw,  data_bus(data and 16#EF#) or data_bus(self.cpu.all.read_reg(reg_psw) and 16#FF00#));
               else
                  self.cpu.all.set_reg(reg_psw,  data_bus(data) or data_bus(self.cpu.all.read_reg(reg_psw) and 16#FF00#));
               end if;
               if self.cpu.all.trace.bus then
                  Ada.Text_IO.Put_Line(toOct(word(self.cpu.all.read_reg(reg_psw) and 16#FFFF#)) & ", " & self.cpu.all.read_reg(10));
               end if;
            elsif taddr = io_ps + 1 then  --  Processor status register MSB
               if self.cpu.all.trace.bus then
                  Ada.Text_IO.Put("BUSB: Writing " & toOct(byte(data and 16#FF#)) & " to PSW MSB.  Result value is ");
               end if;
               if (self.cpu.all.variant = var_1110) then
                  status := BUS_ALIGN;
               else
                  self.cpu.all.set_reg(reg_psw,  data_bus(data)*16#100# or data_bus(self.cpu.all.read_reg(reg_psw) and 16#FF#));
               end if;
               if self.cpu.all.trace.bus then
                  Ada.Text_IO.Put_Line(toOct(word(self.cpu.all.read_reg(reg_psw) and 16#FFFF#)) & ", " & self.cpu.all.read_reg(10));
               end if;
            elsif (taddr = io_r0) and config.reg_bus then
               self.cpu.all.set_reg(reg_r0, data_bus(data));
            elsif (taddr = io_r1) and config.reg_bus then
               self.cpu.all.set_reg(reg_r1, data_bus(data));
            elsif (taddr = io_r2) and config.reg_bus then
               self.cpu.all.set_reg(reg_r2, data_bus(data));
            elsif (taddr = io_r3) and config.reg_bus then
               self.cpu.all.set_reg(reg_r3, data_bus(data));
            elsif (taddr = io_r4) and config.reg_bus then
               self.cpu.all.set_reg(reg_r4, data_bus(data));
            elsif (taddr = io_r5) and config.reg_bus then
               self.cpu.all.set_reg(reg_r5, data_bus(data));
            elsif (taddr = io_sp) and config.reg_bus then
               self.cpu.all.set_reg(reg_ksp, data_bus(data));
            elsif (taddr = io_pc) and config.reg_bus then
               self.cpu.all.set_reg(reg_pc, data_bus(data));
            else
               Ada.Text_IO.Put_Line("BUSB: Writing unassigned I/O address " & toOct(taddr));
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
      taddr  : addr_bus := self.translate(addr, mode, addr_kind, True);
      config : constant BBS.Sim_CPU.CPU.PDP11.features := self.cpu.all.get_features;
      tdata  : byte;
   begin
      self.lr_addr := taddr;
      self.lr_ctl.atype := addr_kind;
      self.lr_ctl.mode := mode;
      self.lr_data := data_bus(data);
      tdata := byte(data and 16#FF#);
      if taddr = bad_addr then
         status := BUS_MMU;
         return;
      end if;
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
            if (taddr = io_r0) and config.reg_bus then
               self.cpu.all.set_reg(reg_r0, data_bus(data));
               return;
            elsif (taddr = io_r1) and config.reg_bus then
               self.cpu.all.set_reg(reg_r1, data_bus(data));
               return;
            elsif (taddr = io_r2) and config.reg_bus then
               self.cpu.all.set_reg(reg_r2, data_bus(data));
               return;
            elsif (taddr = io_r3) and config.reg_bus then
               self.cpu.all.set_reg(reg_r3, data_bus(data));
               return;
            elsif (taddr = io_r4) and config.reg_bus then
               self.cpu.all.set_reg(reg_r4, data_bus(data));
               return;
            elsif (taddr = io_r5) and config.reg_bus then
               self.cpu.all.set_reg(reg_r5, data_bus(data));
               return;
            elsif (taddr = io_sp) and config.reg_bus then
               self.cpu.all.set_reg(reg_ksp, data_bus(data));
               return;
            elsif (taddr = io_pc) and config.reg_bus then
               self.cpu.all.set_reg(reg_pc, data_bus(data));
               return;
            end if;
            if (taddr and 1) = 1 then  --  Check for memory odd address
               status := BUS_ALIGN;
               return;
            end if;
            if self.has_mmu then
               if ((taddr >= mmu0_start) and (taddr <= mmu0_end)) or
                 ((taddr >= mmu1_start) and (taddr <= mmu1_end)) then
                  self.mmu.write(taddr, data_bus(data), bits16, status);
                  return;
               end if;
            end if;
            if self.io_ports.contains(taddr) then
               self.io_ports(taddr).all.write(taddr, data_bus(data), bits16, status);
            elsif taddr = io_csr then
               null;  --  TODO: Optionally interface with hardware switch register.
            elsif taddr = io_ps then  --  Processor status register
               if self.cpu.all.trace.bus then
                  Ada.Text_IO.Put("BUSW: Writing " & toOct(word(data and 16#FFFF#)) & " to PSW.  Result value is ");
               end if;
               if not config.set_PSW_T then
                  self.cpu.all.set_reg(reg_psw,  data_bus(data and 16#FFEF#));
               else
                  self.cpu.all.set_reg(reg_psw,  data_bus(data));
               end if;
               if self.cpu.all.trace.bus then
                  Ada.Text_IO.Put_Line(toOct(word(self.cpu.all.read_reg(10) and 16#FFFF#)) & ", " & self.cpu.all.read_reg(10));
               end if;
            else
               if self.cpu.all.trace.bus then
                  Ada.Text_IO.Put_Line("BUSW: Writing unassigned I/O address " & toOct(taddr));
               end if;
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
--      Ada.Text_IO.Put_Line("WriteP: Writing byte " & toHex(byte(data and 16#FF#)) &
--                             " to address " & toOct(addr));
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
   --  Send a reset signal to devices on the bus, if the bus supports it.  If not.
   --  nothing happens.
   --
   overriding
   procedure reset(self : in out unibus) is
   begin
      for dev of self.devices loop
         Ada.Text_IO.Put_Line("BUS: Resetting device " & dev.name & " - " & dev.description);
         dev.reset;
      end loop;
   end;
   --
end;
