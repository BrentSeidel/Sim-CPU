--
--  Author: Brent Seidel
--  Date: 17-Apr-2026
--
--  This file is part of SimCPU CLI.
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
with GNAT.Sockets;
package body cli.common is
   --
   --  Set the CPU type.
   --  Name is a string representing the desired CPU type.
   --  Returns false if name is unrecognized, otherwise True.
   --
   function set_cpu(name : String) return Boolean is
   begin
      if name = "8080" then
         cpu := new BBS.Sim_CPU.CPU.i8080.i8080;
         bus := new BBS.Sim_CPU.bus.mem8.mem8io(2**16);
         cpu.attach_bus(bus, 1);
         cpu.variant(0);
      elsif name = "8085" then
         cpu := new BBS.Sim_CPU.CPU.i8080.i8080;
         bus := new BBS.Sim_CPU.bus.mem8.mem8io(2**16);
         cpu.attach_bus(bus, 1);
         cpu.variant(1);
      elsif name = "Z80" then
         cpu := new BBS.Sim_CPU.CPU.i8080.i8080;
         bus := new BBS.Sim_CPU.bus.mem8.mem8io(2**16);
         cpu.attach_bus(bus, 1);
         cpu.variant(2);
      elsif name = "68000" then
         cpu := new BBS.Sim_CPU.CPU.m68000.m68000;
         bus := new BBS.Sim_CPU.bus.mem8.mem8mem(2**24);
         cpu.attach_bus(bus, 1);
         cpu.variant(0);
      elsif name = "68008" then
         cpu := new BBS.Sim_CPU.CPU.m68000.m68000;
         bus := new BBS.Sim_CPU.bus.mem8.mem8mem(2**20);
         cpu.attach_bus(bus, 1);
         cpu.variant(1);
      elsif name = "6502" then
         cpu := new BBS.Sim_CPU.CPU.msc6502.msc6502;
         bus := new BBS.Sim_CPU.bus.mem8.mem8mem(2**16);
         cpu.attach_bus(bus, 1);
         cpu.variant(0);
      elsif name = "PDP-11/04" then
         cpu := new BBS.Sim_CPU.CPU.pdp11.pdp11;
         bus := new BBS.Sim_CPU.bus.pdp11.unibus(2**16);
         cpu.attach_bus(bus, 1);
         cpu.variant(2);
      elsif (name = "PDP-11/10") or (name = "PDP-11/05") then
         cpu := new BBS.Sim_CPU.CPU.pdp11.pdp11;
         bus := new BBS.Sim_CPU.bus.pdp11.unibus(2**16);
         cpu.attach_bus(bus, 1);
         cpu.variant(0);
      elsif (name = "PDP-11/20") or (name = "PDP-11/15") then
         cpu := new BBS.Sim_CPU.CPU.pdp11.pdp11;
         bus := new BBS.Sim_CPU.bus.pdp11.unibus(2**16);
         cpu.attach_bus(bus, 1);
         cpu.variant(1);
      else
         return False;
      end if;
      cli.cpu.init;
      cpu_selected := True;
      return True;
   end;
   --
   --  Install hardware device
   --  dev is the name of the device
   --  which_bus is the type of bus (memory or I/O)
   --  port is the address on the bus
   --  extra is additional device dependant information
   --  present is True is extra is present
   --  returns False is unrecognized device or other error
   --
   function install_hw(dev : String; which_bus : BBS.Sim_CPU.bus_type; port : BBS.uint32;
                       except : BBS.uint32; except_present : Boolean; extra : BBS.uint32;
                       extra_present : Boolean) return Boolean is
      tel    : BBS.Sim_CPU.io.serial.telnet.telnet_access;
      dl11   : BBS.Sim_CPU.io.serial.DL11.dl11_access;
      kw11   : BBS.Sim_CPU.io.clock.KW11.kw11_access;
      fd     : floppy_ctrl.fd_access;
      disk   : BBS.Sim_CPU.io.disk.disk_access;
      ptp    : BBS.Sim_CPU.io.tape.ptape_access;
      pc11   : BBS.Sim_CPU.io.tape.PC11.PC11_access;
      mux    : BBS.Sim_CPU.io.serial.mux.mux_access;
      clk    : BBS.Sim_CPU.io.clock.clock_access;
      prn    : BBS.Sim_CPU.io.serial.print8_access;
   begin
      if dev = "TEL" then
         if not except_present then
            Ada.Text_IO.Put_Line("ATTACH TEL missing exception code.");
            return False;
         end if;
         if not extra_present then
            Ada.Text_IO.Put_Line("ATTACH TEL missing telnet port number.");
            return False;
         end if;
         tel := new BBS.Sim_CPU.io.serial.telnet.tel_tty;
         add_device(BBS.Sim_CPU.io.io_access(tel));
         bus.attach_io(BBS.Sim_CPU.io.io_access(tel), port, which_bus);
         tel.setOwner(cpu);
         tel.init(tel, GNAT.Sockets.Port_Type(extra));
         tel.setException(except);
      elsif dev = "DL11" then
         if not except_present then
            Ada.Text_IO.Put_Line("ATTACH DL11 missing exception code.");
            return False;
         end if;
         if not extra_present then
            Ada.Text_IO.Put_Line("ATTACH DL11 missing telnet port number.");
            return False;
         end if;
         dl11 := new BBS.Sim_CPU.io.serial.DL11.DL11x;
         add_device(BBS.Sim_CPU.io.io_access(dl11));
         bus.attach_io(BBS.Sim_CPU.io.io_access(dl11), port, which_bus);
         dl11.setOwner(cpu);
         dl11.init(dl11, GNAT.Sockets.Port_Type(extra));
         dl11.setException(except);
      elsif dev = "MUX" then
         if not except_present then
            Ada.Text_IO.Put_Line("ATTACH MUX missing exception code.");
            return False;
         end if;
         if not extra_present then
            Ada.Text_IO.Put_Line("ATTACH MUX missing telnet port number.");
            return False;
         end if;
         mux := new BBS.Sim_CPU.io.serial.mux.mux_tty;
         add_device(BBS.Sim_CPU.io.io_access(mux));
         bus.attach_io(BBS.Sim_CPU.io.io_access(mux), port, which_bus);
         mux.setOwner(cpu);
         mux.init(mux, GNAT.Sockets.Port_Type(extra));
            mux.setException(except);
      elsif dev = "FD" then
         if not except_present then
            Ada.Text_IO.Put_Line("ATTACH FD missing exception code.");
            return False;
         end if;
         if not extra_present then
            Ada.Text_IO.Put_Line("ATTACH FD missing number of drives.");
            return False;
         end if;
         if extra > 15 then
            Ada.Text_IO.Put_Line("ATTACH FD number of drives greater than 15.");
            return False;
         end if;
         fd := new floppy_ctrl.fd_ctrl(max_num => BBS.uint8(extra and 16#FF#));
         add_device(BBS.Sim_CPU.io.io_access(fd));
         bus.attach_io(BBS.Sim_CPU.io.io_access(fd), port, which_bus);
         fd.setOwner(cpu);
         fd.setException(except);
      elsif dev = "RK11" then
         if not except_present then
            Ada.Text_IO.Put_Line("ATTACH RK11 missing exception code.");
            return False;
         end if;
         disk := new BBS.Sim_CPU.io.disk.RK11.RK11;
         add_device(BBS.Sim_CPU.io.io_access(disk));
         bus.attach_io(BBS.Sim_CPU.io.io_access(disk), port, which_bus);
         disk.setOwner(cpu);
         disk.setException(except);
      elsif dev = "PTP" then
         ptp := new BBS.Sim_CPU.io.tape.ptape;
         add_device(BBS.Sim_CPU.io.io_access(ptp));
         bus.attach_io(BBS.Sim_CPU.io.io_access(ptp), port, which_bus);
      elsif dev = "PC11" then
         if not except_present then
            Ada.Text_IO.Put_Line("ATTACH PC11 missing exception code.");
            return False;
         end if;
         pc11 := new BBS.Sim_CPU.io.tape.PC11.PC11;
         add_device(BBS.Sim_CPU.io.io_access(pc11));
         bus.attach_io(BBS.Sim_CPU.io.io_access(pc11), port, which_bus);
         pc11.setOwner(cpu);
         pc11.setException(except);
      elsif dev = "CLK" then
         if not except_present then
            Ada.Text_IO.Put_Line("ATTACH CLK missing exception code.");
            return False;
         end if;
         clk := new BBS.Sim_CPU.io.clock.clock_device;
         add_device(BBS.Sim_CPU.io.io_access(clk));
         bus.attach_io(BBS.Sim_CPU.io.io_access(clk), port, which_bus);
         clk.setOwner(cpu);
         clk.setException(except);
      elsif dev = "KW11" then
         if not except_present then
            Ada.Text_IO.Put_Line("ATTACH KW11 missing exception code.");
            return False;
         end if;
         kw11 := new BBS.Sim_CPU.io.clock.kw11.kw11;
         add_device(BBS.Sim_CPU.io.io_access(kw11));
         bus.attach_io(BBS.Sim_CPU.io.io_access(kw11), 8#777546#, BBS.Sim_CPU.BUS_MEMORY);
         kw11.setOwner(cpu);
         kw11.setException(except);
         kw11.init(kw11, BBS.Sim_CPU.io.clock.kw11.Hz60);
      elsif dev = "PRN" then
         prn := new BBS.Sim_CPU.io.serial.print8;
         add_device(BBS.Sim_CPU.io.io_access(prn));
         bus.attach_io(BBS.Sim_CPU.io.io_access(prn), port, which_bus);
      else
         return False;
      end if;
      return True;
   end;
end;
