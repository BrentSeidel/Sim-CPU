--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.--
--
with BBS;
use type BBS.uint32;
with BBS.Sim_CPU;
with BBS.Sim_CPU.i8080;
with BBS.Sim_CPU.m68000;
with BBS.Sim_CPU.msc6502;
with BBS.Sim_CPU.disk;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with cli;

procedure SimCPUcli is
   selection : Integer := 0;
begin
   Ada.Text_IO.Put_Line("CPU Simulator Test Program");
   loop
      Ada.Text_IO.Put_Line("Available simulators are:");
      Ada.Text_IO.Put_Line("1. Intel 8080/8085");
      Ada.Text_IO.Put_Line("2. Motorola 68000");
      Ada.Text_IO.Put_Line("3. MOS Technology 6502 (in development)");
      Ada.Text_IO.Put("Selection: ");
      Ada.Integer_Text_IO.Get(selection, 0);
      --
      --  This is just to clear out any text on the rest of the line.
      --
      declare
         dummy : String := Ada.Text_IO.Get_line;
      begin
         null;  --  Nothing to do here.
      end;
      exit when (selection > 0) and (selection < 4);
   end loop;
   if selection = 1 then  --  8080/8085/Z80
      cli.cpu := new BBS.Sim_CPU.i8080.i8080;
      cli.set_var(cli.cpu);
      cli.cpu.init;
      Ada.Text_IO.Put_Line("Simulator name: " & cli.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & cli.cpu.variant(cli.cpu.variant));
      cli.add_device(cli.tel0'Access);
      cli.cpu.attach_io(cli.tel0'Access, 0, BBS.Sim_CPU.BUS_IO);
      cli.tel0.setOwner(cli.cpu);
      cli.tel0.init(cli.tel0'Access, 2171);
      cli.add_device(cli.paper'Access);
      cli.cpu.attach_io(cli.paper'Access, 2, BBS.Sim_CPU.BUS_IO);
      cli.add_device(cli.fd'Access);
      cli.cpu.attach_io(cli.fd'Access, 4, BBS.Sim_CPU.BUS_IO);
      cli.fd.setOwner(cli.cpu);
--      cli.fd.open(0, cli.floppy_ctrl.floppy8_geom, "cpmboot.img");
--      cli.fd.open(1, cli.floppy_ctrl.floppy8_geom, "cpma.cpm");
--      cli.fd.open(2, cli.floppy_ctrl.floppy8_geom, "zork1.cpm");
--      cli.fd.open(3, cli.floppy_ctrl.floppy8_geom, "drv3.img");
   elsif selection = 2 then  --  68000
      cli.cpu := new BBS.Sim_CPU.m68000.m68000;
      cli.set_var(cli.cpu);
      cli.cpu.init;
      Ada.Text_IO.Put_Line("Simulator name: " & cli.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & cli.cpu.variant(cli.cpu.variant));
      cli.cpu.attach_io(cli.clock'Access, 16#400#,BBS.Sim_CPU.BUS_MEMORY);
      cli.add_device(cli.clock'Access);
      cli.clock.setOwner(cli.cpu);
      cli.clock.init(cli.clock'Access);
      cli.clock.setException(256+64);
      cli.add_device(cli.tel0'Access);
      cli.cpu.attach_io(cli.tel0'Access, 16#402#, BBS.Sim_CPU.BUS_MEMORY);
      cli.tel0.setOwner(cli.cpu);
      cli.tel0.init(cli.tel0'Access, 2171);
      cli.tel0.setException(2*256+65);
      cli.add_device(cli.tel1'Access);
      cli.cpu.attach_io(cli.tel1'Access, 16#404#, BBS.Sim_CPU.BUS_MEMORY);
      cli.tel1.setOwner(cli.cpu);
      cli.tel1.init(cli.tel1'Access, 2172);
      cli.tel1.setException(2*256+66);
      cli.add_device(cli.tel2'Access);
      cli.cpu.attach_io(cli.tel2'Access, 16#406#, BBS.Sim_CPU.BUS_MEMORY);
      cli.tel2.setOwner(cli.cpu);
      cli.tel2.init(cli.tel2'Access, 2173);
      cli.tel2.setException(2*256+67);
      cli.add_device(cli.mux'Access);
      cli.cpu.attach_io(cli.mux'Access, 16#408#, BBS.Sim_CPU.BUS_MEMORY);
      cli.mux.setOwner(cli.cpu);
      cli.mux.init(cli.mux'Access, 3141);
      cli.mux.setException(2*256+68);
--      cli.add_device(cli.print'Access);
--      cli.cpu.attach_io(cli.print'Access, 16#00FF_FF02#, BBS.Sim_CPU.BUS_MEMORY);
--      cli.devs.Append(cli.fd'Access);
--      cli.cpu.attach_io(cli.fd'Access, 16#00FF_FF04#, BBS.Sim_CPU.BUS_MEMORY);
   elsif selection = 3 then  --  6502
      cli.cpu := new BBS.Sim_CPU.msc6502.msc6502;
      cli.set_var(cli.cpu);
      cli.cpu.init;
      Ada.Text_IO.Put_Line("Simulator name: " & cli.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & cli.cpu.variant(cli.cpu.variant));
      cli.add_device(cli.tel0'Access);
      cli.cpu.attach_io(cli.tel0'Access, 16#F000#, BBS.Sim_CPU.BUS_MEMORY);
      cli.tel0.setOwner(cli.cpu);
      cli.tel0.init(cli.tel0'Access, 2171);
   else
      Ada.Text_IO.Put_Line("Bad selection.");
   end if;
   cli.cmds;
   cli.tel0.shutdown;
   cli.tel1.shutdown;
   cli.tel2.shutdown;
   cli.clock.shutdown;
   exception
      when error : others =>
         Ada.Text_IO.Put_Line("Last chance exception handler:");
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(error));
         cli.tel0.shutdown;
         cli.tel1.shutdown;
         cli.tel2.shutdown;
         cli.mux.shutdown;
         cli.clock.shutdown;
end SimCPUcli;
