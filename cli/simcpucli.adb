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
with BBS.Sim_CPU.disk;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with cli_util;

procedure SimCPUcli is
   selection : Integer := 0;
begin
   Ada.Text_IO.Put_Line("CPU Simulator Test Program");
   loop
      Ada.Text_IO.Put_Line("Available simulators are:");
      Ada.Text_IO.Put_Line("1. Intel 8080/8085");
      Ada.Text_IO.Put_Line("2. Motorola 68000");
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
      exit when (selection > 0) and (selection < 3);
   end loop;
   if selection = 1 then
      cli_util.set_var(cli_util.i8080);
      cli_util.cpu := cli_util.i8080'Access;
      cli_util.cpu.init;
      Ada.Text_IO.Put_Line("Simulator name: " & cli_util.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & cli_util.cpu.variant(cli_util.cpu.variant));
      cli_util.cpu.attach_io(cli_util.tel0'Access, 0, BBS.Sim_CPU.BUS_IO);
      cli_util.tel0.setOwner(cli_util.cpu);
      cli_util.tel0.init(cli_util.tel0'Access, 2171);
--      cli_util.cpu.attach_io(cli_util.print'Access, 2, BBS.Sim_CPU.BUS_IO);
--      cli_util.print.open("printer.txt");
      cli_util.cpu.attach_io(cli_util.fd'Access, 3, BBS.Sim_CPU.BUS_IO);
      cli_util.fd.setOwner(cli_util.cpu);
      cli_util.fd.open(0, cli_util.floppy_ctrl.floppy8_geom, "cpmboot.img");
      cli_util.fd.open(1, cli_util.floppy_ctrl.floppy8_geom, "cpma.cpm");
      cli_util.fd.open(2, cli_util.floppy_ctrl.floppy8_geom, "zork1.cpm");
      cli_util.fd.open(3, cli_util.floppy_ctrl.floppy8_geom, "drv3.img");
   else  --  Selection is 2 here
      cli_util.set_var(cli_util.m68000);
      cli_util.cpu := cli_util.m68000'Access;
      cli_util.cpu.init;
      Ada.Text_IO.Put_Line("Simulator name: " & cli_util.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & cli_util.cpu.variant(cli_util.cpu.variant));
      cli_util.cpu.attach_io(cli_util.clock'Access, 16#400#,BBS.Sim_CPU.BUS_MEMORY);
      cli_util.clock.setOwner(cli_util.cpu);
      cli_util.clock.init(cli_util.clock'Access);
      cli_util.clock.setException(256+64);
      cli_util.cpu.attach_io(cli_util.tel0'Access, 16#402#, BBS.Sim_CPU.BUS_MEMORY);
      cli_util.tel0.setOwner(cli_util.cpu);
      cli_util.tel0.init(cli_util.tel0'Access, 2171);
      cli_util.tel0.setException(2*256+65);
      cli_util.cpu.attach_io(cli_util.tel1'Access, 16#404#, BBS.Sim_CPU.BUS_MEMORY);
      cli_util.tel1.setOwner(cli_util.cpu);
      cli_util.tel1.init(cli_util.tel1'Access, 2172);
      cli_util.tel1.setException(2*256+66);
      cli_util.cpu.attach_io(cli_util.tel2'Access, 16#406#, BBS.Sim_CPU.BUS_MEMORY);
      cli_util.tel2.setOwner(cli_util.cpu);
      cli_util.tel2.init(cli_util.tel2'Access, 2173);
      cli_util.tel2.setException(2*256+67);
      cli_util.cpu.attach_io(cli_util.mux'Access, 16#408#, BBS.Sim_CPU.BUS_MEMORY);
      cli_util.mux.setOwner(cli_util.cpu);
      cli_util.mux.init(cli_util.mux'Access, 3141);
      cli_util.mux.setException(2*256+68);
--      cli_util.cpu.attach_io(cli_util.print'Access, 16#00FF_FF02#, BBS.Sim_CPU.BUS_MEMORY);
--      cli_util.cpu.attach_io(cli_util.fd'Access, 16#00FF_FF04#, BBS.Sim_CPU.BUS_MEMORY);
   end if;
   cli_util.cmds;
   cli_util.tel0.shutdown;
   cli_util.tel1.shutdown;
   cli_util.tel2.shutdown;
   cli_util.clock.shutdown;
   exception
      when error : others =>
         Ada.Text_IO.Put_Line("Last chance exception handler:");
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(error));
         cli_util.tel0.shutdown;
         cli_util.tel1.shutdown;
         cli_util.tel2.shutdown;
         cli_util.mux.shutdown;
         cli_util.clock.shutdown;
end SimCPUcli;
