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
with test_util;

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
      test_util.i8080.variant(BBS.Sim_CPU.i8080.variants_i8080'Pos(BBS.Sim_CPU.i8080.var_8085));
      test_util.cpu := test_util.i8080'Access;
      test_util.cpu.init;
      Ada.Text_IO.Put_Line("Simulator name: " & test_util.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & test_util.cpu.variant(test_util.cpu.variant));
      test_util.cpu.attach_io(test_util.tel0'Access, 0, BBS.Sim_CPU.BUS_IO);
      test_util.tel0.init(test_util.tel0'Access, 2171);
      test_util.cpu.attach_io(test_util.print'Access, 2, BBS.Sim_CPU.BUS_IO);
      test_util.print.open("printer.txt");
      test_util.cpu.attach_io(test_util.fd'Access, 3, BBS.Sim_CPU.BUS_IO);
      test_util.fd.setOwner(test_util.cpu);
      test_util.fd.open(0, test_util.floppy_ctrl.floppy8_geom, "cpmboot.img");
      test_util.fd.open(1, test_util.floppy_ctrl.floppy8_geom, "cpma.cpm");
      test_util.fd.open(2, test_util.floppy_ctrl.floppy8_geom, "zork1.cpm");
      test_util.fd.open(3, test_util.floppy_ctrl.floppy8_geom, "drv3.img");
   else
      test_util.m68000.variant(BBS.Sim_CPU.m68000.variants_m68000'Pos(BBS.Sim_CPU.m68000.var_68000));
      test_util.cpu := test_util.m68000'Access;
      test_util.cpu.init;
      Ada.Text_IO.Put_Line("Simulator name: " & test_util.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & test_util.cpu.variant(test_util.cpu.variant));
      test_util.cpu.attach_io(test_util.clock'Access, 16#400#,BBS.Sim_CPU.BUS_MEMORY);
      test_util.clock.setOwner(test_util.cpu);
      test_util.clock.init(test_util.clock'Access);
      test_util.clock.setException(256+64);
      test_util.cpu.attach_io(test_util.tel0'Access, 16#402#, BBS.Sim_CPU.BUS_MEMORY);
      test_util.tel0.setOwner(test_util.cpu);
      test_util.tel0.init(test_util.tel0'Access, 2171);
      test_util.tel0.setException(2*256+65);
      test_util.cpu.attach_io(test_util.tel1'Access, 16#404#, BBS.Sim_CPU.BUS_MEMORY);
      test_util.tel1.setOwner(test_util.cpu);
      test_util.tel1.init(test_util.tel1'Access, 2172);
      test_util.tel1.setException(2*256+66);
      test_util.cpu.attach_io(test_util.tel2'Access, 16#406#, BBS.Sim_CPU.BUS_MEMORY);
      test_util.tel2.setOwner(test_util.cpu);
      test_util.tel2.init(test_util.tel2'Access, 2173);
      test_util.tel2.setException(2*256+67);
      test_util.cpu.attach_io(test_util.mux'Access, 16#408#, BBS.Sim_CPU.BUS_MEMORY);
      test_util.mux.setOwner(test_util.cpu);
      test_util.mux.init(test_util.mux'Access, 3141);
      test_util.mux.setException(2*256+68);
--      test_util.cpu.attach_io(test_util.print'Access, 16#00FF_FF02#, BBS.Sim_CPU.BUS_MEMORY);
--      test_util.cpu.attach_io(test_util.fd'Access, 16#00FF_FF04#, BBS.Sim_CPU.BUS_MEMORY);
   end if;
   test_util.cmds;
   test_util.tel0.shutdown;
   test_util.tel1.shutdown;
   test_util.tel2.shutdown;
   test_util.clock.shutdown;
   exception
      when error : others =>
         Ada.Text_IO.Put_Line("Last chance exception handler:");
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(error));
         test_util.tel0.shutdown;
         test_util.tel1.shutdown;
         test_util.tel2.shutdown;
         test_util.mux.shutdown;
         test_util.clock.shutdown;
end SimCPUcli;
