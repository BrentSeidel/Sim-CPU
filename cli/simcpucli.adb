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
with GNAT.Traceback.Symbolic;
with GNAT.Debug_Utilities;

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
   elsif selection = 2 then  --  68000
      cli.cpu := new BBS.Sim_CPU.m68000.m68000;
   elsif selection = 3 then  --  6502
      cli.cpu := new BBS.Sim_CPU.msc6502.msc6502;
   else
      Ada.Text_IO.Put_Line("Bad selection.");
      return;
   end if;
   cli.set_var(cli.cpu);
   cli.cpu.init;
   Ada.Text_IO.Put_Line("Simulator name: " & cli.cpu.name);
   Ada.Text_IO.Put_Line("Simulator variant: " & cli.cpu.variant(cli.cpu.variant));
   cli.cmds;
   for i in cli.dev_table'Range loop
      for dev of cli.dev_table(i) loop
         dev.shutdown;
      end loop;
   end loop;
exception
   when error : others =>
      Ada.Text_IO.Put_Line("simcpucli: Last chance exception handler:");
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(error));
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(error));
      Ada.Text_IO.Put_Line(GNAT.Traceback.Symbolic.Symbolic_Traceback(error));
      for i in cli.dev_table'Range loop
         for dev of cli.dev_table(i) loop
            dev.shutdown;
         end loop;
      end loop;
end SimCPUcli;
