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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
with BBS;
use type BBS.uint32;
with BBS.Sim_CPU;
with BBS.Sim_CPU.cpu.i8080;
with BBS.Sim_CPU.cpu.m68000;
with BBS.Sim_CPU.cpu.msc6502;
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
