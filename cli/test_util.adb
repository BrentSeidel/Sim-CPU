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
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Strings.Maps.Constants;
with BBS;
use type BBS.uint8;
use type BBS.uint32;
with BBS.lisp;
with BBS.Sim_CPU.Lisp;
package body test_util is
   --
   --  Set variant
   --
   procedure set_var(c : in out BBS.Sim_CPU.simulator'Class) is
      max : Integer := c.variants - 1;
      selection : Integer := 0;
   begin
      loop
         Ada.Text_IO.Put_Line("Available variants are:");
         for i in 0 .. max loop
            Ada.Text_IO.Put_Line(Integer'Image(i) & "  " & c.variant(i));
         end loop;
         Ada.Text_IO.Put("Select variant: ");
         Ada.Integer_Text_IO.Get(selection, 0);
         --
         --  This is just to clear out any text on the rest of the line.
         --
         declare
            dummy : String := Ada.Text_IO.Get_line;
         begin
            null;  --  Nothing to do here.
         end;
         exit when (selection >= 0) and (selection <= max);
      end loop;
      c.variant(selection);
   end;
   --
   --  Dump registers
   --
   procedure dump_reg(c : BBS.Sim_CPU.simulator'Class) is
      regs : BBS.uint32 := cpu.registers;
   begin
      for i in 0 .. (regs - 1) loop
         Ada.Text_IO.Put("Reg " & BBS.uint32'Image(i) & " - ");
         Ada.Text_IO.put(CPU.reg_name(i) & " = ");
         Ada.Text_IO.Put_Line(CPU.read_reg(i));
      end loop;
   end;
   --
   procedure New_Line is
   begin
      Ada.Text_IO.New_Line;
   end;
   --
   --  Command loop.  The supported commands are:
   --  BREAK <addr>
   --    Set a breakpoint (currently only one can be active at a time)
   --  CONTINUE
   --    Continue execution
   --  DEP <addr> <value>
   --    Deposit value to a memory location
   --  DUMP <addr>
   --    Display a region of memory
   --  EXIT
   --    EXIT the program
   --  GO <addr>
   --    Start execution at a specified address
   --  LISP
   --    Enter Lisp interpreter
   --  LOAD <filename>
   --    Load data from a file into memory
   --  QUIT
   --    Synonym for EXIT
   --  REG
   --    Display register values
   --  RUN
   --    Execute instructions until halt or breakpoint
   --  STEP
   --    Execute one instruction
   --  TRACE <level>
   --    Print information for each instruction executed
   --  UNBREAK <addr>
   --    Remove a breakpoint
   --
   procedure cmds is
      cmd   : Ada.Strings.Unbounded.Unbounded_String;
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      index : Natural;
      addr  : BBS.uint32;
      value : BBS.uint32;
      level : BBS.uint32;
      exit_flag : Boolean := False;
      char  : Character;
      available : Boolean;
      interrupt : Character := Character'Val(5);  -- Control-E
   begin
      BBS.lisp.init(Ada.Text_IO.Put_Line'Access, Ada.Text_IO.Put'Access,
                New_Line'Access, Ada.Text_IO.Get_Line'Access);
      BBS.Sim_CPU.Lisp.init(cpu);
      loop
         Ada.Text_IO.Put("CMD>");
         Ada.Text_IO.Unbounded_IO.Get_Line(cmd);
         --
         --  Discard any leading spaces
         --
         index := Ada.Strings.Unbounded.Index_Non_Blank(cmd, 1);
         cmd := Ada.Strings.Unbounded.Unbounded_Slice(cmd, index, Ada.Strings.Unbounded.Length(cmd));
         --
         --  Split into command and the rest of the string
         --
         index := Ada.Strings.Unbounded.Index(cmd, " ");
         if index = 0 then
            first := cmd;
            rest := Ada.Strings.Unbounded.Null_Unbounded_String;
         else
            first := Ada.Strings.Unbounded.Unbounded_Slice(cmd, 1, index - 1);
            rest := Ada.Strings.Unbounded.Unbounded_Slice(cmd, index + 1,
                                                          Ada.Strings.Unbounded.Length(cmd));
         end if;
         --
         --  Command to uppercase
         --
         Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
         --
         -- Interpret the command
         --
         if first = ";" then
            Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(rest));
         elsif Ada.Strings.Unbounded.Length(first) = 0 then
            null;    --  Ignore blank lines
         elsif first = "S" or first = "STEP" then
            cpu.run;
            if cpu.halted then
               Ada.Text_IO.Put_Line("CPU is halted");
            else
              dump_reg(cpu.all);
            end if;
         elsif first = "R" or first = "RUN" then
            while not cpu.halted loop
               cpu.run;
               --
               --  On Windows using the git bash shell, this seems to
               --  wait for a character to be available rather than
               --  checking is a character is available.
               --
               if not gitbash then
                  Ada.Text_IO.Get_Immediate(char, available);
                  exit when available and then char = interrupt;
               end if;
            end loop;
            if not gitbash then
               if available and char = interrupt then
                  Ada.Text_IO.Put_Line("User requested break");
               else
                  Ada.Text_IO.Put_Line("CPU Halted");
               end if;
            end if;
            dump_reg(cpu.all);
         elsif first = "REG" then
            dump_reg(cpu.all);
         elsif first = "DEP" then
            Ada.Strings.Unbounded.Translate(rest, Ada.Strings.Maps.Constants.Upper_Case_Map);
            nextValue(addr, rest);
            nextValue(value, rest);
            CPU.set_mem(addr, value);
         elsif first = "TRACE" then
            Ada.Strings.Unbounded.Translate(rest, Ada.Strings.Maps.Constants.Upper_Case_Map);
            nextValue(level, rest);
            CPU.trace(Natural(level));
         elsif first = "D" or first = "DUMP" then
            Ada.Strings.Unbounded.Translate(rest, Ada.Strings.Maps.Constants.Upper_Case_Map);
            nextValue(addr, rest);
            dump_mem(addr);
         elsif first = "GO" then
            Ada.Strings.Unbounded.Translate(rest, Ada.Strings.Maps.Constants.Upper_Case_Map);
            nextValue(addr, rest);
            CPU.start(addr);
         elsif first = "LOAD" then
            Ada.Text_IO.Put_Line("Loading " & Ada.Strings.Unbounded.To_String(rest));
            CPU.load(Ada.Strings.Unbounded.To_String(rest));
         elsif first = "LISP" then
            BBS.lisp.repl;
         elsif first = "CONTINUE" then
            CPU.continue_proc;
         elsif first = "BREAK" then
            nextValue(addr, rest);
            CPU.setBreak(addr);
         elsif first = "UNBREAK" then
            nextValue(addr, rest);
            CPU.clearBreak(addr);
         elsif first = "QUIT" or first = "EXIT" then
            exit_flag := True;
         elsif first = "INT" or first = "INTERRUPT" then
            index := ada.Strings.Unbounded.Index(rest, " ");
            if index = 0 then
               first := rest;
               rest := Ada.Strings.Unbounded.Null_Unbounded_String;
            else
               first := Ada.Strings.Unbounded.Unbounded_Slice(rest, 1, index - 1);
               rest := Ada.Strings.Unbounded.Unbounded_Slice(rest, index + 1,
                                                            Ada.Strings.Unbounded.Length(rest));
            end if;
            if first = "ON" then
               cpu.interrupts(True);
            elsif first = "OFF" then
               cpu.interrupts(False);
            elsif first = "SEND" then
               nextValue(addr, rest);
               cpu.interrupt(addr);
            else
               Ada.Text_IO.Put_Line("Unrecognized option to interrupt command <" & Ada.Strings.Unbounded.To_String(first) &
                  ">");
            end if;
         elsif first = "RESET" then
            CPU.init;
         else
            Ada.Text_IO.Put_Line("Unrecognized command <" & Ada.Strings.Unbounded.To_String(first) & ">");
         end if;
         exit when exit_flag;
      end loop;
   end;
   --
   --  Pull the next hexidecimal value off of a string
   --
   procedure nextValue(v : out BBS.uint32;
                       s : in out Ada.Strings.Unbounded.Unbounded_String) is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      index : Natural;
   begin
      index := ada.Strings.Unbounded.Index(s, " ");
      if index = 0 then
         first := s;
         rest := Ada.Strings.Unbounded.Null_Unbounded_String;
      else
         first := Ada.Strings.Unbounded.Unbounded_Slice(s, 1, index - 1);
         rest := Ada.Strings.Unbounded.Unbounded_Slice(s, index + 1,
                                                       Ada.Strings.Unbounded.Length(s));
      end if;
      v := BBS.Sim_CPU.toHex(Ada.Strings.Unbounded.To_String(first));
      s := rest;
   end;
   --
   --  Memory
   --
   procedure dump_mem(start : BBS.Sim_CPU.addr_bus) is
      addr : BBS.Sim_CPU.addr_bus := start;
      temp : BBS.Sim_CPU.byte;
   begin
      Ada.Text_IO.Put("          ");
      for i in 0 .. 15 loop
         Ada.Text_IO.Put(" " & BBS.Sim_CPU.toHex(BBS.Sim_CPU.byte(i)));
      end loop;
      Ada.Text_IO.New_Line;
      for i in 0 .. 15 loop
         Ada.Text_IO.Put(BBS.Sim_CPU.toHex(addr) & " :");
         for j in 0 .. 15 loop
            Ada.Text_IO.Put(" " & BBS.Sim_CPU.toHex(BBS.Sim_CPU.byte(CPU.read_mem(addr + BBS.Sim_CPU.addr_bus(j)))));
         end loop;
         Ada.Text_IO.Put(" ");
         for j in 0 .. 15 loop
            temp := BBS.Sim_CPU.byte(CPU.read_mem(addr + BBS.Sim_CPU.addr_bus(j)));
            if (temp < 32) or (temp > 126) then
               Ada.Text_IO.Put(".");
            else
               Ada.Text_IO.Put(Character'Val(temp));
            end if;
         end loop;
         addr := addr + 16;
         Ada.Text_IO.New_Line;
      end loop;
   end;
   --
end test_util;
