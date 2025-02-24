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
with Ada.Integer_Text_IO;
with Ada.Tags;
use type Ada.Tags.Tag;
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
with cli.parse;
use type cli.parse.token_type;
package body cli is
   --
   --  Set variant
   --
   procedure set_var(c : in out BBS.Sim_CPU.sim_access) is
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
   --  Do initialization
   --
   procedure init is
   begin
      stdio_buff.init;
      BBS.lisp.init(Ada.Text_IO.Put_Line'Access, Ada.Text_IO.Put'Access,
                New_Line'Access, Ada.Text_IO.Get_Line'Access, stdio_buff'Access);
      BBS.Sim_CPU.Lisp.init(cpu);
   end;
   --
   --  Add a device to the device table
   --
   procedure add_device(dev : BBS.Sim_CPU.io_access) is
      kind : constant BBS.Sim_CPU.dev_type := dev.dev_class;
   begin
      dev_table(kind).Append(dev);
   end;
   --
   --  Command loop.  The supported commands are:
   --  BREAK <addr>
   --    Set a breakpoint (currently only one can be active at a time)
   --  CONTINUE
   --    Continue execution
   --  DEP <addr> <value>
   --    Deposit value to a memory location
   --  DISK <cmds>
   --    Commands for disk drives
   --  DUMP <addr>
   --    Display a region of memory
   --  EXIT
   --    EXIT the program
   --  GO <addr>
   --    Start execution at a specified address
   --  LISP
   --    Enter Lisp interpreter
   --  LIST
   --    List defined devices
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
   --  TAPE <cmds>
   --    Commands for tape drives
   --  TRACE <level>
   --    Print information for each instruction executed
   --  UNBREAK <addr>
   --    Remove a breakpoint
   --
   procedure cmds is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      token : cli.parse.token_type;
      addr  : BBS.uint32;
      value : BBS.uint32;
      level : BBS.uint32;
      char  : Character;
      exit_flag : Boolean := False;
      available : Boolean;
      interrupt : Character := Character'Val(5);  -- Control-E
      index : Natural;
   begin
      init;
      loop
         Ada.Text_IO.Put("CMD>");
         Ada.Text_IO.Unbounded_IO.Get_Line(rest);
         --
         --  Prepare command string
         --
         rest  := cli.parse.trim(rest);
         token := cli.parse.split(first, rest);
         Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
         --
         -- Interpret the command
         --
         if token = cli.parse.Comment then
            Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(rest));
         elsif Ada.Strings.Unbounded.Length(first) = 0 then
            null;    --  Ignore blank lines
         elsif first = "S" or first = "STEP" then
            CPU.continue_proc;
            cpu.run;
              dump_reg(cpu.all);
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
            token := cli.parse.nextHexValue(addr, rest);
            if token /= cli.Parse.Number then
               cli.parse.numErr(token, "DEP", "address");
            else
               token := cli.parse.nextHexValue(value, rest);
               if token /= cli.Parse.Number then
                  cli.parse.numErr(token, "DEP", "value");
               else
                  CPU.set_mem(addr, value);
               end if;
            end if;
         elsif first = "TRACE" then
            token := cli.parse.nextHexValue(level, rest);
            if token /= cli.Parse.Number then
               cli.parse.numErr(token, "TRACE", "value");
            else
               CPU.trace(Natural(level));
            end if;
         elsif first = "DISK" then
            disk_cmd(rest);
         elsif first = "D" or first = "DUMP" then
            token := cli.parse.nextHexValue(addr, rest);
            if token /= cli.Parse.Number then
               cli.parse.numErr(token, "DUMP", "address");
            else
               dump_mem(addr);
            end if;
         elsif first = "GO" then
            token := cli.parse.nextHexValue(addr, rest);
            if token /= cli.Parse.Number then
               cli.parse.numErr(token, "GO", "address");
            else
               CPU.start(addr);
            end if;
         elsif first = "LOAD" then
            Ada.Text_IO.Put_Line("Loading " & Ada.Strings.Unbounded.To_String(rest));
            CPU.load(Ada.Strings.Unbounded.To_String(rest));
         elsif first = "LISP" then
            if Ada.Strings.Unbounded.Length(rest) > 0 then
               Ada.Text_IO.Put_Line("LISP File is <" & Ada.Strings.Unbounded.To_String(rest) & ">");
               file_buff.init(Ada.Strings.Unbounded.To_String(rest));
               BBS.Lisp.set_parser(file_buff'Access);
               BBS.Lisp.repl(False);
               BBS.Lisp.set_parser(stdio_buff'Access);
            else
               BBS.lisp.repl;
            end if;
         elsif first = "C" or first = "CONTINUE" then
            CPU.continue_proc;
         elsif first = "BREAK" then
            token := cli.parse.nextHexValue(addr, rest);
            if token /= cli.Parse.Number then
               cli.parse.numErr(token, "BREAK", "address");
            else
               CPU.setBreak(addr);
            end if;
         elsif first = "UNBREAK" then
            token := cli.parse.nextHexValue(addr, rest);
            if token /= cli.Parse.Number then
               cli.parse.numErr(token, "UNBREAK", "address");
            else
               CPU.clearBreak(addr);
            end if;
         elsif first = "QUIT" or first = "EXIT" then
            exit_flag := True;
         elsif first = "INT" or first = "INTERRUPT" then
            token := cli.parse.split(first, rest);
            Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
            if first = "ON" then
               cpu.interrupts(True);
            elsif first = "OFF" then
               cpu.interrupts(False);
            elsif first = "SEND" then
               token := cli.parse.nextHexValue(addr, rest);
               if token /= cli.Parse.Number then
                  cli.parse.numErr(token, "INTERRUPT SEND", "interrupt");
               else
                  cpu.interrupt(addr);
               end if;
            else
               Ada.Text_IO.Put_Line("Unrecognized option to interrupt command <" & Ada.Strings.Unbounded.To_String(first) &
                  ">");
            end if;
         elsif first = "RESET" then
            CPU.init;
         elsif first = "LIST" then
            Ada.Text_IO.Put_Line("Device list");
            for dev_kind in BBS.Sim_CPU.dev_type'Range loop
               Ada.Text_IO.Put_Line("Devices in group " & BBS.Sim_CPU.dev_type'Image(dev_kind));
               index := dev_table(dev_kind).First_Index;
               for dev of dev_table(dev_kind) loop
                  Ada.Text_IO.Put_Line(BBS.Sim_CPU.dev_type'Image(dev.dev_class) & Natural'Image(index) & ": " &
                     dev.name & " - " & dev.description);
                  Ada.Text_IO.Put_Line("  Base: " & BBS.Sim_CPU.toHex(dev.getBase) &
                     ", Size: " & BBS.Sim_CPU.addr_bus'Image(dev.getSize));
                  if dev'Tag = floppy_ctrl.fd_ctrl'Tag then
                     Ada.Text_IO.Put_Line("  Device is a floppy disk controller");
                  else
                     Ada.Text_IO.Put_Line("  Device is not a floppy disk controller");
                  end if;
                  index := index + 1;
               end loop;
            end loop;
         elsif first = "TAPE" then
            tape_cmd(rest);
         else
            Ada.Text_IO.Put_Line("Unrecognized command <" & Ada.Strings.Unbounded.To_String(first) & ">");
         end if;
         exit when exit_flag;
      end loop;
   end;
   --
   --  Disk commands.  This is called to process the DISK command in the CLI.
   --  Subcommands are:
   --    CLOSE - Close the file attached to a drive
   --    GEOM - Set the geometry for the attached drive
   --    LIST - List the attached drives
   --    OPEN - Attach a file representing a disk image to a drive
   --    READONLY - Set a drive to read-only
   --    READWRITE - Set a drive to read-write
   --
   procedure disk_cmd(s : Ada.Strings.Unbounded.Unbounded_String) is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      token : cli.parse.token_type;
      drive : BBS.uint32;
      index : Natural;
   begin
      rest  := cli.parse.trim(s);
      token := cli.parse.split(first, rest);
      Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
      if first = "LIST" then
         index := dev_table(BBS.Sim_CPU.FD).First_Index;
         for dev of dev_table(BBS.Sim_CPU.FD) loop
            if dev'Tag = floppy_ctrl.fd_ctrl'Tag then
               floppy_info(dev, index);
            end if;
            index := index + 1;
         end loop;
      elsif first = "CLOSE" then
         token := cli.parse.nextDecValue(drive, rest);
         if token /= cli.Parse.Number then
            cli.parse.numErr(token, "DISK CLOSE", "drive number");
            return;
         end if;
         if drive > BBS.uint32(floppy_ctrl.drive_num'Last) then
            Ada.Text_IO.Put_Line("DISK CLOSE: Drive number out of range.");
            return;
         end if;
         Ada.Text_IO.Put_Line("DISK CLOSE: Drive " & BBS.uint32'Image(drive));
         fd.close(floppy_ctrl.drive_num(drive));
      elsif first = "GEOM" then
         token := cli.parse.nextDecValue(drive, rest);
         Ada.Strings.Unbounded.Translate(rest, Ada.Strings.Maps.Constants.Upper_Case_Map);
         if token /= cli.Parse.Number then
            cli.parse.numErr(token, "DISK GEOM", "drive number");
            return;
         end if;
         if drive > BBS.uint32(floppy_ctrl.drive_num'Last) then
            Ada.Text_IO.Put_Line("DISK GEOM: Drive number out of range.");
            return;
         end if;
         if rest = "IBM" then
            fd.setGeometry(floppy_ctrl.drive_num(drive), floppy_ctrl.floppy8_geom);
         elsif rest = "HD" then
            fd.setGeometry(floppy_ctrl.drive_num(drive), floppy_ctrl.hd_geom);
         else
            Ada.Text_IO.Put_Line("DISK GEOM: Unrecognized geometry <" & Ada.Strings.Unbounded.To_String(rest) & ">");
         end if;
      elsif first = "OPEN" then
         token := cli.parse.nextDecValue(drive, rest);
         if token /= cli.Parse.Number then
            cli.parse.numErr(token, "DISK OPEN", "drive number");
            return;
         end if;
         if drive > BBS.uint32(floppy_ctrl.drive_num'Last) then
            Ada.Text_IO.Put_Line("DISK OPEN: Drive number out of range.");
            return;
         end if;
         fd.open(floppy_ctrl.drive_num(drive), floppy_ctrl.floppy8_geom,
            Ada.Strings.Unbounded.To_String(rest));
         Ada.Text_IO.Put_Line("DISK OPEN: Drive " & BBS.uint32'Image(drive) &
            " attaching file <" & Ada.Strings.Unbounded.To_String(rest) & ">");
      elsif first = "READONLY" then
         token := cli.parse.nextDecValue(drive, rest);
         if token /= cli.Parse.Number then
            cli.parse.numErr(token, "DISK READONLY", "drive number");
            return;
         end if;
         if drive > BBS.uint32(floppy_ctrl.drive_num'Last) then
            Ada.Text_IO.Put_Line("DISK READONLY: Drive number out of range.");
            return;
         end if;
         fd.readonly(floppy_ctrl.drive_num(drive), True);
      elsif first = "READWRITE" then
         token := cli.parse.nextDecValue(drive, rest);
         if token /= cli.Parse.Number then
            cli.parse.numErr(token, "DISK READWRITE", "drive number");
            return;
         end if;
         if drive > BBS.uint32(floppy_ctrl.drive_num'Last) then
            Ada.Text_IO.Put_Line("DISK READWRITE: Drive number out of range.");
            return;
         end if;
         fd.readonly(floppy_ctrl.drive_num(drive), False);
      else
         Ada.Text_IO.Put_Line("Unrecognized subcommand to DISK <" & Ada.Strings.Unbounded.To_String(first) & ">");
      end if;
   end;
   --
   --  tape commands.  This is called to process the TAPE command in the CLI.
   --  Subcommands are:
   --    CLOSE - Close the file attached to a drive
   --    LIST - List the attached drives
   --    OPEN - Attach a file to a drive reader or writer
   --
   procedure tape_cmd(s : Ada.Strings.Unbounded.Unbounded_String) is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      token : cli.parse.token_type;
      tape  : BBS.Sim_CPU.serial.tape8_access;
      index : Natural;
   begin
      rest  := cli.parse.trim(s);
      token := cli.parse.split(first, rest);
      Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
      if first = "LIST" then
         index := dev_table(BBS.Sim_CPU.PT).First_Index;
         for dev of dev_table(BBS.Sim_CPU.PT) loop
            tape := BBS.Sim_CPU.serial.tape8_access(dev);
            Ada.Text_IO.Put_Line(BBS.Sim_CPU.dev_type'Image(dev.dev_class) &
                  ": " & dev.name & " - " & dev.description);
            Ada.Text_IO.Put_Line("  Base: " & BBS.Sim_CPU.toHex(dev.getBase) &
                  ", Size: " & BBS.Sim_CPU.addr_bus'Image(dev.getSize));
            index := index + 1;
            Ada.Text_IO.Put("  RDR: ");
            if tape.presentIn then
               Ada.Text_IO.Put_Line(tape.fnameIn);
            else
               Ada.Text_IO.Put_Line("No attached file.");
            end if;
            Ada.Text_IO.Put("  PUN: ");
            if tape.presentOut then
               Ada.Text_IO.Put_Line(tape.fnameOut);
            else
               Ada.Text_IO.Put_Line("No attached file.");
            end if;
         end loop;
      elsif first = "CLOSE" then
         token := cli.parse.split(first, rest);
         Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
         if first = "RDR" then
            paper.closeIn;
         elsif first = "PUN" then
            paper.closeOut;
         else
            Ada.Text_IO.Put_Line("TAPE CLOSE: Unknown device <" & Ada.Strings.Unbounded.To_String(first) & ">");
         end if;
      elsif first = "OPEN" then
         token := cli.parse.split(first, rest);
         Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
         if first = "RDR" then
            paper.openIn(Ada.Strings.Unbounded.To_String(rest));
         elsif first = "PUN" then
            paper.openOut(Ada.Strings.Unbounded.To_String(rest));
         else
            Ada.Text_IO.Put_Line("TAPE OPEN: Unknown device <" & Ada.Strings.Unbounded.To_String(first) & ">");
         end if;
      else
         Ada.Text_IO.Put_Line("Unrecognized subcommand to TAPE <" & Ada.Strings.Unbounded.To_String(first) & ">");
      end if;
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
            if (temp < 32) or (temp > 126) then  --  Check for printable character
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
   --  Print info for a floppy disk controller
   --
   procedure floppy_info(dev : in out BBS.Sim_CPU.io_access; ctrl : Natural) is
      fd   : floppy_ctrl.fd_access := floppy_ctrl.fd_access(dev);
      geom : floppy_ctrl.geometry;
   begin
      Ada.Text_IO.Put_Line(BBS.Sim_CPU.dev_type'Image(fd.dev_class) &
            Natural'Image(ctrl) & ": " & fd.name & " - " & fd.description);
      Ada.Text_IO.Put_Line("  Base: " & BBS.Sim_CPU.toHex(fd.getBase) &
            ", Size: " & BBS.Sim_CPU.addr_bus'Image(fd.getSize));
      for i in 0 .. fd.max_num loop
         Ada.Text_IO.Put("  Drive " & floppy_ctrl.drive_num'Image(i));
         if fd.present(i) then
            geom := fd.getGeometry(i);
            if  fd.readonly(i) then
               Ada.Text_IO.Put(" RO-is attached to ");
            else
               Ada.Text_IO.Put(" RW-is attached to ");
            end if;
            Ada.Text_IO.Put_Line(fd.fname(i));
            Ada.Text_IO.Put_Line("    (" & BBS.Sim_CPU.word'Image(geom.tracks) &
               "," & BBS.Sim_CPU.word'Image(geom.sectors) &
               "," & BBS.Sim_CPU.byte'Image(geom.heads) & ")");
         else
            if  fd.readonly(i) then
               Ada.Text_IO.Put_Line(" RO-has no attached image.");
            else
               Ada.Text_IO.Put_Line(" RW-has no attached image.");
            end if;
         end if;
      end loop;
   end;
   --
end cli;
