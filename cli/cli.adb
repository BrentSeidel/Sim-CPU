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
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Execution_Time;
use type Ada.Execution_Time.CPU_Time;
with Ada.Real_Time;
use type Ada.Real_Time.Time;
with Ada.Integer_Text_IO;
with Ada.Tags;
use type Ada.Tags.Tag;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Strings.Maps.Constants;
with BBS;
use type BBS.uint8;
use type BBS.uint32;
use type BBS.uint64;
with BBS.lisp;
with cli.Lisp;
with cli.parse;
use type cli.parse.token_type;
with GNAT.Sockets;
package body cli is
   --
   --  Dump registers
   --
   procedure dump_reg(c : BBS.Sim_CPU.CPU.simulator'Class) is
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
      cli.Lisp.init;
      if Ada.Command_Line.Argument_Count = 1 then
         process_args;
      end if;
   end;
   --
   --  Process argument
   --
   procedure process_args is
      a : String := Ada.Command_Line.Argument(1);
   begin
      Ada.Text_IO.Put_Line("Passed command line argument <" & a & ">");
      file_buff.init(a);
      if file_buff.Valid then
         BBS.Lisp.set_parser(file_buff'Access);
         BBS.Lisp.repl(False);
         BBS.Lisp.set_parser(stdio_buff'Access);
      end if;
   end;
   --
   --  Add a device to the device table
   --
   procedure add_device(dev : BBS.Sim_CPU.io.io_access) is
      kind : constant BBS.Sim_CPU.io.dev_type := dev.dev_class;
   begin
      dev_table(kind).Append(dev);
   end;
   --
   --  Command loop.  The supported commands are:
   --  ATTACH <device> <addr> <bus> [<dev-specific>]
   --    Attaches a specific I/O device to the bus address
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
         elsif cli.parse.match(first, "ST EP") then
            if cpu_selected then
               CPU.continue_proc;
               cpu.run;
               dump_reg(cpu.all);
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "R UN") then
            if cpu_selected then
               --
               --  On Windows using the git bash shell, Get_Immediate seems to
               --  wait for a character to be available rather than checking if
               --  a character is available.  So the interrupt character can't
               --  be used on gitbash.
               --
               if gitbash then
                  while not cpu.halted loop
                     cpu.run;
                  end loop;
               else
                  while not cpu.halted loop
                     for i in 1 .. pause_count loop
                        cpu.run;
                     end loop;
                     Ada.Text_IO.Get_Immediate(char, available);
                     exit when available and then char = interrupt;
                  end loop;
               end if;
               if not gitbash then
                  if available and char = interrupt then
                     Ada.Text_IO.Put_Line("User requested break");
                  else
                     Ada.Text_IO.Put_Line("CPU Halted");
                  end if;
               end if;
               dump_reg(cpu.all);
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "REG ISTER") then
            if cpu_selected then
               dump_reg(cpu.all);
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "DEP OSITE") then
              if cpu_selected then
               token := cli.parse.nextHexValue(addr, rest);
               if token /= cli.Parse.Number then
                  cli.parse.numErr(token, "DEPOSITE", "address");
               else
                  token := cli.parse.nextHexValue(value, rest);
                  if token /= cli.Parse.Number then
                     cli.parse.numErr(token, "DEPOSITE", "value");
                  else
                     CPU.set_mem(addr, value);
                  end if;
               end if;
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "TRA CE") then
            if cpu_selected then
               token := cli.parse.nextHexValue(level, rest);
               if token /= cli.Parse.Number then
                  cli.parse.numErr(token, "TRACE", "value");
               else
                  CPU.trace(Natural(level));
               end if;
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "DISK") then
            disk_cmd(rest);
         elsif cli.parse.match(first, "D UMP") then
            if cpu_selected then
               token := cli.parse.nextHexValue(addr, rest);
               if token /= cli.Parse.Number then
                  cli.parse.numErr(token, "DUMP", "address");
               else
                  dump_mem(addr);
               end if;
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "GO") then
            if cpu_selected then
               token := cli.parse.nextHexValue(addr, rest);
               if token /= cli.Parse.Number then
                  cli.parse.numErr(token, "GO", "address");
               else
                  CPU.start(addr);
               end if;
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "LOAD") then
            if cpu_selected then
               Ada.Text_IO.Put_Line("Loading " & Ada.Strings.Unbounded.To_String(rest));
               CPU.load(Ada.Strings.Unbounded.To_String(rest));
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "LISP") then
            if Ada.Strings.Unbounded.Length(rest) > 0 then
               Ada.Text_IO.Put_Line("LISP File is <" & Ada.Strings.Unbounded.To_String(rest) & ">");
               file_buff.init(Ada.Strings.Unbounded.To_String(rest));
               BBS.Lisp.set_parser(file_buff'Access);
               BBS.Lisp.repl(False);
               BBS.Lisp.set_parser(stdio_buff'Access);
            else
               BBS.lisp.repl;
            end if;
         elsif cli.parse.match(first, "C ONTINUE") then
            if cpu_selected then
               CPU.continue_proc;
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "BREA K") then
            if cpu_selected then
               token := cli.parse.nextHexValue(addr, rest);
               if token /= cli.Parse.Number then
                  cli.parse.numErr(token, "BREAK", "address");
               else
                  CPU.setBreak(addr);
               end if;
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "UNBR EAK") then
            if cpu_selected then
               token := cli.parse.nextHexValue(addr, rest);
               if token /= cli.Parse.Number then
                  cli.parse.numErr(token, "UNBREAK", "address");
               else
                  CPU.clearBreak(addr);
               end if;
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "QU IT") or cli.parse.match(first, "EX IT") then
            exit_flag := True;
         elsif cli.parse.match(first, "INTE RRUPT") then
            if cpu_selected then
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
                  Ada.Text_IO.Put_Line("Unrecognized option to INTERRUPT command <" & Ada.Strings.Unbounded.To_String(first) &
                                         ">");
               end if;
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "RES ET") then
            if cpu_selected then
               CPU.init;
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "LIST") then
            list(rest);
         elsif cli.parse.match(first, "TAPE") then
            if cpu_selected then
               tape_cmd(rest);
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "PR INT") then
            if cpu_selected then
               print_cmd(rest);
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "ATT ACH") then
            if cpu_selected then
               attach(rest);
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "CPU") then
            set_cpu(rest);
         elsif cli.parse.match(first, "BENC HMARK") then
            if cpu_selected then
               declare
                  count  : BBS.uint64 := 0;
                  start  : Ada.Real_Time.Time;
                  finish : Ada.Real_Time.Time;
                  elapse : Float;
                  cstart  : Ada.Execution_Time.CPU_Time;
                  cfinish : Ada.Execution_Time.CPU_Time;
                  celapse : Float;
               begin
                  --
                  --  On Windows using the git bash shell, Get_Immediate seems to
                  --  wait for a character to be available rather than checking if
                  --  a character is available.  So the interrupt character can't
                  --  be used on gitbash.
                  --
                  if gitbash then
                     start :=  Ada.Real_Time.Clock;
                     cstart := Ada.Execution_Time.Clock;
                     while not cpu.halted loop
                        cpu.run;
                        count := count + 1;
                     end loop;
                     finish := Ada.Real_Time.Clock;
                     cfinish := Ada.Execution_Time.Clock;
                  else
                     start :=  Ada.Real_Time.Clock;
                     cstart := Ada.Execution_Time.Clock;
                     while not cpu.halted loop
                        for i in 1 .. pause_count loop
                           cpu.run;
                           count := count + 1;
                        end loop;
                        Ada.Text_IO.Get_Immediate(char, available);
                        exit when available and then char = interrupt;
                     end loop;
                     finish := Ada.Real_Time.Clock;
                     cfinish := Ada.Execution_Time.Clock;
                  end if;
                  elapse := Float(Ada.Real_Time.To_Duration(finish - start));
                  celapse := Float(Ada.Real_Time.To_Duration(cfinish - cstart));
                  if not gitbash then
                     if available and char = interrupt then
                        Ada.Text_IO.Put_Line("User requested break");
                     else
                        Ada.Text_IO.Put_Line("CPU Halted");
                     end if;
                  end if;
                  Ada.Text_IO.Put(BBS.uint64'Image(count) & " instructions executed in ");
                  float_io.put(elapse, 1, 2, 0);
                  Ada.Text_IO.Put_line(" seconds, or about " & Integer'Image(Integer(Float(count)/elapse)) &
                                         " instructions per second.");
                  Ada.Text_IO.Put(BBS.uint64'Image(count) & " instructions executed in ");
                  float_io.put(celapse, 1, 2, 0);
                  Ada.Text_IO.Put_line(" CPU seconds, or about " & Integer'Image(Integer(Float(count)/celapse)) &
                                         " instructions per CPU second.");
                  dump_reg(cpu.all);
               end;
            else
               Ada.Text_IO.Put_Line("CPU must be selected.");
            end if;
         elsif cli.parse.match(first, "SET") then
            set(rest);
         else
            Ada.Text_IO.Put_Line("Unrecognized command <" & Ada.Strings.Unbounded.To_String(first) & ">");
         end if;
         exit when exit_flag;
      end loop;
   end;
   --
   --  List devices
   --
   procedure list(s : Ada.Strings.Unbounded.Unbounded_String) is
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      name  : Ada.Strings.Unbounded.Unbounded_String;
      dev   : BBS.Sim_CPU.io.io_access;
      token : cli.parse.token_type;
      index : Natural;
      pass  : Boolean;
      prn    : BBS.Sim_CPU.io.serial.print8_access;
      ptp    : BBS.Sim_CPU.io.serial.tape8_access;
   begin
      rest  := cli.parse.trim(s);
      token := cli.parse.split(name, rest);
      --
      --  If no device specified, list attached devices
      if token = cli.parse.Missing then
         Ada.Text_IO.Put_Line("Device list");
         for dev_kind in BBS.Sim_CPU.io.dev_type'Range loop
            Ada.Text_IO.Put_Line("Devices in group " & BBS.Sim_CPU.io.dev_type'Image(dev_kind));
            index := dev_table(dev_kind).First_Index;
            for dev of dev_table(dev_kind) loop
               Ada.Text_IO.Put_Line(make_dev_name(dev, index) & " - " & dev.description);
               Ada.Text_IO.Put_Line("  Base: " & BBS.Sim_CPU.toHex(dev.getBase) &
                                         ", Size: " & BBS.Sim_CPU.addr_bus'Image(dev.getSize));
               if dev'Tag = floppy_ctrl.fd_ctrl'Tag then
                  Ada.Text_IO.Put_Line("  Device is a disk controller");
               else
                  Ada.Text_IO.Put_Line("  Device is not a disk controller");
               end if;
               index := index + 1;
            end loop;
         end loop;
         return;
      end if;
      dev := find_dev_by_name(name, pass);
      if not pass then
         Ada.Text_IO.Put_Line("LIST unable to find device.");
         return;
      end if;
      parse_dev_name(name, rest, index);
      if dev'Tag = floppy_ctrl.fd_ctrl'Tag then          --  Disk
         floppy_info(dev, index);
      elsif dev'Tag = BBS.Sim_CPU.io.serial.tape8'Tag then  --  Tape
         ptp := BBS.Sim_CPU.io.serial.tape8_access(dev);
         Ada.Text_IO.Put_Line(BBS.Sim_CPU.io.dev_type'Image(dev.dev_class) &
                                ": " & dev.name & " - " & dev.description);
         Ada.Text_IO.Put_Line("  Base: " & BBS.Sim_CPU.toHex(dev.getBase) &
                                ", Size: " & BBS.Sim_CPU.addr_bus'Image(dev.getSize));
         Ada.Text_IO.Put("  RDR: ");
         if ptp.presentIn then
            Ada.Text_IO.Put_Line(ptp.fnameIn);
         else
            Ada.Text_IO.Put_Line("No attached file.");
         end if;
         Ada.Text_IO.Put("  PUN: ");
         if ptp.presentOut then
            Ada.Text_IO.Put_Line(ptp.fnameOut);
         else
            Ada.Text_IO.Put_Line("No attached file.");
         end if;
      elsif dev'Tag = BBS.Sim_CPU.io.serial.print8'Tag then  --  Printer
         prn := BBS.Sim_CPU.io.serial.print8_access(dev);
         Ada.Text_IO.Put_Line(BBS.Sim_CPU.io.dev_type'Image(dev.dev_class) &
                                ": " & dev.name & " - " & dev.description);
         Ada.Text_IO.Put_Line("  Base: " & BBS.Sim_CPU.toHex(dev.getBase) &
                                ", Size: " & BBS.Sim_CPU.addr_bus'Image(dev.getSize));
         Ada.Text_IO.Put("  Attached file: ");
         if prn.present then
            Ada.Text_IO.Put_Line(prn.fname);
         else
            Ada.Text_IO.Put_Line("No attached file.");
         end if;
      elsif dev'Tag = BBS.Sim_CPU.io.serial.mux.mux_tty'Tag then  --  Terminal multiplexter
         Ada.Text_IO.Put_Line("Terminal multiplexer");
         Ada.Text_IO.Put_Line(BBS.Sim_CPU.io.dev_type'Image(dev.dev_class) &
                                ": " & dev.name & " - " & dev.description);
         Ada.Text_IO.Put_Line("  Base: " & BBS.Sim_CPU.toHex(dev.getBase) &
                                ", Size: " & BBS.Sim_CPU.addr_bus'Image(dev.getSize));
      elsif dev'Tag = BBS.Sim_CPU.io.serial.telnet.tel_tty'Tag then  --  Single terminal
         Ada.Text_IO.Put_Line("Single terminal interface");
         Ada.Text_IO.Put_Line(BBS.Sim_CPU.io.dev_type'Image(dev.dev_class) &
                                ": " & dev.name & " - " & dev.description);
         Ada.Text_IO.Put_Line("  Base: " & BBS.Sim_CPU.toHex(dev.getBase) &
                                ", Size: " & BBS.Sim_CPU.addr_bus'Image(dev.getSize));
      elsif dev'Tag = BBS.Sim_CPU.io.clock.clock_device'Tag then  --  Clock device
         Ada.Text_IO.Put_Line("Periodic interrupt generator (clock)");
         Ada.Text_IO.Put_Line(BBS.Sim_CPU.io.dev_type'Image(dev.dev_class) &
                                ": " & dev.name & " - " & dev.description);
         Ada.Text_IO.Put_Line("  Base: " & BBS.Sim_CPU.toHex(dev.getBase) &
                                ", Size: " & BBS.Sim_CPU.addr_bus'Image(dev.getSize));
      end if;
   end;
   --
   --  Disk commands.  This is called to process the DISK command in the CLI.
   --  Subcommands are:
   --    CLOSE - Close the file attached to a drive
   --    GEOM - Set the geometry for the attached drive
   --    OPEN - Attach a file representing a disk image to a drive
   --    READONLY - Set a drive to read-only
   --    READWRITE - Set a drive to read-write
   --
   procedure disk_cmd(s : Ada.Strings.Unbounded.Unbounded_String) is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      name  : Ada.Strings.Unbounded.Unbounded_String;
      dev   : BBS.Sim_CPU.io.io_access;
      fd    : floppy_ctrl.fd_access;
      pass  : Boolean;
      token : cli.parse.token_type;
      drive : BBS.uint32;
   begin
      rest  := cli.parse.trim(s);
      token := cli.parse.split(name, rest);
      if token = cli.parse.Missing then
         Ada.Text_IO.Put_Line("DISK missing device name.");
         return;
      end if;
      dev := find_dev_by_name(name, pass);
      if not pass then
         Ada.Text_IO.Put_Line("DISK unable to find device.");
         return;
      end if;
      if dev'Tag /= floppy_ctrl.fd_ctrl'Tag then
         Ada.Text_IO.Put_Line("DISK device is not a disk controller.");
         return;
      end if;
      fd := floppy_ctrl.fd_access(dev);
      token := cli.parse.split(first, rest);
      if token = cli.parse.Missing then
         Ada.Text_IO.Put_Line("DISK missing subcommand.");
         return;
      end if;
      Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
      if first = "CLOSE" then
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
         elsif Ada.Strings.Unbounded.Unbounded_Slice(rest, 1, 1) = "(" then
            rest := Ada.Strings.Unbounded.Unbounded_Slice(rest, 2,
                                                          Ada.Strings.Unbounded.Length(rest));
            declare
               track : BBS.uint32;
               sect  : BBS.uint32;
               head  : BBS.uint32;
               geom  : floppy_ctrl.geometry;
            begin
               token := cli.parse.nextDecValue(track, rest);
               if token /= cli.Parse.Number then
                  cli.parse.numErr(token, "DISK GEOM", "number of tracks");
                  return;
               end if;
               if (track > 16#FFFF#) or (track = 0) then
                  Ada.Text_IO.Put_Line("DISK GEOM: Number of tracks out of range.");
                  return;
               end if;
               geom.tracks := BBS.Sim_CPU.word(track and 16#FFFF#);
               token := cli.parse.nextDecValue(sect, rest);
               if token /= cli.Parse.Number then
                  cli.parse.numErr(token, "DISK GEOM", "number of sectors");
                  return;
               end if;
               if (sect > 16#FFFF#) or (sect = 0) then
                  Ada.Text_IO.Put_Line("DISK GEOM: Number of sectors out of range.");
                  return;
               end if;
               geom.sectors := BBS.Sim_CPU.word(sect and 16#FFFF#);
               token := cli.parse.nextDecValue(head, rest);
               if token /= cli.Parse.Number then
                  cli.parse.numErr(token, "DISK GEOM", "number of heads");
                  return;
               end if;
               --
               --  Currently the number of heads is ignored, so just set to zero
               --  and don't bother range checking.
               --
               geom.heads := 0;
               fd.setGeometry(floppy_ctrl.drive_num(drive), geom);
            end;
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
   --  Tape commands.  This is called to process the TAPE command in the CLI.
   --  Subcommands are:
   --    CLOSE - Close the file attached to a drive
   --    OPEN - Attach a file to a drive reader or writer
   --
   procedure tape_cmd(s : Ada.Strings.Unbounded.Unbounded_String) is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      name  : Ada.Strings.Unbounded.Unbounded_String;
      token : cli.parse.token_type;
      pass  : Boolean;
      dev   : BBS.Sim_CPU.io.io_access;
      tape  : BBS.Sim_CPU.io.serial.tape8_access;
   begin
      rest  := cli.parse.trim(s);
      token := cli.parse.split(name, rest);
      if token = cli.parse.Missing then
         Ada.Text_IO.Put_Line("TAPE missing device name.");
         return;
      end if;
      dev := find_dev_by_name(name, pass);
      if not pass then
         Ada.Text_IO.Put_Line("TAPE unable to find device.");
         return;
      end if;
      if dev'Tag /= BBS.Sim_CPU.io.serial.tape8'Tag then
         Ada.Text_IO.Put_Line("TAPE device is not a tape controller.");
         return;
      end if;
      tape := BBS.Sim_CPU.io.serial.tape8_access(dev);
      token := cli.parse.split(first, rest);
      Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
      if first = "CLOSE" then
         token := cli.parse.split(first, rest);
         Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
         if first = "RDR" then
            tape.closeIn;
         elsif first = "PUN" then
            tape.closeOut;
         else
            Ada.Text_IO.Put_Line("TAPE CLOSE: Unknown device <" & Ada.Strings.Unbounded.To_String(first) & ">");
         end if;
      elsif first = "OPEN" then
         token := cli.parse.split(first, rest);
         Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
         if first = "RDR" then
            tape.openIn(Ada.Strings.Unbounded.To_String(rest));
         elsif first = "PUN" then
            tape.openOut(Ada.Strings.Unbounded.To_String(rest));
         else
            Ada.Text_IO.Put_Line("TAPE OPEN: Unknown device <" & Ada.Strings.Unbounded.To_String(first) & ">");
         end if;
      else
         Ada.Text_IO.Put_Line("Unrecognized subcommand to TAPE <" & Ada.Strings.Unbounded.To_String(first) & ">");
      end if;
   end;
   --
   --  Printer commands.  This is called to process the PRINT command in the CLI.
   --  Subcommands are:
   --    CLOSE - Close the file attached to the printer
   --    OPEN - Attach a file to a printer
   --
   procedure print_cmd(s : Ada.Strings.Unbounded.Unbounded_String) is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      name  : Ada.Strings.Unbounded.Unbounded_String;
      token : cli.parse.token_type;
      pass  : Boolean;
      dev   : BBS.Sim_CPU.io.io_access;
      prn   : BBS.Sim_CPU.io.serial.print8_access;
   begin
      rest  := cli.parse.trim(s);
      token := cli.parse.split(name, rest);
      if token = cli.parse.Missing then
         Ada.Text_IO.Put_Line("PRINT missing device name.");
         return;
      end if;
      dev := find_dev_by_name(name, pass);
      if not pass then
         Ada.Text_IO.Put_Line("PRINT unable to find device.");
         return;
      end if;
      if dev'Tag /= BBS.Sim_CPU.io.serial.print8'Tag then
         Ada.Text_IO.Put_Line("PRINT device is not a printer controller.");
         return;
      end if;
      prn := BBS.Sim_CPU.io.serial.print8_access(dev);
      token := cli.parse.split(first, rest);
      Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
      if first = "CLOSE" then
         prn.close;
      elsif first = "OPEN" then
         prn.open(Ada.Strings.Unbounded.To_String(rest));
      else
         Ada.Text_IO.Put_Line("Unrecognized subcommand to PRINT <" & Ada.Strings.Unbounded.To_String(first) & ">");
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
   procedure floppy_info(dev : in out BBS.Sim_CPU.io.io_access; ctrl : Natural) is
      fd   : floppy_ctrl.fd_access := floppy_ctrl.fd_access(dev);
      geom : floppy_ctrl.geometry;
   begin
      Ada.Text_IO.Put_Line(make_dev_name(BBS.Sim_CPU.io.io_access(fd), ctrl) & " - " & fd.description);
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
   --  Attach an I/O device to a simulation
   --  ATTACH dev addr type user
   --    dev  - Device name
   --    addr - Address of device
   --    type - Type for address (MEM or IO)
   --    user - Additional device specific parameters
   --
   --  Supported devices
   --    CLK, FD, MUX, PRN, PTP, TEL
   --
   procedure attach(s : Ada.Strings.Unbounded.Unbounded_String) is
      token  : cli.parse.token_type;
      dev    : Ada.Strings.Unbounded.Unbounded_String;
      rest   : Ada.Strings.Unbounded.Unbounded_String := s;
      port   : BBS.uint32;
      kind   : Ada.Strings.Unbounded.Unbounded_String;
      which_bus : BBS.Sim_CPU.bus_type;
      tel    : BBS.Sim_CPU.io.serial.telnet.telnet_access;
      fd     : floppy_ctrl.fd_access;
      ptp    : BBS.Sim_CPU.io.serial.tape8_access;
      mux    : BBS.Sim_CPU.io.serial.mux.mux_access;
      clk    : BBS.Sim_CPU.io.clock.clock_access;
      prn    : BBS.Sim_CPU.io.serial.print8_access;
      usern  : BBS.uint32;
      except : BBS.uint32;
   begin
      token := cli.parse.split(dev, rest);
      if token = cli.parse.Missing then
         Ada.Text_IO.Put_Line("ATTACH missing device name");
         return;
      end if;
      Ada.Strings.Unbounded.Translate(dev, Ada.Strings.Maps.Constants.Upper_Case_Map);
      token := cli.parse.nextDecValue(port, rest);
      if token = cli.parse.Missing then
         Ada.Text_IO.Put_Line("ATTACH missing device address");
         return;
      end if;
      token := cli.parse.split(kind, rest);
      if token = cli.parse.Missing then
         Ada.Text_IO.Put_Line("ATTACH missing device address type");
         return;
      end if;
      Ada.Strings.Unbounded.Translate(kind, Ada.Strings.Maps.Constants.Upper_Case_Map);
      if kind = "MEM" then
         which_bus := BBS.Sim_CPU.BUS_MEMORY;
      elsif kind = "IO" then
         which_bus := BBS.Sim_CPU.BUS_IO;
      else
         Ada.Text_IO.Put_Line("ATTACH unrecognized bus type");
         return;
      end if;
      if dev = "TEL" then
         token := cli.parse.nextDecValue(usern, rest);
         if token = cli.parse.Missing then
            Ada.Text_IO.Put_Line("ATTACH TEL missing telnet port number.");
            return;
         end if;
         tel := new BBS.Sim_CPU.io.serial.telnet.tel_tty;
         add_device(BBS.Sim_CPU.io.io_access(tel));
         bus.attach_io(BBS.Sim_CPU.io.io_access(tel), port, which_bus);
         tel.setOwner(cpu);
         tel.init(tel, GNAT.Sockets.Port_Type(usern));
         token := cli.parse.nextDecValue(except, rest);
         if token /= cli.parse.Missing then
            tel.setException(except);
         end if;
      elsif dev = "MUX" then
         token := cli.parse.nextDecValue(usern, rest);
         if token = cli.parse.Missing then
            Ada.Text_IO.Put_Line("ATTACH MUX missing telnet port number.");
            return;
         end if;
         mux := new BBS.Sim_CPU.io.serial.mux.mux_tty;
         add_device(BBS.Sim_CPU.io.io_access(mux));
         bus.attach_io(BBS.Sim_CPU.io.io_access(mux), port, which_bus);
         mux.setOwner(cpu);
         mux.init(mux, GNAT.Sockets.Port_Type(usern));
         token := cli.parse.nextDecValue(except, rest);
         if token /= cli.parse.Missing then
            mux.setException(except);
         end if;
      elsif dev = "FD" then
         token := cli.parse.nextDecValue(usern, rest);
         if token = cli.parse.Missing then
            Ada.Text_IO.Put_Line("ATTACH FD missing number of drives.");
            return;
         end if;
         if usern > 15 then
            Ada.Text_IO.Put_Line("ATTACH FD number of drives greater than 15.");
            return;
         end if;
         fd := new floppy_ctrl.fd_ctrl(max_num => Integer(usern));
         add_device(BBS.Sim_CPU.io.io_access(fd));
         bus.attach_io(BBS.Sim_CPU.io.io_access(fd), port, which_bus);
         fd.setOwner(cpu);
         token := cli.parse.nextDecValue(except, rest);
         if token /= cli.parse.Missing then
            fd.setException(except);
         end if;
      elsif dev = "PTP" then
         ptp := new BBS.Sim_CPU.io.serial.tape8;
         add_device(BBS.Sim_CPU.io.io_access(ptp));
         bus.attach_io(BBS.Sim_CPU.io.io_access(ptp), port, which_bus);
      elsif dev = "CLK" then
         clk := new BBS.Sim_CPU.io.clock.clock_device;
         add_device(BBS.Sim_CPU.io.io_access(clk));
         bus.attach_io(BBS.Sim_CPU.io.io_access(clk), port, which_bus);
         token := cli.parse.nextDecValue(except, rest);
         if token /= cli.parse.Missing then
            clk.setException(except);
         end if;
      elsif dev = "PRN" then
         prn := new BBS.Sim_CPU.io.serial.print8;
         add_device(BBS.Sim_CPU.io.io_access(prn));
         bus.attach_io(BBS.Sim_CPU.io.io_access(prn), port, which_bus);
      else
         Ada.Text_IO.Put_Line("ATTACH unrecognized device");
      end if;
   end;
   --
   --  Make a device name
   --
   function make_dev_name(dev : BBS.Sim_CPU.io.io_access; i : Natural) return String is
      name : Ada.Strings.Unbounded.Unbounded_String;
      num  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      num  := cli.parse.trim(Ada.Strings.Unbounded.To_Unbounded_String(Natural'Image(i + 1)));
      name := Ada.Strings.Unbounded.To_Unbounded_String(dev.name);
      return Ada.Strings.Unbounded.To_String(name & num);
   end;
   --
   --  Parse device name
   --
   procedure parse_dev_name(name : Ada.Strings.Unbounded.Unbounded_String;
                            dev : out Ada.Strings.Unbounded.Unbounded_String;
                            unit : out Natural) is
      dev_name : Ada.Strings.Unbounded.Unbounded_String;
      unit_num : Ada.Strings.Unbounded.Unbounded_String;
      index    : Natural := 1;
      len      : Natural := Ada.Strings.Unbounded.Length(name);
      c        : Character;
   begin
      if len = 0 then
         dev := Ada.Strings.Unbounded.Null_Unbounded_String;
         unit := 0;
      end if;
      c := Ada.Strings.Unbounded.element(name, index);
      --  Get unit designation
      while (index < len) and not cli.parse.isDigit(c) loop
         dev_name := dev_name & c;
         index := index + 1;
         c := Ada.Strings.Unbounded.element(name, index);
      end loop;
      if not cli.parse.isDigit(c) then
         dev_name := dev_name & c;
      end if;
      Ada.Strings.Unbounded.Translate(dev_name, Ada.Strings.Maps.Constants.Upper_Case_Map);
      dev := dev_name;
      --  Get unit number
      if index <= len then
         begin
            unit_num := Ada.Strings.Unbounded.Unbounded_Slice(name, index, len);
            unit := Natural'Value(Ada.Strings.Unbounded.To_String(unit_num));
         exception
            when CONSTRAINT_ERROR =>
               Ada.Text_IO.Put_Line("Unable to parse <" & Ada.Strings.Unbounded.To_String(unit_num) &
                                   "> into a number.");
               unit := 0;
            when e : others =>
               Ada.Text_IO.Put_Line("Some other exception occured parsing device name: " & Ada.Exceptions.Exception_Message(e));
               raise;
         end;
      else
         unit := 0;
      end if;
   end;
   --
   --  Find device by name.  If success is False, the returned value is invalid.
   --
   function find_dev_by_name(name : Ada.Strings.Unbounded.Unbounded_String; success : out Boolean) return BBS.Sim_CPU.io.io_access is
      dev_name : Ada.Strings.Unbounded.Unbounded_String;
      unit_num : Ada.Strings.Unbounded.Unbounded_String;
      index    : Natural := 1;
      len      : Natural := Ada.Strings.Unbounded.Length(name);
      c        : Character;
      dev      : BBS.Sim_CPU.io.io_access;
   begin
      if len = 0 then
         success := False;
         return null;
      end if;
      c := Ada.Strings.Unbounded.element(name, index);
      --  Get unit designation
      while (index < len) and not cli.parse.isDigit(c) loop
         dev_name := dev_name & c;
         index := index + 1;
         c := Ada.Strings.Unbounded.element(name, index);
      end loop;
      if not cli.parse.isDigit(c) then
         dev_name := dev_name & c;
      end if;
      Ada.Strings.Unbounded.Translate(dev_name, Ada.Strings.Maps.Constants.Upper_Case_Map);
      --  Get unit number
      if index <= len then
         begin
            unit_num := Ada.Strings.Unbounded.Unbounded_Slice(name, index, len);
            index := Natural'Value(Ada.Strings.Unbounded.To_String(unit_num));
         exception
            when CONSTRAINT_ERROR =>
               Ada.Text_IO.Put_Line("Unable to parse <" & Ada.Strings.Unbounded.To_String(unit_num) &
                                   "> into a number.");
               index := 0;
            when e : others =>
               Ada.Text_IO.Put_Line("Some other exception occured parsing device name: " & Ada.Exceptions.Exception_Message(e));
               raise;
         end;
      else
         unit_num := Ada.Strings.Unbounded.Null_Unbounded_String;
         index := 0;
      end if;
      --  Search device table
      if index > 0 then
         index := index - 1;
         for dev_kind in BBS.Sim_CPU.io.dev_type'Range loop
            if index <= dev_table(dev_kind).Last_Index then
               dev := dev_table(dev_kind)(index);
               if dev.name = Ada.Strings.Unbounded.To_String(dev_name) then
                  success := True;
                  return dev;
               end if;
            end if;
         end loop;
      end if;
      success := False;
      return null;
   end;
   --
   --  Select the CPU to use or show CPU
   --  SET CPU [<name>]
   --
   procedure set_cpu(s : Ada.Strings.Unbounded.Unbounded_String) is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      name  : Ada.Strings.Unbounded.Unbounded_String;
      token : cli.parse.token_type;
   begin
      rest  := cli.parse.trim(s);
      token := cli.parse.split(name, rest);
      if token = cli.parse.Missing then
         if cpu_selected then
            Ada.Text_IO.Put_Line("CPU: " & cpu.name & " / " & cpu.variant(cpu.variant));
            Ada.Text_IO.Put_Line("MEM: " & BBS.Sim_CPU.addr_bus'Image(bus.mem_size) & "(max) / "&
                                 BBS.Sim_CPU.addr_bus'Image(bus.get_max_addr) & "(limit)");
         else
            Ada.Text_IO.Put_Line("No CPU currently selected");
         end if;
      else
         if cpu_selected then
            Ada.Text_IO.Put_Line("CPU " & cpu.name & " / " & cpu.variant(cpu.variant) &
                                " is already selected.");
         else
            Ada.Strings.Unbounded.Translate(name, Ada.Strings.Maps.Constants.Upper_Case_Map);
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
            else
               Ada.Text_IO.Put_Line("CPU: Unrecognized CPU name");
               return;
            end if;
            cli.cpu.init;
            cpu_selected := True;
            Ada.Text_IO.Put_Line("Simulator name: " & cpu.name);
            Ada.Text_IO.Put_Line("Simulator variant: " & cpu.variant(cpu.variant));
         end if;
      end if;
   end;
   --
   --  Set pause/interrupt options
   --  SET PAUSE CHAR
   --  SET PAUSE COUNT
   --
   procedure set_pause(s : Ada.Strings.Unbounded.Unbounded_String) is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      token : cli.parse.token_type;
      value : BBS.uint32;
   begin
      rest  := cli.parse.trim(s);
      token := cli.parse.split(first, rest);
      if token = cli.parse.Missing then
         Ada.Text_IO.Put_Line("No options to SET PAUSE");
         Ada.Text_IO.Put_Line("Count:     " & Integer'Image(pause_count));
         Ada.Text_IO.Put_Line("Character: " & Integer'Image(Character'Pos(interrupt)));
         return;
      end if;
      Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
      if first = "CHAR" then
         token := cli.parse.nextDecValue(value, rest);
         if token /= cli.parse.Number then
            Ada.Text_IO.Put_Line("Expected a number for SET PAUSE CHAR");
            Ada.Text_IO.Put_Line("Character: " & Integer'Image(Character'Pos(interrupt)));
            return;
         end if;
         if value >= 0 and value <= 255 then
            interrupt := Character'Val(value);
         else
            Ada.Text_IO.Put_Line("SET PAUSE CHAR value out of range.");
         end if;
      elsif first = "COUNT" then
         token := cli.parse.nextDecValue(value, rest);
         if token /= cli.parse.Number then
            Ada.Text_IO.Put_Line("Expected a number for SET PAUSE COUNT");
            Ada.Text_IO.Put_Line("Count:     " & Integer'Image(pause_count));
            return;
         end if;
         pause_count := Integer(value);
      else
         Ada.Text_IO.Put_Line("Unable to SET PAUSE <" & Ada.Strings.Unbounded.To_String(first) & ">");
      end if;
   end;
   --
   --  Set options
   --  SET CPU
   --  SET PAUSE
   --
   procedure set(s : Ada.Strings.Unbounded.Unbounded_String) is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      token : cli.parse.token_type;
   begin
      rest  := cli.parse.trim(s);
      token := cli.parse.split(first, rest);
      if token = cli.parse.Missing then
         Ada.Text_IO.Put_Line("No options to SET");
         return;
      end if;
      Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
      if first = "CPU" then
         set_cpu(rest);
      elsif first = "PAUSE" then
         set_pause(rest);
      else
         Ada.Text_IO.Put_Line("Unable to SET <" & Ada.Strings.Unbounded.To_String(first) & ">");
      end if;
   end;
   --
end cli;
