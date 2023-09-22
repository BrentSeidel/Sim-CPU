with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Strings.Maps.Constants;
with BBS.embed;
use type BBS.embed.uint8;
use type BBS.embed.uint16;
use type BBS.embed.uint32;
package body test_util is
   --
   --  Dump registers
   --
   procedure dump_reg(c : BBS.Sim_CPU.simulator'Class) is
   regs : BBS.embed.uint32 := cpu.registers;
   begin
   for i in 0 .. (regs - 1) loop
      Ada.Text_IO.Put("Reg " & BBS.embed.uint32'Image(i) & " - ");
      Ada.Text_IO.put(CPU.reg_name(i) & " = ");
      Ada.Text_IO.Put_Line(CPU.read_reg(i));
   end loop;
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
   --
   procedure cmds is
      cmd   : Ada.Strings.Unbounded.Unbounded_String;
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      index : Natural;
      addr  : BBS.embed.uint32;
      value : BBS.embed.uint32;
      level : BBS.embed.uint32;
      exit_flag : Boolean := False;
      char  : Character;
      available : Boolean;
      interrupt : Character := Character'Val(5);  -- Control-E
   begin
      loop
         Ada.Text_IO.Put("CMD>");
         Ada.Text_IO.Unbounded_IO.Get_Line(cmd);
         index := ada.Strings.Unbounded.Index(cmd, " ");
         if index = 0 then
            first := cmd;
            rest := Ada.Strings.Unbounded.Null_Unbounded_String;
         else
            first := Ada.Strings.Unbounded.Unbounded_Slice(cmd, 1, index - 1);
            rest := Ada.Strings.Unbounded.Unbounded_Slice(cmd, index + 1,
                                                          Ada.Strings.Unbounded.Length(cmd));
         end if;
         Ada.Strings.Unbounded.Translate(first, Ada.Strings.Maps.Constants.Upper_Case_Map);
         Ada.Text_IO.Put_Line("Command given is <" & Ada.Strings.Unbounded.To_String(first) & ">");
         if first = ";" then
            Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(rest));
         elsif first = "STEP" then
            cpu.run;
            if cpu.halted then
               Ada.Text_IO.Put_Line("CPU is halted");
            else
              dump_reg(cpu);
            end if;
         elsif first = "RUN" then
            while not cpu.halted loop
               cpu.run;
               Ada.Text_IO.Get_Immediate(char, available);
               exit when available and then char = interrupt;
            end loop;
            if available and char = interrupt then
              Ada.Text_IO.Put_Line("User requested break");
            else
              Ada.Text_IO.Put_Line("CPU Halted");
            end if;
            dump_reg(cpu);
         elsif first = "REG" then
            dump_reg(cpu);
         elsif first = "DEP" then
            Ada.Strings.Unbounded.Translate(rest, Ada.Strings.Maps.Constants.Upper_Case_Map);
            nextValue(addr, rest);
            nextValue(value, rest);
            CPU.set_mem(addr, value);
         elsif first = "TRACE" then
            Ada.Strings.Unbounded.Translate(rest, Ada.Strings.Maps.Constants.Upper_Case_Map);
            nextValue(level, rest);
            CPU.trace(Natural(level));
         elsif first = "DUMP" then
            Ada.Strings.Unbounded.Translate(rest, Ada.Strings.Maps.Constants.Upper_Case_Map);
            nextValue(addr, rest);
            dump_mem(BBS.Sim_CPU.word(addr and 16#FFFF#));
         elsif first = "GO" then
            Ada.Strings.Unbounded.Translate(rest, Ada.Strings.Maps.Constants.Upper_Case_Map);
            nextValue(addr, rest);
            CPU.start(addr);
         elsif first = "LOAD" then
            Ada.Text_IO.Put_Line("Loading " & Ada.Strings.Unbounded.To_String(rest));
            CPU.load(Ada.Strings.Unbounded.To_String(rest));
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
            tel.shutdown;
         else
            Ada.Text_IO.Put_Line("Unrecognized command <" & Ada.Strings.Unbounded.To_String(first) & ">");
         end if;
         exit when exit_flag;
      end loop;
   end;
   --
   --  Pull the next hexidecimal value off of a string
   --
   procedure nextValue(v : out BBS.embed.uint32;
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
   procedure dump_mem(start : BBS.Sim_CPU.word) is
      addr : BBS.Sim_CPU.word := start;
   begin
      for i in 0 .. 15 loop
         Ada.Text_IO.Put(BBS.Sim_CPU.toHex(addr and 16#FFFF#) & " :");
         for j in 0 .. 15 loop
            Ada.Text_IO.Put(" " & BBS.Sim_CPU.toHex(BBS.Sim_CPU.byte(CPU.read_mem(BBS.Sim_CPU.addr_bus(addr + BBS.Sim_CPU.word(j))))));
         end loop;
         addr := addr + 16;
         Ada.Text_IO.New_Line;
      end loop;
   end;
   --
end test_util;
