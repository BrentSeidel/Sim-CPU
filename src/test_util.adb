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
   --  Command loop
   --
   procedure cmds is
      cmd   : Ada.Strings.Unbounded.Unbounded_String;
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      index : Natural;
      addr  : BBS.embed.uint32;
      value : BBS.embed.uint32;
      exit_flag : Boolean := False;
   begin
      loop
         Ada.Text_IO.Put("CMD>");
         Ada.Text_IO.Unbounded_IO.Get_Line(cmd);
         Ada.Strings.Unbounded.Translate(cmd, Ada.Strings.Maps.Constants.Upper_Case_Map);
         index := ada.Strings.Unbounded.Index(cmd, " ");
         if index = 0 then
            first := cmd;
            rest := Ada.Strings.Unbounded.Null_Unbounded_String;
         else
            first := Ada.Strings.Unbounded.Unbounded_Slice(cmd, 1, index - 1);
            rest := Ada.Strings.Unbounded.Unbounded_Slice(cmd, index + 1,
                                                          Ada.Strings.Unbounded.Length(cmd));
         end if;
         if first = ";" then
            Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(rest));
         elsif first = "STEP" then
            if cpu.halted then
               Ada.Text_IO.Put_Line("CPU is halted");
            end if;
            cpu.run;
         elsif first = "REG" then
            dump_reg(cpu);
         elsif first = "DEP" then
            nextValue(addr, rest);
            nextValue(value, rest);
            CPU.set_mem(addr, value);
         elsif first = "DUMP" then
            nextValue(addr, rest);
            dump_mem(BBS.Sim_CPU.word(addr and 16#FFFF#));
         elsif first = "GO" then
            nextValue(addr, rest);
            CPU.start(addr);
         elsif first = "QUIT" then
            exit_flag := True;
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
      v := toHex(Ada.Strings.Unbounded.To_String(first));
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
   --  Return a value from a hexidecimal string
   --
   function toHex(s : String) return BBS.embed.uint32 is
      v : BBS.embed.uint32 := 0;
   begin
      for i in s'Range loop
         exit when not isHex(s(i));
         v := v*16#10# + hexDigit(s(i));
      end loop;
      return v;
   end;
   --
   --  Return the hexidecimal digit
   --
   function hexDigit(c : Character) return BBS.embed.uint32 is
   begin
      case c is
         when '0' =>
            return 0;
         when '1' =>
            return 1;
         when '2' =>
            return 2;
         when '3' =>
            return 3;
         when '4' =>
            return 4;
         when '5' =>
            return 5;
         when '6' =>
            return 6;
         when '7' =>
            return 7;
         when '8' =>
            return 8;
         when '9' =>
            return 9;
         when 'A' | 'a' =>
            return 10;
         when 'B' | 'b' =>
            return 11;
         when 'C' | 'c' =>
            return 12;
         when 'D' | 'd' =>
            return 13;
         when 'E' | 'e' =>
            return 14;
         when 'F' | 'f' =>
            return 15;
         when others =>
            return 0;
      end case;
   end;
   --
end test_util;
