with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Strings.Maps.Constants;
with BBS.embed;
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
      exit_flag : Boolean := False;
   begin
      loop
         dump_reg(cpu);
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
         Ada.Text_IO.Put_Line("first is <" & Ada.Strings.Unbounded.To_String(first) & ">");
         Ada.Text_IO.Put_Line("rest is <" & Ada.Strings.Unbounded.To_String(rest) & ">");
         if first = "STEP" then
            cpu.run;
         elsif first = "EXIT" then
            exit_flag := True;
         end if;
         exit when exit_flag;
      end loop;
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
