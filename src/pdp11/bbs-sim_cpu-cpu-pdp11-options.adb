--
--  Author: Brent Seidel
--  Date: 27-Jul-2026
--
--  This file is part of SimCPU.
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
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
package body BBS.Sim_CPU.CPU.pdp11.options is
   --
   --  This package contains routines for reading and writing PDP-11 options.  Note
   --  that this will need to be updated anytime the "features" record is changed.
   --
   procedure set_option(self : in out pdp11; opt : String; value : String) is
      option : constant String := Ada.Strings.Fixed.Translate(opt, Ada.Strings.Maps.Constants.Upper_Case_Map);
      val    : constant String := Ada.Strings.Fixed.Translate(value, Ada.Strings.Maps.Constants.Upper_Case_Map);
      valid  : Boolean := False;
      temp   : Boolean;
      stack  : word;
      switch : ad_bus;

   begin
      temp := parse_boolean(val, valid);
      if option = opt_extra then
         if valid then
            self.config.has_extra := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_RTT then
         if valid then
            self.config.has_RTT := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_EIS then
         if valid then
            self.config.has_EIS := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_FIS then
         if valid then
            self.config.has_FIS := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_FPP then
         if valid then
            self.config.has_FPP := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_CIS then
         if valid then
            self.config.has_CIS := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_USER then
         if valid then
            self.config.has_user := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_SUPER then
         if valid then
            self.config.has_super := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_ID then
         if valid then
            self.config.has_id := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_MMU18 then
         if valid then
            self.config.has_MMU18 := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_MMU22 then
         if valid then
            self.config.has_MMU22 := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_MTFPS then
         if valid then
            self.config.has_MTFPS := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_SWAB then
         if valid then
            self.config.SWAB_V := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_PSW_T then
         if valid then
            self.config.set_PSW_T := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_rValue then
         if valid then
            self.config.reg_value := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_rBus then
         if valid then
            self.config.reg_bus := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_JMPREG then
         if valid then
            self.config.reg_10_4 := Temp;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as Boolean");
         end if;
      elsif option = opt_stack then
         stack := parse_word(val, valid);
         if valid then
            self.config.stack_limit := stack;
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as a word value");
         end if;
      elsif option = opt_switch then
         switch := ad_bus(parse_word(val, valid));
         if valid then
            self.bus.set_sr_ad(switch);
         else
            Ada.Text_IO.Put_Line("CPU: Could not interpret <" & val & "> as a switch value");
         end if;
      else
         Ada.Text_IO.Put_Line("CPU: Unrecognized option <" & option & ">");
      end if;
   end;
   --
   function get_option(self : in out pdp11; opt : String) return String is
      option : constant String := Ada.Strings.Fixed.Translate(opt, Ada.Strings.Maps.Constants.Upper_Case_Map);
   begin
      if option = opt_extra then
         return Boolean'Image(self.config.has_extra);
      elsif option = opt_RTT then
         return Boolean'Image(self.config.has_RTT);
      elsif option = opt_EIS then
         return Boolean'Image(self.config.has_EIS);
      elsif option = opt_FIS then
         return Boolean'Image(self.config.has_FIS);
      elsif option = opt_FPP then
         return Boolean'Image(self.config.has_FPP);
      elsif option = opt_CIS then
         return Boolean'Image(self.config.has_CIS);
      elsif option = opt_USER then
         return Boolean'Image(self.config.has_user);
      elsif option = opt_SUPER then
         return Boolean'Image(self.config.has_super);
      elsif option = opt_ID then
         return Boolean'Image(self.config.has_id);
      elsif option = opt_MMU18 then
         return Boolean'Image(self.config.has_MMU18);
      elsif option = opt_MMU22 then
         return Boolean'Image(self.config.has_MMU22);
      elsif option = opt_MTFPS then
         return Boolean'Image(self.config.has_MTFPS);
      elsif option = opt_SWAB then
         return Boolean'Image(self.config.SWAB_V);
      elsif option = opt_PSW_T then
         return Boolean'Image(self.config.set_PSW_T);
      elsif option = opt_rValue then
         return Boolean'Image(self.config.reg_value);
      elsif option = opt_rBus then
         return Boolean'Image(self.config.reg_bus);
      elsif option = opt_JMPREG then
         return Boolean'Image(self.config.reg_10_4);
      elsif option = opt_stack then
         return word'Image(self.config.stack_limit);
      elsif option = opt_switch then
         return toOct(word(self.bus.get_sr_ad and 16#FFFF#));
      else
         Ada.Text_IO.Put_Line("CPU: Unrecognized option <" & option & ">");
      end if;
      return "No value";
   end;
   --
   procedure list_options(self : in out pdp11) is
   begin
      if self.config.has_extra then
         Ada.Text_IO.Put_Line("Has extra instruction set (SXT, XOR, MARK, SOB, and RTT)");
      else
         Ada.Text_IO.Put_Line("Does not have extra instruction set");
      end if;
      --
      if self.config.has_RTT then
         Ada.Text_IO.Put_Line("Has RTT instruction");
      else
         Ada.Text_IO.Put_Line("Does not have RTT instruction");
      end if;
      --
      if self.config.has_EIS then
         Ada.Text_IO.Put_Line("Has EIS instructions (ASH, ASHC, DIV, MUL)");
      else
         Ada.Text_IO.Put_Line("Does not have EIS instructions");
      end if;
      --
      if self.config.has_FIS then
         Ada.Text_IO.Put_Line("Has FIS instructions");
      else
         Ada.Text_IO.Put_Line("Does not have FIS instructions");
      end if;
      --
      if self.config.has_FPP then
         Ada.Text_IO.Put_Line("Has FPP instructions");
      else
         Ada.Text_IO.Put_Line("Does not have FPP instructions");
      end if;
      --
      if self.config.has_CIS then
         Ada.Text_IO.Put_Line("Has CIS instructions");
      else
         Ada.Text_IO.Put_Line("Does not have CIS instructions");
      end if;
      --
      if self.config.has_user then
         Ada.Text_IO.Put_Line("Suppports user mode");
      else
         Ada.Text_IO.Put_Line("Does not support user mode");
      end if;
      --
      if self.config.has_super then
         Ada.Text_IO.Put_Line("Suppports supervisor mode");
      else
         Ada.Text_IO.Put_Line("Does not support supervisor mode");
      end if;
      --
      if self.config.has_ID then
         Ada.Text_IO.Put_Line("Suppports separate instruction and data memory");
      else
         Ada.Text_IO.Put_Line("Does not support separate instruction and data memory");
      end if;
      --
      if self.config.has_MMU18 then
         Ada.Text_IO.Put_Line("Has 18 bit MMU");
      else
         Ada.Text_IO.Put_Line("Does not have 18 bit MMU");
      end if;
      --
      if self.config.has_MMU22 then
         Ada.Text_IO.Put_Line("Has 22 bit MMU");
      else
         Ada.Text_IO.Put_Line("Does not have 22 bit MMU");
      end if;
      --
      if self.config.has_MTFPS then
         Ada.Text_IO.Put_Line("Has MFPS and MTPS instructions");
      else
         Ada.Text_IO.Put_Line("Does not have MFPS and MTPS instructions");
      end if;
      --
      if self.config.SWAB_V then
         Ada.Text_IO.Put_Line("SWAB instruction clears the V bit");
      else
         Ada.Text_IO.Put_Line("SWAB instruction leaves the V bit unchanged");
      end if;
      --
      if self.config.set_PSW_T then
         Ada.Text_IO.Put_Line("T bit in PSW can be set by directly writing to it");
      else
         Ada.Text_IO.Put_Line("T bit in PSW can not be set by directly writing to it");
      end if;
      --
      if self.config.reg_value then
         Ada.Text_IO.Put_Line("In OP Rx,-(Rx)+ type instructions, use original value of Rx");
      else
         Ada.Text_IO.Put_Line("In OP Rx,-(Rx)+ type instructions, use updated value of Rx");
      end if;
      --
      if self.config.reg_bus then
         Ada.Text_IO.Put_Line("Registers can be read at certain Unibus addresses");
      else
         Ada.Text_IO.Put_Line("Registers can not be read at certain Unibus addresses");
      end if;
      --
      if self.config.reg_10_4 then
         Ada.Text_IO.Put_Line("JMP/JSR to a register traps to 10 (illegal instruction)");
      else
         Ada.Text_IO.Put_Line("JMP/JSR to a register traps to 4 (bus error)");
      end if;
      --
      Ada.Text_IO.Put_Line("Stack limit is set to " & word'Image(self.config.stack_limit) & " decimal");
      --
      Ada.Text_IO.Put_Line("Switch register set to " & toOct(word(self.bus.get_sr_ad and 16#FFFF#)) & " octal");
   end;
   --
   --  Tries to parse a string as a boolean.  If it can't match anything, then
   --  valid is set to False and the return value should be ignored.
   --
   function parse_boolean(v : String; valid : in out Boolean) return Boolean is
   begin
      valid := True;
      if (v = "TRUE") or (v = "T") or (v = "YES") or (v = "Y") or (v = "1") then
         return True;
      end if;
      if (v = "FALSE") or (v = "F") or (v = "NO") or (v = "N") or (v = "0") then
         return False;
      end if;
      valid := False;
      return False;
   end;
   --
   --  Tries to parse a string as a word.  If it can't match anything, then
   --  valid is set to False and the return value should be ignored.
   --
   function parse_word(v : String; valid : in out Boolean) return word is
      temp : word;
   begin
      valid := True;
      temp := word'Value(v);
      return temp;
   exception
      when Constraint_Error =>
         valid := False;
         return 0;
      when e : Others =>
         Ada.Text_IO.Put_Line("Unexpected exception while parsing word: " &
                                Ada.Exceptions.Exception_Information(e));
         valid := False;
         return 0;
   end;
   --
end;
