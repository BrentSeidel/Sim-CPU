--
--  Author: Brent Seidel
--  Date: 11-May-2026
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
--  Code for PDP-11 instructions with the 4 MSBs set to 7.
--
with Ada.Text_IO;
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.CPU.pdp11.exceptions;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package body BBS.Sim_CPU.CPU.PDP11.Line_7 is
   --
   --  Decode instruction.  This is a bit complicated as different instructions
   --  have differing number of code bits.
   --
   --  15|14 13 12|11 10  9| 8  7  6| 5  4  3| 2  1  0  Octal
   --  15 14 13 12|11 10  9  8| 7  6  5  4| 3  2  1  0  Hexadecimal
   --   0  1  1  1| X  X  X  R  R  R| M  M  M| R  R  R  Single op instructions
   --
   --
   procedure decode(self : in out PDP11) is
   begin
      case self.instr.frop.code is
         when 0 =>  --  MUL (EIS)
            if self.config.has_EIS then
               MUL(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Disabled Line 7 instruction: MUL (EIS), " & toOct(self.instr.b));
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
         when 1 =>  --  DIV (EIS)
            if self.config.has_EIS then
               DIV(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Disabled Line 7 instruction: DIV (EIS), " & toOct(self.instr.b));
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
         when 2 =>  --  ASH (EIS)
            if self.config.has_EIS then
               ASH(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Disabled Line 7 instruction: ASH (EIS), " & toOct(self.instr.b));
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
         when 3 =>  --  ASHC (EIS)
            if self.config.has_EIS then
               ASHC(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Disabled Line 7 instruction: ASHC (EIS), " & toOct(self.instr.b));
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
         when 4 =>  --  XOR (Extra)
            if self.config.has_extra then
               pXOR(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Disabled Line 7 instruction: XOR (EIS), " & toOct(self.instr.b));
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
         when 5 =>  --  FIS and unused instructions
            Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                 & "), Unimplemented Line 7 instruction: FIS and Unused, " & toOct(self.instr.b));
            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
         when 6 =>  --  Unused
            Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                 & "), Unimplemented Line 7 instruction: Unused, " & toOct(self.instr.b));
            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
         when 7 =>  --  SOB (Extra)
            if self.config.has_extra then
               SOB(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Disabled Line 7 instruction: SOB (Extra), " & toOct(self.instr.b));
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
      end case;
   end;
   --
   --  Routines for instructions
   --
   --  Extra instructions
   procedure SOB(self : in out PDP11) is
      offset : constant word := (word(self.instr.fbr.offset) and 16#3F#)*2;
      reg    : constant reg_num := self.instr.frop.reg_src;
      val    : constant word := self.get_regw(reg) - 1;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("SOB " & reg_str(reg) & "," & toOct(self.pc - offset));
      end if;
      self.set_regw(reg, val);
      if val /= 0 then
         self.pc := self.pc - offset;
      end if;
   end;
   --
   --  EIS instructions
   procedure MUL(self : in out PDP11) is
      ea_src : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      reg1   : constant reg_num := self.instr.frop.reg_src;
      reg2   : constant reg_num := reg1 or 1;
      val1   : constant long := sign_extend(self.get_regw(reg1));
      val2   : constant long := sign_extend(self.get_ea(ea_src));
      res    : constant long := val1 * val2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("MUL " & self.put_ea(ea_src)& ","   & reg_str(reg1) );
      end if;
      self.set_regw(reg1, word((res/16#1_0000#) and 16#FFFF#));
      self.set_regw(reg2, word(res and 16#FFFF#));
      self.psw.overflow := False;
      self.psw.negative := (res and 16#8000_0000#) /= 0;
      if self.psw.negative then
         self.psw.carry := (not res) + 1 > 16#8000#;
      else
         self.psw.carry := res >= 16#8000#;
      end if;
      self.psw.zero := (res = 0);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(self.get_ea(reg1, 0, data_word), "Modify", self.inst_pc));
         Ada.Text_IO.Put_Line(self.put_data(self.get_ea(reg2, 0, data_word), "Modify", self.inst_pc));
         Ada.Text_IO.Put_Line(self.put_data(ea_src, "Read", self.inst_pc));
      end if;
   end;
   --
   procedure DIV(self : in out PDP11) is
      ea_src : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      reg1   : constant reg_num := self.instr.frop.reg_src;
      reg2   : constant reg_num := reg1 or 1;
      val1   : constant long := long(self.get_regw(reg1))*16#1_0000# + long(self.get_regw(reg2));
      val2   : constant long := sign_extend(self.get_ea(ea_src));
      quot   : long;
      remain : long;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("DIV "  & self.put_ea(ea_src) & "," & reg_str(reg1));
      end if;
      if val2 = 0 then
         self.psw.overflow := True;
         self.psw.carry := True;
      else
         self.psw.carry := False;
         quot := int_to_uint32(uint32_to_int(val1)/uint32_to_int(val2));
         remain := int_to_uint32(uint32_to_int(val1) rem uint32_to_int(val2));
         self.psw.zero := (quot = 0);
         self.psw.negative := (quot and 16#8000_0000#) /= 0;
         if self.psw.negative then
            self.psw.overflow := (not quot) + 1 > 16#8000#;
         else
            self.psw.overflow := quot >= 16#8000#;
         end if;
         self.set_regw(reg1, word(quot and 16#FFFF#));
         self.set_regw(reg2, word(remain and 16#FFFF#));
      end if;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(self.get_ea(reg1, 0, data_word), "Modify", self.inst_pc));
         Ada.Text_IO.Put_Line(self.put_data(self.get_ea(reg2, 0, data_word), "Modify", self.inst_pc));
         Ada.Text_IO.Put_Line(self.put_data(ea_src, "Read", self.inst_pc));
      end if;
   end;
   --
   procedure ASH(self : in out PDP11) is
      ea_src : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      reg    : constant reg_num := self.instr.frop.reg_src;
      shift  : word := self.get_ea(ea_src) and 16#3F#;
      val    : word := self.get_regw(reg);
      sign   : Boolean := (val and 16#8000#) /= 0;
      last_sign : Boolean := (val and 16#8000#) /= 0;
      msb    : word;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("ASH " & reg_str(reg) & ","  & self.put_ea(ea_src));
      end if;
      self.psw.overflow := False;
      if (shift and 16#20#) /= 0 then  --  Shift Right
         shift := shift or 16#FFC0#;
         shift := (not shift) + 1;
         for i in 1 .. shift loop
            msb := (val and 16#8000#);
            self.psw.carry := (val and 16#0001#) /= 0;
            val := (val / 2) or msb;
            sign := (val and 16#8000#) /=0;
            if sign /= last_sign then
               self.psw.overflow := True;
            end if;
            last_sign := sign;
         end loop;
      else  --  Shift Left
         for i in 1 .. shift loop
            self.psw.carry := (val and 16#8000#) /= 0;
            val := val * 2;
            sign := (val and 16#8000#) /= 0;
            if sign /= last_sign then
               self.psw.overflow := True;
            end if;
            last_sign := sign;
         end loop;
      end if;
      self.set_regw(reg, val);
      self.psw.zero := (val = 0);
      self.psw.negative := sign;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(self.get_ea(reg, 0, data_word), "Modify", self.inst_pc));
         Ada.Text_IO.Put_Line(self.put_data(ea_src, "Read", self.inst_pc));
      end if;
   end;
   --
   procedure ASHC(self : in out PDP11) is
      ea_src : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      reg1   : constant reg_num := self.instr.frop.reg_src;
      reg2   : constant reg_num := reg1 or 1;
      shift  : word := self.get_ea(ea_src) and 16#3F#;
      val    : long;
      sign   : Boolean;
      last_sign : Boolean;
      msb    : long;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("ASHC " & reg_str(reg1) & ","  & self.put_ea(ea_src));
      end if;
      val := long(self.get_regw(reg1));
      sign := (val and 16#8000#) /= 0;
      last_sign := (val and 16#8000#) /= 0;
      self.psw.overflow := False;
      val := val*16#0001_0000# + long(self.get_regw(reg2));
      if (shift and 16#20#) /= 0 then  --  Shift Right
         shift := shift or 16#FFC0#;
         shift := (not shift) + 1;
         for i in 1 .. shift loop
            msb := (val and 16#8000_0000#);
            self.psw.carry := (val and 16#0001#) /= 0;
            val := (val / 2) or msb;
            sign := (val and 16#8000_0000#) /=0;
            if sign /= last_sign then
               self.psw.overflow := True;
            end if;
            last_sign := sign;
         end loop;
      else  --  Shift Left
         for i in 1 .. shift loop
            self.psw.carry := (val and 16#8000_0000#) /= 0;
            val := val * 2;
            sign := (val and 16#8000_0000#) /= 0;
            if sign /= last_sign then
               self.psw.overflow := True;
            end if;
            last_sign := sign;
         end loop;
      end if;
      self.set_regw(reg1, word((val/16#0001_0000#) and 16#FFFF#));
      self.set_regw(reg2, word(val and 16#FFFF#));
      self.psw.zero := (val = 0);
      self.psw.negative := sign;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(self.get_ea(reg1, 0, data_word), "Modify", self.inst_pc));
         Ada.Text_IO.Put_Line(self.put_data(self.get_ea(reg2, 0, data_word), "Modify", self.inst_pc));
         Ada.Text_IO.Put_Line(self.put_data(ea_src, "Read", self.inst_pc));
      end if;
   end;
   --
   procedure pXOR(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      reg     : constant reg_num := self.instr.frop.reg_src;
      val     : constant word := self.get_ea(ea_dest) xor self.get_regw(reg);
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("XOR " & reg_str(reg) & ","  & self.put_ea(ea_dest));
      end if;
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := False;
      self.set_ea(ea_dest, val);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(self.get_ea(reg, 0, data_word), "Read", self.inst_pc));
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
end;
