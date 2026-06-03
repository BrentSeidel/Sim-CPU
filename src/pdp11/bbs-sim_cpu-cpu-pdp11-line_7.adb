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
      case instr.frop.code is
         when 0 =>  --  MUL (EIS)
            if self.config.has_EIS then
               MUL(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Unimplemented Line 7 instruction: MUL (EIS), " & toOct(instr.b));
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
         when 1 =>  --  DIV (EIS)
            if self.config.has_EIS then
               DIV(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Unimplemented Line 7 instruction: DIV (EIS), " & toOct(instr.b));
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
         when 2 =>  --  ASH (EIS)
            if self.config.has_EIS then
               ASH(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Unimplemented Line 7 instruction: ASH (EIS), " & toOct(instr.b));
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
         when 3 =>  --  ASHC (EIS)
            if self.config.has_EIS then
               ASHC(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Unimplemented Line 7 instruction: ASHC (EIS), " & toOct(instr.b));
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
         when 4 =>  --  XOR (Extra)
            if self.config.has_extra then
               pXOR(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Disabled Line 7 instruction: XOR (EIS), " & toOct(instr.b));
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
         when 5 =>  --  FIS and unused instructions
            Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                 & "), Unimplemented Line 7 instruction: FIS and Unused, " & toOct(instr.b));
            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
         when 6 =>  --  Unused
            Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                 & "), Unimplemented Line 7 instruction: Unused, " & toOct(instr.b));
            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
         when 7 =>  --  SOB (Extra)
            if self.config.has_extra then
               SOB(self);
            else
               Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                    & "), Disabled Line 7 instruction: SOB (Extra), " & toOct(instr.b));
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
      offset : constant word := (word(instr.fbr.offset) and 16#3F#)*2;
      reg    : constant reg_num := instr.frop.reg_src;
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
   begin
      null;
   end;
   --
   procedure DIV(self : in out PDP11) is
   begin
      null;
   end;
   --
   procedure ASH(self : in out PDP11) is
   begin
      null;
   end;
   --
   procedure ASHC(self : in out PDP11) is
   begin
      null;
   end;
   --
   procedure pXOR(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      reg     : constant reg_num := instr.frop.reg_src;
      val     : constant word := self.get_ea(ea_dest) xor self.get_regw(reg);
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("XOR " & reg_str(reg) & ","  & self.put_ea(ea_dest));
      end if;
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := False;
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
end;
