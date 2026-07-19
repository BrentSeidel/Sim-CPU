--
--  Author: Brent Seidel
--  Date: 24-Dec-2025
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
--  Code for PDP-11 instructions with the 4 MSBs set to 8.
--
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.CPU.pdp11.exceptions;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package body BBS.Sim_CPU.CPU.PDP11.Line_8 is
   --
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                           target => status_word);
   --
   --  Decode instruction.  This is a bit complicated as different instructions
   --  have differing number of code bits.
   --
   --  15|14 13 12|11 10  9| 8  7  6| 5  4  3| 2  1  0  Octal
   --  15 14 13 12|11 10  9  8| 7  6  5  4| 3  2  1  0  Hexadecimal
   --   1  0  0  0| C  C  C  C| B  B  B  B  B  B  B  B  Branch instructions
   --   1  0  0  0| C  C  C  C  C  C| M  M  M| R  R  R  Single op instructions
   --
   --
   procedure decode(self : in out PDP11) is
   begin
      case self.instr.fbr.code is
         when 0 =>  --  BPL
            BPL(self);
         when 1 =>  --  BMI
            BMI(self);
         when 2 =>  --  BHI
            BHI(self);
         when 3 =>  --  BLOS
            BLOS(self);
         when 4 =>  --  BVC
            BVC(self);
         when 5 =>  --  BVS
            BVS(self);
         when 6 =>  --  BCC (BHIS)
            BCC(self);
         when 7 =>  --  BCS (BLO)
            BCS(self);
         when 8 =>  --  EMT
            if self.trace.instr then
               Ada.Text_IO.Put_Line("EMT " & toOct(self.instr.fbr.offset));
            end if;
            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_030_emt);
         when 9 =>  --  TRAP
            if self.trace.instr then
               Ada.Text_IO.Put_Line("TRAP " & toOct(self.instr.fbr.offset));
            end if;
            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_034_trap);
         when others =>
            case self.instr.f1.code is
               when 8#50# =>  --  CLRB (clear)
                  CLRB(self);
               when 8#51# =>  --  COMB (complement or NOT)
                  COMB(self);
               when 8#52# =>  --  INCB (incrememt)
                  INCB(self);
               when 8#53# =>  --  DECB (decremement)
                  DECB(self);
               when 8#54# =>  --  NEGB (negate)
                  NEGB(self);
               when 8#55# =>  --  ADCB (add carry)
                  ADCB(self);
               when 8#56# =>  --  SBCB (subtract borrow)
                  SBCB(self);
               when 8#57# =>  --  TSTB (test)
                  TSTB(self);
               when 8#60# =>  --  RORB (rotate right)
                  RORB(self);
               when 8#61# =>  --  ROLB (rotate left)
                  ROLB(self);
               when 8#62# =>  --  ASRB (arithmatic shift right)
                  ASRB(self);
               when 8#63# =>  --  ASLB (arithmatic shift left)
                  ASLB(self);
               when 8#64# =>  --  MTPS (if has_MTFPS)
                  if self.config.has_MTFPS then
                     MTPS(self);
                  else
                     Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                          & "), Disabled MTPS instruction: " & toOct(self.instr.b));
                     BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                        BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                  end if;
               when 8#65# =>  --  MFPD  (if has_MMU18 or has_MMU22 feature set)
                  if self.config.has_MMU18 or self.config.has_MMU22 then
                     MFPD(self);
                  else
                     Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                          & "), Unimplemented MFPD instruction: " & toOct(self.instr.b));
                     BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                        BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                  end if;
               when 8#66# =>  --  MTPD  (if has_MMU18 or has_MMU22 feature set)
                  if self.config.has_MMU18 or self.config.has_MMU22 then
                     MTPD(self);
                  else
                     Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                          & "), Unimplemented MTPD instruction: " & toOct(self.instr.b));
                     BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                        BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                  end if;
               when 8#67# =>  --  MFPS (if has_MTFPS)
                  if self.config.has_MTFPS then
                     MFPS(self);
                  else
                     Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                          & "), Disabled MFPS instruction: " & toOct(self.instr.b));
                     BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                        BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                  end if;
               when others =>
                  Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                       & "), Unimplemented Line 8 instruction: " & toOct(self.instr.b));
                  BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                     BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end case;
      end case;
   end;
   --
   --  Routines for instructions
   --
   --  Clear byte
   --
   procedure CLRB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("CLRB " & self.put_ea(ea_dest));
      end if;
      self.set_ea(ea_dest, 0);
      self.psw.zero     := True;
      self.psw.negative := False;
      self.psw.overflow := False;
      self.psw.carry    := False;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Write byte", self.inst_pc));
      end if;
   end;
   --
   --  Complement byte (same as logical NOT)
   --
   procedure COMB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : constant word := not self.get_ea(ea_dest);
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("COMB " & self.put_ea(ea_dest));
      end if;
      self.set_ea(ea_dest, val);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := False;
      self.psw.carry    := True;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify byte", self.inst_pc));
      end if;
   end;
   --
   --  Increment
   --
   procedure INCB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : constant word := self.get_ea(ea_dest) + 1;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("INCB " & self.put_ea(ea_dest));
      end if;
      self.set_ea(ea_dest, val);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := (val = 16#80#);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify byte", self.inst_pc));
      end if;
   end;
   --
   --  Decrement
   --
   procedure DECB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : constant word := self.get_ea(ea_dest) - 1;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("DECB " & self.put_ea(ea_dest));
      end if;
      self.set_ea(ea_dest, val);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := (val = 16#7F#);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify byte", self.inst_pc));
      end if;
   end;
   --
   --  Negate
   --
   procedure NEGB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : constant word := (not self.get_ea(ea_dest)) + 1;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("NEGB " & self.put_ea(ea_dest));
      end if;
      self.set_ea(ea_dest, val);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := ((val and 16#FF#) = 16#80#);
      self.psw.carry := ((val and 16#FF#) /= 0);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify byte", self.inst_pc));
      end if;
   end;
   --
   procedure ADCB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : constant word := self.get_ea(ea_dest);
      sum     : word;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("ADCB " & self.put_ea(ea_dest));
      end if;
      self.psw.overflow := False;
      if self.psw.carry then
         if val = 8#177# then
            self.psw.overflow := True;
         end if;
         sum := val + 1;
      else
         sum := val;
      end if;
      self.set_ea(ea_dest, word(sum and 16#FF#));
      self.psw.zero     := ((sum and 16#FF#) = 0);
      self.psw.negative := ((sum and 16#80#) /= 0);
      self.psw.carry    := ((sum and 16#FF00#) /= 0);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify byte", self.inst_pc));
      end if;
   end;
   --
   procedure SBCB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : constant word := self.get_ea(ea_dest);
      diff    : word;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("SBCB " & self.put_ea(ea_dest));
      end if;
      self.psw.overflow := False;
      if self.psw.carry then
         diff := val - 1;
         if val = 8#200# then
            self.psw.overflow := True;
         end if;
      else
         diff := val;
      end if;
      self.set_ea(ea_dest, word(diff and 16#FF#));
      self.psw.zero     := ((diff and 16#FF#) = 0);
      self.psw.negative := ((diff and 16#80#) /= 0);
      self.psw.carry    := ((diff and 16#FF00#) /= 0);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify byte", self.inst_pc));
      end if;
   end;
   --
   procedure TSTB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : constant word := self.get_ea(ea_dest);
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("TSTB " & self.put_ea(ea_dest));
      end if;
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.carry    := False;
      self.psw.overflow := False;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Read byte", self.inst_pc));
      end if;
   end;
   --
   procedure RORB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : uint32 := uint32(self.get_ea(ea_dest));
      temp    : uint32;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("RORB " & self.put_ea(ea_dest));
      end if;
      if self.psw.carry then
         temp := 16#80#;
      else
         temp := 0;
      end if;
      temp := temp + val/2;
      self.set_ea(ea_dest, word(temp and 16#FF#));
      if not self.bus_error then
         self.psw.carry    := (val and 1) = 1;
         self.psw.zero     := ((temp and 16#FF#) = 0);
         self.psw.negative := ((temp and 16#80#) /= 0);
         self.psw.overflow := self.psw.carry xor self.psw.negative;
      end if;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify byte", self.inst_pc));
      end if;
   end;
   --
   procedure ROLB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : uint32 := uint32(self.get_ea(ea_dest));
      temp    : uint32;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("ROLB " & self.put_ea(ea_dest));
      end if;
      if self.psw.carry then
         temp := 1;
      else
         temp := 0;
      end if;
      temp := temp + val * 2;
      self.set_ea(ea_dest, word(temp and 16#FF#));
      if not self.bus_error then
         self.psw.carry    := (val and 16#80#) /= 0;
         self.psw.zero     := ((temp and 16#FF#) = 0);
         self.psw.negative := ((temp and 16#80#) /= 0);
         self.psw.overflow := self.psw.carry xor self.psw.negative;
      end if;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify byte", self.inst_pc));
      end if;
   end;
   --
   procedure ASRB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : word := self.get_ea(ea_dest);
      temp    : word;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("ASRB " & self.put_ea(ea_dest));
      end if;
      self.psw.carry := (val and 1) = 1;
      temp := val and 16#80#;
      val := temp + val/2;
      self.set_ea(ea_dest, val);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify byte", self.inst_pc));
      end if;
   end;
   --
   procedure ASLB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : word := self.get_ea(ea_dest);
      temp    : word;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("ASLB " & self.put_ea(ea_dest));
      end if;
      self.psw.carry := (val and 16#80#) /= 0;
      val := val*2;
      self.set_ea(ea_dest, val);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify byte", self.inst_pc));
      end if;
   end;
   --
   procedure BPL(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BPL " & toOct(self.pc + offset));
      end if;
      if not self.psw.negative then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BMI(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BMI " & toOct(self.pc + offset));
      end if;
      if self.psw.negative then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BHI(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BHI " & toOct(self.pc + offset));
      end if;
      if (not self.psw.carry) and (not self.psw.zero) then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BLOS(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BLOS " & toOct(self.pc + offset));
      end if;
      if self.psw.carry or self.psw.zero then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BVC(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BVC " & toOct(self.pc + offset));
      end if;
      if not self.psw.overflow then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BVS(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BVS " & toOct(self.pc + offset));
      end if;
      if self.psw.overflow then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BCC(self : in out PDP11) is  --  Also BHIS
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BCC " & toOct(self.pc + offset));
      end if;
      if not self.psw.carry then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BCS(self : in out PDP11) is  --  Also BLO
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BCS " & toOct(self.pc + offset));
      end if;
      if self.psw.carry then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure MFPS(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val     : word := psw_to_word(self.psw) and 16#FF#;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("MFPS " & self.put_ea(ea_dest));
      end if;
      --
      --  Do things a little special if destination is a register.
      --
      if (self.instr.f2.mode_dest = 0) then
         if (val and 16#80#) /= 0 then
            val := val or 16#FF00#;
         end if;
         self.set_regw(self.instr.f2.reg_dest, val);
      else
         self.set_ea(ea_dest, val);
      end if;
      self.psw.negative := (val and 16#80#) /= 0;
      self.psw.zero := val = 0;
      self.psw.overflow := False;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Write byte", self.inst_pc));
      end if;
   end;
   --
   procedure MTPS(self : in out PDP11) is
      ea_src : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_byte);
      val    : word := self.get_ea(ea_src) and 16#EF#;  --  T bit can't be set
      psw    : word := psw_to_word(self.psw) and 16#FF00#;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("MTPS " & self.put_ea(ea_src));
      end if;
      self.psw := word_to_psw(val or psw);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_src, "Read byte", self.inst_pc));
      end if;
   end;
   --
   --  Documentation for MFPI and MTPI is rather sparse.  Most of the special logic
   --  is determined by what makes the diagnostic routines pass.  Until separate
   --  I/D space is implemented, MFPI is the same as MFPD and MTPI is the same as
   --  MTPD.
   --
   procedure MFPD(self : in out PDP11) is      psw : constant status_word := self.psw;
      ea_src  : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : word;
      mode    : cpu_mode;
   begin
      --
      --  User mode shouldn't be able to get any kernel mode information.
      --
      if (self.psw.curr_mode = mode_kern) then
         mode := self.psw.prev_mode;
      else
         mode := mode_user;
      end if;
      if self.trace.instr then
         Ada.Text_IO.Put_Line("MFPD " & self.put_ea(ea_src) & " current " & cpu_mode'Image(self.psw.curr_mode)
                             & ", previous " & cpu_mode'Image(self.psw.prev_mode));
      end if;
      self.psw.curr_mode := mode;
      val := self.get_ea(ea_src, mode);
      self.psw := psw;
      --
      --  Diagnostic FKAAC0.BIC expects that user mode be able to read the KSP.
      --  I don't agree with this, but here is a hack to get it to pass.  This might
      --  also only be possible when the MMU is disabled.
      --
      if (ea_src.kind = register) then
         if (ea_src.reg = 6) and (psw.curr_mode = mode_user) and (psw.prev_mode = mode_kern) then
            val := self.ksp;
         end if;
      end if;
      if not self.bus_error then
         declare
            ea_dest : constant operand := self.get_ea(6, 4, data_word);  --  Push to stack
         begin
            self.set_ea(ea_dest, val);
         end;
         self.psw.zero     := (val = 0);
         self.psw.negative := ((val and 16#8000#) /= 0);
         self.psw.overflow := False;
      else
         self.undo_ea(ea_src);
      end if;
   end;
   --
   procedure MTPD(self : in out PDP11) is
      ea_src : constant operand := self.get_ea(6, 2, data_word);  --  Pop from stack
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      psw     : constant status_word := self.psw;
      val     : constant word := self.get_ea(ea_src);
      mode    : cpu_mode;
   begin
      --
      --  User mode shouldn't be able to set any kernel mode information.
      --
      if (self.psw.curr_mode = mode_kern) then
         mode := self.psw.prev_mode;
      else
         mode := mode_user;
      end if;
      if self.trace.instr then
         Ada.Text_IO.Put_Line("MTPD " & self.put_ea(ea_dest) & " current " & cpu_mode'Image(self.psw.curr_mode)
                             & ", previous " & cpu_mode'Image(self.psw.prev_mode));
         Ada.Text_IO.Put_Line("MTPD: SP is " & toOct(self.get_regw(6)) & " dest value is " & toOct(val));
      end if;
      self.psw.curr_mode := mode;
      self.set_ea(ea_dest, val, mode);
      --
      --  Diagnostic FKAAC0.BIC expects that user mode be able to set the KSP.
      --  I don't agree with this, but here is a hack to get it to pass.  This might
      --  also only be possible when the MMU is disabled.
      --
      if (ea_dest.kind = register) then
         if (ea_dest.reg = 6) and (psw.curr_mode = mode_user) and (psw.prev_mode = mode_kern) then
            self.ksp := val;
         end if;
      end if;
      self.psw := psw;
      if not self.bus_error then
         self.psw.zero     := (val = 0);
         self.psw.negative := ((val and 16#8000#) /= 0);
         self.psw.overflow := False;
         --
         --  Oddly, MTPI seems to require this while MTPD doesn't.
         --
--      else
--         self.undo_ea(ea_dest);
      end if;
   end;
   --
end;
