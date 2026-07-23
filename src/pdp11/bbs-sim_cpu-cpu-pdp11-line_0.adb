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
--  Code for PDP-11 instructions with the 4 MSBs set to 0.
--
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.CPU.pdp11.exceptions;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package body BBS.Sim_CPU.CPU.PDP11.Line_0 is
   --
   --  Decode instruction.  This is a bit complicated as different instructions
   --  have differing number of code bits.
   --
   --  15|14 13 12|11 10  9| 8  7  6| 5  4  3| 2  1  0  Octal
   --  15 14 13 12|11 10  9  8| 7  6  5  4| 3  2  1  0  Hexadecimal
   --   0  0  0  0| 0  0  0  0  1  0  1| S| N| Z| V| C  Set/clear condition code
   --   0  0  0  0| C  C  C| R  R  R| M  M  M| R  R  R  Register plus op instructions
   --   0  0  0  0| C  C  C  C  C  C| M  M  M| R  R  R  Single op instructions
   --   0  0  0  0| C  C  C  C| B  B  B  B  B  B  B  B  Branch/EMT/TRAP instructions
   --
   procedure decode(self : in out PDP11) is
   begin
      case self.instr.fbr.code is
         when 1 =>  --  BR (unconditional branch)
            BR(self);
         when 2 =>  --  BNE (branch if not equal (zero flag clear))
            BNE(self);
         when 3 =>  --  BEQ (branch if equal (zero flag set))
            BEQ(self);
         when 4 =>  --  BGE
            BGE(self);
         when 5 =>  --  BLT
            BLT(self);
         when 6 =>  --  BGT
            BGT(self);
         when 7 =>  --  BLE
            BLE(self);
         when others =>
            case self.instr.f1.code is
               when 8#01# =>  --  JMP (jump instruction)
                  JMP(self);
               when 8#02# =>  --  Condition codes and others
                  if self.instr.fcc.code = 5 then
                     codes(self);
                  elsif self.instr.freg.code = 8#20# then
                     RTS(self);
                  else
                     Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                          & "), Unimplemented Line 0 instruction: " & toOct(self.instr.b));
                     BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                        BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                  end if;
               when 8#03# =>  --  SWAB (swap bytes instruction)
                  SWAB(self);
               when 8#50# =>  --  CLR (clear)
                  CLR(self);
               when 8#51# =>  --  COM (complement or NOT)
                  COM(self);
               when 8#52# =>  --  INC (incrememt)
                  INC(self);
               when 8#53# =>  --  DEC (decremement)
                  DEC(self);
               when 8#54# =>  --  NEG (negate)
                  NEG(self);
               when 8#55# =>  --  ADC (add carry)
                  ADC(self);
               when 8#56# =>  --  SBC (subtract borrow)
                  SBC(self);
               when 8#57# =>  --  TST (test)
                  TST(self);
               when 8#60# =>  --  ROR (rotate right)
                  ROR(self);
               when 8#61# =>  --  ROL (rotate left)
                  ROL(self);
               when 8#62# =>  --  ASR (arithmatic shift right)
                  ASR(self);
               when 8#63# =>  --  ASL (arithmatic shift left)
                  ASL(self);
               when 8#64# =>  --  MARK (if has_extra feature set)
                  if self.config.has_extra then
                     MARK(self);
                  else
                     Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                          & "), Disabled Line 0 instruction (MARK): "
                                          & toOct(self.instr.b));
                     BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self, BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                  end if;
               when 8#65# =>  --  MFPI  (if has_MMU18 or has_MMU22 feature set)
                  if self.config.has_MMU18 or self.config.has_MMU22 then
                     MFPI(self);
                  else
                     Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                          & "), Unimplemented MFPI instruction: " & toOct(self.instr.b));
                     BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                        BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                  end if;
               when 8#66# =>  --  MTPI  (if has_MMU18 or has_MMU22 feature set)
                  if self.config.has_MMU18 or self.config.has_MMU22 then
                     MTPI(self);
                  else
                     Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                          & "), Unimplemented MTPI instruction: " & toOct(self.instr.b));
                     BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                        BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                  end if;
               when 8#67# =>  --  SXT (if has_extra feature set)
                  if self.config.has_extra then
                     SXT(self);
                  else
                     Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                          & "), Disabled Line 0 instruction (SXT): " & toOct(self.instr.b));
                     BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self, BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                  end if;
               when others =>
                  case self.instr.frop.code is
                     when 4 =>  --  JSR (jump to subroutine)
                        JSR(self);
                     when others =>
                        case self.instr.b is
                           when 0 =>  --  HALT
                              if self.psw.curr_mode = mode_kern then
                                 if self.trace.instr then
                                    Ada.Text_IO.Put_Line("HALT");
                                 end if;
                                 self.cpu_halt := True;
                              else
                                 if self.trace.instr then
                                    Ada.Text_IO.Put_Line("HALT (not available in user mode)");
                                 end if;
                                 BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                                    BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                              end if;
                           when 1 =>  --  WAIT
                              if not self.waiting then
                                 if self.trace.instr then
                                    Ada.Text_IO.Put_Line("WAIT");
                                 end if;
                              end if;
                              self.waiting := True;    --  Set waiting flag
                              self.pc := self.pc - 2;  --  Decrement PC so instruction is executed again
                           when 2 =>  --  RTI
                              RTI(self, False);
                           when 3 =>  --  BPT
                              if self.trace.instr then
                                 Ada.Text_IO.Put_Line("BPT");
                              end if;
                              BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                                 BBS.Sim_CPU.CPU.pdp11.exceptions.ex_014_trace);
                           when 4 =>  --  IOT
                              if self.trace.instr then
                                 Ada.Text_IO.Put_Line("IOT");
                              end if;
                              BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                                 BBS.Sim_CPU.CPU.pdp11.exceptions.ex_020_iot);
                           when 5 =>  --  RESET
                              RESET(self);
                           when 6 =>  --  RTT (if has_extra feature set)
                              --
                              --  I've seen a comment that the PDP-11/04 has RTT, but it
                              --  is different from other implementations.  But, no more
                              --  details on how it might be different.  Until I find
                              --  such information, I'll leave it the same.
                              --
                              if self.config.has_RTT then
                                 RTI(self, True);
                              else
                                 Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                                      & "), Disabled Line 0 instruction (RTT): " & toOct(self.instr.b));
                                 BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                                 BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                              end if;
                              when 7 =>  --  MFPT (PDP-11/44,24 only?  Maybe on newer models)
                              Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                                   & "), Unimplemented Line 0 instruction (MFPT): " & toOct(self.instr.b));
                              BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self, BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                           when others =>
                              Ada.Text_IO.Put_Line(toOct(self.inst_pc) & " (" & toHex(self.inst_pc)
                                                   & "), Unimplemented Line 0 instruction: " & toOct(self.instr.b));
                              BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                                 BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
                        end case;
                  end case;
            end case;
      end case;
   end;
   --
   --  Routines for instructions
   --
   --  Swap bytes
   --
   procedure SWAB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : word := self.get_ea(ea_dest, True);
      b1      : constant byte := byte(val and 16#FF#);
      b2      : constant byte := byte((val/16#100#) and 16#FF#);
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("SWAB " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      val := word(b1)*16#100# + word(b2);
      self.set_ea(ea_dest, val);
      self.psw.zero     := (b2 = 0);  --  Zero is set if low order byte of result is zero, not full result
      self.psw.negative := ((b2 and 16#80#) /= 0);
      self.psw.carry    := False;
      if self.config.SWAB_V then
         self.psw.overflow := False;
      end if;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
   --  Processor reset
   --
   procedure RESET(self : in out PDP11) is
   begin
      if self.psw.curr_mode = mode_kern then
         if self.trace.instr then
            Ada.Text_IO.Put_Line("RESET");
         end if;
         self.bus.reset;
         BBS.Sim_CPU.CPU.pdp11.exceptions.flush_exceptions(self);
      else
         if self.trace.instr then
            Ada.Text_IO.Put_Line("RESET (not available in user mode)");
         end if;
      end if;
   end;
   --
   --  Clear word
   --
   --  There is a curious case when clearing the PSW at Unibus address 777_776.  It
   --  seems that at least some of the diagnosics expect the clear to happen after
   --  the flags are set, thus actually clearing the zero flag.
   --
   procedure CLR(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("CLR " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      self.psw.zero     := True;
      self.psw.negative := False;
      self.psw.overflow := False;
      self.psw.carry    := False;
      self.set_ea(ea_dest, 0);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Write", self.inst_pc));
      end if;
   end;
   --
   --  Complement word (same as logical NOT)
   --
   procedure COM(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : constant word := not self.get_ea(ea_dest, True);
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("COM " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      self.set_ea(ea_dest, val);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := False;
      self.psw.carry    := True;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
   --  Increment
   --
   procedure INC(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest, True) + 1;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("INC " & self.put_ea(ea_dest));
      end if;
      self.set_ea(ea_dest, val);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := (val = 16#8000#);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
   --  Decrement
   --
   procedure DEC(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest, True) - 1;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("DEC " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      self.set_ea(ea_dest, val);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := (val = 16#7FFF#);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
   --  Negate
   --
   procedure NEG(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : constant word := (not self.get_ea(ea_dest, True)) + 1;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("NEG " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      self.set_ea(ea_dest, val);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := (val = 16#8000#);
      self.psw.carry := (val /= 0);
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
   procedure ADC(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest, True);
      sum     : uint32;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("ADC " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      self.psw.overflow := False;
      if self.psw.carry then
         if val = 8#077_777# then
            self.psw.overflow := True;
         end if;
         sum := uint32(val) + 1;
      else
         sum := uint32(val);
      end if;
      self.set_ea(ea_dest, word(sum and 16#FFFF#));
      if not self.bus_error then
         self.psw.zero     := ((sum and 16#FFFF#) = 0);
         self.psw.negative := ((sum and 16#8000#) /= 0);
         self.psw.carry    := ((sum and 16#FFFF_0000#) /= 0);
      end if;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
   procedure SBC(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest, True);
      diff    : uint32;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("SBC " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      self.psw.overflow := False;
      if self.psw.carry then
         diff := uint32(val) - 1;
         if val = 8#100_000# then
            self.psw.overflow := True;
         end if;
      else
         diff := uint32(val);
      end if;
      self.set_ea(ea_dest, word(diff and 16#FFFF#));
      if not self.bus_error then
         self.psw.zero     := (diff = 0);
         self.psw.negative := ((diff and 16#8000#) /= 0);
         self.psw.carry    := ((diff and 16#FFFF_0000#) /= 0);
      end if;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
   procedure TST(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest, True);
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("TST " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.carry    := False;
      self.psw.overflow := False;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Read", self.inst_pc));
      end if;
   end;
   --
   procedure ROR(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : uint32 := uint32(self.get_ea(ea_dest, True));
      temp    : uint32;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("ROR " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      if self.psw.carry then
         temp := 16#8000#;
      else
         temp := 0;
      end if;
      temp := temp + val/2;
      self.set_ea(ea_dest, word(temp and 16#FFFF#));
      if not self.bus_error then
         self.psw.carry    := (val and 1) = 1;
         self.psw.zero     := ((temp and 16#FFFF#) = 0);
         self.psw.negative := ((temp and 16#8000#) /= 0);
         self.psw.overflow := self.psw.carry xor self.psw.negative;
      end if;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
   procedure ROL(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : uint32 := uint32(self.get_ea(ea_dest, True));
      temp    : uint32;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("ROL " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      if self.psw.carry then
         temp := 1;
      else
         temp := 0;
      end if;
      temp := temp + val * 2;
      self.set_ea(ea_dest, word(temp and 16#FFFF#));
      if not self.bus_error then
         self.psw.carry    := (val and 16#8000#) /= 0;
         self.psw.zero     := ((temp and 16#FFFF#) = 0);
         self.psw.negative := ((temp and 16#8000#) /= 0);
         self.psw.overflow := self.psw.carry xor self.psw.negative;
      end if;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
   procedure ASR(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : word := self.get_ea(ea_dest, True);
      temp    : word;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("ASR " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      self.psw.carry := (val and 1) = 1;
      temp := val and 16#8000#;
      val := temp + val/2;
      self.set_ea(ea_dest, val);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
   procedure ASL(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      val     : word := self.get_ea(ea_dest, True);
      temp    : word;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("ASL " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      self.psw.carry := (val and 16#8000#) /= 0;
      val := val*2;
      self.set_ea(ea_dest, val);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
      if self.trace.data then
         Ada.Text_IO.Put_Line(self.put_data(ea_dest, "Modify", self.inst_pc));
      end if;
   end;
   --
   procedure JMP(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      temp    : word;
   begin
      --
      --  JMP to a register is an illegal condition.  Some PDP-11s trap to 004,
      --  others to 010.  This is controlled by the reg_10_4 configuration parameter.
      --
      if self.trace.instr then
         Ada.Text_IO.Put_Line("JMP " & self.put_ea(ea_dest));
      end if;
      if (self.instr.frop.mode_dest) = 0 then
         if self.config.reg_10_4 then
            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
         else
            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_004_assorted);
         end if;
         return;
      end if;
      temp := self.get_ea(ea_dest, True);
      if not self.bus_error then
         self.pc := ea_dest.address;
         if self.trace.control then
            Ada.Text_IO.Put_Line(self.put_target(self.pc, "JMP target", self.inst_pc));
         end if;
      else
         self.undo_ea(ea_dest);
      end if;
   end;
   --
   procedure BR(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BR " & toOct(self.pc + offset));
      end if;
      self.pc := self.pc + offset;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.pc, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BNE(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BNE " & toOct(self.pc + offset));
      end if;
      if not self.psw.zero then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BEQ(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BEQ " & toOct(self.pc + offset));
      end if;
      if self.psw.zero then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BGE(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BGE " & toOct(self.pc + offset));
      end if;
      if self.psw.overflow = self.psw.negative then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BLT(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BLT " & toOct(self.pc + offset));
      end if;
      if self.psw.overflow /= self.psw.negative then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BGT(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BGT " & toOct(self.pc + offset));
      end if;
      if (self.psw.overflow = self.psw.negative) and not self.psw.zero then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   procedure BLE(self : in out PDP11) is
      offset : constant word := word(sign_extend(self.instr.fbr.offset))*2;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("BLE " & toOct(self.pc + offset));
      end if;
      if (self.psw.overflow /= self.psw.negative) or self.psw.zero then
         self.pc := self.pc + offset;
      end if;
      if self.trace.control then
         Ada.Text_IO.Put_Line(self.put_target(self.inst_pc + offset + 2, "Branch target", self.inst_pc));
      end if;
   end;
   --
   --  Condition codes
   procedure codes(self : in out PDP11) is
   begin
      if self.instr.fcc.set then  --  Set condition codes
         if self.trace.instr then
            Ada.Text_IO.Put_Line("SCC");
         end if;
         if self.instr.fcc.carry then
            self.psw.carry := True;
         end if;
         if self.instr.fcc.overflow then
            self.psw.overflow := True;
         end if;
         if self.instr.fcc.zero then
            self.psw.zero := True;
         end if;
         if self.instr.fcc.negative then
            self.psw.negative := True;
         end if;
      else  --  Clear condition codes
         if self.trace.instr then
            Ada.Text_IO.Put_Line("CCC");
         end if;
         if self.instr.fcc.carry then
            self.psw.carry := False;
         end if;
         if self.instr.fcc.overflow then
            self.psw.overflow := False;
         end if;
         if self.instr.fcc.zero then
            self.psw.zero := False;
         end if;
         if self.instr.fcc.negative then
            self.psw.negative := False;
         end if;
      end if;
   end;
   --
   --  Subroutines
   procedure JSR(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.frop.reg_dest, self.instr.frop.mode_dest, data_word);
      ea_sp   : constant operand := self.get_ea(6, 4, data_word);  --  Push value to stack
      reg     : constant reg_num := self.instr.frop.reg_src;
      temp    : word;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("JSR " & reg_str(reg) & "," & self.put_ea(ea_dest) & "  ;  SP is " & toOct(self.get_regw(6)));
      end if;
      --
      --  JSR to a register is an illegal condition.  Some PDP-11s trap to 004,
      --  others to 010.  As more models are implemented, code will be added to
      --  trap appropriately.  Right now, this is adequate for the 05/10 (and others)
      --
      if (self.instr.frop.mode_dest) = 0 then
         if self.config.reg_10_4 then
            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
         else
            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_004_assorted);
         end if;
         self.set_regw(6, self.get_regw(6) + 2);
         return;
      end if;
      if self.bus_error then
         return;
      end if;
      temp := self.get_ea(ea_dest, True);
      if not self.bus_error then
         temp := ea_dest.address;
         self.set_ea(ea_sp, self.get_regw(reg));
         if not self.bus_error then
            self.set_regw(reg, self.pc);
            self.pc := temp;
            if self.trace.control then
               Ada.Text_IO.Put_Line(self.put_target(self.pc, "JSR target", self.inst_pc));
            end if;
         end if;
      else
         self.undo_ea(ea_dest);
      end if;
   end;
   --
   procedure RTS(self : in out PDP11) is
      ea_pc   : constant operand := self.get_ea(6, 2, data_word);  --  Pop PC from stack
      reg     : constant reg_num := self.instr.freg.reg;
      temp    : word;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("RTS " & reg_str(reg));
      end if;
      temp := self.get_ea(ea_pc);
      if not self.bus_error then
         self.pc := self.get_regw(reg);
         self.set_regw(reg, temp);
         if self.trace.control then
            Ada.Text_IO.Put_Line(self.put_target(self.pc, "RET target", self.inst_pc));
         end if;
      else
         self.undo_ea(ea_pc);
         Ada.Text_IO.Put_Line("CPU: Bus error during RTS pop of PC");
      end if;
   end;
   --
   --  Set trap to False for RTI or True for RTT
   --
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                           target => status_word);
   procedure RTI(self : in out PDP11; trap : Boolean) is
      ea_pc   : constant operand := self.get_ea(6, 2, data_word);  --  Pop PC from stack
      ea_psw  : constant operand := self.get_ea(6, 2, data_word);  --  Pop PSW from stack
      old_psw : constant status_word := self.psw;
      new_psw : status_word;
      temp_pc : word;
      temp    : word;
   begin
      if self.trace.instr then
         if trap then
            Ada.Text_IO.Put_Line("RTT");
         else
            Ada.Text_IO.Put_Line("RTI");
         end if;
      end if;
      temp_pc := self.get_ea(ea_pc);
      if not self.bus_error then
         temp := self.memory(addr_bus(temp_pc));
         if not self.bus_error then
            self.pc  := temp_pc;
            new_psw := word_to_psw(self.get_ea(ea_psw));
            if not self.bus_error then
               new_psw.prev_mode := old_psw.curr_mode;
               if new_psw.curr_mode < old_psw.curr_mode then
                  new_psw.curr_mode := old_psw.curr_mode;
               end if;
               self.psw := new_psw;
               if trap then
                  self.trace_delay := 1;
               else
                  self.trace_delay := 0;
               end if;
               if self.trace.control then
                  if trap then
                     Ada.Text_IO.Put_Line(self.put_target(self.pc, "RTT target", self.inst_pc));
                  else
                     Ada.Text_IO.Put_Line(self.put_target(self.pc, "RTI target", self.inst_pc));
                  end if;
               end if;
            else
               Ada.Text_IO.Put_Line("CPU: Bus error during RTI/RTT pop of PSW");
               self.undo_ea(ea_psw);
            end if;
         else
            Ada.Text_IO.Put_Line("CPU: Bus error during RTI/RTT probe of new PC");
         end if;
      else
         Ada.Text_IO.Put_Line("CPU: Bus error during RTI/RTT pop of PC");
         self.undo_ea(ea_pc);
         self.undo_ea(ea_psw);
      end if;
   end;
   --
   --  Extra PDP-11 instructions (not in base set, but most, if not all later models)
   --
   procedure MARK(self : in out PDP11) is
      count   : constant word := word(self.instr.fbr.offset) and 16#3F#;
      temp_sp : word;
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("MARK " & toOct(byte(count and 16#FF#)));
      end if;
      self.set_regw(6, self.pc + 2*count);
      temp_sp := self.get_regw(6);
      self.set_regw(6, temp_sp + 2);
      self.pc := self.r5;
      self.r5 := self.memory(addr_bus(temp_sp));
   end;
   --
   procedure SXT(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
   begin
      if self.trace.instr then
         Ada.Text_IO.Put_Line("SXT " & self.put_ea(ea_dest));
      end if;
      if self.bus_error then
         return;
      end if;
      self.psw.overflow := False;
      if self.psw.negative then
         self.psw.zero  := False;  --  Processor handbook doesn't say this, but it seems like it should
         self.set_ea(ea_dest, 16#FFFF#);
      else
         self.psw.zero  := True;
         self.set_ea(ea_dest, 0);
      end if;
   end;
   --
   --  Documentation for MFPI and MTPI is rather sparse.  Most of the special logic
   --  is determined by what makes the diagnostic routines pass.  Until separate
   --  I/D space is implemented, MFPI is the same as MFPD and MTPI is the same as
   --  MTPD.
   --
   procedure MFPI(self : in out PDP11) is
      psw : constant status_word := self.psw;
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
         Ada.Text_IO.Put_Line("MFPI " & self.put_ea(ea_src) & " current " & cpu_mode'Image(self.psw.curr_mode)
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
   procedure MTPI(self : in out PDP11) is
      ea_src : constant operand := self.get_ea(6, 2, data_word);  --  Pop from stack
      ea_dest : constant operand := self.get_ea(self.instr.f2.reg_dest, self.instr.f2.mode_dest, data_word);
      psw     : constant status_word := self.psw;
      val     : constant word := self.get_ea(ea_src, True);
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
         Ada.Text_IO.Put_Line("MTPI " & self.put_ea(ea_dest) & " current " & cpu_mode'Image(self.psw.curr_mode)
                              & ", previous " & cpu_mode'Image(self.psw.prev_mode));
         Ada.Text_IO.Put_Line("MTPI: SP is " & toOct(self.get_regw(6)) & " dest value is " & toOct(val));
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
      else
         --
         --  Oddly, MTPI seems to require this while MTPD doesn't.
         --
         self.undo_ea(ea_dest);
      end if;
   end;
   --
end;
