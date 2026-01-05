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
   --   0  0  0  0| C  C  C  C| B  B  B  B  B  B  B  B  Branch instructions
   --   0  0  0  0| C  C  C  C  C  C| M  M  M| R  R  R  Single op instructions
   --   0  0  0  0| 0  0  0  0  1  0  1| S| N| Z| V| C  Set/clear condition code
   --
   procedure decode(self : in out PDP11) is
   begin
      case instr.fbr.code is
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
            case instr.f1.code is
               when 8#01# =>  --  JMP (jump instruction)
                  JMP(self);
               when 8#02# =>  --  Condition codes and others
                  if instr.fcc.code = 5 then
                     codes(self);
                  else
                     Ada.Text_IO.Put_Line("Unimplemented Line 0 instruction: " & toOct(instr.b));
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
               when others =>
                  Ada.Text_IO.Put_Line("Unimplemented Line 0 instruction: " & toOct(instr.b));
            end case;
      end case;
   end;
   --
   --  Routines for instructions
   --
   --  Swap bytes
   --
   procedure SWAB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : word := self.get_ea(ea_dest);
      b1      : constant byte := byte(val and 16#FF#);
      b2      : constant byte := byte((val/16#100#) and 16#FF#);
   begin
      val := word(b2) + word(b1)*16#100#;
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := False;
      self.psw.carry    := False;
   end;
   --
   --  Clear word
   --
   procedure CLR(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
   begin
      self.set_ea(ea_dest, 0);
      self.post_ea(ea_dest);
      self.psw.zero     := True;
      self.psw.negative := False;
      self.psw.overflow := False;
      self.psw.carry    := False;
   end;
   --
   --  Complement word (same as logical NOT)
   --
   procedure COM(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : constant word := not self.get_ea(ea_dest);
   begin
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := False;
      self.psw.carry    := True;
   end;
   --
   --  Increment
   --
   procedure INC(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest) + 1;
   begin
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := (val = 16#8000#);
   end;
   --
   --  Decrement
   --
   procedure DEC(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest) - 1;
   begin
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := (val = 16#7FFF#);
   end;
   --
   --  Negate
   --
   procedure NEG(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : constant word := (not self.get_ea(ea_dest)) + 1;
   begin
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := (val = 16#8000#);
      if val = 0 then
         self.psw.carry := False;
      end if;
   end;
   --
   procedure ADC(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest);
      sum     : uint32;
   begin
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
      self.post_ea(ea_dest);
      self.psw.zero     := (sum = 0);
      self.psw.negative := ((sum and 16#8000#) /= 0);
      self.psw.carry    := ((sum and 16#FFFF_0000#) /= 0);
   end;
   --
   procedure SBC(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest);
      diff    : uint32;
   begin
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
      self.post_ea(ea_dest);
      self.psw.zero     := (diff = 0);
      self.psw.negative := ((diff and 16#8000#) /= 0);
      self.psw.carry    := ((diff and 16#FFFF_0000#) /= 0);
   end;
   --
   procedure TST(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest);
   begin
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.carry    := False;
      self.psw.overflow := False;
   end;
   --
   procedure ROR(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : uint32 := uint32(self.get_ea(ea_dest));
      temp    : uint32;
   begin
      if self.psw.carry then
         temp := 16#8000#;
      else
         temp := 0;
      end if;
      self.psw.carry := (val and 1) = 1;
      temp := temp + val/2;
      self.set_ea(ea_dest, word(temp and 16#FFFF#));
      self.post_ea(ea_dest);
      self.psw.zero     := ((temp and 16#FFFF#) = 0);
      self.psw.negative := ((temp and 16#8000#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
   end;
   --
   procedure ROL(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : uint32 := uint32(self.get_ea(ea_dest));
      temp    : uint32;
   begin
      if self.psw.carry then
         temp := 1;
      else
         temp := 0;
      end if;
      self.psw.carry := (val and 16#8000#) /= 0;
      temp := temp + val * 2;
      self.set_ea(ea_dest, word(temp and 16#FFFF#));
      self.post_ea(ea_dest);
      self.psw.zero     := ((temp and 16#FFFF#) = 0);
      self.psw.negative := ((temp and 16#8000#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
   end;
   --
   procedure ASR(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : word := self.get_ea(ea_dest);
      temp    : word;
   begin
      self.psw.carry := (val and 1) = 1;
      temp := val and 16#8000#;
      val := temp + val/2;
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
   end;
   --
   procedure ASL(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : word := self.get_ea(ea_dest);
      temp    : word;
   begin
      self.psw.carry := (val and 16#8000#) /= 0;
      val := val*2;
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := (val = 0);
      self.psw.negative := ((val and 16#8000#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
   end;
   --
   procedure JMP(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      addr    : constant word := ea_dest.address;
   begin
      --
      --  JMP to a register is an illegal condition.  Some PDP-11s trap to 004,
      --  others to 010.  As more models are implemented, code will be added to
      --  trap appropriately.  Right now, this is adequate for the 05/10 (and others)
      --
      if ea_dest.mode = 0 then
         BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                            BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
         return;
      end if;
      self.pc := addr;
   end;
   --
   procedure BR(self : in out PDP11) is
      offset : constant word := word(sign_extend(instr.fbr.offset))*2;
   begin
      self.pc := self.pc + offset;
   end;
   --
   procedure BNE(self : in out PDP11) is
      offset : constant word := word(sign_extend(instr.fbr.offset))*2;
   begin
      if not self.psw.zero then
         self.pc := self.pc + offset;
      end if;
   end;
   --
   procedure BEQ(self : in out PDP11) is
      offset : constant word := word(sign_extend(instr.fbr.offset))*2;
   begin
      if self.psw.zero then
         self.pc := self.pc + offset;
      end if;
   end;
   --
   procedure BGE(self : in out PDP11) is
      offset : constant word := word(sign_extend(instr.fbr.offset))*2;
   begin
      if self.psw.overflow = self.psw.negative then
         self.pc := self.pc + offset;
      end if;
   end;
   --
   procedure BLT(self : in out PDP11) is
      offset : constant word := word(sign_extend(instr.fbr.offset))*2;
   begin
      if self.psw.overflow /= self.psw.negative then
         self.pc := self.pc + offset;
      end if;
   end;
   --
   procedure BGT(self : in out PDP11) is
      offset : constant word := word(sign_extend(instr.fbr.offset))*2;
   begin
      if (self.psw.overflow = self.psw.negative) and not self.psw.zero then
         self.pc := self.pc + offset;
      end if;
   end;
   --
   procedure BLE(self : in out PDP11) is
      offset : constant word := word(sign_extend(instr.fbr.offset))*2;
   begin
      if (self.psw.overflow /= self.psw.negative) or self.psw.zero then
         self.pc := self.pc + offset;
      end if;
   end;
   --
   --  Condition codes
   procedure codes(self : in out PDP11) is
   begin
      if instr.fcc.set then  --  Set condition codes
         if instr.fcc.carry then
            self.psw.carry := True;
         end if;
         if instr.fcc.overflow then
            self.psw.overflow := True;
         end if;
         if instr.fcc.zero then
            self.psw.zero := True;
         end if;
         if instr.fcc.negative then
            self.psw.negative := True;
         end if;
      else  --  Clear condition codes
         if instr.fcc.carry then
            self.psw.carry := False;
         end if;
         if instr.fcc.overflow then
            self.psw.overflow := False;
         end if;
         if instr.fcc.zero then
            self.psw.zero := False;
         end if;
         if instr.fcc.negative then
            self.psw.negative := False;
         end if;
      end if;
   end;
   --
end;
