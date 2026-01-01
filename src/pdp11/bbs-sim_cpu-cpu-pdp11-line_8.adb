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
package body BBS.Sim_CPU.CPU.PDP11.Line_8 is
   --
   --  Decode instruction
   --
   procedure decode(self : in out PDP11) is
   begin
      case instr.f1.code is
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
         when others =>
            Ada.Text_IO.Put_Line("Unimplemented Line 8 instruction.");
      end case;
   end;
   --
   --  Routines for instructions
   --
   --  Clear byte
   --
   procedure CLRB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
   begin
      self.set_ea(ea_dest, 0);
      self.post_ea(ea_dest);
      self.psw.zero     := True;
      self.psw.negative := False;
      self.psw.overflow := False;
      self.psw.carry    := False;
   end;
   --
   --  Complement byte (same as logical NOT)
   --
   procedure COMB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
      val     : constant word := not self.get_ea(ea_dest);
   begin
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := False;
      self.psw.carry    := True;
   end;
   --
   --  Increment
   --
   procedure INCB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
      val     : constant word := self.get_ea(ea_dest) + 1;
   begin
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := (val = 16#80#);
   end;
   --
   --  Decrement
   --
   procedure DECB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
      val     : constant word := self.get_ea(ea_dest) - 1;
   begin
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := (val = 16#7F#);
   end;
   --
   --  Negate
   --
   procedure NEGB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
      val     : constant word := (not self.get_ea(ea_dest)) + 1;
   begin
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := ((val and 16#FF#) = 16#80#);
      if (val and 16#FF#) = 0 then
         self.psw.carry := False;
      end if;
   end;
   --
   procedure ADCB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
      val     : constant word := self.get_ea(ea_dest);
      sum     : word;
   begin
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
      self.post_ea(ea_dest);
      self.psw.zero     := ((sum and 16#FF#) = 0);
      self.psw.negative := ((sum and 16#80#) /= 0);
      self.psw.carry    := ((sum and 16#FF00#) /= 0);
   end;
   --
   procedure SBCB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
      val     : constant word := self.get_ea(ea_dest);
      diff    : word;
   begin
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
      self.post_ea(ea_dest);
      self.psw.zero     := ((diff and 16#FF#) = 0);
      self.psw.negative := ((diff and 16#80#) /= 0);
      self.psw.carry    := ((diff and 16#FF00#) /= 0);
   end;
   --
   procedure TSTB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
      val     : constant word := self.get_ea(ea_dest);
   begin
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.carry    := False;
      self.psw.overflow := False;
   end;
   --
   procedure RORB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
      val     : uint32 := uint32(self.get_ea(ea_dest));
      temp    : uint32;
   begin
      if self.psw.carry then
         temp := 16#80#;
      else
         temp := 0;
      end if;
      self.psw.carry := (val and 1) = 1;
      temp := temp + val/2;
      self.set_ea(ea_dest, word(temp and 16#FF#));
      self.post_ea(ea_dest);
      self.psw.zero     := ((temp and 16#FF#) = 0);
      self.psw.negative := ((temp and 16#80#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
   end;
   --
   procedure ROLB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
      val     : uint32 := uint32(self.get_ea(ea_dest));
      temp    : uint32;
   begin
      if self.psw.carry then
         temp := 1;
      else
         temp := 0;
      end if;
      self.psw.carry := (val and 16#80#) /= 0;
      temp := temp + val * 2;
      self.set_ea(ea_dest, word(temp and 16#FF#));
      self.post_ea(ea_dest);
      self.psw.zero     := ((temp and 16#FF#) = 0);
      self.psw.negative := ((temp and 16#80#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
   end;
   --
   procedure ASRB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
      val     : word := self.get_ea(ea_dest);
      temp    : word;
   begin
      self.psw.carry := (val and 1) = 1;
      temp := val and 16#80#;
      val := temp + val/2;
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
   end;
   --
   procedure ASLB(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_byte);
      val     : word := self.get_ea(ea_dest);
      temp    : word;
   begin
      self.psw.carry := (val and 16#80#) /= 0;
      val := val*2;
      self.set_ea(ea_dest, val);
      self.post_ea(ea_dest);
      self.psw.zero     := ((val and 16#FF#) = 0);
      self.psw.negative := ((val and 16#80#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
   end;
   --
   procedure BPL(self : in out PDP11) is
   begin
      null;
   end;
   --
   procedure BMI(self : in out PDP11) is
   begin
      null;
   end;
   --
   procedure BHI(self : in out PDP11) is
   begin
      null;
   end;
   --
   procedure BLOS(self : in out PDP11) is
   begin
      null;
   end;
   --
   procedure BVC(self : in out PDP11) is
   begin
      null;
   end;
   --
   procedure BVS(self : in out PDP11) is
   begin
      null;
   end;
   --
   procedure BCC(self : in out PDP11) is  --  Also BHIS
   begin
      null;
   end;
   --
   procedure BCS(self : in out PDP11) is  --  Also BLO
   begin
      null;
   end;
   --
end;
