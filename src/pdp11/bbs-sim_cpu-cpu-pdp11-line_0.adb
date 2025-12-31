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
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.io;
with Ada.Text_IO;
use type BBS.Sim_CPU.io.io_access;
package body BBS.Sim_CPU.CPU.PDP11.Line_0 is
   --
   --  Decode instruction
   --
   procedure decode(self : in out PDP11) is
   begin
      case instr.f1.code is
--         when 8#01# =>  --  JMP (jump instruction)
--            null;
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
         when others =>
            Ada.Text_IO.Put_Line("Unimplemented Line 0 instruction.");
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
      Ada.Text_IO.Put_Line("ROR: Mode " & toHex(byte(ea_dest.mode)) & ", Reg " & toHex(byte(ea_dest.reg)));
      Ada.Text_IO.Put_Line("     Value " & toHex(val) & ", new value " & toHex(temp));
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
      Ada.Text_IO.Put_Line("ROL: Mode " & toHex(byte(ea_dest.mode)) & ", Reg " & toHex(byte(ea_dest.reg)));
      Ada.Text_IO.Put_Line("     Value " & toHex(val) & ", new value " & toHex(temp));
      self.set_ea(ea_dest, word(temp and 16#FFFF#));
      self.post_ea(ea_dest);
      self.psw.zero     := ((temp and 16#FFFF#) = 0);
      self.psw.negative := ((temp and 16#8000#) /= 0);
      self.psw.overflow := self.psw.carry xor self.psw.negative;
   end;
   --
   procedure ASR(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest);
   begin
      Ada.Text_IO.Put_Line("ASR: Mode " & toHex(byte(ea_dest.mode)) & ", Reg " & toHex(byte(ea_dest.reg)));
      Ada.Text_IO.Put_Line("     Value " & toHex(val));
   end;
   --
   procedure ASL(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest);
   begin
      Ada.Text_IO.Put_Line("ASL: Mode " & toHex(byte(ea_dest.mode)) & ", Reg " & toHex(byte(ea_dest.reg)));
      Ada.Text_IO.Put_Line("     Value " & toHex(val));
   end;
   --
   procedure JMP(self : in out PDP11) is
      ea_dest : constant operand := self.get_ea(instr.f2.reg_dest, instr.f2.mode_dest, data_word);
      val     : constant word := self.get_ea(ea_dest);
   begin
      Ada.Text_IO.Put_Line("JMP: Mode " & toHex(byte(ea_dest.mode)) & ", Reg " & toHex(byte(ea_dest.reg)));
      Ada.Text_IO.Put_Line("     Value " & toHex(val));
   end;
   --
end;
