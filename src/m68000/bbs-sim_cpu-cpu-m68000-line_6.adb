--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.--
--
with Ada.Text_IO;
package body BBS.Sim_CPU.CPU.m68000.line_6 is
   --
   --  Package for decoding Line 6 instructions - Bcc/BSR/BRA
   --
   procedure decode_6(self : in out m68000) is
      base_pc : constant long := self.pc;
      disp    : long;
      branch  : Boolean;
   begin
      --
      --  Get branch displacement
      --
      disp := sign_extend(self.instr.bcc.disp);
      if disp = 0 then  --  For 68020 and later add check for disp = FF
         disp := sign_extend(self.get_ext);
      end if;
      --
      --  Check conditions
      --
      case self.instr.bcc.cond is
         when 0 =>  --  Always (BRA)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BRA " & toHex(base_pc + disp));
            end if;
            branch := True;
         when 1 =>  --  Branch to subroutine
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BSR " & toHex(base_pc + disp));
            end if;
            branch := True;
            if self.psw.super then  --  Use supervisor SP
               self.ssp := self.ssp - 4;
               self.memory(addr_bus(self.ssp), self.pc);
            else  --  Use user SO
               self.usp := self.usp - 4;
               self.memory(addr_bus(self.usp), self.pc);
            end if;
         when 2 =>  -- Hi (HI)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BHI " & toHex(base_pc + disp));
            end if;
            branch := (not self.psw.carry) and (not self.psw.zero);
         when 3 =>  --  Low or same (LS)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BLS " & toHex(base_pc + disp));
            end if;
            branch := self.psw.carry or self.psw.zero;
         when 4 =>  --  Carry clear (CC)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BCC " & toHex(base_pc + disp));
            end if;
            branch := not self.psw.carry;
         when 5 =>  --  Carry set (CS)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BCS " & toHex(base_pc + disp));
            end if;
            branch := self.psw.carry;
         when 6 =>  --  Not equal (NE)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BNE " & toHex(base_pc + disp));
            end if;
            branch := not self.psw.zero;
         when 7 =>  --  Equal (EQ)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BEQ " & toHex(base_pc + disp));
            end if;
            branch := self.psw.zero;
         when 8 =>  --  Overflow clear (VC)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BVC " & toHex(base_pc + disp));
            end if;
            branch := not self.psw.overflow;
         when 9 =>  --  Overflow set (VS)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BVS " & toHex(base_pc + disp));
            end if;
            branch := self.psw.overflow;
         when 10 =>  --  Plus (PL)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BPL " & toHex(base_pc + disp));
            end if;
             branch := not self.psw.negative;
         when 11 =>  --  Minus (MI)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BMI " & toHex(base_pc + disp));
            end if;
            branch := self.psw.negative;
         when 12 =>  --  Greater or equal (GE)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BGE " & toHex(base_pc + disp));
            end if;
            branch := (self.psw.negative and self.psw.overflow) or
                      (not self.psw.negative and not self.psw.overflow);
         when 13 =>  --  Less than (LT)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BLT " & toHex(base_pc + disp));
            end if;
            branch := (self.psw.negative and not self.psw.overflow) or
                      (not self.psw.negative and self.psw.overflow);
         when 14 =>  --  Greater than (GT)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BGT " & toHex(base_pc + disp));
            end if;
            branch := (self.psw.negative and self.psw.overflow and not self.psw.zero) or
                      (not self.psw.negative and not self.psw.overflow and not self.psw.zero);
         when 15 =>  --  Less or equal (LE)
            if self.trace.instr then
               Ada.Text_IO.Put_Line("BLE " & toHex(base_pc + disp));
            end if;
            branch := (self.psw.zero) or
                      (self.psw.negative and not self.psw.overflow) or
                      (not self.psw.negative and self.psw.overflow);
      end case;
      --
      --  Perform the branch
      --
      if branch then
         if (base_pc + disp) = self.inst_pc then
            Ada.Text_IO.Put_Line("  Infinite loop detected - halting.");
            self.cpu_halt := True;
         end if;
         self.pc := base_pc + disp;
      end if;
   end;
end;
