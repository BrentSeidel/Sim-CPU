with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_6 is
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
      disp := sign_extend(instr_bcc.disp);
      if disp = 0 then  --  For 68020 and later add check for disp = FF
         disp := sign_extend(self.get_ext);
      end if;
--      Ada.Text_IO.Put_Line("Decoding Bcc/BSR/BRA to " & toHex(base_pc + disp) & " instructions");
      --
      --  Check conditions
      --
      case instr_bcc.cond is
         when 0 =>  --  Always (BRA)
            branch := True;
         when 1 =>  --  Branch to subroutine
            branch := True;
            if self.psw.super then  --  Use supervisor SP
               self.ssp := self.ssp - 4;
               self.memory(addr_bus(self.ssp), self.pc);
            else  --  Use user SO
               self.usp := self.usp - 4;
               self.memory(addr_bus(self.usp), self.pc);
            end if;
         when 2 =>  -- Hi (HI)
            branch := (not self.psw.carry) and (not self.psw.zero);
         when 3 =>  --  Low or same (LS)
            branch := self.psw.carry or self.psw.zero;
         when 4 =>  --  Carry clear (CC)
            branch := not self.psw.carry;
         when 5 =>  --  Carry set (CS)
            branch := self.psw.carry;
         when 6 =>  --  Not equal (NE)
            branch := not self.psw.zero;
         when 7 =>  --  Equal (EQ)
            branch := self.psw.zero;
         when 8 =>  --  Overflow clear (VC)
            branch := not self.psw.overflow;
         when 9 =>  --  Overflow set (VS)
            branch := self.psw.overflow;
         when 10 =>  --  Plus (PL)
             branch := not self.psw.negative;
         when 11 =>  --  Minus (MI)
            branch := self.psw.negative;
         when 12 =>  --  Greater or equal (GE)
            branch := (self.psw.negative and self.psw.overflow) or
                      (not self.psw.negative and not self.psw.overflow);
         when 13 =>  --  Less than (LT)
            branch := (self.psw.negative and not self.psw.overflow) or
                      (not self.psw.negative and self.psw.overflow);
         when 14 =>  --  Greater than (GT)
            branch := (self.psw.negative and self.psw.overflow and not self.psw.zero) or
                      (not self.psw.negative and not self.psw.overflow and not self.psw.zero);
         when 15 =>  --  Less or equal (LE)
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

