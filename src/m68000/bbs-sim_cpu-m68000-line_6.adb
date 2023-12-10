with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_6 is
   --
   --  Package for decoding Line 6 instructions - Bcc/BSR/BRA
   --
   procedure decode_6(self : in out m68000) is
      disp    : long;
      base_pc : long := self.pc;
      branch  : Boolean := False;
   begin
      Ada.Text_IO.Put_Line("Decoding Bcc/BSR/BRA instructions");
      --
      --  Get branch displacement
      --
      disp := sign_extend(instr_bcc.disp);
      Ada.Text_IO.Put_Line("Instruction displacement " & toHex(disp));
      if disp = 0 then  --  For 68020 and later add check for disp = FF
         disp := sign_extend(self.get_ext);
         Ada.Text_IO.Put_Line("  Extension displacement " & toHex(disp));
      end if;
      --
      --  Check conditions
      --
      case instr_bcc.cond is
         when 0 =>  --  Always (BRA)
            branch := True;
         when 1 =>  --  Branch to subroutine (Yet to implement)
            null;
         when 2 =>  -- Hi (HI)
            branch := not self.psw.carry and self.psw.zero;
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
         Ada.Text_IO.Put_Line("Performing branch.  Current PC is " & toHex(base_pc));
         Ada.Text_IO.Put_Line("  Displacement is " & toHex(disp) &
            ", destination is " & toHex(base_pc + disp));
         self.pc := base_pc + disp;
      end if;
   end;
end;

