with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_5 is
   --
   --  Package for decoding Group 5 - ADDQ/SUBQ/Scc/DBcc/TRAPcc
   --
   procedure decode_5(self : in out m68000) is
   begin
      if instr_dbcc.code = 16#19# then  --  DBcc instructions
         decode_DBcc(self);
      elsif instr_addq.code = False then  --  Add quick instruction
         decode_ADDQ(self);
      end if;
   end;
   --
   procedure decode_ADDQ(self : in out m68000) is
      reg_y  : reg_num := instr_addq.reg_y;
      mode_y : mode_code := instr_addq.mode_y;
      op1    : long;
      op2    : long;
      sum    : long;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
      Ada.Text_IO.Put_Line("ADDQ instruction encountered.");
      op1 := long(instr_addq.data);
      if op1 = 0 then  --  Data value of 0 means actual value of 8.
         op1 := 8;
      end if;
      if instr_addq.mode_y = 1 then
         instr_addq.size := data_long;
      end if;
      Smsb := False;  --  Op1 high bit is never going to be 1.
      case instr_addq.size is
         when data_byte =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_byte);
            begin
               op2 := self.get_ea(ea);
               sum := op1 + op2;
               if instr_addq.mode_y /= 1 then
                  self.set_ea(ea, sum and 16#FF#);
                  self.psw.zero := (sum and 16#FF#) = 0;
                  Rmsb := msb(sum);
                  Dmsb := msb(op2);
               end if;
               self.post_ea(ea);
            end;
         when data_word =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_word);
            begin
               op2 := self.get_ea(ea);
               sum := op1 + op2;
               if instr_addq.mode_y /= 1 then
                  self.set_ea(ea, sum and 16#FFFF#);
                  self.psw.zero := (sum and 16#FFFF#) = 0;
                  Rmsb := msb(sum);
                  Dmsb := msb(op2);
               end if;
               self.post_ea(ea);
            end;
         when data_long =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_long);
            begin
               op2 := self.get_ea(ea);
               sum := op1 + op2;
               if instr_addq.mode_y /= 1 then
                  self.set_ea(ea, sum);
                  self.psw.zero := (sum and 16#FFFF_FFFF#) = 0;
                  Rmsb := msb(sum);
                  Dmsb := msb(op2);
               end if;
               self.post_ea(ea);
            end;
         when others =>
            Ada.Text_IO.Put_Line("Invalid size for ADDQ instruction.");
      end case;
      if instr_addq.mode_y /= 1 then
         self.psw.negative := Rmsb;
         self.psw.Carry    := (Smsb and Dmsb) or ((not Rmsb) and Dmsb)
                           or (Smsb and (not Rmsb));
         self.psw.Extend   := self.psw.Carry;
         self.psw.Overflow := (Smsb and Dmsb and (not Rmsb))
                           or ((not Smsb) and (not Dmsb) and Rmsb);
      end if;
   end;
   --
   procedure decode_DBcc(self : in out m68000) is
      disp      : long;
      base_pc   : long := self.pc;
      condition : Boolean := False;
      reg_y     : reg_num := instr_dbcc.reg_y;
      reg_val   : word;
   begin
      Ada.Text_IO.Put_Line("DBcc group instruction encountered.");
      disp := sign_extend(self.get_ext);
      --
      --  Check conditions
      --
      case instr_dbcc.cond is
         when 0 =>  --  Always
            condition := True;
         when 1 =>  --  Never
            condition := False;
         when 2 =>  -- Hi (HI)
            condition := not self.psw.carry and self.psw.zero;
         when 3 =>  --  Low or same (LS)
            condition := self.psw.carry or self.psw.zero;
         when 4 =>  --  Carry clear (CC)
            condition := not self.psw.carry;
         when 5 =>  --  Carry set (CS)
            condition := self.psw.carry;
         when 6 =>  --  Not equal (NE)
            condition := not self.psw.zero;
         when 7 =>  --  Equal (EQ)
            condition := self.psw.zero;
         when 8 =>  --  Overflow clear (VC)
            condition := not self.psw.overflow;
         when 9 =>  --  Overflow set (VS)
            condition := self.psw.overflow;
         when 10 =>  --  Plus (PL)
             condition := not self.psw.negative;
         when 11 =>  --  Minus (MI)
            condition := self.psw.negative;
         when 12 =>  --  Greater or equal (GE)
            condition := (self.psw.negative and self.psw.overflow) or
                         (not self.psw.negative and not self.psw.overflow);
         when 13 =>  --  Less than (LT)
            condition := (self.psw.negative and not self.psw.overflow) or
                         (not self.psw.negative and self.psw.overflow);
         when 14 =>  --  Greater than (GT)
            condition := (self.psw.negative and self.psw.overflow and not self.psw.zero) or
                         (not self.psw.negative and not self.psw.overflow and not self.psw.zero);
         when 15 =>  --  Less or equal (LE)
            condition := (self.psw.zero) or
                         (self.psw.negative and not self.psw.overflow) or
                         (not self.psw.negative and self.psw.overflow);
      end case;
      --
      --  Perform the branch
      --
      if not condition then
         reg_val := self.get_regw(Data, reg_y) - 1;
         self.set_regw(Data, reg_y, reg_val);
         if reg_val /= 16#FFFF# then
            Ada.Text_IO.Put_Line("Performing branch.  Current PC is " & toHex(base_pc));
            Ada.Text_IO.Put_Line("  Displacement is " & toHex(disp) &
               ", destination is " & toHex(base_pc + disp));
            self.pc := base_pc + disp;
         end if;
      end if;
   end;
end;
