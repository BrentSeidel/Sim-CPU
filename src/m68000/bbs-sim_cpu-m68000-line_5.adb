with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_5 is
   --
   --  Package for decoding Group 5 - ADDQ/SUBQ/Scc/DBcc/TRAPcc
   --
   procedure decode_5(self : in out m68000) is
     reg_y  : uint3 := instr_addq.reg_y;
     mode_y : uint3 := instr_addq.mode_y;
     op1    : long;
     op2    : long;
     sum    : long;
     Smsb   : Boolean;
     Dmsb   : Boolean;
     Rmsb   : Boolean;
   begin
      if instr_addq.code = False then  --  Add quick instruction
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
                  self.post_ea(reg_y, mode_y, data_byte);
                  op2 := self.get_ea(ea, data_byte);
                  sum := op1 + op2;
                  if instr_addq.mode_y /= 1 then
                     self.set_ea(ea, sum and 16#FF#, data_byte);
                     self.psw.zero := (sum and 16#FF#) = 0;
                     self.psw.negative := (sum and 16#80#) = 16#80#;
                     Rmsb := (sum and 16#80#) = 16#80#;
                     Dmsb := (op2 and 16#80#) = 16#80#;
                  end if;
               end;
            when data_word =>
               declare
                  ea : operand := self.get_ea(reg_y, mode_y, data_word);
               begin
                  self.post_ea(reg_y, mode_y, data_word);
                  op2 := self.get_ea(ea, data_word);
                  sum := op1 + op2;
                  if instr_addq.mode_y /= 1 then
                     self.set_ea(ea, sum and 16#FFFF#, data_word);
                     self.psw.zero := (sum and 16#FFFF#) = 0;
                     self.psw.negative := (sum and 16#8000#) = 16#8000#;
                     Rmsb := (sum and 16#8000#) = 16#8000#;
                     Dmsb := (op2 and 16#8000#) = 16#8000#;
                  end if;
               end;
            when data_long =>
               declare
                  ea : operand := self.get_ea(reg_y, mode_y, data_long);
               begin
                  self.post_ea(reg_y, mode_y, data_long);
                  op2 := self.get_ea(ea, data_long);
                  sum := op1 + op2;
                  if instr_addq.mode_y /= 1 then
                     self.set_ea(ea, sum, data_long);
                     self.psw.zero := (sum and 16#FFFF_FFFF#) = 0;
                     self.psw.negative := (sum and 16#8000_0000#) = 16#8000_0000#;
                     Rmsb := (sum and 16#8000_0000#) = 16#8000_0000#;
                     Dmsb := (op2 and 16#8000_0000#) = 16#8000_0000#;
                  end if;
               end;
            when others =>
               Ada.Text_IO.Put_Line("Invalid size for ADDI instruction.");
         end case;
         if instr_addq.mode_y /= 1 then
            self.psw.Carry    := (Smsb and Dmsb) or ((not Rmsb) and Dmsb)
                              or (Smsb and (not Rmsb));
            self.psw.Extend   := self.psw.Carry;
            self.psw.Overflow := (Smsb and Dmsb and (not Rmsb))
                              or ((not Smsb) and (not Dmsb) and Rmsb);
         end if;
      end if;
   end;
   --
end;
