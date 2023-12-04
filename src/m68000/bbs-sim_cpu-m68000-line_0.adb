with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_0 is
   --
   --  Package for decoding Line 0 instructions -  - Bit manipulation/MOVEP/Immediate
   --
   procedure decode_0(self : in out m68000) is
     reg_y  : uint3 := instr_addi.reg_y;
     mode_y : uint3 := instr_addi.mode_y;
     op1    : long;
     op2    : long;
     sum    : long;
     Smsb   : Boolean;
     Dmsb   : Boolean;
     Rmsb   : Boolean;
   begin
      if instr_addi.code = 6 then  --  Add immediate instruction
         Ada.Text_IO.Put_Line("ADDI instruction encountered.");
         case instr_addi.size is
            when data_byte =>
               declare
                  ea : operand := self.get_ea(reg_y, mode_y, data_byte);
               begin
                  self.post_ea(reg_y, mode_y, data_byte);
                  op1 := long(self.get_ext and 16#FF#);
                  op2 := self.get_ea(ea, data_byte);
                  sum := op1 + op2;
                  self.set_ea(ea, sum and 16#FF#, data_byte);
                  self.psw.zero := (sum and 16#FF#) = 0;
                  self.psw.negative := (sum and 16#80#) = 16#80#;
                  Rmsb := (sum and 16#80#) = 16#80#;
                  Smsb := (op1 and 16#80#) = 16#80#;
                  Dmsb := (op2 and 16#80#) = 16#80#;
               end;
            when data_word =>
               declare
                  ea : operand := self.get_ea(reg_y, mode_y, data_word);
               begin
                  self.post_ea(reg_y, mode_y, data_word);
                  op1 := long(self.get_ext);
                  op2 := self.get_ea(ea, data_word);
                  sum := op1 + op2;
                  self.set_ea(ea, sum and 16#FFFF#, data_word);
                  self.psw.zero := (sum and 16#FFFF#) = 0;
                  self.psw.negative := (sum and 16#8000#) = 16#8000#;
                  Rmsb := (sum and 16#8000#) = 16#8000#;
                  Smsb := (op1 and 16#8000#) = 16#8000#;
                  Dmsb := (op2 and 16#8000#) = 16#8000#;
               end;
            when data_long =>
               declare
                  ea : operand := self.get_ea(reg_y, mode_y, data_long);
                  ext1 : long;
                  ext2 : long;
               begin
                  self.post_ea(reg_y, mode_y, data_long);
                  ext1 := long(self.get_ext);
                  ext2 := long(self.get_ext);
                  op1 := (ext1 and 16#FFFF#)*16#0001_0000# + ext2;
                  op2 := self.get_ea(ea, data_long);
                  sum := op1 + op2;
                  self.set_ea(ea, sum, data_long);
                  self.psw.zero := (sum and 16#FFFF_FFFF#) = 0;
                  self.psw.negative := (sum and 16#8000_0000#) = 16#8000_0000#;
                  Rmsb := (sum and 16#8000_0000#) = 16#8000_0000#;
                  Smsb := (op1 and 16#8000_0000#) = 16#8000_0000#;
                  Dmsb := (op2 and 16#8000_0000#) = 16#8000_0000#;
               end;
            when others =>
               Ada.Text_IO.Put_Line("Invalid size for ADDI instruction.");
         end case;
         self.psw.Carry := (Smsb and Dmsb) or ((not Rmsb) and Dmsb)
                        or (Smsb and (not Rmsb));
         self.psw.Extend := self.psw.Carry;
         self.psw.Overflow := (Smsb and Dmsb and (not Rmsb))
                           or ((not Smsb) and (not Dmsb) and Rmsb);
      end if;
   end;
   --
end;
