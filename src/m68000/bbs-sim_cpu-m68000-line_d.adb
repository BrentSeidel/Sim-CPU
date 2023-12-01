with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_d is
   --
   --  Package for decoding Line D (13) instructions - ADD/ADDX
   --
   procedure decode_d(self : in out m68000) is
     reg_x  : uint3 := instr_add.reg_x;
     reg_y  : uint3 := instr_add.reg_y;
     mode_y : uint3 := instr_add.mode_y;
     opmode : uint3 := instr_add.opmode;
     op1    : long;
     op2    : long;
     sum    : long;
     Smsb   : Boolean;
     Dmsb   : Boolean;
     Rmsb   : Boolean;
   begin
      Ada.Text_IO.Put_Line("ADD instruction with mode " & uint3'Image(opmode));
      case opmode is
        when 0 =>  --  Byte <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              self.post_ea(reg_y, mode_y, data_byte);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_byte);
              sum := op1 + op2;
              self.set_reg(Data, reg_x, sum and 16#FF#);
           end;
        when 1 =>  --  Word <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              self.post_ea(reg_y, mode_y, data_word);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_word);
              sum := op1 + op2;
              self.set_reg(Data, reg_x, sum and 16#FFFF#);
           end;
        when 2 =>  --  Long <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              self.post_ea(reg_y, mode_y, data_long);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_long);
              sum := op1 + op2;
              self.set_reg(Data, reg_x, sum);
           end;
        when 3 =>  --  Word <ea> + An -> An
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              self.post_ea(reg_y, mode_y, data_word);
              op1 := self.get_reg(Address, reg_x);
              op2 := self.get_ea(ea, data_word);
              sum := op1 + op2;
              self.set_reg(Address, reg_x, sum and 16#FFFF#);
           end;
        when 4 =>  --  Byte Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              self.post_ea(reg_y, mode_y, data_byte);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_byte);
              sum := op1 + op2;
              self.set_ea(ea, sum and 16#FF#, data_byte);
           end;
        when 5 =>  --  Word Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              self.post_ea(reg_y, mode_y, data_word);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_word);
              sum := op1 + op2;
              self.set_ea(ea, sum and 16#FFFF#, data_word);
           end;
        when 6 =>  --  Long Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              self.post_ea(reg_y, mode_y, data_long);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_long);
              sum := op1 + op2;
              self.set_ea(ea, sum, data_long);
           end;
        when 7 =>  --  Long <ea> + An -> An
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              self.post_ea(reg_y, mode_y, data_long);
              op1 := self.get_reg(Address, reg_x);
              op2 := self.get_ea(ea, data_long);
              sum := op1 + op2;
              self.set_reg(Address, reg_x, sum);
           end;
      end case;
      --
      --  Compute condition codes
      --
      case opmode is
         when 0 =>  --  Byte size
            if (sum and 16#FF#) = 0 then
               self.psw.zero := True;
            else
               self.psw.zero := False;
            end if;
            if (sum and 16#80#) = 16#80# then
               self.psw.negative := True;
               Rmsb := True;
            else
               self.psw.negative := False;
               Rmsb := False;
            end if;
            if (op1 and 16#80#) = 16#80# then
               Smsb := True;
            else
               Smsb := False;
            end if;
            if (op2 and 16#80#) = 16#80# then
               Dmsb := True;
            else
               Dmsb := False;
            end if;
         when 1 | 5 =>  --  Word size
            if (sum and 16#FFFF#) = 0 then
               self.psw.zero := True;
            else
               self.psw.zero := False;
            end if;
            if (sum and 16#8000#) = 16#8000# then
               self.psw.negative := True;
               Rmsb := True;
            else
               self.psw.negative := False;
               Rmsb := False;
            end if;
            if (op1 and 16#8000#) = 16#8000# then
               Smsb := True;
            else
               Smsb := False;
            end if;
            if (op2 and 16#8000#) = 16#8000# then
               Dmsb := True;
            else
               Dmsb := False;
            end if;
         when 2 | 6 =>  --  Long size
            if (sum and 16#FFFF_FFFF#) = 0 then
               self.psw.zero := True;
            else
               self.psw.zero := False;
            end if;
            if (sum and 16#8000_0000#) = 16#8000_0000# then
               self.psw.negative := True;
               Rmsb := True;
            else
               self.psw.negative := False;
               Rmsb := False;
            end if;
            if (op1 and 16#8000_0000#) = 16#8000_0000# then
               Smsb := True;
            else
               Smsb := False;
            end if;
            if (op2 and 16#8000_0000#) = 16#8000_0000# then
               Dmsb := True;
            else
               Dmsb := False;
            end if;
         when others =>  --  Modes 3 & 7 do not affect condition codes
            null;
      end case;
      --
      --  Carry, Extend, and Overflow
      --
      if (opmode /= 3) and (opmode /= 7) then
         self.psw.Carry := (Smsb and Dmsb) or ((not Rmsb) and Dmsb)
                        or (Smsb and (not Rmsb));
         self.psw.Extend := self.psw.Carry;
         self.psw.Overflow := (Smsb and Dmsb and (not Rmsb))
                           or ((not Smsb) and (not Dmsb) and Rmsb);
      end if;
   end;
   --
end;
