with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_d is
   --
   --  Package for decoding Line D (13) instructions - ADD/ADDX
   --
   procedure decode_d(self : in out m68000) is
     reg_x  : reg_num := instr_add.reg_x;
     reg_y  : reg_num := instr_add.reg_y;
     mode_y : reg_num := instr_add.mode_y;
     opmode : reg_num := instr_add.opmode;
     op1    : long;
     op2    : long;
   begin
      Ada.Text_IO.Put_Line("ADD instruction with mode " & reg_num'Image(opmode));
      case opmode is
        when 0 =>  --  Byte <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              self.post_ea(reg_y, mode_y, data_byte);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_byte);
              self.set_reg(Data, reg_x, (op1 + op2) and 16#FF#);
           end;
        when 1 =>  --  Word <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              self.post_ea(reg_y, mode_y, data_word);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_word);
              self.set_reg(Data, reg_x, (op1 + op2) and 16#FFFF#);
           end;
        when 2 =>  --  Long <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              self.post_ea(reg_y, mode_y, data_long);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_long);
              self.set_reg(Data, reg_x, op1 + op2);
           end;
        when 3 =>  --  Word <ea> + An -> An
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              self.post_ea(reg_y, mode_y, data_word);
              op1 := self.get_reg(Address, reg_x);
              op2 := self.get_ea(ea, data_word);
              self.set_reg(Address, reg_x, (op1 + op2) and 16#FFFF#);
           end;
        when 4 =>  --  Byte Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              self.post_ea(reg_y, mode_y, data_byte);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_byte);
              self.set_ea(ea, (op1 + op2) and 16#FF#, data_byte);
           end;
        when 5 =>  --  Word Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              self.post_ea(reg_y, mode_y, data_word);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_word);
              self.set_ea(ea, (op1 + op2) and 16#FFFF#, data_word);
           end;
        when 6 =>  --  Long Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              self.post_ea(reg_y, mode_y, data_long);
              op1 := self.get_reg(Data, reg_x);
              op2 := self.get_ea(ea, data_long);
              self.set_ea(ea, op1 + op2, data_long);
           end;
        when 7 =>  --  Word <ea> + An -> An
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              self.post_ea(reg_y, mode_y, data_long);
              op1 := self.get_reg(Address, reg_x);
              op2 := self.get_ea(ea, data_long);
              self.set_reg(Address, reg_x, op1 + op2);
           end;
        when others =>
           Ada.Text_IO.Put_Line("Unrecognized Opmode in ADD.");
      end case;
   end;
   --
end;
