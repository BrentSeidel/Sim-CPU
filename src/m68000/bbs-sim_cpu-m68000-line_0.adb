with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with BBS.Sim_CPU.m68000.exceptions;
package body BBS.Sim_CPU.m68000.line_0 is
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                           target => status_word);
   --
   --  Package for decoding Line 0 instructions -  - Bit manipulation/MOVEP/Immediate
   --
   procedure decode_0(self : in out m68000) is
   begin
      if instr_addi.code = 2 then  --  And immediate instruction
         decode_ANDI(self);
      elsif instr_addi.code = 6 then  --  Add immediate instruction
         decode_ADDI(self);
      elsif instr_addi.code = 16#A# then  --  Exclusive OR immediate instruction
         decode_EORI(self);
      elsif instr_addi.code = 16#C# then  --  Compare immediate instruction
         decode_CMPI(self);
      elsif instr_bit.code = 4 or (instr_bit.code = 0 and instr_bit.reg_x = 4) then
         decode_BTST(self);
      elsif instr_bit.code = 5 or (instr_bit.code = 1 and instr_bit.reg_x = 4) then
         decode_BCHG(self);
      elsif instr_bit.code = 6 or (instr_bit.code = 2 and instr_bit.reg_x = 4) then
         decode_BCLR(self);
      elsif instr_bit.code = 7 or (instr_bit.code = 3 and instr_bit.reg_x = 4) then
         decode_BSET(self);
      else
         Ada.Text_IO.Put_Line("Unrecognied line 0 instruction, bit code = " &
            uint3'Image(instr_bit.code) & ", reg = " & uint3'Image(instr_bit.reg_x) &
            ", or AND/ADD/EOR code " & uint4'Image(instr_addi.code));
      end if;
   end;
   --
   procedure decode_ADDI(self : in out m68000) is
      reg_y  : uint3 := instr_addi.reg_y;
      mode_y : uint3 := instr_addi.mode_y;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
      Ada.Text_IO.Put_Line("ADDI instruction encountered.");
      case instr_addi.size is
         when data_byte =>
            declare
               ea  : operand := self.get_ea(reg_y, mode_y, data_byte);
               op1 : byte;
               op2 : byte;
               sum : byte;
            begin
               op1 := byte(self.get_ext and 16#FF#);
               op2 := byte(self.get_ea(ea) and 16#FF#);
               sum := op1 + op2;
               self.set_ea(ea, long(sum));
               self.psw.zero := (sum = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when data_word =>
            declare
               ea  : operand := self.get_ea(reg_y, mode_y, data_word);
               op1 : word;
               op2 : word;
               sum : word;
            begin
               op1 := self.get_ext;
               op2 := word(self.get_ea(ea) and 16#FFFF#);
               sum := op1 + op2;
               self.set_ea(ea, long(sum));
               self.psw.zero := (sum = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when data_long =>
            declare
               ea  : operand := self.get_ea(reg_y, mode_y, data_long);
               ext1 : long;
               ext2 : long;
               op1 : long;
               op2 : long;
               sum : long;
            begin
               ext1 := long(self.get_ext);
               ext2 := long(self.get_ext);
               op1 := (ext1 and 16#FFFF#)*16#0001_0000# + ext2;
               op2 := self.get_ea(ea);
               sum := op1 + op2;
               self.set_ea(ea, sum);
               self.psw.zero := (sum = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when others =>
            Ada.Text_IO.Put_Line("Invalid size for ADDI instruction.");
      end case;
      self.psw.negative := Rmsb;
      self.psw.Carry := (Smsb and Dmsb) or ((not Rmsb) and Dmsb)
                        or (Smsb and (not Rmsb));
      self.psw.Extend := self.psw.Carry;
      self.psw.Overflow := (Smsb and Dmsb and (not Rmsb))
                           or ((not Smsb) and (not Dmsb) and Rmsb);
   end;
   --
   procedure decode_ANDI(self : in out m68000) is
     reg_y  : uint3 := instr_addi.reg_y;
     mode_y : uint3 := instr_addi.mode_y;
     op1    : long;
     op2    : long;
     sum    : long;
   begin
      Ada.Text_IO.Put_Line("ANDI instruction encountered.");
      case instr_addi.size is
         when data_byte =>
            if (mode_y = 7) and (reg_y = 4) then  --  ANDI to CCR
               declare
                 psw  : word := psw_to_word(self.psw);
                 mask : word := self.get_ext and 16#FF#;
               begin
                  mask := mask and psw;
                  mask := mask or (psw and 16#FF00#);
                  self.psw := word_to_psw(mask);
               end;
            else
               declare
                  ea : operand := self.get_ea(reg_y, mode_y, data_byte);
               begin
                  op1 := long(self.get_ext and 16#FF#);
                  op2 := self.get_ea(ea);
                  sum := op1 and op2;
                  self.set_ea(ea, sum and 16#FF#);
                  self.psw.zero := (sum and 16#FF#) = 0;
                  self.psw.negative := msb(sum);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
                  self.post_ea(ea);
               end;
            end if;
         when data_word =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_word);
            begin
               op1 := long(self.get_ext);
               op2 := self.get_ea(ea);
               sum := op1 and op2;
               self.set_ea(ea, sum and 16#FFFF#);
               self.psw.zero := (sum and 16#FFFF#) = 0;
               self.psw.negative := msb(sum);
               self.psw.Carry := False;
               self.psw.Overflow := False;
               self.post_ea(ea);
            end;
         when data_long =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_long);
               ext1 : long;
               ext2 : long;
            begin
               ext1 := long(self.get_ext);
               ext2 := long(self.get_ext);
               op1 := (ext1 and 16#FFFF#)*16#0001_0000# + ext2;
               op2 := self.get_ea(ea);
               sum := op1 and op2;
               self.set_ea(ea, sum);
               self.psw.zero := (sum and 16#FFFF_FFFF#) = 0;
               self.psw.negative := msb(sum);
               self.psw.Carry := False;
               self.psw.Overflow := False;
               self.post_ea(ea);
            end;
         when others =>
            Ada.Text_IO.Put_Line("Invalid size for ANDI instruction.");
      end case;
   end;
   --
   procedure decode_CMPI(self : in out m68000) is
      reg_y  : uint3 := instr_addi.reg_y;
      mode_y : uint3 := instr_addi.mode_y;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
      Ada.Text_IO.Put_Line("CMPI instruction encountered.");
      case instr_addi.size is
         when data_byte =>
            declare
               ea  : operand := self.get_ea(reg_y, mode_y, data_byte);
               op1 : byte;
               op2 : byte;
               sum : byte;
            begin
               op1 := byte(self.get_ext and 16#FF#);
               op2 := byte(self.get_ea(ea));
               sum := op2 - op1;
               self.psw.zero := (sum  = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when data_word =>
            declare
               ea  : operand := self.get_ea(reg_y, mode_y, data_word);
               op1 : word;
               op2 : word;
               sum : word;
            begin
               op1 := word(self.get_ext);
               op2 := word(self.get_ea(ea));
               sum := op2 - op1;
               self.psw.zero := (sum = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when data_long =>
            declare
               ea   : operand := self.get_ea(reg_y, mode_y, data_long);
               ext1 : long;
               ext2 : long;
               op1  : long;
               op2  : long;
               sum  : long;
            begin
               ext1 := long(self.get_ext);
               ext2 := long(self.get_ext);
               op1 := (ext1 and 16#FFFF#)*16#0001_0000# + ext2;
               op2 := self.get_ea(ea);
               sum := op2 - op1;
               self.psw.zero := (sum  = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when others =>
            Ada.Text_IO.Put_Line("Invalid size for CMPI instruction.");
      end case;
      self.psw.negative := Rmsb;
      self.psw.overflow := ((not Smsb) and Dmsb and (not Rmsb)) or
                            (Smsb and (not Dmsb) and Rmsb);
      self.psw.carry := (Smsb and not Dmsb) or (Rmsb and not Dmsb) or (Smsb and Rmsb);
   end;
   --
   --  Bit instructions
   --
   procedure decode_BCHG(self : in out m68000) is
      bit_num : long;
      vall    : long;
   begin
      Ada.Text_IO.Put_Line("Executing BCHG instruction");
      if instr_bit.code = 5 then  --  Bit number specified in register
         bit_num := self.get_regl(Data, instr_bit.reg_x);
      else  --  Bit number specified in next word
         bit_num := long(self.get_ext and 16#FF#);
      end if;
      if instr_bit.mode_y = 0 then  --  Destination is a data register
         bit_num := bit_num and 16#1F#;  --  32 bits in a long
         vall := self.get_regl(Data, instr_bit.reg_y);
         self.psw.zero := (vall and bit_pos(bit_num)) = 0;
         vall := vall xor bit_pos(bit_num);
         self.set_regl(Data, instr_bit.reg_y, vall);
      else  --  Destination is other
         declare
            ea   : operand := self.get_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
            valb : byte := byte(self.get_ea(ea));
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (valb and byte(bit_pos(bit_num))) = 0;
            valb := valb xor byte(bit_pos(bit_num));
            self.set_ea(ea, long(valb));
            self.post_ea(ea);
         end;
      end if;
   end;
   --
   procedure decode_BCLR(self : in out m68000) is
      bit_num : long;
      vall    : long;
   begin
      Ada.Text_IO.Put_Line("Executing BCLR instruction");
      if instr_bit.code = 6 then  --  Bit number specified in register
         bit_num := self.get_regl(Data, instr_bit.reg_x);
      else  --  Bit number specified in next word
         bit_num := long(self.get_ext and 16#FF#);
      end if;
      if instr_bit.mode_y = 0 then  --  Destination is a data register
         bit_num := bit_num and 16#1F#;  --  32 bits in a long
         vall := self.get_regl(Data, instr_bit.reg_y);
         self.psw.zero := (vall and bit_pos(bit_num)) = 0;
         vall := vall and not bit_pos(bit_num);
         self.set_regl(Data, instr_bit.reg_y, vall);
      else  --  Destination is other
         declare
            ea   : operand := self.get_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
            valb : byte := byte(self.get_ea(ea));
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (valb and byte(bit_pos(bit_num))) = 0;
            valb := valb and not byte(bit_pos(bit_num));
            self.set_ea(ea, long(valb));
            self.post_ea(ea);
         end;
      end if;
   end;
   --
   procedure decode_BSET(self : in out m68000) is
      bit_num : long;
      vall    : long;
   begin
      Ada.Text_IO.Put_Line("Executing BSET instruction");
      if instr_bit.code = 7 then  --  Bit number specified in register
         bit_num := self.get_regl(Data, instr_bit.reg_x);
      else  --  Bit number specified in next word
         bit_num := long(self.get_ext and 16#FF#);
      end if;
      if instr_bit.mode_y = 0 then  --  Destination is a data register
         bit_num := bit_num and 16#1F#;  --  32 bits in a long
         vall := self.get_regl(Data, instr_bit.reg_y);
         self.psw.zero := (vall and bit_pos(bit_num)) = 0;
         vall := vall or bit_pos(bit_num);
         self.set_regl(Data, instr_bit.reg_y, vall);
      else  --  Destination is other
         declare
            ea   : operand := self.get_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
            valb : byte := byte(self.get_ea(ea));
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (valb and byte(bit_pos(bit_num))) = 0;
            valb := valb or byte(bit_pos(bit_num));
            self.set_ea(ea, long(valb));
            self.post_ea(ea);
         end;
      end if;
   end;
   --
   procedure decode_BTST(self : in out m68000) is
      bit_num : long;
      vall    : long;
   begin
      Ada.Text_IO.Put_Line("Executing BTST instruction");
      if instr_bit.code = 4 then  --  Bit number specified in register
         bit_num := self.get_regl(Data, instr_bit.reg_x);
      else  --  Bit number specified in next word
         bit_num := long(self.get_ext and 16#FF#);
      end if;
      --
      if instr_bit.mode_y = 0 then  --  Destination is a data register
         bit_num := bit_num and 16#1F#;  --  32 bits in a long
         vall := self.get_regl(Data, instr_bit.reg_y);
         self.psw.zero := (vall and bit_pos(bit_num)) = 0;
      else  --  Destination is other
         declare
            ea   : operand := self.get_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
            valb : byte := byte(self.get_ea(ea));
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (valb and byte(bit_pos(bit_num))) = 0;
            self.post_ea(ea);
         end;
      end if;
   end;
   --
   procedure decode_EORI(self : in out m68000) is
      reg_y  : uint3 := instr_addi.reg_y;
      mode_y : uint3 := instr_addi.mode_y;
   begin
      Ada.Text_IO.Put_Line("EORI instruction encountered.");
      case instr_addi.size is
         when data_byte =>
            if (mode_y = 7) and (reg_y = 4) then  --  EORI to CCR
               declare
                 psw  : word := psw_to_word(self.psw);
                 mask : word := self.get_ext and 16#FF#;
               begin
                  mask := mask xor psw;
                  mask := mask or (psw and 16#FF00#);
                  self.psw := word_to_psw(mask);
               end;
            else
               declare
                  ea  : operand := self.get_ea(reg_y, mode_y, data_byte);
                  op1 : byte;
                  op2 : byte;
                  sum : byte;
               begin
                  op1 := byte(self.get_ext and 16#FF#);
                  op2 := byte(self.get_ea(ea) and 16#FF#);
                  sum := op1 xor op2;
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum = 0);
                  self.psw.negative := msb(sum);
                  self.post_ea(ea);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
               end;
            end if;
         when data_word =>
            if (mode_y = 7) and (reg_y = 4) then  --  EORI to SR
               declare
                 psw  : word := psw_to_word(self.psw);
                 mask : word := self.get_ext;
               begin
                  if not self.psw.super then
                     BBS.Sim_CPU.m68000.exceptions.process_exception(self,
                        BBS.Sim_CPU.m68000.exceptions.ex_8_priv_viol);
                  else
                     self.psw := word_to_psw(mask xor psw);
                  end if;
               end;
            else
               declare
                  ea  : operand := self.get_ea(reg_y, mode_y, data_word);
                  op1 : word;
                  op2 : word;
                  sum : word;
               begin
                  op1 := self.get_ext;
                  op2 := word(self.get_ea(ea) and 16#FFFF#);
                  sum := op1 xor op2;
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum  = 0);
                  self.psw.negative := msb(sum);
                  self.post_ea(ea);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
               end;
            end if;
         when data_long =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_long);
               ext1 : long;
               ext2 : long;
               op1 : long;
               op2 : long;
               sum : long;
            begin
               ext1 := long(self.get_ext);
               ext2 := long(self.get_ext);
               op1 := (ext1 and 16#FFFF#)*16#0001_0000# + ext2;
               op2 := self.get_ea(ea);
               sum := op1 xor op2;
               self.set_ea(ea, sum);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
               self.post_ea(ea);
               self.psw.Carry := False;
               self.psw.Overflow := False;
            end;
         when others =>
            Ada.Text_IO.Put_Line("Invalid size for ADDI instruction.");
      end case;
   end;
end;
