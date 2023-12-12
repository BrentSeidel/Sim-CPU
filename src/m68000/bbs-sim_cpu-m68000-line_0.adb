with Ada.Unchecked_Conversion;
with Ada.Text_IO;
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
            uint3'Image(instr_bit.code) & ", reg = " & uint3'Image(instr_bit.reg_x));
      end if;
   end;
   --
   procedure decode_ADDI(self : in out m68000) is
     reg_y  : uint3 := instr_addi.reg_y;
     mode_y : uint3 := instr_addi.mode_y;
     op1    : long;
     op2    : long;
     sum    : long;
     Smsb   : Boolean;
     Dmsb   : Boolean;
     Rmsb   : Boolean;
   begin
      Ada.Text_IO.Put_Line("ADDI instruction encountered.");
      case instr_addi.size is
         when data_byte =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_byte);
            begin
               op1 := long(self.get_ext and 16#FF#);
               op2 := self.get_ea(ea, data_byte);
               sum := op1 + op2;
               self.set_ea(ea, sum and 16#FF#, data_byte);
               self.psw.zero := (sum and 16#FF#) = 0;
               self.psw.negative := (sum and 16#80#) = 16#80#;
               Rmsb := (sum and 16#80#) = 16#80#;
               Smsb := (op1 and 16#80#) = 16#80#;
               Dmsb := (op2 and 16#80#) = 16#80#;
               self.post_ea(reg_y, mode_y, data_byte);
            end;
         when data_word =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_word);
            begin
               op1 := long(self.get_ext);
               op2 := self.get_ea(ea, data_word);
               sum := op1 + op2;
               self.set_ea(ea, sum and 16#FFFF#, data_word);
               self.psw.zero := (sum and 16#FFFF#) = 0;
               self.psw.negative := (sum and 16#8000#) = 16#8000#;
               Rmsb := (sum and 16#8000#) = 16#8000#;
               Smsb := (op1 and 16#8000#) = 16#8000#;
               Dmsb := (op2 and 16#8000#) = 16#8000#;
               self.post_ea(reg_y, mode_y, data_word);
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
               op2 := self.get_ea(ea, data_long);
               sum := op1 + op2;
               self.set_ea(ea, sum, data_long);
               self.psw.zero := (sum and 16#FFFF_FFFF#) = 0;
               self.psw.negative := (sum and 16#8000_0000#) = 16#8000_0000#;
               Rmsb := (sum and 16#8000_0000#) = 16#8000_0000#;
               Smsb := (op1 and 16#8000_0000#) = 16#8000_0000#;
               Dmsb := (op2 and 16#8000_0000#) = 16#8000_0000#;
               self.post_ea(reg_y, mode_y, data_long);
            end;
         when others =>
            Ada.Text_IO.Put_Line("Invalid size for ADDI instruction.");
      end case;
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
                  op2 := self.get_ea(ea, data_byte);
                  sum := op1 and op2;
                  self.set_ea(ea, sum and 16#FF#, data_byte);
                  self.psw.zero := (sum and 16#FF#) = 0;
                  self.psw.negative := (sum and 16#80#) = 16#80#;
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
                  self.post_ea(reg_y, mode_y, data_byte);
               end;
            end if;
         when data_word =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_word);
            begin
               op1 := long(self.get_ext);
               op2 := self.get_ea(ea, data_word);
               sum := op1 and op2;
               self.set_ea(ea, sum and 16#FFFF#, data_word);
               self.psw.zero := (sum and 16#FFFF#) = 0;
               self.psw.negative := (sum and 16#8000#) = 16#8000#;
               self.psw.Carry := False;
               self.psw.Overflow := False;
               self.post_ea(reg_y, mode_y, data_word);
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
               op2 := self.get_ea(ea, data_long);
               sum := op1 and op2;
               self.set_ea(ea, sum, data_long);
               self.psw.zero := (sum and 16#FFFF_FFFF#) = 0;
               self.psw.negative := (sum and 16#8000_0000#) = 16#8000_0000#;
               self.psw.Carry := False;
               self.psw.Overflow := False;
               self.post_ea(reg_y, mode_y, data_long);
            end;
         when others =>
            Ada.Text_IO.Put_Line("Invalid size for ANDI instruction.");
      end case;
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
            valb : byte := byte(self.get_ea(ea, data_byte));
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (valb and byte(bit_pos(bit_num))) = 0;
            valb := valb xor byte(bit_pos(bit_num));
            self.set_ea(ea, long(valb), data_byte);
            self.post_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
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
            valb : byte := byte(self.get_ea(ea, data_byte));
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (valb and byte(bit_pos(bit_num))) = 0;
            valb := valb and not byte(bit_pos(bit_num));
            self.set_ea(ea, long(valb), data_byte);
            self.post_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
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
            valb : byte := byte(self.get_ea(ea, data_byte));
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (valb and byte(bit_pos(bit_num))) = 0;
            valb := valb or byte(bit_pos(bit_num));
            self.set_ea(ea, long(valb), data_byte);
            self.post_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
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
            valb : byte := byte(self.get_ea(ea, data_byte));
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (valb and byte(bit_pos(bit_num))) = 0;
            self.post_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
         end;
      end if;
   end;
end;
