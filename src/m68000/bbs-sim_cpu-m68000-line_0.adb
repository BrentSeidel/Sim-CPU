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
      if (instr_imm.code = 2) and (instr_imm.size /= data_long_long) then  --  AND immediate instruction
         decode_ANDI(self);
      elsif (instr_imm.code = 6) and (instr_imm.size /= data_long_long) then  --  Add immediate instruction
         decode_ADDI(self);
      elsif (instr_imm.code = 16#A#) and (instr_imm.size /= data_long_long) then  --  Exclusive OR immediate instruction
         decode_EORI(self);
      elsif (instr_imm.code = 16#C#) and (instr_imm.size /= data_long_long) then  --  Compare immediate instruction
         decode_CMPI(self);
      elsif (instr_imm.code = 0) and (instr_imm.size /= data_long_long) then  --  OR immediate instruction
         decode_ORI(self);
      elsif (instr_imm.code = 4) and (instr_imm.size /= data_long_long) then  --  SUB immediate instruction
         decode_SUBI(self);
      elsif ((instr_movep.code = 1) and ((instr_movep.mode = 4) or
            (instr_movep.mode = 5) or (instr_movep.mode = 6) or (instr_movep.mode = 7))) then
         decode_MOVEP(self);
      elsif instr_bit.code = 4 or (instr_bit.code = 0 and instr_bit.reg_x = 4) then
         decode_BTST(self);
      elsif instr_bit.code = 5 or (instr_bit.code = 1 and instr_bit.reg_x = 4) then
         decode_BCHG(self);
      elsif instr_bit.code = 6 or (instr_bit.code = 2 and instr_bit.reg_x = 4) then
         decode_BCLR(self);
      elsif instr_bit.code = 7 or (instr_bit.code = 3 and instr_bit.reg_x = 4) then
         decode_BSET(self);
      else
         BBS.Sim_CPU.m68000.exceptions.process_exception(self,
            BBS.Sim_CPU.m68000.exceptions.ex_4_ill_inst);
      end if;
   end;
   --
   procedure decode_ADDI(self : in out m68000) is
      reg_y  : reg_num := instr_imm.reg_y;
      mode_y : mode_code := instr_imm.mode_y;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
      ext1   : word;
      ext2   : word;
   begin
--      Ada.Text_IO.Put_Line("Processing ADDI instruction.");
      ext1 := self.get_ext;
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            declare
               ea  : operand := self.get_ea(reg_y, mode_y, data_byte);
               op1 : byte;
               op2 : byte;
               sum : byte;
            begin
               op1 := byte(ext1 and 16#FF#);
               op2 := byte(self.get_ea(ea) and 16#FF#);
               sum := op1 + op2;
               Ada.Text_IO.Put_Line("  ADDI.B " & toHex(op1) & "," &
                  toHex(op2) & " = " & toHex(sum));
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
               op1 := ext1;
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
               op1 : long;
               op2 : long;
               sum : long;
            begin
               op1 := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
               op2 := self.get_ea(ea);
               sum := op1 + op2;
               self.set_ea(ea, sum);
               self.psw.zero := (sum = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when others =>  --  Should never happen due to previous conditions
            null;
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
      reg_y  : reg_num := instr_imm.reg_y;
      mode_y : mode_code := instr_imm.mode_y;
      ext1   : word;
      ext2   : word;
   begin
--      Ada.Text_IO.Put_Line("Processing ANDI instruction.");
      ext1 := self.get_ext;
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            if (mode_y = 7) and (reg_y = 4) then  --  ANDI to CCR
               declare
                 psw  : word := psw_to_word(self.psw);
                 mask : word := ext1 and 16#FF#;
               begin
                  mask := mask and psw;
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
                  op1 := byte(ext1 and 16#FF#);
                  op2 := byte(self.get_ea(ea) and 16#FF#);
                  sum := op1 and op2;
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum = 0);
                  self.psw.negative := msb(sum);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
                  self.post_ea(ea);
               end;
            end if;
         when data_word =>
            if (mode_y = 7) and (reg_y = 4) then  --  ANDI to SR
               declare
                 psw  : word := psw_to_word(self.psw);
               begin
                  if self.psw.super then
                     self.psw := word_to_psw(ext1 and psw);
                  else
                     BBS.Sim_CPU.m68000.exceptions.process_exception(self,
                        BBS.Sim_CPU.m68000.exceptions.ex_8_priv_viol);
                  end if;
               end;
            else
               declare
                  ea  : operand := self.get_ea(reg_y, mode_y, data_word);
                  op1 : word;
                  op2 : word;
                  sum : word;
               begin
                  op1 := ext1;
                  op2 := word(self.get_ea(ea) and 16#FFFF#);
                  sum := op1 and op2;
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum = 0);
                  self.psw.negative := msb(sum);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
                  self.post_ea(ea);
               end;
            end if;
         when data_long =>
            declare
               ea  : operand := self.get_ea(reg_y, mode_y, data_long);
               op1 : long;
               op2 : long;
               sum : long;
            begin
               op1 := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
               op2 := self.get_ea(ea);
               sum := op1 and op2;
               self.set_ea(ea, sum);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
               self.psw.Carry := False;
               self.psw.Overflow := False;
               self.post_ea(ea);
            end;
         when others =>  --  Should never happen due to previous conditions
            null;
      end case;
   end;
   --
   procedure decode_CMPI(self : in out m68000) is
      reg_y  : reg_num := instr_imm.reg_y;
      mode_y : mode_code := instr_imm.mode_y;
      ext1   : word;
      ext2   : word;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
--      Ada.Text_IO.Put_Line("Processing CMPI instruction.");
      ext1 := self.get_ext;
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            declare
               ea  : operand := self.get_ea(reg_y, mode_y, data_byte);
               op1 : byte;
               op2 : byte;
               sum : byte;
            begin
               op1 := byte(ext1 and 16#FF#);
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
               op1 := ext1;
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
               op1  : long;
               op2  : long;
               sum  : long;
            begin
               op1 := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
               op2 := self.get_ea(ea);
               sum := op2 - op1;
               self.psw.zero := (sum  = 0);
               Rmsb := msb(sum);
               Smsb := msb(op1);
               Dmsb := msb(op2);
               self.post_ea(ea);
            end;
         when others =>  --  Should never happen due to previous conditions
            null;
      end case;
      self.psw.negative := Rmsb;
      self.psw.overflow := ((not Smsb) and Dmsb and (not Rmsb)) or
                            (Smsb and (not Dmsb) and Rmsb);
      self.psw.carry := (Smsb and not Dmsb) or (Rmsb and not Dmsb) or (Smsb and Rmsb);
   end;
   --
   procedure decode_EORI(self : in out m68000) is
      reg_y  : reg_num := instr_imm.reg_y;
      mode_y : mode_code := instr_imm.mode_y;
      ext1   : word;
      ext2   : word;
   begin
--      Ada.Text_IO.Put_Line("Processing EORI instruction.");
      ext1 := self.get_ext;
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            if (mode_y = 7) and (reg_y = 4) then  --  EORI to CCR
               declare
                 psw  : word := psw_to_word(self.psw);
                 mask : word := ext1 and 16#FF#;
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
                  op1 := byte(ext1 and 16#FF#);
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
               begin
                  if not self.psw.super then
                     BBS.Sim_CPU.m68000.exceptions.process_exception(self,
                        BBS.Sim_CPU.m68000.exceptions.ex_8_priv_viol);
                  else
                     self.psw := word_to_psw(ext1 xor psw);
                  end if;
               end;
            else
               declare
                  ea  : operand := self.get_ea(reg_y, mode_y, data_word);
                  op1 : word;
                  op2 : word;
                  sum : word;
               begin
                  op1 := ext1;
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
               ea  : operand := self.get_ea(reg_y, mode_y, data_long);
               op1 : long;
               op2 : long;
               sum : long;
            begin
               op1 := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
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
   --
   procedure decode_ORI(self : in out m68000) is
      reg_y  : reg_num := instr_imm.reg_y;
      mode_y : mode_code := instr_imm.mode_y;
      ext1   : word;
      ext2   : word;
   begin
--      Ada.Text_IO.Put_Line("Processing ORI instruction.");
      ext1 := self.get_ext;
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            if (mode_y = 7) and (reg_y = 4) then  --  ORI to CCR
               declare
                 psw  : word := psw_to_word(self.psw);
                 mask : word := ext1 and 16#FF#;
               begin
                  mask := mask or psw;
                  self.psw := word_to_psw(mask);
               end;
            else
               declare
                  ea  : operand := self.get_ea(reg_y, mode_y, data_byte);
                  op1 : byte;
                  op2 : byte;
                  sum : byte;
               begin
                  op1 := byte(ext1 and 16#FF#);
                  op2 := byte(self.get_ea(ea) and 16#FF#);
                  sum := op1 or op2;
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum = 0);
                  self.psw.negative := msb(sum);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
                  self.post_ea(ea);
               end;
            end if;
         when data_word =>
            if (mode_y = 7) and (reg_y = 4) then  --  EORI to SR
               declare
                 psw  : word := psw_to_word(self.psw);
               begin
                  if not self.psw.super then
                     BBS.Sim_CPU.m68000.exceptions.process_exception(self,
                        BBS.Sim_CPU.m68000.exceptions.ex_8_priv_viol);
                  else
                     self.psw := word_to_psw(ext1 or psw);
                  end if;
               end;
            else
               declare
                  ea  : operand := self.get_ea(reg_y, mode_y, data_word);
                  op1 : word;
                  op2 : word;
                  sum : word;
               begin
                  op1 := ext1;
                  op2 := word(self.get_ea(ea) and 16#FFFF#);
                  sum := op1 or op2;
                  self.set_ea(ea, long(sum));
                  self.psw.zero := (sum and 16#FFFF#) = 0;
                  self.psw.negative := msb(sum);
                  self.psw.Carry := False;
                  self.psw.Overflow := False;
                  self.post_ea(ea);
               end;
            end if;
         when data_long =>
            declare
               ea  : operand := self.get_ea(reg_y, mode_y, data_long);
               op1 : long;
               op2 : long;
               sum : long;
            begin
               op1 := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
               op2 := self.get_ea(ea);
               sum := op1 or op2;
               self.set_ea(ea, sum);
               self.psw.zero := (sum = 0);
               self.psw.negative := msb(sum);
               self.psw.Carry := False;
               self.psw.Overflow := False;
               self.post_ea(ea);
            end;
         when others =>  --  Should never happen due to previous conditions
            null;
      end case;
   end;
   --
   procedure decode_SUBI(self : in out m68000) is
      reg_y  : reg_num := instr_imm.reg_y;
      mode_y : mode_code := instr_imm.mode_y;
      ext1   : word;
      ext2   : word;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
--      Ada.Text_IO.Put_Line("Processing SUBI instruction.");
      ext1 := self.get_ext;
      if instr_imm.size = data_long then
         ext2 := self.get_ext;
      end if;
      case instr_imm.size is
         when data_byte =>
            declare
               ea   : operand := self.get_ea(reg_y, mode_y, data_byte);
               src  : byte;
               dest : byte;
               diff : byte;
            begin
               src  := byte(ext1 and 16#FF#);
               dest := byte(self.get_ea(ea) and 16#FF#);
               diff := dest - src;
               self.set_ea(ea, long(diff));
               self.psw.zero := (diff = 0);
               Rmsb := msb(diff);
               Smsb := msb(src);
               Dmsb := msb(dest);
               self.post_ea(ea);
            end;
         when data_word =>
            declare
               ea   : operand := self.get_ea(reg_y, mode_y, data_word);
               src  : word;
               dest : word;
               diff : word;
            begin
               src  := ext1;
               dest := word(self.get_ea(ea) and 16#FFFF#);
               diff := dest - src;
               self.set_ea(ea, long(diff));
               self.psw.zero := (diff = 0);
               Rmsb := msb(diff);
               Smsb := msb(src);
               Dmsb := msb(dest);
               self.post_ea(ea);
            end;
         when data_long =>
            declare
               ea   : operand := self.get_ea(reg_y, mode_y, data_long);
               src  : long;
               dest : long;
               diff : long;
            begin
               src  := (long(ext1) and 16#FFFF#)*16#0001_0000# + long(ext2);
               dest := self.get_ea(ea);
               diff := dest - src;
               self.set_ea(ea, diff);
               self.psw.zero := (diff = 0);
               Rmsb := msb(diff);
               Smsb := msb(src);
               Dmsb := msb(dest);
               self.post_ea(ea);
            end;
         when others =>  --  Should never happen due to previous conditions
            null;
      end case;
      self.psw.negative := Rmsb;
      self.psw.Carry := (Smsb and (not Dmsb)) or (Rmsb and (not Dmsb))
                        or (Smsb and Rmsb);
      self.psw.Extend := self.psw.Carry;
      self.psw.Overflow := ((not Smsb) and Dmsb and (not Rmsb))
                           or (Smsb and (not Dmsb) and Rmsb);
   end;
   --
   --  Move to peripheral
   --
   procedure decode_MOVEP(self : in out m68000) is
      disp : long := sign_extend(self.get_ext);
      base : long := self.get_regl(Address, instr_movep.reg_y) + disp;
      mode : uint3 := instr_movep.mode;
      reg  : reg_num := instr_movep.reg_x;
      val  : long;
      temp : byte;
   begin
--      Ada.Text_IO.Put_Line("Processing MOVEP instruction.");
      case mode is
         when 4 =>  --  Transfer word from memory to register
            temp := self.memory(base);
            val := long(temp)*16#100#;
            temp := self.memory(base+2);
            val := val + long(temp);
            self.set_regw(Data, reg, word(val and 16#ffff#));
         when 5 =>  --  Transfer long from memory to register
            temp := self.memory(base);
            val  := long(temp)*16#0100_0000#;
            temp := self.memory(base + 2);
            val  := val + long(temp)*16#0001_0000#;
            temp := self.memory(base + 4);
            val  := val + long(temp)*16#0000_0100#;
            temp := self.memory(base + 6);
            val  := val + long(temp);
            self.set_regl(Data, reg, val);
         when 6 =>  --  Transfer word from register to memory
            val  := self.get_regl(Data, reg) and 16#ffff#;
            self.memory(base, byte((val/16#100#) and 16#ff#));
            self.memory(base + 2, byte(val and 16#ff#));
         when 7 =>  --  Transfer long from register to memory
            val  := self.get_regl(Data, reg);
            self.memory(base, byte((val/16#0100_0000#) and 16#ff#));
            self.memory(base+2, byte((val/16#0001_0000#) and 16#ff#));
            self.memory(base+4, byte((val/16#0000_0100#) and 16#ff#));
            self.memory(base+6, byte(val and 16#ff#));
         when others =>  --  Should never happen due to other conditions
            null;
      end case;
   end;
   --
   --  Bit instructions
   --
   procedure decode_BCHG(self : in out m68000) is
      bit_num : long;
      vall    : long;
   begin
--      Ada.Text_IO.Put_Line("Processing BCHG instruction.");
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
--      Ada.Text_IO.Put_Line("Processing BCLR instruction.");
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
--      Ada.Text_IO.Put_Line("Processing BSET instruction.");
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
   begin
--      Ada.Text_IO.Put_Line("Processing BTST instruction.");
      if instr_bit.code = 4 then  --  Bit number specified in register
         bit_num := self.get_regl(Data, instr_bit.reg_x);
      else  --  Bit number specified in next word
         bit_num := long(self.get_ext and 16#FF#);
      end if;
      --
      if instr_bit.mode_y = 0 then  --  Destination is a data register
         declare
            val    : long;
         begin
            bit_num := bit_num and 16#1F#;  --  32 bits in a long
            val := self.get_regl(Data, instr_bit.reg_y);
            self.psw.zero := (val and bit_pos(bit_num)) = 0;
         end;
      else  --  Destination is other
         declare
            ea   : operand := self.get_ea(instr_bit.reg_y, instr_bit.mode_y, data_byte);
            val : byte := byte(self.get_ea(ea) and 16#FF#);
         begin
            bit_num := bit_num and 16#07#;  --  8 bits in a byte
            self.psw.zero := (val and byte(bit_pos(bit_num) and 16#FF#)) = 0;
            self.post_ea(ea);
         end;
      end if;
   end;
end;
