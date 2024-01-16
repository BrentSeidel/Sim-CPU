with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS.embed;
use type BBS.embed.int32;
with BBS.Sim_CPU.m68000.exceptions;
package body BBS.Sim_CPU.m68000.line_8 is
   function uint32_to_int32 is new Ada.Unchecked_Conversion(source => long,
                                                           target => BBS.embed.int32);
   function int32_to_uint32 is new Ada.Unchecked_Conversion(source => BBS.embed.int32,
                                                           target => long);
   --
   --  Package for decoding Group 8 - OR/DIV/SBCD
   --
   procedure decode_8(self : in out m68000) is
   begin
      if instr_2op.code = 7 then  --  DIVS instructions
         decode_DIVS(self);
      elsif instr_sbcd.code = 16#10# then
         decode_SBCD(self);
      elsif instr_2op.code = 3 then  --  DIVU instruction
         decode_DIVU(self);
      elsif (instr_2op.code = 0) or (instr_2op.code = 1) or (instr_2op.code = 2) or
            (instr_2op.code = 4) or (instr_2op.code = 5) or (instr_2op.code = 6) then
         decode_OR(self);
      end if;
   end;
   --
   procedure decode_DIVS(self : in out m68000) is
      reg_y  : reg_num := instr_2op.reg_y;
      mode_y : mode_code := instr_2op.mode_y;
      reg_x  : reg_num := instr_2op.reg_x;
      ea     : operand := self.get_ea(reg_y, mode_y, data_word);
      op1    : BBS.embed.int32 := uint32_to_int32(self.get_regl(Data, reg_x));
      op2    : BBS.embed.int32 := uint32_to_int32(sign_extend(word(self.get_ea(ea) and 16#FFFF#)));
      result : BBS.embed.int32;
      remain : BBS.embed.int32;
      resl   : long;
      reml   : long;
   begin
      self.post_ea(ea);
      Ada.Text_IO.Put_Line("Processing DIVS instruction.");
      if op2 = 0 then  --  Divide by 0 exception
         BBS.Sim_CPU.m68000.exceptions.process_exception(self,
               BBS.Sim_CPU.m68000.exceptions.ex_5_div0);
         return;
      end if;
      result := op1/op2;
      remain := op1 rem op2;
      resl := int32_to_uint32(result);
      reml := int32_to_uint32(remain);
      self.psw.carry := False;
      self.psw.negative := (result < 0);
      if (resl /= sign_extend(word(resl and 16#FFFF#))) then
         self.psw.overflow := True;
         self.psw.zero := (op1 = 0);
      else
         self.psw.overflow := False;
         self.psw.zero := (result = 0);
         resl := resl and 16#FFFF#;
         resl := resl or ((reml and 16#FFFF#)*16#0001_0000#);
         self.set_regl(data, reg_x, resl);
      end if;
   end;
   --
   procedure decode_DIVU(self : in out m68000) is
      reg_y  : reg_num := instr_2op.reg_y;
      mode_y : mode_code := instr_2op.mode_y;
      reg_x  : reg_num := instr_2op.reg_x;
      ea     : operand := self.get_ea(reg_y, mode_y, data_word);
      op1    : long := self.get_regl(Data, reg_x);
      op2    : long := long(word(self.get_ea(ea) and 16#FFFF#));
      result : long;
      remain : long;
   begin
      self.post_ea(ea);
      Ada.Text_IO.Put_Line("Processing DIVU instruction.");
      if op2 = 0 then  --  Divide by 0 exception
         BBS.Sim_CPU.m68000.exceptions.process_exception(self,
               BBS.Sim_CPU.m68000.exceptions.ex_5_div0);
         return;
      end if;
      result := op1/op2;
      remain := op1 rem op2;
      self.psw.carry := False;
      self.psw.negative := (result < 0);
      if (result /= long(word(result and 16#FFFF#))) then
         self.psw.overflow := True;
         self.psw.zero := (op1 = 0);
      else
         self.psw.zero := (result = 0);
         self.psw.overflow := False;
         result := result and 16#FFFF#;
         result := result or ((remain and 16#FFFF#)*16#0001_0000#);
         self.set_regl(data, reg_x, result);
      end if;
   end;
   --
   procedure decode_OR(self : in out m68000) is
      reg_x  : reg_num := instr_2op.reg_x;
      reg_y  : reg_num := instr_2op.reg_y;
      mode_y : mode_code := instr_2op.mode_y;
      opmode : uint3 := instr_2op.code;
      op1    : long;
      op2    : long;
      sum    : long;
   begin
      Ada.Text_IO.Put_Line("Processing OR instruction");
      case opmode is
        when 0 =>  --  Byte <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              op1 := long(self.get_regb(Data, reg_x));
              op2 := self.get_ea(ea);
              sum := op1 or op2;
              self.set_regb(Data, reg_x, byte(sum and 16#FF#));
              self.post_ea(ea);
           end;
        when 1 =>  --  Word <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              op1 := long(self.get_regw(Data, reg_x));
              op2 := self.get_ea(ea);
              sum := op1 or op2;
              self.set_regw(Data, reg_x, word(sum and 16#FFFF#));
              self.post_ea(ea);
           end;
        when 2 =>  --  Long <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              op1 := self.get_regl(Data, reg_x);
              op2 := self.get_ea(ea);
              sum := op1 or op2;
              self.set_regl(Data, reg_x, sum);
              self.post_ea(ea);
           end;
        when 4 =>  --  Byte Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              op1 := long(self.get_regb(Data, reg_x));
              op2 := self.get_ea(ea);
              sum := op1 or op2;
              self.set_ea(ea, sum and 16#FF#);
              self.post_ea(ea);
           end;
        when 5 =>  --  Word Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              op1 := long(self.get_regw(Data, reg_x));
              op2 := self.get_ea(ea);
              sum := op1 or op2;
              self.set_ea(ea, sum and 16#FFFF#);
              self.post_ea(ea);
           end;
        when 6 =>  --  Long Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              op1 := self.get_regl(Data, reg_x);
              op2 := self.get_ea(ea);
              sum := op1 or op2;
              self.set_ea(ea, sum);
              self.post_ea(ea);
           end;
        when others =>  --  Should not happen (DIVS/DIVU instructions
           Ada.Text_IO.Put_Line("OR unrecognized options");
      end case;
      --
      --  Compute condition codes
      --
      case opmode is
         when 0 =>  --  Byte size
            self.psw.zero := (sum and 16#FF#) = 0;
            self.psw.negative := (sum and 16#80#) = 16#80#;
         when 1 | 5 =>  --  Word size
            self.psw.zero := (sum and 16#FFFF#) = 0;
            self.psw.negative := (sum and 16#8000#) = 16#8000#;
         when 2 | 6 =>  --  Long size
            self.psw.zero := (sum and 16#FFFF_FFFF#) = 0;
            self.psw.negative := (sum and 16#8000_0000#) = 16#8000_0000#;
         when others =>  --  Modes 3 & 7 are DIVS/DIVU instructions
            null;
      end case;
      --
      --  Carry, Extend, and Overflow
      --
      self.psw.Carry := False;
      self.psw.Overflow := False;
   end;
   --
   procedure decode_SBCD(self : in out m68000) is
      reg_x : reg_num := instr_sbcd.reg_x;
      reg_y : reg_num := instr_sbcd.reg_y;
      dest  : byte;
      src   : byte;
      addr1 : long;
      addr2 : long;
   begin
      Ada.Text_IO.Put_Line("Processing SBCD instruction");
      if instr_sbcd.reg_mem = data then
         dest := self.get_regb(data, reg_x);
         src  := self.get_regb(data, reg_y);
      else
         addr1 := self.get_regl(address, reg_x) - 1;
         addr2 := self.get_regl(address, reg_y) - 1;
         self.set_regl(address, reg_x, addr1);
         self.set_regl(address, reg_y, addr2);
         dest := self.memory(addr1);
         src  := self.memory(addr2);
      end if;
      dest := bcd_to_byte(dest);
      src  := bcd_to_byte(src);
      dest := dest - src;
      if self.psw.extend then
         dest := dest - 1;
      end if;
      if dest /= 0 then
         self.psw.zero := False;
      end if;
      if dest > 100 then
         self.psw.extend := True;
         self.psw.carry  := True;
         dest := dest - 156;
      else
         self.psw.extend := False;
         self.psw.carry  := False;
      end if;
      if instr_sbcd.reg_mem = data then
         self.set_regb(data, reg_x, byte_to_bcd(dest));
      else
         self.memory(addr1, byte_to_bcd(dest));
      end if;
   end;
end;
