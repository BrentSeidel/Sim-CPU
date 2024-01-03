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
      if instr_div.code = 7 then  --  DIVS instructions
         decode_DIVS(self);
      elsif instr_div.code = 3 then  --  DIVU instruction
         decode_DIVU(self);
      end if;
   end;
   --
   procedure decode_DIVS(self : in out m68000) is
      reg_y  : reg_num := instr_div.reg_y;
      mode_y : mode_code := instr_div.mode_y;
      reg_x  : reg_num := instr_div.reg_x;
      ea     : operand := self.get_ea(reg_y, mode_y, data_word);
      op1    : BBS.embed.int32 := uint32_to_int32(self.get_regl(Data, reg_x));
      op2    : BBS.embed.int32 := uint32_to_int32(sign_extend(word(self.get_ea(ea) and 16#FFFF#)));
      result : BBS.embed.int32;
      remain : BBS.embed.int32;
      resl   : long;
      reml   : long;
   begin
      self.post_ea(ea);
      Ada.Text_IO.Put_Line("DIVS instruction encountered.");
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
      reg_y  : reg_num := instr_div.reg_y;
      mode_y : mode_code := instr_div.mode_y;
      reg_x  : reg_num := instr_div.reg_x;
      ea     : operand := self.get_ea(reg_y, mode_y, data_word);
      op1    : long := self.get_regl(Data, reg_x);
      op2    : long := long(word(self.get_ea(ea) and 16#FFFF#));
      result : long;
      remain : long;
   begin
      self.post_ea(ea);
      Ada.Text_IO.Put_Line("DIVU instruction encountered.");
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
end;
