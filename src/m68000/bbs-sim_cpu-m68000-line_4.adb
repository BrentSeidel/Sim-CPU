with Ada.Text_IO;
with BBS.embed;
use type BBS.embed.int16;
with BBS.Sim_CPU.m68000.exceptions;
package body BBS.Sim_CPU.m68000.line_4 is
   --
   --  Package for decoding Line 6 instructions - Miscellaneous
   --
   procedure decode_4(self : in out m68000) is
   begin
      if (instr_clr.code = 2) and (instr_clr.size /= data_long_long) then
         decode_CLR(self);
      elsif (not instr_chk.code) and ((instr_chk.size = 3) or
                                      (instr_chk.size = 2)) then
         --
         --  Later processors will allow a word size = 2 (long)
         --
         decode_CHK(self);
      elsif ((instr_ext.code0 = 0) and (instr_ext.code1 = 4) and
            ((instr_ext.mode = 2) or (instr_ext.mode = 3))) then
         decode_EXT(self);
      else
         Ada.Text_IO.Put_Line("Unimplemented miscellaneous instruction.");
      end if;
   end;
   --
   procedure decode_CHK(self : in out m68000) is
      reg_y  : uint3 := instr_chk.reg_y;
      mode_y : uint3 := instr_chk.mode_y;
   begin
      Ada.Text_IO.Put_Line("CHK instruction");
      if instr_chk.size = 3 then  --  Word size
         declare
            val : BBS.embed.int16 := BBS.embed.uint16_to_int16(word(self.get_regw(Data, instr_chk.reg_x) and 16#FFFF#));
            ea  : operand := self.get_ea(reg_y, mode_y, data_word);
            lim : BBS.embed.int16 := BBS.embed.uint16_to_int16(word(self.get_ea(ea) and 16#FFFF#));
         begin
            self.post_ea(ea);
            if (val < 0) or (val > lim) then
               if val < 0 then
                  self.psw.negative := True;
               else
                  self.psw.negative := False;
               end if;
               BBS.Sim_CPU.m68000.exceptions.process_exception(self, BBS.Sim_CPU.m68000.exceptions.ex_6_CHK);
            end if;
         end;
      else  --  Long size, implemented in 68020 or later processors
         BBS.Sim_CPU.m68000.exceptions.process_exception(self, BBS.Sim_CPU.m68000.exceptions.ex_4_ill_inst);
      end if;
      Ada.Text_IO.Put_Line("Decoding CHK instruction.");
   end;
   --
   procedure decode_CLR(self : in out m68000) is
      reg_y  : uint3 := instr_clr.reg_y;
      mode_y : uint3 := instr_clr.mode_y;
   begin
      Ada.Text_IO.Put_Line("CLR instruction");
      case instr_clr.size is
         when data_byte =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_byte);
            begin
               self.set_ea(ea, 0);
               self.post_ea(ea);
            end;
         when data_word =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_word);
            begin
               self.set_ea(ea, 0);
               self.post_ea(ea);
            end;
         when data_long =>
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_long);
            begin
               self.set_ea(ea, 0);
               self.post_ea(ea);
            end;
         when others =>  --  Should never happen due to check above.
            null;
      end case;
      self.psw.negative := False;
      self.psw.zero := True;
      self.psw.overflow := False;
      self.psw.carry := False;
   end;
   --
   procedure decode_EXT(self : in out m68000) is
      reg  : uint3 := instr_ext.reg_y;
      mode : uint3 := instr_ext.mode;
      val  : long := self.get_regl(Data, reg);
   begin
      Ada.Text_IO.Put_Line("Processing EXT instruction");
      if mode = 3 then  --  Extend word to long
         Ada.Text_IO.Put_Line("  Initial word " & toHex(word(val and 16#FFFF#)));
         val := sign_extend(word(val and 16#FFFF#));
         self.psw.negative := msb(val);
         self.set_regl(Data, reg, val);
         Ada.Text_IO.Put_Line("  Extended long " & toHex(val));
      elsif mode = 2 then  --  Extend byte to word
         Ada.Text_IO.Put_Line("  Initial byte " & toHex(byte(val and 16#FF#)));
         self.psw.negative := msb(byte(val and 16#FF#));
         val := sign_extend(byte(val and 16#FF#)) and 16#FFFF#;
         self.set_regw(Data, reg, word(val and 16#FFFF#));
         Ada.Text_IO.Put_Line("  Extended word " & toHex(word(val and 16#FFFF#)));
      else
         Ada.Text_IO.Put_Line("Unrecognized option for EXT");
      end if;
      self.psw.zero := (val = 0);
      self.psw.overflow := False;
      self.psw.carry := False;
   end;
end;

