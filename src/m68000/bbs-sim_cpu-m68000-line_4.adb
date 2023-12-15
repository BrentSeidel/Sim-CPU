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
      Ada.Text_IO.Put_Line("Decoding miscellaneous instructions");
      if (instr_clr.code = 2) and (instr_clr.size /= data_long_long) then
         decode_CLR(self);
      elsif (not instr_chk.code) and ((instr_chk.size = 3) or
                                      (instr_chk.size = 2)) then
         --
         --  Later processors will allow a word size = 2 (long)
         --
         decode_CHK(self);
      else
         Ada.Text_IO.Put_Line("Unimplemented miscellaneous instruction.");
      end if;
   end;
   --
   procedure decode_CHK(self : in out m68000) is
      reg_y  : uint3 := instr_chk.reg_y;
      mode_y : uint3 := instr_chk.mode_y;
   begin
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
end;

