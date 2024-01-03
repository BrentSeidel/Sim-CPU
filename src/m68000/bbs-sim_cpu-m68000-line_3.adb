with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with BBS.Sim_CPU.m68000.exceptions;
package body BBS.Sim_CPU.m68000.line_3 is
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                           target => status_word);
   --
   --  Package for decoding Line 1 instructions - Move byte
   --
   procedure decode_3(self : in out m68000) is
      mode_x : mode_code := instr_move.mode_x;
      reg_x  : reg_num := instr_move.reg_x;
   begin
      --
      --  Check to make sure that addressing modes are legal.
      --  Mode X of 1 is a MOVEA instruction.  Not sure why it was made
      --  a separate instruction.  It's handled just the same as another
      --  addressing mode for MOVE.W.
      --
      if ((mode_x = 7) and (reg_x = 2)) or  --  Not d(PC)
         ((mode_x = 7) and (reg_x = 3)) or  --  Not d(PC,Xi)
         ((mode_x = 7) and (reg_x = 4)) then  --  Not immediate
         Ada.Text_IO.Put_Line("Invalid destination mode for MOVE.W");
      else
         decode_MOVEW(self);
      end if;
   end;
   --
   procedure decode_MOVEW(self : in out m68000) is
      ea_src  : operand := self.get_ea(instr_move.reg_y, instr_move.mode_y, data_word);
      ea_dest : operand := self.get_ea(instr_move.reg_x, instr_move.mode_x, data_word);
      val     : long;
   begin
      Ada.Text_IO.Put_Line("Processing MOVE.W instruction");
      val := self.get_ea(ea_src) and 16#ffff#;
      if instr_move.mode_x = 1 then  --  Sign extend for MOVEA
         val := sign_extend(word(val and 16#FFFF#));
         self.set_regl(Address, instr_move.reg_x, val);
      else
         self.set_ea(ea_dest, val);
      end if;
      if instr_move.mode_x /= 1 then   --  Don't set condition codes for MOVEA
         self.psw.negative := (val and 16#8000#) = 16#8000#;
         self.psw.zero := (val and 16#ffff#) = 0;
         self.psw.overflow := False;
         self.psw.carry := False;
      end if;
   end;
end;
