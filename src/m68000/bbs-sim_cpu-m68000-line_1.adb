with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with BBS.Sim_CPU.m68000.exceptions;
package body BBS.Sim_CPU.m68000.line_1 is
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                           target => status_word);
   --
   --  Package for decoding Line 1 instructions - Move byte
   --
   procedure decode_1(self : in out m68000) is
      mode_x : mode_code := instr_move.mode_x;
      reg_x  : reg_num := instr_move.reg_x;
   begin
      --
      --  Check to make sure that addressing modes are legal.
      --
      if (mode_x = 1) or  --  Not An.  Note that MOVEA is not legal for byte size
        ((mode_x = 7) and (reg_x = 2)) or  --  Not d(PC)
        ((mode_x = 7) and (reg_x = 3)) or  --  Not d(PC,Xi)
        ((mode_x = 7) and (reg_x = 4)) then  --  Not immediate
         Ada.Text_IO.Put_Line("Invalid destination mode for MOVE.B");
      else
         decode_MOVEB(self);
      end if;
   end;
   --
   procedure decode_MOVEB(self : in out m68000) is
      ea_src  : operand := self.get_ea(instr_move.reg_y, instr_move.mode_y, data_byte);
      ea_dest : operand := self.get_ea(instr_move.reg_x, instr_move.mode_x, data_byte);
      val     : long;
   begin
      Ada.Text_IO.Put_Line("Processing MOVE.B instruction");
      val := self.get_ea(ea_src) and 16#FF#;
      self.set_ea(ea_dest, val);
      self.post_ea(ea_src);
      self.post_ea(ea_dest);
      self.psw.negative := (val and 16#80#) = 16#80#;
      self.psw.zero := (val and 16#FF#) = 0;
      self.psw.overflow := False;
      self.psw.carry := False;
   end;
end;
