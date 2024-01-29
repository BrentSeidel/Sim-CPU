with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with BBS.Sim_CPU.m68000.exceptions;
package body BBS.Sim_CPU.m68000.line_2 is
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                           target => status_word);
   --
   --  Package for decoding Line 1 instructions - Move long
   --
   procedure decode_2(self : in out m68000) is
      mode_x : constant mode_code := instr_move.mode_x;
      reg_x  : constant reg_num := instr_move.reg_x;
   begin
      --
      --  Check to make sure that addressing modes are legal.
      --  Mode X of 1 is a MOVEA instruction.  Not sure why it was made
      --  a separate instruction.  It's handled just the same as another
      --  addressing mode for MOVE.L.
      --
      if ((mode_x = 7) and (reg_x = 2)) or  --  Not d(PC)
         ((mode_x = 7) and (reg_x = 3)) or  --  Not d(PC,Xi)
         ((mode_x = 7) and (reg_x = 4)) then  --  Not immediate
         Ada.Text_IO.Put_Line("Invalid destination mode for MOVE.LB");
         BBS.Sim_CPU.m68000.exceptions.process_exception(self,
            BBS.Sim_CPU.m68000.exceptions.ex_4_ill_inst);
      else
         decode_MOVEL(self);
      end if;
   end;
   --
   procedure decode_MOVEL(self : in out m68000) is
      ea_src  : constant operand := self.get_ea(instr_move.reg_y, instr_move.mode_y, data_long);
      ea_dest : constant operand := self.get_ea(instr_move.reg_x, instr_move.mode_x, data_long);
      val     : constant long := self.get_ea(ea_src);
   begin
--      Ada.Text_IO.Put_Line("Processing MOVE.L instruction " & toHex(instr));
      self.set_ea(ea_dest, val);
      self.post_ea(ea_src);
      self.post_ea(ea_dest);
      if instr_move.mode_x /= 1 then   --  Don't set condition codes for MOVEA
         self.psw.negative := (val and 16#8000_0000#) = 16#8000_0000#;
         self.psw.zero := (val = 0);
         self.psw.overflow := False;
         self.psw.carry := False;
      end if;
   end;
end;
