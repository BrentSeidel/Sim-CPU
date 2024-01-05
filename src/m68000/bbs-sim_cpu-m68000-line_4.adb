with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS.embed;
use type BBS.embed.int16;
with BBS.Sim_CPU.m68000.exceptions;
package body BBS.Sim_CPU.m68000.line_4 is
   --
   --  Package for decoding Line 6 instructions - Miscellaneous
   --
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                        target => status_word);
   --
   procedure decode_4(self : in out m68000) is
   begin
      if instr = 16#4AFC# then  --  The one instruction always guarenteed to be illegal
         decode_ILLEGAL(self);
      elsif (instr_1ea.code = 16#3b#) and ((instr_1ea.mode_y = 2) or
            (instr_1ea.mode_y = 5) or (instr_1ea.mode_y = 6) or
            (instr_1ea.mode_y = 7)) then
         --
         --  Only some addressing modes are available for JSR and JMP
         --
         decode_JMP(self);
      elsif (instr_1ea.code = 16#3a#) and ((instr_1ea.mode_y = 2) or
            (instr_1ea.mode_y = 5) or (instr_1ea.mode_y = 6) or
            (instr_1ea.mode_y = 7))then
         decode_JSR(self);
      elsif (instr_1ea.code = 16#13#) and (instr_1ea.mode_y /= 1) then
         decode_MOVEtCCR(self);
      elsif (instr_1ea.code = 16#1b#) and (instr_1ea.mode_y /= 1) then
         decode_MOVEtSR(self);
      elsif (instr_1ea.code = 16#03#) and (instr_1ea.mode_y /= 1) and
            not ((instr_1ea.mode_y = 7) and ((instr_1ea.reg_y = 2) or
               (instr_1ea.reg_y = 3) or (instr_1ea.reg_y = 4))) then
         decode_MOVEfSR(self);
      elsif (instr_lea.code = 7) and ((instr_lea.mode_y = 2) or
            (instr_lea.mode_y = 5) or (instr_lea.mode_y = 6) or
            (instr_lea.mode_y = 7))then
         decode_LEA(self);
      elsif (instr_musp.code = 16#e6#) then
         decode_MtfUSP(self);
      elsif (instr_link.code = 16#1ca#) then
         decode_LINK(self);
      elsif (instr_clr.code = 2) and (instr_clr.size /= data_long_long) then
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
      elsif (instr_movem.code0 = 1) and instr_movem.code1 and
            (instr_movem.mode_y /=0) and (instr_movem.mode_y /= 1) then
         decode_MOVEM(self);
      else
         Ada.Text_IO.Put_Line("Unimplemented miscellaneous instruction.");
      end if;
   end;
   --
   procedure decode_CHK(self : in out m68000) is
      reg_y  : reg_num := instr_chk.reg_y;
      mode_y : mode_code := instr_chk.mode_y;
   begin
      Ada.Text_IO.Put_Line("Processing CHK instruction");
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
      else  --  Long size, implemented in 68020 and later processors
         BBS.Sim_CPU.m68000.exceptions.process_exception(self, BBS.Sim_CPU.m68000.exceptions.ex_4_ill_inst);
      end if;
   end;
   --
   procedure decode_CLR(self : in out m68000) is
      reg_y  : reg_num := instr_clr.reg_y;
      mode_y : mode_code := instr_clr.mode_y;
   begin
      Ada.Text_IO.Put_Line("Processing CLR instruction");
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
      reg  : reg_num := instr_ext.reg_y;
      mode : uint3 := instr_ext.mode;
      val  : long := self.get_regl(Data, reg);
   begin
      Ada.Text_IO.Put_Line("Processing EXT instruction");
      if mode = 3 then  --  Extend word to long
         val := sign_extend(word(val and 16#FFFF#));
         self.psw.negative := msb(val);
         self.set_regl(Data, reg, val);
      elsif mode = 2 then  --  Extend byte to word
         self.psw.negative := msb(byte(val and 16#FF#));
         val := sign_extend(byte(val and 16#FF#)) and 16#FFFF#;
         self.set_regw(Data, reg, word(val and 16#FFFF#));
      else
         Ada.Text_IO.Put_Line("Unrecognized option for EXT");
      end if;
      self.psw.zero := (val = 0);
      self.psw.overflow := False;
      self.psw.carry := False;
   end;
   --
   procedure decode_ILLEGAL(self : in out m68000) is
   begin
      Ada.Text_IO.Put_Line("Processing ILLEGAL instruction");
      BBS.Sim_CPU.m68000.exceptions.process_exception(self, BBS.Sim_CPU.m68000.exceptions.ex_4_ill_inst);
   end;
   --
   procedure decode_JMP(self : in out m68000) is
      ea : operand := self.get_ea(instr_1ea.reg_y, instr_1ea.mode_y, data_long);
   begin
      Ada.Text_IO.Put_Line("Processing JMP instruction");
      if ea.kind = memory_address then
         self.pc := ea.address;
      else
         Ada.Text_IO.Put_Line("  Invalid addressing mode for JMP.");
      end if;
   end;
   --
   procedure decode_JSR(self : in out m68000) is
      ea : operand := self.get_ea(instr_1ea.reg_y, instr_1ea.mode_y, data_long);
   begin
      Ada.Text_IO.Put_Line("Processing JSR instruction");
      if ea.kind = memory_address then
         self.push(self.psw.super, self.pc);
         self.pc := ea.address;
      else
         Ada.Text_IO.Put_Line("  Invalid addressing mode for JSR.");
      end if;
   end;
   --
   procedure decode_LEA(self : in out m68000) is
      ea : operand := self.get_ea(instr_lea.reg_y, instr_lea.mode_y, data_long);
   begin
      Ada.Text_IO.Put_Line("Processing LEA instruction");
      if ea.kind = memory_address then
         self.set_regl(Address, instr_lea.reg_x, long(ea.address));
      else
         Ada.Text_IO.Put_Line("  Invalid addressing mode for LEA");
      end if;
   end;
   --
   procedure decode_LINK(self : in out m68000) is
      reg  : long := self.get_regl(Address, instr_link.reg_y);
      disp : long := sign_extend(self.get_ext);
   begin
      Ada.Text_IO.Put_Line("Processing LINK instruction");
      if self.psw.super then
         self.push(True, reg);
         self.set_regl(Address, instr_link.reg_y, self.ssp);
         self.ssp := self.ssp + disp;
      else
         self.push(False, reg);
         self.set_regl(Address, instr_link.reg_y, self.usp);
         self.usp := self.usp + disp;
      end if;
   end;
   --
   procedure decode_MOVEtCCR(self : in out m68000) is
      ea  : operand := self.get_ea(instr_1ea.reg_y, instr_1ea.mode_y, data_word);
      psw : word := psw_to_word(self.psw) and 16#ff00#;
   begin
      Ada.Text_IO.Put_Line("Processing MOVE to CCR");
      psw := psw or word(self.get_ea(ea) and 16#FF#);
      self.psw := word_to_psw(psw);
      self.post_ea(ea);
   end;
   --
   procedure decode_MOVEtSR(self : in out m68000) is
      ea  : operand := self.get_ea(instr_1ea.reg_y, instr_1ea.mode_y, data_word);
   begin
      Ada.Text_IO.Put_Line("Processing MOVE to SR");
      if self.psw.super then
         self.psw := word_to_psw(word(self.get_ea(ea) and 16#FF#));
         self.post_ea(ea);  -- Don't do post-increment if exception
      else
         BBS.Sim_CPU.m68000.exceptions.process_exception(self, BBS.Sim_CPU.m68000.exceptions.ex_8_priv_viol);
      end if;
   end;
   --
   procedure decode_MOVEfSR(self : in out m68000) is
      ea  : operand := self.get_ea(instr_1ea.reg_y, instr_1ea.mode_y, data_word);
      psw : word := psw_to_word(self.psw);
   begin
      Ada.Text_IO.Put_Line("Processing MOVE from SR");
      --
      --  Note that this is a privileged instruction on 68010 and later.
      --
      if self.psw.super or (self.cpu_model = var_68008) or (self.cpu_model = var_68000) then
         self.set_ea(ea, long(psw));
         self.post_ea(ea);  --  Don't do post-increment if exception
      else
         BBS.Sim_CPU.m68000.exceptions.process_exception(self, BBS.Sim_CPU.m68000.exceptions.ex_8_priv_viol);
      end if;
   end;
   --
   procedure decode_MtfUSP(self : in out m68000) is
   begin
      Ada.Text_IO.Put_Line("Processing MOVE to/from USP");
      if self.psw.super then
         if instr_musp.dir then
            self.set_regl(Address, instr_musp.reg_y, self.usp);
         else
            self.usp := self.get_regl(Address, instr_musp.reg_y);
         end if;
      else
         BBS.Sim_CPU.m68000.exceptions.process_exception(self, BBS.Sim_CPU.m68000.exceptions.ex_8_priv_viol);
      end if;
   end;
   --
   --  The MOVEM instruction is a rather complicated one to implement as
   --  different addressing modes are valid depending on whether registers
   --  are moving to or from memory.  Also using pre-decrement (register
   --  to memory only), the order of registers in the extension word is
   --  reversed (this actually makes moving registers to stack and back
   --  easier.
   --
   procedure decode_MOVEM(self : in out m68000) is
      reg_y    : reg_num := instr_movem.reg_y;
      mode_y   : mode_code := instr_movem.mode_y;
      reg_list : word := self.get_ext;
      size     : data_size;
      addr     : addr_bus;
      vlong    : long;
      vword    : word;
   begin
      Ada.Text_IO.Put_Line("Processing MOVEM instruction");
      if instr_movem.size then
         size := data_long;
      else
         size := data_word;
      end if;
      if instr_movem.dir then  --  Move memory to registers
         if mode_y = 3 then  --  Post-increment
            addr := self.get_regl(Address, reg_y);
            for num in 0 .. 15 loop
               if (reg_list and (2**num)) /= 0 then
                  if size = data_long then
                     vlong := self.memory(addr);
                     if num < 8 then
                        self.set_regl(Data, reg_num(num), vlong);
                     else
                        self.set_regl(Address, reg_num(num - 8), vlong);
                     end if;
                     addr := addr + 4;
                  else
                     vword := self.memory(addr);
                     if num < 8 then
                        self.set_regl(Data, reg_num(num), sign_extend(vword));
                     else
                        self.set_regl(Address, reg_num(num - 8), sign_extend(vword));
                     end if;
                     addr := addr + 2;
                  end if;
                  self.set_regl(Address, reg_y, addr);
               end if;
            end loop;
         else
            declare
               ea : operand := self.get_ea(reg_y, mode_y, size);
            begin
               addr := ea.address;
               for num in 0 .. 15 loop
                  if (reg_list and (2**num)) /= 0 then
                     if size = data_long then
                        vlong := self.memory(addr);
                        if num < 8 then
                           self.set_regl(Data, reg_num(num), vlong);
                        else
                           self.set_regl(Address, reg_num(num - 8), vlong);
                        end if;
                        addr := addr + 4;
                     else
                        vword := self.memory(addr);
                        if num < 8 then
                           self.set_regl(Data, reg_num(num), sign_extend(vword));
                        else
                           self.set_regl(Address, reg_num(num - 8), sign_extend(vword));
                        end if;
                        addr := addr + 2;
                     end if;
                  end if;
               end loop;
            end;
         end if;
      else  --  Move registers to memory
         if mode_y = 4 then  --  Pre-decrement
            addr := self.get_regl(Address, reg_y);
            for num in 0 .. 15 loop
               if (reg_list and (2**num)) /= 0 then
                  if size = data_long then
                     addr := addr - 4;
                     if num < 8 then
                        self.memory(addr, self.get_regl(Address, 7-reg_num(num)));
                     else
                        self.memory(addr, self.get_regl(Data, 7-reg_num(num - 8)));
                     end if;
                  else
                     addr := addr - 2;
                     if num < 8 then
                        self.memory(addr, self.get_regw(Address, 7-reg_num(num)));
                     else
                        self.memory(addr, self.get_regw(Data, 7-reg_num(num - 8)));
                     end if;
                  end if;
                  self.set_regl(Address, reg_y, addr);
               end if;
            end loop;
         else
            declare
               ea : operand := self.get_ea(reg_y, mode_y, size);
            begin
               addr := ea.address;
               for num in 0 .. 15 loop
                  if (reg_list and (2**num)) /= 0 then
                     if size = data_long then
                        if num < 8 then
                           self.memory(addr, self.get_regl(Data, reg_num(num)));
                        else
                           self.memory(addr, self.get_regl(Address, reg_num(num - 8)));
                        end if;
                        addr := addr + 4;
                     else
                        if num < 8 then
                           self.memory(addr, self.get_regw(Data, reg_num(num)));
                        else
                           self.memory(addr, self.get_regw(Address, reg_num(num - 8)));
                        end if;
                        addr := addr + 2;
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end if;
   end;
end;

