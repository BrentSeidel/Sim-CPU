with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS.Sim_CPU.m68000.exceptions;
package body BBS.Sim_CPU.m68000.line_b is
   function uint32_to_int32 is
      new Ada.Unchecked_Conversion(source => uint32, target => int32);
   function int32_to_uint32 is
      new Ada.Unchecked_Conversion(source => int32, target => uint32);

   --
   --  Package for decoding Line 6 instructions - CMP/EOR
   --
   procedure decode_b(self : in out m68000) is
      mode : constant uint3 := instr_cmp.opmode;
   begin
      if (mode = 0) or (mode = 1) or (mode = 2) or (mode = 3) or (mode = 7) then
         decode_CMP(self);
      elsif ((mode = 4) or (mode = 5) or (mode = 6)) and (instr_cmpm.code1 = 1) then
         decode_CMPM(self);
      elsif (mode = 4) or (mode = 5) or (mode = 6) then
         decode_EOR(self);
      else
         BBS.Sim_CPU.m68000.exceptions.process_exception(self,
            BBS.Sim_CPU.m68000.exceptions.ex_4_ill_inst);
      end if;
   end;
   --
   procedure decode_CMP(self : in out m68000) is
      reg_y  : constant reg_num := instr_cmp.reg_y;
      mode_y : constant mode_code := instr_cmp.mode_y;
      reg_x  : constant reg_num := instr_cmp.reg_x;
      mode   : constant uint3 := instr_cmp.opmode;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
      case mode is
         when 0 =>  --  CMP.B
            declare
               ea   : constant operand := self.get_ea(reg_y, mode_y, data_byte);
               src  : constant byte := byte(self.get_ea(ea) and 16#FF#);
               dest : constant byte := self.get_regb(Data, reg_x);
               diff : constant byte := dest - src;
            begin
--               Ada.Text_IO.Put_Line("Processing CMP.B instruction");
               Smsb := msb(src);
               Dmsb := msb(dest);
               Rmsb := msb(diff);
               self.psw.zero := (diff = 0);
               self.post_ea(ea);
            end;
         when 1 =>  --  CMP.W
            declare
               ea   : constant operand := self.get_ea(reg_y, mode_y, data_word);
               src  : constant word := word(self.get_ea(ea) and 16#FFFF#);
               dest : constant word := self.get_regw(Data, reg_x);
               diff : constant word := dest - src;
            begin
--               Ada.Text_IO.Put_Line("Processing CMP.W instruction");
               Smsb := msb(src);
               Dmsb := msb(dest);
               Rmsb := msb(diff);
               self.psw.zero := (diff = 0);
               self.post_ea(ea);
            end;
         when 2 =>  --  CMP.L
            declare
               ea   : constant operand := self.get_ea(reg_y, mode_y, data_long);
               src  : constant long := self.get_ea(ea);
               dest : constant long := self.get_regl(Data, reg_x);
               diff : constant long := dest - src;
            begin
--               Ada.Text_IO.Put_Line("Processing CMP.L instruction");
               Smsb := msb(src);
               Dmsb := msb(dest);
               Rmsb := msb(diff);
               self.psw.zero := (diff = 0);
               self.post_ea(ea);
            end;
         when 3 =>  --  CMPA.W
            --
            --  Note that according to the documentation, the source is
            --  read as a word and sign extended to a long, while the
            --  destination (register) is read as a long.  This can lead
            --  to unexpected results where CMPA.W A0,A0 does not set the
            --  Z flag.
            --
            declare
               ea   : constant operand := self.get_ea(reg_y, mode_y, data_word);
               src  : constant long := sign_extend(word(self.get_ea(ea) and 16#FFFF#));
               dest : constant long := self.get_regl(Address, reg_x);
               diff : constant long := dest - src;
            begin
--               Ada.Text_IO.Put_Line("Processing CMPA.W instruction");
               Smsb := msb(src);
               Dmsb := msb(dest);
               Rmsb := msb(diff);
               self.psw.zero := (diff = 0);
               self.post_ea(ea);
            end;
         when 7 =>  --  CMPA.L
            declare
               ea   : constant operand := self.get_ea(reg_y, mode_y, data_long);
               src  : constant long := self.get_ea(ea);
               dest : constant long := self.get_regl(Address, reg_x);
               diff : constant long := dest - src;
            begin
--               Ada.Text_IO.Put_Line("Processing CMPA.L instruction");
               Smsb := msb(src);
               Dmsb := msb(dest);
               Rmsb := msb(diff);
               self.psw.zero := (diff = 0);
               self.post_ea(ea);
            end;
         when others =>  -- Should never happen based on conditions above.
            Ada.Text_IO.Put_Line("  Unimplemented CMP mode");
      end case;
      self.psw.negative := Rmsb;
      self.psw.overflow := ((not Smsb) and Dmsb and (not Rmsb)) or
                            (Smsb and (not Dmsb) and Rmsb);
      self.psw.carry := (Smsb and (not Dmsb)) or (Rmsb and (not Dmsb)) or (Smsb and Rmsb);
   end;
   --
   procedure decode_CMPM(self : in out m68000) is
      reg_x  : constant reg_num := instr_cmpm.reg_x;
      reg_y  : constant reg_num := instr_cmpm.reg_y;
      size   : constant data_size := instr_cmpm.size;
      ea_x   : constant operand := self.get_ea(reg_x, 3, size);
      ea_y   : constant operand := self.get_ea(reg_y, 3, size);
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
      case size is
         when data_byte =>  --  CMPM.B
            declare
               dest : constant byte := byte(self.get_ea(ea_x) and 16#FF#);
               src  : constant byte := byte(self.get_ea(ea_y) and 16#FF#);
               diff : constant byte := dest - src;
            begin
--               Ada.Text_IO.Put_Line("Decoding CMPM.B instruction");
               Smsb := msb(src);
               Dmsb := msb(dest);
               Rmsb := msb(diff);
               self.psw.zero := (diff = 0);
               self.post_ea(ea_x);
               self.post_ea(ea_y);
            end;
         when data_word =>  --  CMPM.W
            declare
               dest : constant word := word(self.get_ea(ea_x) and 16#FFFF#);
               src  : constant word := word(self.get_ea(ea_y) and 16#FFFF#);
               diff : constant word := dest - src;
            begin
--               Ada.Text_IO.Put_Line("Decoding CMPM.W instruction");
               Smsb := msb(src);
               Dmsb := msb(dest);
               Rmsb := msb(diff);
               self.psw.zero := (diff = 0);
               self.post_ea(ea_x);
               self.post_ea(ea_y);
            end;
         when data_long =>  --  CMPM.L
            declare
               dest : constant long := self.get_ea(ea_x);
               src  : constant long := self.get_ea(ea_y);
               diff : constant long := dest - src;
            begin
--               Ada.Text_IO.Put_Line("Decoding CMPM.L instruction");
               Smsb := msb(src);
               Dmsb := msb(dest);
               Rmsb := msb(diff);
               self.psw.zero := (diff = 0);
               self.post_ea(ea_x);
               self.post_ea(ea_y);
            end;
         when others =>  --  Should never happen due to previous checks
            Ada.Text_IO.Put_Line("Unimplemented CMPM data size");
      end case;
      self.psw.negative := Rmsb;
      self.psw.overflow := ((not Smsb) and Dmsb and (not Rmsb)) or
                            (Smsb and (not Dmsb) and Rmsb);
      self.psw.carry := (Smsb and (not Dmsb)) or (Rmsb and (not Dmsb)) or (Smsb and Rmsb);

   end;
   --
   procedure decode_EOR(self : in out m68000) is
      reg_y  : constant reg_num := instr_cmp.reg_y;
      mode_y : constant mode_code := instr_cmp.mode_y;
      reg_x  : constant reg_num := instr_cmp.reg_x;
      mode   : constant uint3 := instr_cmp.opmode;
   begin
--      Ada.Text_IO.Put_Line("Decoding EOR instruction");
      case mode is
         when 4 =>  --  EOR.B
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_byte);
               b1  : constant byte := byte(self.get_ea(ea));
               b2  : constant byte := self.get_regb(Data, reg_x);
               res : constant byte := b1 xor b2;
            begin
               self.set_ea(ea, long(res));
               self.post_ea(ea);
               self.psw.zero := (res = 0);
               self.psw.negative := msb(res);
            end;
         when 5 =>  --  EOR.W
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_word);
               w1  : constant word := word(self.get_ea(ea));
               w2  : constant word := self.get_regw(Data, reg_x);
               res : constant word := w1 xor w2;
            begin
               self.set_ea(ea, long(res));
               self.post_ea(ea);
               self.psw.zero := (res = 0);
               self.psw.negative := msb(res);
            end;
         when 6 =>  --  EOR.L
            declare
               ea  : constant operand := self.get_ea(reg_y, mode_y, data_long);
               l1  : constant long := self.get_ea(ea);
               l2  : constant long := self.get_regl(Data, reg_x);
               res : constant long := l1 xor l2;
            begin
               self.set_ea(ea, long(res));
               self.post_ea(ea);
               self.psw.zero := (res = 0);
               self.psw.negative := msb(res);
            end;
         when others =>  --  Should never happen due to previous checks
            Ada.Text_IO.Put_Line("Unimplemented EOR data size.");
      end case;
      self.psw.overflow := False;
      self.psw.carry := False;
   end;
end;

