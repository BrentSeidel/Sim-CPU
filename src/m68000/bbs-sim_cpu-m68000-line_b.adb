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
      mode : uint3 := instr_cmp.opmode;
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
      reg_y  : reg_num := instr_cmp.reg_y;
      mode_y : mode_code := instr_cmp.mode_y;
      reg_x  : reg_num := instr_cmp.reg_x;
      mode   : uint3 := instr_cmp.opmode;
      src    : int32;
      dest   : int32;
      result : int32;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
      Ada.Text_IO.Put_Line("Decoding CMP instruction with mode " & uint3'Image(mode));
      case mode is
         when 0 =>  --  CMP.B
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_byte);
               b1 : byte := byte(self.get_ea(ea));
               b2 : byte := byte(self.get_regb(Data, reg_x));
            begin
               src := uint32_to_int32(sign_extend(b1));
               dest := uint32_to_int32(sign_extend(b2));
               result := dest - src;
               Smsb := msb(int32_to_uint32(src));
               Dmsb := msb(int32_to_uint32(dest));
               Rmsb := msb(int32_to_uint32(result));
               self.post_ea(ea);
            end;
         when 1 =>  --  CMP.W
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_word);
               w1 : word := word(self.get_ea(ea));
               w2 : word := word(self.get_regw(Data, reg_x));
            begin
               src := uint32_to_int32(sign_extend(w1));
               dest := uint32_to_int32(sign_extend(w2));
               result := dest - src;
               Smsb := msb(int32_to_uint32(src));
               Dmsb := msb(int32_to_uint32(dest));
               Rmsb := msb(int32_to_uint32(result));
               self.post_ea(ea);
            end;
         when 2 =>  --  CMP.L
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_long);
            begin
               src := uint32_to_int32(self.get_ea(ea));
               dest := uint32_to_int32(self.get_regl(Data, reg_x));
               result := dest - src;
               Smsb := msb(int32_to_uint32(src));
               Dmsb := msb(int32_to_uint32(dest));
               Rmsb := msb(int32_to_uint32(result));
               self.post_ea(ea);
            end;
         when 3 =>  --  CMPA.W
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_word);
               w1 : word := word(self.get_ea(ea));
               w2 : word := word(self.get_regw(Address, reg_x));
            begin
               src := uint32_to_int32(sign_extend(w1));
               dest := uint32_to_int32(sign_extend(w2));
               result := dest - src;
               Smsb := msb(int32_to_uint32(src));
               Dmsb := msb(int32_to_uint32(dest));
               Rmsb := msb(int32_to_uint32(result));
               self.post_ea(ea);
            end;
         when 7 =>  --  CMPA.L
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_long);
            begin
               src := uint32_to_int32(self.get_ea(ea));
               dest := uint32_to_int32(self.get_regl(Address, reg_x));
               result := dest - src;
               Smsb := msb(int32_to_uint32(src));
               Dmsb := msb(int32_to_uint32(dest));
               Rmsb := msb(int32_to_uint32(result));
               self.post_ea(ea);
            end;
         when others =>  -- Should never happen based on conditions above.
            Ada.Text_IO.Put_Line("  Unimplemented CMP mode");
      end case;
      self.psw.negative := Rmsb;
      self.psw.zero := (result = 0);
      self.psw.overflow := ((not Smsb) and Dmsb and (not Rmsb)) or
                            (Smsb and (not Dmsb) and Rmsb);
      self.psw.carry := (Smsb and not Dmsb) or (Rmsb and not Dmsb) or (Smsb and Rmsb);
   end;
   --
   procedure decode_CMPM(self : in out m68000) is
      reg_x : reg_num := instr_cmpm.reg_x;
      reg_y : reg_num := instr_cmpm.reg_y;
      size  : data_size := instr_cmpm.size;
      ea_x  : operand := self.get_ea(reg_x, 3, instr_cmpm.size);
      ea_y  : operand := self.get_ea(reg_y, 3, instr_cmpm.size);
      src    : int32;
      dest   : int32;
      result : int32;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
      case instr_cmpm.size is
         when data_byte =>  --  CMPM.B
            declare
               x : byte := byte(self.get_ea(ea_x));
               y : byte := byte(self.get_ea(ea_y));
            begin
               src := uint32_to_int32(sign_extend(y));
               dest := uint32_to_int32(sign_extend(x));
               Ada.Text_IO.Put_Line("Decoding CMPM.B instruction");
               result := dest - src;
               Smsb := msb(int32_to_uint32(src));
               Dmsb := msb(int32_to_uint32(dest));
               Rmsb := msb(int32_to_uint32(result));
               self.post_ea(ea_x);
               self.post_ea(ea_y);
            end;
         when data_word =>  --  CMPM.W
            declare
               x : word := word(self.get_ea(ea_x));
               y : word := word(self.get_ea(ea_y));
            begin
               src := uint32_to_int32(sign_extend(y));
               dest := uint32_to_int32(sign_extend(x));
               Ada.Text_IO.Put_Line("Decoding CMPM.W instruction");
               result := dest - src;
               Smsb := msb(int32_to_uint32(src));
               Dmsb := msb(int32_to_uint32(dest));
               Rmsb := msb(int32_to_uint32(result));
               self.post_ea(ea_x);
               self.post_ea(ea_y);
            end;
         when data_long =>  --  CMPM.L
            src := uint32_to_int32(self.get_ea(ea_y));
            dest := uint32_to_int32(self.get_ea(ea_x));
            Ada.Text_IO.Put_Line("Decoding CMPM.L instruction");
            result := dest - src;
            Smsb := msb(int32_to_uint32(src));
            Dmsb := msb(int32_to_uint32(dest));
            Rmsb := msb(int32_to_uint32(result));
            self.post_ea(ea_x);
            self.post_ea(ea_y);
         when others =>  --  Should never happen due to previous checks
            Ada.Text_IO.Put_Line("Unimplemented CMPM data size");
      end case;
      self.psw.negative := Rmsb;
      self.psw.zero := (result = 0);
      self.psw.overflow := ((not Smsb) and Dmsb and (not Rmsb)) or
                            (Smsb and (not Dmsb) and Rmsb);
      self.psw.carry := (Smsb and not Dmsb) or (Rmsb and not Dmsb) or (Smsb and Rmsb);
   end;
   --
   procedure decode_EOR(self : in out m68000) is
      reg_y  : reg_num := instr_cmp.reg_y;
      mode_y : mode_code := instr_cmp.mode_y;
      reg_x  : reg_num := instr_cmp.reg_x;
      mode   : uint3 := instr_cmp.opmode;
   begin
      Ada.Text_IO.Put_Line("Decoding EOR instruction");
      case mode is
         when 4 =>  --  EOR.B
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_byte);
               b1 : byte := byte(self.get_ea(ea));
               b2 : byte := self.get_regb(Data, reg_x);
               res : byte;
            begin
               res := b1 xor b2;
               self.set_ea(ea, long(res));
               self.post_ea(ea);
               self.psw.zero := (res = 0);
               self.psw.negative := msb(res);
            end;
         when 5 =>  --  EOR.W
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_word);
               w1 : word := word(self.get_ea(ea));
               w2 : word := self.get_regw(Data, reg_x);
               res : word;
            begin
               res := w1 xor w2;
               self.set_ea(ea, long(res));
               self.post_ea(ea);
               self.psw.zero := (res = 0);
               self.psw.negative := msb(res);
            end;
         when 6 =>  --  EOR.L
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_long);
               l1 : long := self.get_ea(ea);
               l2 : long := self.get_regl(Data, reg_x);
               res : long;
            begin
               res := l1 xor l2;
               self.set_ea(ea, long(res));
               self.post_ea(ea);
               self.psw.zero := (res = 0);
               self.psw.negative := msb(res);
            end;
         when others =>  --  Should never happen due to previos checks
            Ada.Text_IO.Put_Line("Unimplemented EOR data size.");
      end case;
      self.psw.overflow := False;
      self.psw.carry := False;
   end;
end;

