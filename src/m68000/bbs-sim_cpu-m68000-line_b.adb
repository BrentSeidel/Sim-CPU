with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS.embed;
use type BBS.embed.int32;
package body BBS.Sim_CPU.m68000.line_b is
   function uint32_to_int32 is
      new Ada.Unchecked_Conversion(source => BBS.embed.uint32, target => BBS.embed.int32);
   function int32_to_uint32 is
      new Ada.Unchecked_Conversion(source => BBS.embed.int32, target => BBS.embed.uint32);

   --
   --  Package for decoding Line 6 instructions - CMP/EOR
   --
   procedure decode_b(self : in out m68000) is
      mode : uint3 := instr_cmp.opmode;
   begin
      if (mode = 0) or (mode = 1) or (mode = 2) or (mode = 3) or (mode = 7) then
         decode_cmp(self);
      elsif ((mode = 4) or (mode = 5) or (mode = 6)) and (instr_cmpm.code1 = 1) then
         decode_cmpm(self);
      else
         Ada.Text_IO.Put_Line("Unimplemented CMP/EOR instructions");
      end if;
   end;
   --
   procedure decode_cmp(self : in out m68000) is
      reg_y  : uint3 := instr_cmp.reg_y;
      mode_y : uint3 := instr_cmp.mode_y;
      reg_x  : uint3 := instr_cmp.reg_x;
      mode   : uint3 := instr_cmp.opmode;
      src    : BBS.embed.int32;
      dest   : BBS.embed.int32;
      result : BBS.embed.int32;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
      Ada.Text_IO.Put_Line("Decoding CMP instruction with mode " & uint3'Image(mode));
      case mode is
         when 0 =>  --  CMP.B
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_byte);
               b1 : byte := byte(self.get_ea(ea, data_byte));
               b2 : byte := byte(self.get_regb(Data, reg_x));
            begin
               Ada.Text_IO.Put_Line("  Decoding CMP.B instruction");
               src := uint32_to_int32(sign_extend(b1));
               dest := uint32_to_int32(sign_extend(b2));
               result := dest - src;
               Smsb := (int32_to_uint32(src) and 16#80#) = 16#80#;
               Dmsb := (int32_to_uint32(dest) and 16#80#) = 16#80#;
               Rmsb := (int32_to_uint32(result) and 16#80#) = 16#80#;
               self.post_ea(reg_y, mode_y, data_byte);
            end;
         when 1 =>  --  CMP.W
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_word);
               w1 : word := word(self.get_ea(ea, data_word));
               w2 : word := word(self.get_regw(Data, reg_x));
            begin
               Ada.Text_IO.Put_Line("  Decoding CMP.W instruction");
               src := uint32_to_int32(sign_extend(w1));
               dest := uint32_to_int32(sign_extend(w2));
               result := dest - src;
               Smsb := (int32_to_uint32(src) and 16#8000#) = 16#8000#;
               Dmsb := (int32_to_uint32(dest) and 16#8000#) = 16#8000#;
               Rmsb := (int32_to_uint32(result) and 16#8000#) = 16#8000#;
               self.post_ea(reg_y, mode_y, data_word);
            end;
         when 2 =>  --  CMP.L
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_long);
            begin
               Ada.Text_IO.Put_Line("  Decoding CMP.L instruction");
               src := uint32_to_int32(self.get_ea(ea, data_long));
               dest := uint32_to_int32(self.get_regl(Data, reg_x));
               result := dest - src;
               Smsb := (int32_to_uint32(src) and 16#8000_0000#) = 16#8000_0000#;
               Dmsb := (int32_to_uint32(dest) and 16#8000_0000#) = 16#8000_0000#;
               Rmsb := (int32_to_uint32(result) and 16#8000_0000#) = 16#8000_0000#;
               self.post_ea(reg_y, mode_y, data_long);
            end;
         when 3 =>  --  CMPA.W
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_word);
               w1 : word := word(self.get_ea(ea, data_word));
               w2 : word := word(self.get_regw(Address, reg_x));
            begin
               Ada.Text_IO.Put_Line("  Decoding CMPA.W instruction");
               src := uint32_to_int32(sign_extend(w1));
               dest := uint32_to_int32(sign_extend(w2));
               result := dest - src;
               Smsb := (int32_to_uint32(src) and 16#8000#) = 16#8000#;
               Dmsb := (int32_to_uint32(dest) and 16#8000#) = 16#8000#;
               Rmsb := (int32_to_uint32(result) and 16#8000#) = 16#8000#;
               self.post_ea(reg_y, mode_y, data_word);
            end;
         when 7 =>  --  CMPA.L
            declare
               ea : operand := self.get_ea(reg_y, mode_y, data_long);
            begin
               Ada.Text_IO.Put_Line("  Decoding CMPA.L instruction");
               src := uint32_to_int32(self.get_ea(ea, data_long));
               dest := uint32_to_int32(self.get_regl(Address, reg_x));
               result := dest - src;
               Smsb := (int32_to_uint32(src) and 16#8000_0000#) = 16#8000_0000#;
               Dmsb := (int32_to_uint32(dest) and 16#8000_0000#) = 16#8000_0000#;
               Rmsb := (int32_to_uint32(result) and 16#8000_0000#) = 16#8000_0000#;
               self.post_ea(reg_y, mode_y, data_long);
            end;
         when others =>
            Ada.Text_IO.Put_Line("  Unimplemented CMP mode");
      end case;
      self.psw.negative := Rmsb;
      self.psw.zero := (result = 0);
      self.psw.overflow := ((not Smsb) and Dmsb and (not Rmsb)) or
                            (Smsb and (not Dmsb) and Rmsb);
      self.psw.carry := (Smsb and not Dmsb) or (Rmsb and not Dmsb) or (Smsb and Rmsb);
   end;
   --
   procedure decode_cmpm(self : in out m68000) is
      reg_x : uint3 := instr_cmpm.reg_x;
      reg_y : uint3 := instr_cmpm.reg_y;
      size  : data_size := instr_cmpm.size;
      ea_x  : operand := self.get_ea(reg_x, 3, instr_cmpm.size);
      ea_y  : operand := self.get_ea(reg_y, 3, instr_cmpm.size);
      src    : BBS.embed.int32;
      dest   : BBS.embed.int32;
      result : BBS.embed.int32;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
      case instr_cmpm.size is
         when data_byte =>  --  CMPM.B
            declare
               x : byte := byte(self.get_ea(ea_x, data_byte));
               y : byte := byte(self.get_ea(ea_y, data_byte));
            begin
               src := uint32_to_int32(sign_extend(y));
               dest := uint32_to_int32(sign_extend(x));
               Ada.Text_IO.Put_Line("Decoding CMPM.B instruction");
               result := dest - src;
               Smsb := (int32_to_uint32(src) and 16#80#) = 16#80#;
               Dmsb := (int32_to_uint32(dest) and 16#80#) = 16#80#;
               Rmsb := (int32_to_uint32(result) and 16#80#) = 16#80#;
               self.post_ea(reg_x, 3, data_byte);
               self.post_ea(reg_y, 3, data_byte);
            end;
         when data_word =>  --  CMPM.W
            declare
               x : word := word(self.get_ea(ea_x, data_word));
               y : word := word(self.get_ea(ea_y, data_word));
            begin
               src := uint32_to_int32(sign_extend(y));
               dest := uint32_to_int32(sign_extend(x));
               Ada.Text_IO.Put_Line("Decoding CMPM.W instruction");
               result := dest - src;
               Smsb := (int32_to_uint32(src) and 16#80#) = 16#80#;
               Dmsb := (int32_to_uint32(dest) and 16#80#) = 16#80#;
               Rmsb := (int32_to_uint32(result) and 16#80#) = 16#80#;
               self.post_ea(reg_x, 3, data_word);
               self.post_ea(reg_y, 3, data_word);
            end;
         when data_long =>  --  CMPM.L
            src := uint32_to_int32(self.get_ea(ea_y, data_long));
            dest := uint32_to_int32(self.get_ea(ea_x, data_long));
               Ada.Text_IO.Put_Line("Decoding CMPM.L instruction");
            result := dest - src;
            Smsb := (int32_to_uint32(src) and 16#80#) = 16#80#;
            Dmsb := (int32_to_uint32(dest) and 16#80#) = 16#80#;
            Rmsb := (int32_to_uint32(result) and 16#80#) = 16#80#;
            self.post_ea(reg_x, 3, data_long);
            self.post_ea(reg_y, 3, data_long);
         when others =>  --  Should never happen due to previous checks
            Ada.Text_IO.Put_Line("Unimplemented CMPM data size");
      end case;
      self.psw.negative := Rmsb;
      self.psw.zero := (result = 0);
      self.psw.overflow := ((not Smsb) and Dmsb and (not Rmsb)) or
                            (Smsb and (not Dmsb) and Rmsb);
      self.psw.carry := (Smsb and not Dmsb) or (Rmsb and not Dmsb) or (Smsb and Rmsb);
   end;
end;

