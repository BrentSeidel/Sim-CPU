with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_9 is
   --
   --  Package for decoding Line9 instructions - SUB/SUBX
   --
   procedure decode_9(self : in out m68000) is
   begin
      --
      --  Note that some addressing modes for the SUB instruction are
      --  unusable and have been repurposed for SUBX instructions.  Need
      --  to check for that.
      --
      if instr_subx.code1 = 0 and instr_subx.code2 and instr_subx.size /= data_long_long then
         decode_SUBX(self);
      else
         decode_SUB(self);
      end if;
   end;
   --
   procedure decode_SUB(self : in out m68000) is
      reg_x  : reg_num := instr_sub.reg_x;
      reg_y  : reg_num := instr_sub.reg_y;
      mode_y : mode_code := instr_sub.mode_y;
      opmode : uint3 := instr_sub.opmode;
      src    : long;
      dest   : long;
      diff   : long;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
      Ada.Text_IO.Put_Line("Processing SUB instruction.");
      case opmode is
        when 0 =>  --  Byte Dn - <ea> -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              dest := long(self.get_regb(Data, reg_x));
              src  := self.get_ea(ea);
              diff := dest - src;
              self.set_regb(Data, reg_x, byte(diff and 16#FF#));
              self.post_ea(ea);
           end;
        when 1 =>  --  Word Dn - <ea> -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              dest := long(self.get_regw(Data, reg_x));
              src  := self.get_ea(ea);
              diff := dest - src;
              self.set_regw(Data, reg_x, word(diff and 16#FFFF#));
              self.post_ea(ea);
           end;
        when 2 =>  --  Long Dn - <ea> -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              dest := self.get_regl(Data, reg_x);
              src  := self.get_ea(ea);
              diff := dest - src;
              self.set_regl(Data, reg_x, diff);
              self.post_ea(ea);
           end;
        when 3 =>  --  Word An - <ea> -> An (SUBA instruction)
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              dest := long(self.get_regw(Address, reg_x));
              src  := self.get_ea(ea);
              diff := dest - src;
              self.set_regw(Address, reg_x, word(diff and 16#FFFF#));
              self.post_ea(ea);
           end;
        when 4 =>  --  Byte <ea> - Dn -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              src  := long(self.get_regb(Data, reg_x));
              dest := self.get_ea(ea);
              diff := dest - src;
              self.set_ea(ea, diff and 16#FF#);
              self.post_ea(ea);
           end;
        when 5 =>  --  Word <ea> - Dn -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              src  := long(self.get_regw(Data, reg_x));
              dest := self.get_ea(ea);
              diff := dest - src;
              self.set_ea(ea, diff and 16#FFFF#);
              self.post_ea(ea);
           end;
        when 6 =>  --  Long <ea> - Dn -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              src  := self.get_regl(Data, reg_x);
              dest := self.get_ea(ea);
              diff := dest - src;
              self.set_ea(ea, diff);
              self.post_ea(ea);
           end;
        when 7 =>  --  Long An - <ea> -> An (SUBA instruction)
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              dest := self.get_regl(Address, reg_x);
              src  := self.get_ea(ea);
              diff := dest - src;
              self.set_regl(Address, reg_x, diff);
              self.post_ea(ea);
           end;
      end case;
      --
      --  Compute condition codes
      --
      case opmode is
         when 0 =>  --  Byte size
            self.psw.zero := (diff and 16#FF#) = 0;
            Rmsb := (diff and 16#80#) /= 0;
            Smsb := (src  and 16#80#) /= 0;
            Dmsb := (dest and 16#80#) /= 0;
         when 1 | 5 =>  --  Word size
            self.psw.zero := (diff and 16#FFFF#) = 0;
            Rmsb := (diff and 16#8000#) /= 0;
            Smsb := (src  and 16#8000#) /= 0;
            Dmsb := (dest and 16#8000#) /= 0;
         when 2 | 6 =>  --  Long size
            self.psw.zero := (diff and 16#FFFF_FFFF#) = 0;
            Rmsb := (diff and 16#8000_0000#) /= 0;
            Smsb := (src  and 16#8000_0000#) /= 0;
            Dmsb := (dest and 16#8000_0000#) /= 0;
         when others =>  --  Modes 3 & 7 do not affect condition codes
            null;
      end case;
      --
      --  Carry, Extend, and Overflow
      --
      if (opmode /= 3) and (opmode /= 7) then
         self.psw.Carry := (Smsb and (not Dmsb)) or (Rmsb and (not Dmsb))
                        or (Smsb and Rmsb);
         self.psw.Extend := self.psw.Carry;
         self.psw.Overflow := ((not Smsb) and Dmsb and (not Rmsb))
                           or (Smsb and (not Dmsb) and Rmsb);
         self.psw.Negative := Rmsb;
      end if;
   end;
   --
   procedure decode_SUBX(self : in out m68000) is
      reg_x   : reg_num := instr_subx.reg_x;
      reg_y   : reg_num := instr_subx.reg_y;
      reg_mem : reg_type := instr_subx.reg_mem;
      Smsb    : Boolean;
      Dmsb    : Boolean;
      Rmsb    : Boolean;
   begin
      Ada.Text_IO.Put_Line("Processing SUBX instruction");
      case instr_subx.size is
         when data_byte =>
            declare
               dest : byte;
               src  : byte;
               diff : byte;
            begin
               if reg_mem = data then
                  dest := self.get_regb(data, reg_x);
                  src  := self.get_regb(data, reg_y);
               else
                  self.set_regl(address, reg_x,
                     self.get_regl(address, reg_x) - 1);
                  self.set_regl(address, reg_y,
                     self.get_regl(address, reg_y) - 1);
                  dest := self.memory(self.get_regl(address, reg_x));
                  src  := self.memory(self.get_regl(address, reg_y));
               end if;
               diff := dest - src;
               if self.psw.extend then
                  diff := diff - 1;
               end if;
               if diff /= 0 then
                  self.psw.zero := False;
               end if;
               if reg_mem = data then
                  self.set_regb(data, reg_x, diff);
               else
                  self.memory(self.get_regl(address, reg_x), diff);
               end if;
               Rmsb := msb(diff);
               Smsb := msb(src);
               Dmsb := msb(dest);
            end;
         when data_word =>
            declare
               dest : word;
               src  : word;
               diff : word;
            begin
               if reg_mem = data then
                  dest := self.get_regw(data, reg_x);
                  src  := self.get_regw(data, reg_y);
               else
                  self.set_regl(address, reg_x,
                     self.get_regl(address, reg_x) - 2);
                  self.set_regl(address, reg_y,
                     self.get_regl(address, reg_y) - 2);
                  dest := self.memory(self.get_regl(address, reg_x));
                  src  := self.memory(self.get_regl(address, reg_y));
               end if;
               diff := dest - src;
               if self.psw.extend then
                  diff := diff - 1;
               end if;
               if diff /= 0 then
                  self.psw.zero := False;
               end if;
               if reg_mem = data then
                  self.set_regw(data, reg_x, diff);
               else
                  self.memory(self.get_regl(address, reg_x), diff);
               end if;
               Rmsb := msb(diff);
               Smsb := msb(src);
               Dmsb := msb(dest);
            end;
         when data_long =>
            declare
               dest : long;
               src  : long;
               diff : long;
            begin
               if reg_mem = data then
                  dest := self.get_regl(data, reg_x);
                  src  := self.get_regl(data, reg_y);
               else
                  self.set_regl(address, reg_x,
                     self.get_regl(address, reg_x) - 4);
                  self.set_regl(address, reg_y,
                     self.get_regl(address, reg_y) - 4);
                  dest := self.memory(self.get_regl(address, reg_x));
                  src  := self.memory(self.get_regl(address, reg_y));
               end if;
               diff := dest - src;
               if self.psw.extend then
                  diff := diff - 1;
               end if;
               if diff /= 0 then
                  self.psw.zero := False;
               end if;
               if reg_mem = data then
                  self.set_regl(data, reg_x, diff);
               else
                  self.memory(self.get_regl(address, reg_x), diff);
               end if;
               Rmsb := msb(diff);
               Smsb := msb(src);
               Dmsb := msb(dest);
            end;
         when others =>
            null;
      end case;
      --
      --  Carry, Extend, and Overflow
      --
      self.psw.Carry := (Smsb and (not Dmsb)) or ((not Dmsb) and Rmsb)
                     or (Smsb and Rmsb);
      self.psw.Extend := self.psw.Carry;
      self.psw.Negative := Rmsb;
      self.psw.Overflow := ((not Smsb) and Dmsb and (not Rmsb))
                        or (Smsb and (not Dmsb) and Rmsb);
   end;
end;
