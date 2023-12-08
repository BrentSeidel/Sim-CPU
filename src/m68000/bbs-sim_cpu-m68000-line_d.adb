with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.line_d is
   --
   --  Package for decoding Line D (13) instructions - ADD/ADDA/ADDX
   --
   procedure decode_d(self : in out m68000) is
   begin
      --
      --  Note that some addressing modes for the ADD instruction are
      --  unusable and have been repurposed for ADDX instructions.  Need
      --  to check for that.
      --
      if instr_addx.code1 = 0 and instr_addx.code2 and instr_addx.size /= data_long_long then
         Ada.Text_IO.Put_Line("ADDX with reg x = " & uint3'Image(instr_addx.reg_x) &
            ", reg_y = " & uint3'Image(instr_addx.reg_y));
         addx_instr(self);
      else
         Ada.Text_IO.Put_Line("ADD with reg x = " & uint3'Image(instr_add.reg_x) &
            ", reg_y = " & uint3'Image(instr_add.reg_y));
         add_instr(self);
      end if;
   end;
   --
   procedure add_instr(self : in out m68000) is
      reg_x  : uint3 := instr_add.reg_x;
      reg_y  : uint3 := instr_add.reg_y;
      mode_y : uint3 := instr_add.mode_y;
      opmode : uint3 := instr_add.opmode;
      op1    : long;
      op2    : long;
      sum    : long;
      Smsb   : Boolean;
      Dmsb   : Boolean;
      Rmsb   : Boolean;
   begin
      case opmode is
        when 0 =>  --  Byte <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              self.post_ea(reg_y, mode_y, data_byte);
              op1 := long(self.get_regb(Data, reg_x));
              op2 := self.get_ea(ea, data_byte);
              sum := op1 + op2;
              self.set_regb(Data, reg_x, byte(sum and 16#FF#));
           end;
        when 1 =>  --  Word <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              self.post_ea(reg_y, mode_y, data_word);
              op1 := long(self.get_regw(Data, reg_x));
              op2 := self.get_ea(ea, data_word);
              sum := op1 + op2;
              self.set_regw(Data, reg_x, word(sum and 16#FFFF#));
           end;
        when 2 =>  --  Long <ea> + Dn -> Dn
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              self.post_ea(reg_y, mode_y, data_long);
              op1 := self.get_regl(Data, reg_x);
              op2 := self.get_ea(ea, data_long);
              sum := op1 + op2;
              self.set_regl(Data, reg_x, sum);
           end;
        when 3 =>  --  Word <ea> + An -> An (ADDA instruction)
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              self.post_ea(reg_y, mode_y, data_word);
              op1 := long(self.get_regw(Address, reg_x));
              op2 := self.get_ea(ea, data_word);
              sum := op1 + op2;
              self.set_regw(Address, reg_x, word(sum and 16#FFFF#));
           end;
        when 4 =>  --  Byte Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_byte);
           begin
              op1 := long(self.get_regb(Data, reg_x));
              op2 := self.get_ea(ea, data_byte);
              sum := op1 + op2;
              self.set_ea(ea, sum and 16#FF#, data_byte);
              self.post_ea(reg_y, mode_y, data_byte);
           end;
        when 5 =>  --  Word Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_word);
           begin
              op1 := long(self.get_regw(Data, reg_x));
              op2 := self.get_ea(ea, data_word);
              sum := op1 + op2;
              self.set_ea(ea, sum and 16#FFFF#, data_word);
              self.post_ea(reg_y, mode_y, data_word);
           end;
        when 6 =>  --  Long Dn + <ea> -> <ea>
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              op1 := self.get_regl(Data, reg_x);
              op2 := self.get_ea(ea, data_long);
              sum := op1 + op2;
              self.set_ea(ea, sum, data_long);
              self.post_ea(reg_y, mode_y, data_long);
           end;
        when 7 =>  --  Long <ea> + An -> An (ADDA instruction)
           declare
              ea : operand := self.get_ea(reg_y, mode_y, data_long);
           begin
              self.post_ea(reg_y, mode_y, data_long);
              op1 := self.get_regl(Address, reg_x);
              op2 := self.get_ea(ea, data_long);
              sum := op1 + op2;
              self.set_regl(Address, reg_x, sum);
           end;
      end case;
      --
      --  Compute condition codes
      --
      case opmode is
         when 0 =>  --  Byte size
            self.psw.zero := (sum and 16#FF#) = 0;
            self.psw.negative := (sum and 16#80#) = 16#80#;
            Rmsb := (sum and 16#80#) = 16#80#;
            Smsb := (op1 and 16#80#) = 16#80#;
            Dmsb := (op2 and 16#80#) = 16#80#;
         when 1 | 5 =>  --  Word size
            self.psw.zero := (sum and 16#FFFF#) = 0;
            self.psw.negative := (sum and 16#8000#) = 16#8000#;
            Rmsb := (sum and 16#8000#) = 16#8000#;
            Smsb := (op1 and 16#8000#) = 16#8000#;
            Dmsb := (op2 and 16#8000#) = 16#8000#;
         when 2 | 6 =>  --  Long size
            self.psw.zero := (sum and 16#FFFF_FFFF#) = 0;
            self.psw.negative := (sum and 16#8000_0000#) = 16#8000_0000#;
            Rmsb := (sum and 16#8000_0000#) = 16#8000_0000#;
            Smsb := (op1 and 16#8000_0000#) = 16#8000_0000#;
            Dmsb := (op2 and 16#8000_0000#) = 16#8000_0000#;
         when others =>  --  Modes 3 & 7 do not affect condition codes
            null;
      end case;
      --
      --  Carry, Extend, and Overflow
      --
      if (opmode /= 3) and (opmode /= 7) then
         self.psw.Carry := (Smsb and Dmsb) or ((not Rmsb) and Dmsb)
                        or (Smsb and (not Rmsb));
         self.psw.Extend := self.psw.Carry;
         self.psw.Overflow := (Smsb and Dmsb and (not Rmsb))
                           or ((not Smsb) and (not Dmsb) and Rmsb);
      end if;
   end;
   --
   procedure addx_instr(self : in out m68000) is
      reg_x   : uint3 := instr_addx.reg_x;
      reg_y   : uint3 := instr_addx.reg_y;
      reg_mem : reg_type := instr_addx.reg_mem;
      Smsb    : Boolean;
      Dmsb    : Boolean;
      Rmsb    : Boolean;
   begin
      case instr_addx.size is
         when data_byte =>
            declare
               op1 : byte;
               op2 : byte;
               sum : byte;
            begin
               if reg_mem = data then
                  op1 := self.get_regb(data, reg_x);
                  op2 := self.get_regb(data, reg_y);
               else
                  self.set_regl(address, reg_x,
                     self.get_regl(address, reg_x) - 1);
                  self.set_regl(address, reg_y,
                     self.get_regl(address, reg_y) - 1);
                  op1 := self.memory(self.get_regl(address, reg_x));
                  op2 := self.memory(self.get_regl(address, reg_y));
               end if;
               sum := op1 + op2;
               if self.psw.extend then
                  sum := sum + 1;
               end if;
               if sum /= 0 then
                  self.psw.zero := False;
               end if;
               if reg_mem = data then
                  self.set_regb(data, reg_x, sum);
               else
                  self.memory(self.get_regl(address, reg_x), sum);
               end if;
               Rmsb := (sum and 16#80#) = 16#80#;
               Smsb := (op1 and 16#80#) = 16#80#;
               Dmsb := (op2 and 16#80#) = 16#80#;
            end;
         when data_word =>
            declare
               op1 : word;
               op2 : word;
               sum : word;
            begin
               if reg_mem = data then
                  op1 := self.get_regw(data, reg_x);
                  op2 := self.get_regw(data, reg_y);
               else
                  self.set_regl(address, reg_x,
                     self.get_regl(address, reg_x) - 2);
                  self.set_regl(address, reg_y,
                     self.get_regl(address, reg_y) - 2);
                  op1 := self.memory(self.get_regl(address, reg_x));
                  op2 := self.memory(self.get_regl(address, reg_y));
               end if;
               sum := op1 + op2;
               if self.psw.extend then
                  sum := sum + 1;
               end if;
               if sum /= 0 then
                  self.psw.zero := False;
               end if;
               if reg_mem = data then
                  self.set_regw(data, reg_x, sum);
               else
                  self.memory(self.get_regl(address, reg_x), sum);
               end if;
               Rmsb := (sum and 16#8000#) = 16#8000#;
               Smsb := (op1 and 16#8000#) = 16#8000#;
               Dmsb := (op2 and 16#8000#) = 16#8000#;
            end;
         when data_long =>
            declare
               op1 : long;
               op2 : long;
               sum : long;
            begin
               if reg_mem = data then
                  op1 := self.get_regl(data, reg_x);
                  op2 := self.get_regl(data, reg_y);
               else
                  self.set_regl(address, reg_x,
                     self.get_regl(address, reg_x) - 4);
                  self.set_regl(address, reg_y,
                     self.get_regl(address, reg_y) - 4);
                  op1 := self.memory(self.get_regl(address, reg_x));
                  op2 := self.memory(self.get_regl(address, reg_y));
               end if;
               sum := op1 + op2;
               if self.psw.extend then
                  sum := sum + 1;
               end if;
               if sum /= 0 then
                  self.psw.zero := False;
               end if;
               if reg_mem = data then
                  self.set_regl(data, reg_x, sum);
               else
                  self.memory(self.get_regl(address, reg_x), sum);
               end if;
               Rmsb := (sum and 16#8000_0000#) = 16#8000_0000#;
               Smsb := (op1 and 16#8000_0000#) = 16#8000_0000#;
               Dmsb := (op2 and 16#8000_0000#) = 16#8000_0000#;
            end;
         when others =>
            null;
      end case;
      --
      --  Carry, Extend, and Overflow
      --
      self.psw.Carry := (Smsb and Dmsb) or ((not Rmsb) and Dmsb)
                     or (Smsb and (not Rmsb));
      self.psw.Extend := self.psw.Carry;
      self.psw.Overflow := (Smsb and Dmsb and (not Rmsb))
                        or ((not Smsb) and (not Dmsb) and Rmsb);
   end;
end;
