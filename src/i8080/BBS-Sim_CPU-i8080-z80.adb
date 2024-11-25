--
--  Author: Brent Seidel
--  Date: 5-Nov-2024
--
--  This file is part of SimCPU.
--  SimCPU is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  SimCPU is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
with Ada.Text_IO;
package body BBS.Sim_CPU.i8080.z80 is
   --
   --  Convert the Z-80 flags to a string representation
   --
   function flags(f : status_word) return String is
   begin
      return (if f.sign then "S" else "-") &
            (if f.zero then "Z" else "-") & "*" &
            (if f.aux_carry then "A" else "-") & "*" &
            (if f.parity then "P" else "-") &
            (if f.addsub then "N" else "-") &
            (if f.carry then "C" else "-");
   end;
   --
   --  Perform Z-80 DAA instruction
   --
   function daa(a : byte; f : in out status_word) return byte is
      temp8 : byte := a;
      lsd   : constant byte := a and 16#0F#;
      msd   : byte;
      p     : byte := 0;
   begin
      if f.addsub then   --  After subtraction
--
--  From Zilog book
--  C before   Upper digit  H before  Lower digit  Add value  C after
--      0          0-9          0        0-9          00         0*
--      0          0-8          1        6-F          FA         0*
--      1          7-F          0        0-9          A0         1*
--      1          6-F          1        6-F          9A         1*
--  There are a number of conditions not in this table.  Perhaps some
--  of them don't occur due to the nature of decimal operations.
--  These have been implemented as a "best guess".
--  The last condition seems to be in error.  Perhaps the add value should be 10?
--
         msd := (temp8/16#10# and 16#0F#);
         if f.carry then
            if f.aux_carry then
--               if lsd > 5 then
--                  temp8 := temp8 + 16#00#;
--               end if;
               if msd > 5 then
                  temp8 := temp8 + 16#10#;
               end if;
            else
               if lsd > 9 then
                  temp8 := temp8 - 6;  --  What should be done here???
               end if;
               if msd > 6 then
                  temp8 := temp8 + 16#A0#;
               end if;
            end if;
         else  --  Carry is zero
            if f.aux_carry then
               if lsd > 5 then
                  temp8 := temp8 + 16#0A#;
               end if;
               if msd < 9 then
                  temp8 := temp8 + 16#F0#;
               end if;
            else
               null;
            end if;
         end if;
      else              --  After addition
         if ((temp8 and 16#0F#) > 6) or f.aux_carry then
            if lsd > 16#0F# then
               f.aux_carry := True;
            else
               f.aux_carry := False;
            end if;
            temp8 := temp8 + 6;
         end if;
         msd := (temp8/16#10# and 16#0F#);
         if ( msd > 6) or f.carry then
            if (msd + 6) > 16#0F# then
               f.carry := True;
            else
               f.carry := False;
            end if;
            temp8 := temp8 + 16#60#;
         end if;
      end if;
      f.zero := (temp8 = 0);
      f.sign := ((temp8 and 16#80#) = 16#80#);
      f.aux_carry := False;  --  The manual says "see intructions", but provides no instructions.
      p := p + (if ((temp8 and 16#01#) = 16#01#) then 1 else 0);
      p := p + (if ((temp8 and 16#02#) = 16#02#) then 1 else 0);
      p := p + (if ((temp8 and 16#04#) = 16#04#) then 1 else 0);
      p := p + (if ((temp8 and 16#08#) = 16#08#) then 1 else 0);
      p := p + (if ((temp8 and 16#10#) = 16#10#) then 1 else 0);
      p := p + (if ((temp8 and 16#20#) = 16#20#) then 1 else 0);
      p := p + (if ((temp8 and 16#40#) = 16#40#) then 1 else 0);
      p := p + (if ((temp8 and 16#80#) = 16#80#) then 1 else 0);
      f.parity := not ((p and 16#01#) = 16#01#);  --  True is even parity
      return temp8;
   end;
   --
   --  Z-80 Two byte instructions
   --
   procedure prefix_cb(self : in out i8080) is
      inst    : byte;
      reg1    : reg8_index;
      bit_num : byte range 0 .. 7;
      bits    : constant array (byte range 0 .. 7) of byte := (16#01#, 16#02#, 16#04#, 16#08#,
                                                   16#10#, 16#20#, 16#40#, 16#80#);
      temp8   : byte;
      temp16  : word;
--      temppsw : status_word;
   begin
      inst := self.get_next;
      Ada.Text_IO.Put_Line("Processing CB extension code " & toHex(inst));
      case inst is
         when 16#00# .. 16#07# =>  --  RLC r, RLC (HL)
            reg1 := inst and 16#07#;
            temp16 := word(self.reg8(reg1, False))*2;
            if temp16 > 16#FF# then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#08# .. 16#0f# =>  --  RRC r, RRC (HL)
            reg1 := inst and 16#07#;
            temp8  := self.reg8(reg1, False);
            temp16 := word(temp8)/2;
            if (temp8 and 16#01#) = 16#01# then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
            if self.f.carry then
               temp16 := temp16 + 16#80#;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#10# .. 16#17# =>  --  RL r, RL (HL)
            reg1 := inst and 16#07#;
            temp16 := word(self.reg8(reg1, False))*2;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            if temp16 > 16#FF# then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#18# .. 16#1f# =>  --  RR r, RR (HL)
            reg1 := inst and 16#07#;
            temp8  := self.reg8(reg1, False);
            temp16 := word(temp8)/2;
            if self.f.carry then
               temp16 := temp16 + 16#80#;
            end if;
            if (temp8 and 16#01#) = 16#01# then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#20# .. 16#27# =>  --  SLA r, SLA (HL)
            reg1 := inst and 16#07#;
            temp16 := word(self.reg8(reg1, False))*2;
            if temp16 > 16#FF# then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#28# .. 16#2f# =>  --  SRA r, SRA (HL)
            reg1 := inst and 16#07#;
            temp8  := self.reg8(reg1, False);
            temp16 := word(temp8)/2;
            if (temp8 and 16#80#) = 16#80# then
               temp16 := temp16 + 16#80#;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#30# .. 16#37# =>  --  SLL r, SLL (HL) (undocumented)
            reg1 := inst and 16#07#;
            temp16 := word(self.reg8(reg1, False))*2 + 1;
            if temp16 > 16#FF# then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#38# .. 16#3f# =>  --  SRL r, SRL (HL)
            reg1 := inst and 16#07#;
            temp8  := self.reg8(reg1, False)/2;
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#40# .. 16#7f# =>  --  BIT b,r, BIT b,(HL)
            reg1    := inst and 16#07#;
            bit_num := inst/8 and 16#07#;
            temp8   := self.reg8(reg1, False);
            self.f.zero      := ((temp8 and bits(bit_num)) = 0);
            self.f.aux_carry := True;
            self.f.addsub    := False;
         when 16#80# .. 16#BF# =>  --  RES b,r, RES b,(HL)
            reg1    := inst and 16#07#;
            bit_num := inst/8 and 16#07#;
            temp8   := self.reg8(reg1, False);
            temp8   := temp8 and not bits(bit_num);
            self.reg8(reg1, temp8, False);
         when 16#C0# .. 16#FF# =>  --  SET b,r, SET b,(HL)
            reg1    := inst and 16#07#;
            bit_num := inst/8 and 16#07#;
            temp8   := self.reg8(reg1, False);
            temp8   := temp8 or bits(bit_num);
            self.reg8(reg1, temp8, False);
         when others =>
            Ada.Text_IO.Put_Line("Processing unrecognized CB extension code " & toHex(inst));
            self.unimplemented(self.pc, inst);
      end case;
   end;
   --
   --  =>ED40  IN B,(C)
   --  =>ED41  OUT (C),B
   --  =>ED42  SBC HL,BC
   --  =>ED43  LD (nn),BC
   --  ED44 NEG
   --  ED45 RETN
   --  ED46 IM 0
   --  ED47 LD I,A
   --  =>ED48 IN C,(C)
   --  =>ED49 OUT (C),C
   --  =>ED4A ADC HL,BC
   --  =>ED4B LD BC,(nn)
   --  ED4C NEG∗∗
   --  ED4D RETI
   --  ED4E IM 0∗∗
   --  ED4F LD R,A
   --  =>ED50 IN D,(C)
   --  =>ED51 OUT (C),D
   --  =>ED52 SBC HL,DE
   --  =>ED53 LD (nn),DE
   --  ED54 NEG∗∗
   --  ED55 RETN∗∗
   --  ED56 IM 1
   --  ED57 LD A,I
   --  =>ED58 IN E,(C)
   --  =>ED59 OUT (C),E ED5A
   --  =>ED5A ADC HL,DE
   --  =>ED5B LD DE,(nn)
   --  ED5C NEG∗∗
   --  ED5D RETN∗∗
   --  ED5E IM 2
   --  ED5F LD A,R
   --  =>ED60 IN H,(C)
   --  =>ED61 OUT (C),H
   --  =>ED62 SBC HL,HL
   --  =>ED63 LD (nn),HL
   --  ED64 NEG∗∗
   --  ED65 RETN∗∗
   --  ED66 IM 0∗∗
   --  ED67 RRD
   --  =>ED68 IN L,(C)
   --  =>ED69 OUT (C),L
   --  =>ED6A ADC HL,HL
   --  =>ED6B LD HL,(nn)
   --  ED6C NEG∗∗
   --  ED6D RETN∗∗
   --  ED6E IM 0∗∗
   --  ED6F RLD
   --  =>ED70 IN (C) / IN F,(C)∗∗
   --  =>ED71 OUT (C),0∗∗
   --  =>ED72 SBC HL,SP
   --  =>ED73 LD (nn),SP
   --  ED74 NEG∗∗
   --  ED75 RETN∗∗
   --  ED76 IM 1∗∗
   --  ED77 NOP∗∗
   --  =>ED78 IN A,(C)
   --  =>ED79 OUT (C),A
   --  =>ED7A ADC HL,SP
   --  =>ED7B LD SP,(nn)
   --  ED7C NEG∗∗
   --  ED7D RETN∗∗
   --  ED7E IM 2∗∗
   --  ED7F NOP∗∗
   --
   --  ** Undocumented instruction
   --  => Implemented below
   --
   procedure prefix_ed(self : in out i8080) is
      inst    : byte;
      temp8   : byte;
      data    : byte;
      temp16a : word;
      temp16b : word;
      temp16c : word;
      reg1    : reg8_index;
      reg2    : reg16_index;
   begin
      inst := self.get_next;
      Ada.Text_IO.Put_Line("Processing ED extension code " & toHex(inst));
      --
      --  EB group instructions that reference HL are not overridden by
      --  DD or FD prefixes to use IX or IY.
      --
      self.ptr := use_hl;
      --
      --  Note that for the IN/OUT r,(C) instructions, the actual Z-80 will
      --  put the contents of register B on the high 8 bits of the address
      --  lines thus letting these instructions access 65536 I/O ports.  This
      --  is not implemented here, but may be sometime if the need arises.
      --
      case inst is
         when 16#40# | 16#48# | 16#50# | 16#58# | 16#60# | 16#68# | 16#78# =>  --  IN r,(C)
            temp8 := self.c;
            reg1  := (inst/8) and 7;
            if self.in_override and (addr_bus(temp8) = self.in_over_addr) then
               data := byte(self.in_over_data and 16#FF#);
            else
               data := self.port(temp8);
            end if;
            self.reg8(reg1, data, False);
            self.setf(data);
            self.f.aux_carry := False;
            self.f.addsub := False;
            self.in_override := False;
         when 16#70# =>  --  IN F,(C)  (undocumented)
            temp8 := self.c;
            if self.in_override and (addr_bus(temp8) = self.in_over_addr) then
               data := byte(self.in_over_data and 16#FF#);
            else
               data := self.port(temp8);
            end if;
            self.setf(data);
            self.f.aux_carry := False;
            self.f.addsub := False;
            self.in_override := False;
         when 16#41# | 16#49# | 16#51# | 16#59# | 16#61# | 16#69# | 16#79# =>  --  OUT (C),r
            reg1  := (inst/8) and 7;
            temp8 := self.c;
            data  := self.reg8(reg1, False);
            self.port(temp8, data);
            self.last_out_addr := addr_bus(temp8);
            self.last_out_data := data_bus(data);
         when 16#71# =>  --  OUT (C),0  (undocumented)
            temp8 := self.c;
            self.port(temp8, 0);
            self.last_out_addr := addr_bus(temp8);
            self.last_out_data := data_bus(0);
         when 16#42# | 16#52# | 16#62# | 16#72# =>  --  SBC HL,r
            reg2 := (inst/16#10#) and 3;
            temp16a := word(self.h)*16#100# + word(self.l);
            temp16b := self.reg16(reg2, True);
            if self.f.carry then
               temp16b := temp16b + 1;
            end if;
            temp16c := temp16a - temp16b;
            if ((temp16a and 16#80#) /= (temp16b and 16#80#)) and
               (byte(temp16c and 16#80#) /= byte(temp16a and 16#80#))then
               self.f.parity := True;
            else
               self.f.parity := False;
            end if;
            if (((temp16a and 16#0fff#) - (temp16b and 16#0fff#)) and 16#f000#) = 0 then
               self.f.aux_carry := False;
            else
               self.f.aux_carry := True;
            end if;
            if ((uint32(temp16a) - uint32(temp16b)) and 16#f_0000#) = 0 then
               self.f.carry := False;
            else
               self.f.carry := True;
            end if;
            self.f.sign := (temp16c and 16#8000#) /= 0;
            self.f.zero := (temp16c = 0);
            self.f.addsub := True;
            self.h := byte(temp16c/16#100# and 16#ff#);
            self.l := byte(temp16c and 16#ff#);
         when 16#43# | 16#53# | 16#63# | 16#73# =>  --  LD (nn),dd
            temp16a := word(self.get_next);
            temp16a := temp16a + word(self.get_next)*16#100#;
            reg2 := (inst/16#10#) and 3;
            temp16b := self.reg16(reg2, True);
            self.memory(temp16a, byte(temp16b and 16#ff#), ADDR_DATA);
            self.memory(temp16a + 1, byte((temp16b/16#100#) and 16#ff#), ADDR_DATA);
         when 16#4A# | 16#5A# | 16#6A# | 16#7A# =>  --  ADC HL,r
            reg2 := (inst/16#10#) and 3;
            temp16a := word(self.h)*16#100# + word(self.l);
            temp16b := self.reg16(reg2, True);
            if self.f.carry then
               temp16b := temp16b + 1;
            end if;
            temp16c := temp16a + temp16b;
            if ((temp16a and 16#80#) /= (temp16b and 16#80#)) and
               (byte(temp16c and 16#80#) /= byte(temp16a and 16#80#))then
               self.f.parity := True;
            else
               self.f.parity := False;
            end if;
            if (((temp16a and 16#0fff#) + (temp16b and 16#0fff#)) and 16#f000#) = 0 then
               self.f.aux_carry := False;
            else
               self.f.aux_carry := True;
            end if;
            if ((uint32(temp16a) + uint32(temp16b)) and 16#f_0000#) = 0 then
               self.f.carry := False;
            else
               self.f.carry := True;
            end if;
            self.f.sign := (temp16c and 16#8000#) /= 0;
            self.f.zero := (temp16c = 0);
            self.f.addsub := False;
            self.h := byte(temp16c/16#100# and 16#ff#);
            self.l := byte(temp16c and 16#ff#);
         when 16#4B# | 16#5B# | 16#6B# | 16#7B# =>  --  LD dd,(nn)
            temp16a := word(self.get_next);
            temp16a := temp16a + word(self.get_next)*16#100#;
            temp16b := word(self.memory(temp16a, ADDR_DATA)) + word(self.memory(temp16a + 1, ADDR_DATA))*16#100#;
            reg2 := (inst/16#10#) and 3;
            self.reg16(reg2, temp16b, True);
            temp16b := self.reg16(reg2, True);
        when others =>
            Ada.Text_IO.Put_Line("Processing unrecognized ED extension code " & toHex(inst));
            self.unimplemented(self.pc, inst);
      end case;
   end;
   --
end;
