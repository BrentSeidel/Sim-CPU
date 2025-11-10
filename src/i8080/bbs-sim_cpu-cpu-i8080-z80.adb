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
package body BBS.Sim_CPU.CPU.i8080.z80 is
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
            f.aux_carry := (lsd > 16#0F#);
            temp8 := temp8 + 6;
         end if;
         msd := (temp8/16#10# and 16#0F#);
         if ( msd > 6) or f.carry then
            f.carry := ((msd + 6) > 16#0F#);
            temp8 := temp8 + 16#60#;
         end if;
      end if;
      f.zero := (temp8 = 0);
      f.sign := ((temp8 and 16#80#) = 16#80#);
      f.aux_carry := False;  --  The manual says "see intructions", but provides no instructions.
      p := temp8 xor (temp8/2) xor (temp8/4) xor (temp8/8) xor
        (temp8/16) xor (temp8/32) xor (temp8/64) xor (temp8/128);
      f.parity := not ((p and 16#01#) = 16#01#);  --  True is even parity
      return temp8;
   end;
   --
   --  Z-80 Two byte instructions
   --
   procedure prefix_cb(self : in out i8080) is
      inst    : constant byte := self.get_next;
      reg1    : constant reg8_index := inst and 16#07#;
      bit_num : byte range 0 .. 7;
      bits    : constant array (byte range 0 .. 7) of byte := (16#01#, 16#02#, 16#04#, 16#08#,
                                                   16#10#, 16#20#, 16#40#, 16#80#);
      temp8   : byte;
      temp16  : word;
   begin
      case inst is
         when 16#00# .. 16#07# =>  --  RLC r, RLC (HL)
            temp16 := word(self.reg8(reg1, False))*2;
            self.f.carry := (temp16 > 16#FF#);
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#08# .. 16#0f# =>  --  RRC r, RRC (HL)
            temp8  := self.reg8(reg1, False);
            temp16 := word(temp8)/2;
            self.f.carry := ((temp8 and 16#01#) = 16#01#);
            if self.f.carry then
               temp16 := temp16 + 16#80#;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#10# .. 16#17# =>  --  RL r, RL (HL)
            temp16 := word(self.reg8(reg1, False))*2;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            self.f.carry := (temp16 > 16#FF#);
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#18# .. 16#1f# =>  --  RR r, RR (HL)
            temp8  := self.reg8(reg1, False);
            temp16 := word(temp8)/2;
            if self.f.carry then
               temp16 := temp16 + 16#80#;
            end if;
            self.f.carry := ((temp8 and 16#01#) = 16#01#);
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#20# .. 16#27# =>  --  SLA r, SLA (HL)
            temp16 := word(self.reg8(reg1, False))*2;
            self.f.carry := (temp16 > 16#FF#);
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#28# .. 16#2f# =>  --  SRA r, SRA (HL)
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
            temp16 := word(self.reg8(reg1, False))*2 + 1;
            self.f.carry := (temp16 > 16#FF#);
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#38# .. 16#3f# =>  --  SRL r, SRL (HL)
            temp8  := self.reg8(reg1, False)/2;
            self.reg8(reg1, temp8, False);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#40# .. 16#7f# =>  --  BIT b,r, BIT b,(HL)
            bit_num := inst/8 and 16#07#;
            temp8   := self.reg8(reg1, False);
            self.f.zero      := ((temp8 and bits(bit_num)) = 0);
            self.f.aux_carry := True;
            self.f.addsub    := False;
         when 16#80# .. 16#BF# =>  --  RES b,r, RES b,(HL)
            bit_num := inst/8 and 16#07#;
            temp8   := self.reg8(reg1, False);
            temp8   := temp8 and not bits(bit_num);
            self.reg8(reg1, temp8, False);
         when 16#C0# .. 16#FF# =>  --  SET b,r, SET b,(HL)
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
   procedure prefix_ed(self : in out i8080) is
      inst    : constant byte := self.get_next;
      temp    : bus_stat;
      temp8   : byte;
      data    : byte;
      temp16a : word;
      temp16b : word;
      temp16c : word;
      reg1    : reg8_index;
      reg2    : reg16_index;
   begin
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
            reg1 := (inst/8) and 7;
            data := self.port(self.c);
            self.reg8(reg1, data, False);
            self.setf(data);
            self.f.aux_carry := False;
            self.f.addsub := False;
            self.in_override := False;
         when 16#70# =>  --  IN F,(C)  (undocumented)
            data := self.port(self.c);
            self.setf(data);
            self.f.aux_carry := False;
            self.f.addsub := False;
            self.in_override := False;
         when 16#41# | 16#49# | 16#51# | 16#59# | 16#61# | 16#69# | 16#79# =>  --  OUT (C),r
            reg1 := (inst/8) and 7;
            data := self.reg8(reg1, False);
            self.port(self.c, data);
         when 16#71# =>  --  OUT (C),0  (undocumented)
            self.port(self.c, 0);
         when 16#42# | 16#52# | 16#62# | 16#72# =>  --  SBC HL,r
            reg2 := (inst/16#10#) and 3;
            temp16a := word(self.h)*16#100# + word(self.l);
            temp16b := self.reg16(reg2, True);
            if self.f.carry then
               temp16b := temp16b + 1;
            end if;
            temp16c := temp16a - temp16b;
            self.f.parity := ((temp16a and 16#80#) /= (temp16b and 16#80#)) and
                              (byte(temp16c and 16#80#) /= byte(temp16a and 16#80#));
            self.f.aux_carry := (((temp16a and 16#0fff#) - (temp16b and 16#0fff#)) and 16#f000#) /= 0;
            self.f.carry  := ((uint32(temp16a) - uint32(temp16b)) and 16#f_0000#) /= 0;
            self.f.sign   := (temp16c and 16#8000#) /= 0;
            self.f.zero   := (temp16c = 0);
            self.f.addsub := True;
            self.h := byte(temp16c/16#100# and 16#ff#);
            self.l := byte(temp16c and 16#ff#);
         when 16#43#  | 16#53# | 16#63# | 16#73# =>  --  LD (nn),dd
            temp16a := word(self.get_next);
            temp16a := temp16a + word(self.get_next)*16#100#;
            reg2 := (inst/16#10#) and 3;
            temp16b := self.reg16(reg2, True);
            self.bus.writel16l(addr_bus(temp16a), temp16b, PROC_KERN, ADDR_DATA, temp);
         when 16#44# | 16#4C#| 16#54# | 16#5C# |
              16#64# | 16#6C# | 16#74# | 16#7C# =>  --  NEG (16#44# is documented, others are undocumented)
            temp8 := self.a;
            self.f.parity := (temp8 = 16#80#);
            self.f.carry  := (temp8 /= 0);
            temp8 := not temp8;
            self.a := temp8 + 1;
            self.f.aux_carry := ((temp8 and 16#0f#) + 1) > 16#0f#;
            self.f.addsub    := True;
            self.f.zero      := (self.a = 0);
            self.f.sign      := ((self.a and 16#80#) /= 0);
         when 16#45# | 16#4D# | 16#55# | 16#5D# |   --  Officially, 16#45# is RETN, 16#4D# is RETI
              16#65# | 16#6D# | 16#75# | 16#7D# =>  --  They are implemented the same.  Other values are undocumented
            self.int_enable := self.iff2;
            self.ret;
         when 16#46# | 16#4E# | 16#66# | 16#6E# =>  --  IM 0
            self.int_mode := 0;
         when 16#47# =>  --  LD I,A
            self.i := self.a;
         when 16#4A# | 16#5A# | 16#6A# | 16#7A# =>  --  ADC HL,r
            reg2 := (inst/16#10#) and 3;
            temp16a := word(self.h)*16#100# + word(self.l);
            temp16b := self.reg16(reg2, True);
            if self.f.carry then
               temp16b := temp16b + 1;
            end if;
            temp16c := temp16a + temp16b;
            self.f.parity := ((temp16a and 16#80#) /= (temp16b and 16#80#)) and
                              (byte(temp16c and 16#80#) /= byte(temp16a and 16#80#));
            self.f.aux_carry := (((temp16a and 16#0fff#) + (temp16b and 16#0fff#)) and 16#f000#) /= 0;
            self.f.carry  := ((uint32(temp16a) + uint32(temp16b)) and 16#f_0000#) /= 0;
            self.f.sign   := (temp16c and 16#8000#) /= 0;
            self.f.zero   := (temp16c = 0);
            self.f.addsub := False;
            self.h := byte(temp16c/16#100# and 16#ff#);
            self.l := byte(temp16c and 16#ff#);
         when 16#4B# | 16#5B# | 16#6B# | 16#7B# =>  --  LD dd,(nn)
            temp16a := word(self.get_next);
            temp16a := temp16a + word(self.get_next)*16#100#;
            temp16b := self.bus.readl16l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp);
            reg2 := (inst/16#10#) and 3;
            self.reg16(reg2, temp16b, True);
            temp16b := self.reg16(reg2, True);
         when 16#4F# =>  --  LD R,A
            self.r := self.a;
         when 16#56# | 16#76# =>  --  IM 1
            self.int_mode := 1;
         when 16#57# =>  --  LD A,I
            self.a := self.i;
            self.f.sign   := (self.a and 16#80#) /= 0;
            self.f.zero   := (self.a = 0);
            self.f.aux_carry := False;
            self.f.parity := self.iff2;
            self.f.addsub := False;
         when 16#5E# | 16#7E# =>  --  IM 2
            self.int_mode := 2;
         when 16#5F# =>  --  LD A,R
            self.a := self.r;
            self.f.sign   := (self.a and 16#80#) /= 0;
            self.f.zero   := (self.a = 0);
            self.f.aux_carry := False;
            self.f.parity := self.iff2;
            self.f.addsub := False;
         when 16#67# =>  --  RRD
            temp16a := word(self.h)*16#100# + word(self.l);
            temp16b := word(self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp)) or word((self.a and 16#0f#))*16#100#;
            self.a  := (self.a and 16#f0#) or byte(temp16b and 16#0f#);
            self.bus.writel8l(addr_bus(temp16a), byte((temp16b/16#10#) and 16#ff#), PROC_KERN, ADDR_DATA, temp);
            self.setf(self.a);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#6F# =>  --  RLD
            temp16a := word(self.h)*16#100# + word(self.l);
            temp16b := word(self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp))*16#10# or word(self.a and 16#0f#);
            self.a  := (self.a and 16#f0#) or byte((temp16b/16#100#) and 16#0f#);
            self.bus.writel8l(addr_bus(temp16a), byte(temp16b and 16#ff#), PROC_KERN, ADDR_DATA, temp);
            self.setf(self.a);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#77# | 16#7F# =>  --  NOP (undocumented)
            null;
         when 16#A0# =>  --  LDI
            temp16a := word(self.h)*16#100# + word(self.l);
            temp16b := word(self.d)*16#100# + word(self.e);
            self.bus.writel8l(addr_bus(temp16b), self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp), PROC_KERN, ADDR_DATA, temp);
            self.mod16(REG16_HL, 1);
            self.mod16(REG16_DE, 1);
            self.mod16(REG16_BC, -1);
            self.f.parity := (word(self.b)*16#100# + word(self.c) /= 0);
            self.f.aux_carry := False;
            self.f.addsub := False;
         when 16#A1# =>  --  CPI
            temp16a := word(self.h)*16#100# + word(self.l);
            temp8   := self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp);
            self.mod16(REG16_HL, 1);
            self.mod16(REG16_BC, -1);
            self.f.parity := (word(self.b)*16#100# + word(self.c) /= 0);
            self.f.addsub := True;
         when 16#A2# =>  --  INI
            temp16a := word(self.h)*16#100# + word(self.l);
            data := self.port(self.c);
            self.bus.writel8l(addr_bus(temp16a), data, PROC_KERN, ADDR_DATA, temp);
            self.mod16(REG16_HL, 1);
            self.f.aux_carry := ((self.b and 16#0F#) - 1 > 16#0F#);
            self.b := self.b - 1;
            self.setf(self.b);
            self.f.addsub := True;
            self.in_override := False;
         when 16#A3# =>  --  OUTI
            temp16a := word(self.h)*16#100# + word(self.l);
            data    := self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp);
            self.port(self.c, data);
            self.mod16(REG16_HL, 1);
            self.f.aux_carry := ((self.b and 16#0F#) - 1 > 16#0F#);
            self.b := self.b - 1;
            self.setf(self.b);
            self.f.addsub := True;
         when 16#A8# =>  --  LDD
            temp16a := word(self.h)*16#100# + word(self.l);
            temp16b := word(self.d)*16#100# + word(self.e);
            self.bus.writel8l(addr_bus(temp16b), self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp), PROC_KERN, ADDR_DATA, temp);
            self.mod16(REG16_HL, -1);
            self.mod16(REG16_DE, -1);
            self.mod16(REG16_BC, -1);
            self.f.parity := (word(self.b)*16#100# + word(self.c) /= 0);
            self.f.aux_carry := False;
            self.f.addsub := False;
         when 16#A9# =>  --  CPD
            temp16a := word(self.h)*16#100# + word(self.l);
            temp8   := self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp);
            temp8   := self.subf(self.a, temp8, False);
            self.mod16(REG16_HL, -1);
            self.mod16(REG16_BC, -1);
            self.f.parity := (word(self.b)*16#100# + word(self.c) /= 1);
            self.f.addsub := True;
         when 16#AA# =>  --  IND
            temp16a := word(self.h)*16#100# + word(self.l);
            data := self.port(self.c);
            self.bus.writel8l(addr_bus(temp16a), data, PROC_KERN, ADDR_DATA, temp);
            self.mod16(REG16_HL, -1);
            self.f.aux_carry := ((self.b and 16#0F#) - 1 > 16#0F#);
            self.b := self.b - 1;
            self.setf(self.b);
            self.f.addsub := True;
            self.in_override := False;
         when 16#AB# =>  --  OUTD
            temp16a := word(self.h)*16#100# + word(self.l);
            data    := self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp);
            self.port(self.c, data);
            self.mod16(REG16_HL, -1);
            self.f.aux_carry := ((self.b and 16#0F#) - 1 > 16#0F#);
            self.b := self.b - 1;
            self.setf(self.b);
            self.f.addsub := True;
         when 16#B0# =>  --  LDIR
            temp16a := word(self.h)*16#100# + word(self.l);
            temp16b := word(self.d)*16#100# + word(self.e);
            self.bus.writel8l(addr_bus(temp16b), self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp), PROC_KERN, ADDR_DATA, temp);
            self.mod16(REG16_HL, 1);
            self.mod16(REG16_DE, 1);
            self.mod16(REG16_BC, -1);
            self.f.parity := (word(self.b)*16#100# + word(self.c) /= 0);
            self.f.aux_carry := False;
            self.f.addsub := False;
            --
            --  Instruction is repeated until BC is equal to 0.
            --
            if self.f.parity then
               self.pc := self.pc - 2;
            end if;
         when 16#B1# =>  --  CPIR
            temp16a := word(self.h)*16#100# + word(self.l);
            temp8   := self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp);
            temp8   := self.subf(self.a, temp8, False);
            self.mod16(REG16_HL, 1);
            self.mod16(REG16_BC, -1);
            self.f.parity := (word(self.b)*16#100# + word(self.c) /= 0);
            self.f.addsub := True;
            --
            --  Instruction is repeated until BC is equal to 0 or a match is found.
            --
            if self.f.parity and (not self.f.zero) then
               self.pc := self.pc - 2;
            end if;
         when 16#B2# =>  --  INIR
            temp16a := word(self.h)*16#100# + word(self.l);
            data := self.port(self.c);
            self.bus.writel8l(addr_bus(temp16a), data, PROC_KERN, ADDR_DATA, temp);
            self.mod16(REG16_HL, 1);
            self.f.aux_carry := ((self.b and 16#0F#) - 1 > 16#0F#);
            self.b := self.b - 1;
            self.setf(self.b);
            self.f.addsub := True;
            self.in_override := False;
            --
            --  Instruction is repeated until B is equal to 0.
            --
            if not self.f.zero then
               self.pc := self.pc - 2;
            end if;
         when 16#B3# =>  --  OTIR
            temp16a := word(self.h)*16#100# + word(self.l);
            data    := self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp);
            self.port(self.c, data);
            self.mod16(REG16_HL, 1);
            self.f.aux_carry := ((self.b and 16#0F#) - 1 > 16#0F#);
            self.b := self.b - 1;
            self.setf(self.b);
            self.f.addsub := True;
            --
            --  Instruction is repeated until B is equal to 0.
            --
            if not self.f.zero then
               self.pc := self.pc - 2;
            end if;
         when 16#B8# =>  --  LDDR
            temp16a := word(self.h)*16#100# + word(self.l);
            temp16b := word(self.d)*16#100# + word(self.e);
            self.bus.writel8l(addr_bus(temp16b), self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp), PROC_KERN, ADDR_DATA, temp);
            self.mod16(REG16_HL, -1);
            self.mod16(REG16_DE, -1);
            self.mod16(REG16_BC, -1);
            self.f.parity := (word(self.b)*16#100# + word(self.c) /= 0);
            self.f.aux_carry := False;
            self.f.addsub := False;
            --
            --  Instruction is repeated until BC is equal to 0.
            --
            if self.f.parity then
               self.pc := self.pc - 2;
            end if;
         when 16#B9# =>  --  CPDR
            temp16a := word(self.h)*16#100# + word(self.l);
            temp8   := self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp);
            temp8   := self.subf(self.a, temp8, False);
            self.mod16(REG16_HL, -1);
            self.mod16(REG16_BC, -1);
            self.f.parity := (word(self.b)*16#100# + word(self.c) /= 0);
            self.f.addsub := True;
            --
            --  Instruction is repeated until BC is equal to 0 or a match is found.
            --
            if self.f.parity and (not self.f.zero) then
               self.pc := self.pc - 2;
            end if;
         when 16#BA# =>  --  INDR
            temp16a := word(self.h)*16#100# + word(self.l);
            data := self.port(self.c);
            self.bus.writel8l(addr_bus(temp16a), data, PROC_KERN, ADDR_DATA, temp);
            self.mod16(REG16_HL, -1);
            self.f.aux_carry := ((self.b and 16#0F#) - 1 > 16#0F#);
            self.b := self.b - 1;
            self.setf(self.b);
            self.f.addsub := True;
            self.in_override := False;
            --
            --  Instruction is repeated until B is equal to 0.
            --
            if not self.f.zero then
               self.pc := self.pc - 2;
            end if;
         when 16#BB# =>  --  OTDR
            temp16a := word(self.h)*16#100# + word(self.l);
            data    := self.bus.readl8l(addr_bus(temp16a), PROC_KERN, ADDR_DATA, temp);
            self.port(self.c, data);
            self.mod16(REG16_HL, -1);
            self.f.aux_carry := ((self.b and 16#0F#) - 1 > 16#0F#);
            self.b := self.b - 1;
            self.setf(self.b);
            self.f.addsub := True;
            --
            --  Instruction is repeated until B is equal to 0.
            --
            if not self.f.zero then
               self.pc := self.pc - 2;
            end if;
         when others =>
            Ada.Text_IO.Put_Line("Processing unrecognized ED extension code " & toHex(inst));
            self.unimplemented(self.pc, inst);
      end case;
   end;
   --
end;
