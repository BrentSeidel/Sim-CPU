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
      reg1    : byte;
      temp8   : byte;
      temp16  : word;
--      temppsw : status_word;
   begin
      inst := self.get_next;
      Ada.Text_IO.Put_Line("Processing CB extension code " & toHex(inst));
      case inst is
         when 16#00# | 16#01# | 16#02# | 16#03# |
              16#04# | 16#05# | 16#06# | 16#07# =>  --  RLC r, RLC (HL)
            reg1 := inst and 16#07#;
            temp16 := word(self.reg8(reg1))*2;
            if temp16 > 16#FF# then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#08# | 16#09# | 16#0a# | 16#0b# |
              16#0c# | 16#0d# | 16#0e# | 16#0f# =>  --  RRC r, RRC (HL)
            reg1 := inst and 16#07#;
            temp8  := self.reg8(reg1);
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
            self.reg8(reg1, temp8);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#10# | 16#11# | 16#12# | 16#13# |
              16#14# | 16#15# | 16#16# | 16#17# =>  --  RL r, RL (HL)
            reg1 := inst and 16#07#;
            temp16 := word(self.reg8(reg1))*2;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            if temp16 > 16#FF# then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#18# | 16#19# | 16#1a# | 16#1b# |
              16#1c# | 16#1d# | 16#1e# | 16#1f# =>  --  RR r, RR (HL)
            reg1 := inst and 16#07#;
            temp8  := self.reg8(reg1);
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
            self.reg8(reg1, temp8);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#20# | 16#21# | 16#22# | 16#23# |
              16#24# | 16#25# | 16#26# | 16#27# =>  --  SLA r, SLA (HL)
            reg1 := inst and 16#07#;
            temp16 := word(self.reg8(reg1))*2;
            if temp16 > 16#FF# then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#28# | 16#29# | 16#2a# | 16#2b# |
              16#2c# | 16#2d# | 16#2e# | 16#2f# =>  --  SRA r, SRA (HL)
            reg1 := inst and 16#07#;
            temp8  := self.reg8(reg1);
            temp16 := word(temp8)/2;
            if (temp8 and 16#80#) = 16#80# then
               temp16 := temp16 + 16#80#;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#30# | 16#31# | 16#32# | 16#33# |   --  Undocumented
              16#34# | 16#35# | 16#36# | 16#37# =>  --  SLL r, SLL (HL)
            Ada.Text_IO.Put_Line("Processing undocumented SLL instruction");
            reg1 := inst and 16#07#;
            temp16 := word(self.reg8(reg1))*2;
            if temp16 > 16#FF# then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
            temp8 := byte(temp16 and 16#FF#);
            self.reg8(reg1, temp8);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when 16#38# | 16#39# | 16#3a# | 16#3b# |
              16#3c# | 16#3d# | 16#3e# | 16#3f# =>  --  SRL r, SR: (HL)
            reg1 := inst and 16#07#;
            temp8  := self.reg8(reg1)/2;
            self.reg8(reg1, temp8);
            self.setf(temp8);
            self.f.aux_carry := False;
            self.f.addsub    := False;
         when others =>
            Ada.Text_IO.Put_Line("Unrecognized Z80 CB prefixed instruction");
            self.unimplemented(self.pc, inst);
      end case;
   end;
   --
   procedure prefix_dd(self : in out i8080) is
   begin
      Ada.Text_IO.Put_Line("Z-80 DD Prefix");
   end;
   --
   procedure prefix_ed(self : in out i8080) is
   begin
      Ada.Text_IO.Put_Line("Z-80 ED Prefix");
   end;
   --
   procedure prefix_fd(self : in out i8080) is
   begin
      Ada.Text_IO.Put_Line("Z-80 FD Prefix");
   end;
   --
end;
