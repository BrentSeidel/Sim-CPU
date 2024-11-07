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
end;
