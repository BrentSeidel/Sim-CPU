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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.--
--
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
      temp8 : byte;
   begin
      temp8 := a;
      if ((temp8 and 16#0F#) > 6) or f.aux_carry then
         if ((temp8 and 16#0F#) + 6) > 16#0F# then
            f.aux_carry := True;
         else
            f.aux_carry := False;
         end if;
         temp8 := temp8 + 6;
      end if;
      if ((temp8/16#10# and 16#0F#) > 6) or f.carry then
         if ((temp8/16#10# and 16#0F#) + 6) > 16#0F# then
            f.carry := True;
         else
            f.carry := False;
         end if;
         temp8 := temp8 + 16#60#;
      end if;
      return temp8;
   end;
end;
