--
--  Author: Brent Seidel
--  Date: 8-Jun-2025
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
with Ada.Strings.Fixed;
with Ada.Text_IO;
package body BBS.Sim_CPU is
   --
   --  Simulator switches and lights
   --
   --
   --  Utility functions
   --
   hex_digit : String := "0123456789ABCDEF";
   --
   function toHex(value : byte) return String is
   begin
      return hex_digit(Integer(value/16#10#) + 1) & hex_digit(Integer(value and 16#0F#) + 1);
   end;
   --
   function toHex(value : word) return String is
   begin
      return hex_digit(Integer(value/16#1000#) + 1) &
        hex_digit(Integer((value/16#100#) and 16#0F#) + 1) &
        hex_digit(Integer((value/16#10#) and 16#0F#) + 1) &
        hex_digit(Integer(value and 16#0F#) + 1);
   end;
   --
   function toHex(value : uint32) return String is
   begin
      return hex_digit(Integer((value/16#1000_0000#) and 16#0F#) + 1) &
        hex_digit(Integer((value/16#0100_0000#) and 16#0F#) + 1) &
        hex_digit(Integer((value/16#0010_0000#) and 16#0F#) + 1) &
        hex_digit(Integer((value/16#0001_0000#) and 16#0F#) + 1) &
        hex_digit(Integer((value/16#0000_1000#) and 16#0F#) + 1) &
        hex_digit(Integer((value/16#0000_0100#) and 16#0F#) + 1) &
        hex_digit(Integer((value/16#0000_0010#) and 16#0F#) + 1) &
        hex_digit(Integer(value and 16#0F#) + 1);
   end;
   --
   --  Return a value from a hexidecimal string
   --
   function toHex(s : String) return uint32 is
      v : uint32 := 0;
   begin
      for i in s'Range loop
         exit when not isHex(s(i));
         v := v*16#10# + hexDigit(s(i));
      end loop;
      return v;
   end;
   --
   --  Return the hexidecimal digit
   --
   function hexDigit(c : Character) return uint32 is
   begin
      case c is
         when '0' =>
            return 0;
         when '1' =>
            return 1;
         when '2' =>
            return 2;
         when '3' =>
            return 3;
         when '4' =>
            return 4;
         when '5' =>
            return 5;
         when '6' =>
            return 6;
         when '7' =>
            return 7;
         when '8' =>
            return 8;
         when '9' =>
            return 9;
         when 'A' | 'a' =>
            return 10;
         when 'B' | 'b' =>
            return 11;
         when 'C' | 'c' =>
            return 12;
         when 'D' | 'd' =>
            return 13;
         when 'E' | 'e' =>
            return 14;
         when 'F' | 'f' =>
            return 15;
         when others =>
            return 0;
      end case;
   end;
   --
   --  ----------------------------------------------------------------------
   --
   --  Routines to help parsing data files.  This is to help avoid duplication
   --  of code in the child packages.
   --
   --  Parse a line of an Intex Hex file.
   --
   --  s     - The input string to parse
   --  count - The number of data bytes in the record
   --  addr  - The memory address for the data bytes
   --  rec   - The record type
   --  data  - An array containing the data bytes (0 .. count) are valid
   --  valid - True for a valid record parsed.
   --
   procedure IntelHex(s : String; count : out byte; addr : out word; rec : out byte;
                      data : out page; valid : out Boolean) is
      start : Natural := Ada.Strings.Fixed.Index(s, ":");
      ptr   : Natural := start + 1;
      t1    : uint32;
      t2    : uint32;
      check : byte;
   begin
      count := 0;
      addr  := 0;
      rec   := 1;
      data  := (others => 0);
      valid := False;
      --
      --  Get byte count
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := byte(t1);
      count := byte(t1);
      --
      -- Get address
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + byte(t1);
      if isHex(s(ptr)) then
         t2 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t2 := t2*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + byte(t2);
      addr := word(t1*16#100# + t2);
      --
      --  Get record type
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + byte(t1);
      rec := byte(t1);
      --
      --  Get data
      --
      if count > 0 then
         for i in 0 .. (count - 1) loop
            if isHex(s(ptr)) then
               t1 := hexDigit(s(ptr));
            else
               return;
            end if;
            ptr := ptr + 1;
            if isHex(s(ptr)) then
               t1 := t1*16 + hexDigit(s(ptr));
            else
               return;
            end if;
            ptr := ptr + 1;
            check := check + byte(t1);
            data(Integer(i)) := byte(t1);
         end loop;
      end if;
      --
      --  Check checksum
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + byte(t1);
      if check = 0 then
         valid := True;
      else
         Ada.Text_IO.Put_Line("Checksum value for " & s & " is " & toHex(check));
      end if;
   end;
   --
   --  Parse a line of an S-Record Hex file.
   --
   --  s     - The input string to parse
   --  count - The number of data bytes in the record
   --  addr  - The memory address for the data bytes
   --  rec   - The record type
   --  data  - An array containing the data bytes (0 .. count) are valid
   --  valid - True for a valid record parsed.
   --
   --  Valid S-Record Record Types
   --  0 - Header
   --  1 - Data (16 bit address)
   --  2 - Data (24 bit address)
   --  3 - Data (32 bit address)
   --  4 - Reserved
   --  5 - Count (16 bit count)
   --  6 - Count (24 bit count)
   --  7 - Termination (32 bit start address)
   --  8 - Termination (24 bit start address)
   --  9 - Termination (16 bit start address)
   --
   procedure S_Record(s : String; count : out byte; addr : out ad_bus; rec : out byte;
                      data : out page; valid : out Boolean) is
      start : Natural := s'First;
      ptr   : Natural := start + 1;
      t1    : uint32 := 0;
      t2    : uint32 := 0;
      check : byte := 0;
      local_count : byte;
   begin
      count := 0;
      addr  := 0;
      rec   := 1;
      data  := (others => 0);
      valid := False;
      --
      --  Valid lines start with an S.
      if s(start) /= 'S' then
        return;
      end if;
      --
      --  Valid record types are 0-9.
      --
      rec := byte(hexDigit(s(ptr)));
      if rec < 0 or rec > 9 then
        return;
      end if;
      --
      --  Record type 0 is header with no address
      --
      if rec = 0 then
         valid := True;
      end if;
      --
      --  Record type 4 is reserved
      --
      if rec = 4 then
        return;
      end if;
      ptr := ptr + 1;
      --
      --  Get byte count
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := byte(t1);
      count := byte(t1);
      local_count := count;
      --
      --  Get address 16-bit
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + byte(t1);
      if isHex(s(ptr)) then
         t2 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t2 := t2*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + byte(t2);
      addr := ad_bus(t1*16#100# + t2);
      local_count := local_count - 2;
      --
      --  Record types 2 and 8 have 24 bit addresses.
      --
      if rec = 2 or rec = 8 or rec = 3 or rec = 7 then
        if isHex(s(ptr)) then
           t1 := hexDigit(s(ptr));
        else
           return;
        end if;
        ptr := ptr + 1;
        if isHex(s(ptr)) then
           t1 := t1*16 + hexDigit(s(ptr));
        else
           return;
        end if;
        ptr := ptr + 1;
        check := check + byte(t1);
        addr := addr*16#100# + ad_bus(t1);
        local_count := local_count - 1;
      end if;
      --
      --  Record types 3 and 7 have 32 bit addresses.
      --
      if rec = 3 or rec = 7 then
        if isHex(s(ptr)) then
           t1 := hexDigit(s(ptr));
        else
           return;
        end if;
        ptr := ptr + 1;
        if isHex(s(ptr)) then
           t1 := t1*16 + hexDigit(s(ptr));
        else
           return;
        end if;
        ptr := ptr + 1;
        check := check + byte(t1);
        addr := addr*16#100# + ad_bus(t1);
        local_count := local_count - 1;
      end if;
      --
      --  Now collect the data
      --
      if local_count > 0 then
         for i in 1 .. (local_count - 1) loop
            if isHex(s(ptr)) then
               t1 := hexDigit(s(ptr));
            else
               return;
            end if;
            ptr := ptr + 1;
            if isHex(s(ptr)) then
               t1 := t1*16 + hexDigit(s(ptr));
            else
               return;
            end if;
            ptr := ptr + 1;
            check := check + byte(t1);
            data(Integer(i - 1)) := byte(t1);
         end loop;
      end if;
      --
      --  Check checksum
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + byte(t1);
      if check = 255 then
         valid := True;
      else
         Ada.Text_IO.Put_Line("Checksum value for " & s & " is " & toHex(check));
      end if;
   end;
   --
end BBS.Sim_CPU;
