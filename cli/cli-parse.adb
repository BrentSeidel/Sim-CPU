--
--  Author: Brent Seidel
--  Date: 11-Dec-2024
--
--  This file is part of SimCPU CLI.
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
package body cli.parse is
   --
   --  Discard any leading spaces
   --
   function trim(s : Ada.Strings.Unbounded.Unbounded_String) return Ada.Strings.Unbounded.Unbounded_String is
      index : Natural;
   begin
      index := Ada.Strings.Unbounded.Index_Non_Blank(s, 1);
      if index > 0 then
         return Ada.Strings.Unbounded.Unbounded_Slice(s, index, Ada.Strings.Unbounded.Length(s));
      else
         return s;
      end if;
   end;
   --
   --  Split on whitespace
   --
   function split(first : out Ada.Strings.Unbounded.Unbounded_String;
                  rest : in out Ada.Strings.Unbounded.Unbounded_String)
      return token_type is
      temp  : Ada.Strings.Unbounded.Unbounded_String;
      index : Natural;
   begin
      index := ada.Strings.Unbounded.Index(rest, " ");
      if index = 0 then
         first := rest;
         rest  := Ada.Strings.Unbounded.Null_Unbounded_String;
      else
         first := Ada.Strings.Unbounded.Unbounded_Slice(rest, 1, index - 1);
         rest  := Ada.Strings.Unbounded.Unbounded_Slice(rest, index + 1,
                                                        Ada.Strings.Unbounded.Length(rest));
         rest := trim(rest);
      end if;
      if Ada.Strings.Unbounded.element(first, 1) = ';' then
         return Comment;
      else
         return Word;
      end if;
   end;
   --
   --  Interpret the next token as a hexidecimal number.
   --
   function nextHexValue(v : out BBS.uint32;
                       s : in out Ada.Strings.Unbounded.Unbounded_String)
      return token_type is
      t     : token_type;
      first : Ada.Strings.Unbounded.Unbounded_String;
   begin
      t := split(first, s);
      t := toHex(v,Ada.Strings.Unbounded.To_String(first));
      return t;
   end;
   --
   --  Return a value from a hexidecimal string
   --
   function toHex(v : out BBS.uint32; s : String) return token_type is
   begin
      v := 0;
      for i in s'Range loop
         if not isHex(s(i)) then
            return Error;
         end if;
         v := v*16#10# + hexDigit(s(i));
      end loop;
      return Number;
   end;
   --
   --  Return the hexidecimal digit
   --
   function hexDigit(c : Character) return BBS.uint32 is
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
end cli.parse;
