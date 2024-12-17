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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
package body cli.parse is
   --
   --  Discard any leading spaces
   --
   function trim(s : Ada.Strings.Unbounded.Unbounded_String) return Ada.Strings.Unbounded.Unbounded_String is
      index : Natural;
   begin
      index := Ada.Strings.Unbounded.Index(s, blackspace);
      if index > 0 then
         return Ada.Strings.Unbounded.Unbounded_Slice(s, index, Ada.Strings.Unbounded.Length(s));
      else
         return s;
      end if;
   end;
   --
   --  Split on whitespace.  String is passed in in "rest".  The next
   --  token is returned in "first" and "rest" is updated to have that
   --  token removed.
   --
   function split(first : out Ada.Strings.Unbounded.Unbounded_String;
                  rest : in out Ada.Strings.Unbounded.Unbounded_String)
      return token_type is
      temp  : Ada.Strings.Unbounded.Unbounded_String;
      index : Natural;
   begin
      index := ada.Strings.Unbounded.Index(rest, whitespace);
      if index = 0 then
         first := rest;
         rest  := Ada.Strings.Unbounded.Null_Unbounded_String;
      else
         first := Ada.Strings.Unbounded.Unbounded_Slice(rest, 1, index - 1);
         rest  := Ada.Strings.Unbounded.Unbounded_Slice(rest, index + 1,
                                                        Ada.Strings.Unbounded.Length(rest));
         rest := trim(rest);
      end if;
      --
      --  Check for comments.
      --  Note that any text not separated from the comment character is
      --  included in the token "first".  This may cause some oddities if
      --  comments are being echoed.  Since comments are suppse to be
      --  ignored, this shouldn't be a big issue.
      --
      if Ada.Strings.Unbounded.element(first, 1) = comment_char then
         return Comment;
      else
         return Word;
      end if;
   end;
   --
   --  Interpret the next token as an unsigned hexidecimal number.
   --
   function nextHexValue(v : out BBS.uint32;
                       s : in out Ada.Strings.Unbounded.Unbounded_String)
      return token_type is
      t     : token_type;
      first : Ada.Strings.Unbounded.Unbounded_String;
   begin
      t := split(first, s);
      t := toUnsigned(v, 16, Ada.Strings.Unbounded.To_String(first));
      return t;
   end;
   --
   --  Convert a string to uint32 in a specified base (2 to 36).  No sign
   --  character is allowed and the result is wrapped to a 32 bit value.
   --
   function toUnsigned(v : out BBS.uint32; b : Natural; s : String) return token_type is
      t : BBS.uint32;
   begin
      v := 0;
      if (b < 2) or (b > 36) then
         return Error;
      end if;
      for i in s'Range loop
         if not (isDigit(s(i)) or isAlpha(s(i))) then
            return Error;
         end if;
         t := parseDigit(s(i));
         if t >= BBS.uint32(b) then
            return Error;
         end if;
         v := v*BBS.uint32(b) + t;
      end loop;
      return Number;
   end;
   --
   --  Convert a string to an integer in a specified base (2 to 36).  The
   --  first character may be a + or - sign character.  If results exceed
   --  the Integer range, an exception is expected.
   --
   function toInteger(v : out Integer; b : Natural; s : String) return token_type is
      start : Integer := s'First;
      t     : Integer;
      neg   : Boolean := False;  --  Default to a positive number
   begin
      v := 0;
      if (b < 2) or (b > 36) then
         return Error;
      end if;
      --
      --  Check first character for '+' or '-'
      --
      if s(start) = '-' then
         neg := True;
         start := start + 1;
      elsif s(start) = '+' then
         start := start + 1;  --  Ignore a leading plus sign
      end if;
      for i in start .. s'Last loop
         if not (isDigit(s(i)) or isAlpha(s(i))) then
            return Error;
         end if;
         t := Integer(parseDigit(s(i)));
         if t >= Integer(b) then
            return Error;
         end if;
         v := v*Integer(b) + t;
      end loop;
      if neg then
         v := -v;
      end if;
      return Number;
   end;
   --
   --  Parses a single digit in any base up to 36 (10 numbers and 26 letters).
   --  Upper and lower case letters are treated the same.
   --
   function parseDigit(c : Character) return BBS.uint32 is
   begin
      case c is
         when '0' =>  return 0;
         when '1' =>  return 1;
         when '2' =>  return 2;
         when '3' =>  return 3;
         when '4' =>  return 4;
         when '5' =>  return 5;
         when '6' =>  return 6;
         when '7' =>  return 7;
         when '8' =>  return 8;
         when '9' =>  return 9;
         when 'A' | 'a' =>  return 10;
         when 'B' | 'b' =>  return 11;
         when 'C' | 'c' =>  return 12;
         when 'D' | 'd' =>  return 13;
         when 'E' | 'e' =>  return 14;
         when 'F' | 'f' =>  return 15;
         when 'G' | 'g' =>  return 16;
         when 'H' | 'h' =>  return 17;
         when 'I' | 'i' =>  return 18;
         when 'J' | 'j' =>  return 19;
         when 'K' | 'k' =>  return 20;
         when 'L' | 'l' =>  return 21;
         when 'M' | 'm' =>  return 22;
         when 'N' | 'n' =>  return 23;
         when 'O' | 'o' =>  return 24;
         when 'P' | 'p' =>  return 25;
         when 'Q' | 'q' =>  return 26;
         when 'R' | 'r' =>  return 27;
         when 'S' | 's' =>  return 28;
         when 'T' | 't' =>  return 29;
         when 'U' | 'u' =>  return 30;
         when 'V' | 'v' =>  return 31;
         when 'W' | 'w' =>  return 32;
         when 'X' | 'x' =>  return 33;
         when 'Y' | 'y' =>  return 34;
         when 'Z' | 'z' =>  return 35;
         when others =>  return 0;
      end case;
   end;
   --
end cli.parse;
