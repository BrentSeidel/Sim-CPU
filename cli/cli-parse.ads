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
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings;
with Ada.Strings.Maps;
use type Ada.Strings.Maps.Character_Set;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with BBS;
use type BBS.uint32;
--
--  It is likely that this package will eventually get separated out into
--  its own project.  This will enable common CLI processing for other
--  projects.
--
package cli.parse is
   --
   --  Indicates type of the next token.
   --
   type token_type is (Word, quoted_string, Comment, Number, Error, Missing);
   --
   --  Discard any leading spaces
   --
   function trim(s : Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String;
   --
   --  Split on whitespace.  String is passed in in "rest".  The next
   --  token is returned in "first" and "rest" is updated to have that
   --  token removed.
   --
   function split(first : out Ada.Strings.Unbounded.Unbounded_String;
                  rest : in out Ada.Strings.Unbounded.Unbounded_String)
      return token_type;
   --
   --  Interpret the next token as an unsigned hexidecimal number.
   --
   function nextHexValue(v : out BBS.uint32;
                       s : in out Ada.Strings.Unbounded.Unbounded_String)
      return token_type;
   --
   --  Interpret the next token as an unsigned decimal number.
   --
   function nextDecValue(v : out BBS.uint32;
                       s : in out Ada.Strings.Unbounded.Unbounded_String)
      return token_type;
   --
   --  Is character a decimal digit?
   --
   function isDigit(c : Character) return Boolean is (c >= '0' and c <= '9')
     with Global => Null;
   pragma Pure_Function(isDigit);
   --
   --  Is character an alphabetic character
   --
   function isAlpha(c : Character) return Boolean is
     ((c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z'))
     with Global => Null;
   pragma Pure_Function(isAlpha);
   --
   -- Is character a hexidecimal digit?
   --
   function isHex(c : Character) return Boolean is
     ((c >= '0' and c <= '9') or (c >= 'A' and c <= 'F') or (c >= 'a' and c <= 'f'))
     with Global => Null;
   pragma Pure_Function(isHex);
   --
   --  Print error messages for number.
   --    m - Module name, printed in front of message
   --    v - Value name, printed at end of message
   --  The error message is of the format:
   --  <m>: Error in number for <v> (if t = Error)
   --  <m>: Missing number for <v>  (if t = Missing)
   --  No message for other values of t.
   --
   procedure numErr(t : token_type; m : String; v : String);
   --
private
   --
   --  Space, back-space, tab, line-feed, vertical-tab, form-feed, and carriage return
   --
   whitespace : constant Ada.Strings.Maps.Character_Set :=
         Ada.Strings.Maps.To_Set(' ' & Character'Val(8) & Character'Val(9) &
         Character'Val(10) & Character'Val(11) & Character'Val(12) & Character'Val(13));
   --
   blackspace : constant Ada.Strings.Maps.Character_Set := not whitespace;
   --
   --  Character indicating the rest of the line is a comment.  A character_set
   --  is used here so that multiple comment characters can be used, if needed.
   --
   comment_chars : constant Ada.Strings.Maps.Character_Set :=
         Ada.Strings.Maps.To_Set(';');
   --
   --  Convert a string to uint32 in a specified base (2 to 36).  No sign
   --  character is allowed and the result is wrapped to a 32 bit value.
   --
   function toUnsigned(v : out BBS.uint32; b : Natural; s : String) return token_type
      with pre => ((b > 1) and (b <= 36));
   --
   --  Convert a string to an integer in a specified base (2 to 36).  The
   --  first character may be a + or - sign character.  If results exceed
   --  the Integer range, an exception is expected.
   --
   function toInteger(v : out Integer; b : Natural; s : String) return token_type
      with pre => ((b > 1) and (b <= 36));
   --
   function parseDigit(c : Character) return BBS.uint32
      with pre => (isDigit(c) or isAlpha(c));
   pragma Pure_Function(parseDigit);

end cli.parse;
