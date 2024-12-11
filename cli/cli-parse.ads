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
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Strings.Maps.Constants;
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
   type token_type is (Word, quoted_string, Comment, Number, Error);
   --
   --  Discard any leading spaces
   --
   function trim(s : Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String;
   --
   --  Split on whitespace
   --
   function split(first : out Ada.Strings.Unbounded.Unbounded_String;
                  rest : in out Ada.Strings.Unbounded.Unbounded_String)
      return token_type;
   --
   --  Interpret the next token as a hexidecimal number.
   --
   function nextHexValue(v : out BBS.uint32;
                       s : in out Ada.Strings.Unbounded.Unbounded_String)
      return token_type;
private
   function isHex(c : Character) return Boolean is
     ((c >= '0' and c <= '9') or (c >= 'A' and c <= 'F') or (c >= 'a' and c <= 'f'))
       with Global => Null;
   pragma Pure_Function(isHex);
   function toHex(v : out BBS.uint32; s : String) return token_type;
   function hexDigit(c : Character) return BBS.uint32
      with pre => (isHex(c));

end cli.parse;
