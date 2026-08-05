--
--  Author: Brent Seidel
--  Date: 27-Jul-2026
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
package BBS.Sim_CPU.CPU.pdp11.options is
   --
   --  This package contains routines for reading and writing PDP-11 options.
   --
   --  Sets a specific option
   --
   procedure set_option(self : in out pdp11; opt : String; value : String);
   --
   --  Returns the value of a specific option as a string
   --
   function get_option(self : in out pdp11; opt : String) return String;
   --
   --  Prints the full list of options
   --
   procedure list_options(self : in out pdp11);
   --
private
   --
   --  Tries to parse a string as a boolean.  If it can't match anything, then
   --  valid is set to False and the return value should be ignored.
   --
   function parse_boolean(v : String; valid : in out Boolean) return Boolean;
   --
   --  Tries to parse a string as a word.  If it can't match anything, then
   --  valid is set to False and the return value should be ignored.
   --
   function parse_word(v : String; valid : in out Boolean) return word;
   --
   --  Constants for option names
   --
   opt_extra  : constant String := "EXTRA";
   opt_RTT    : constant String := "RTT";
   opt_EIS    : constant String := "EIS";
   opt_FIS    : constant String := "FIS";
   opt_FPP    : constant String := "FPP";
   opt_CIS    : constant String := "CIS";
   opt_USER   : constant String := "USER";
   opt_SUPER  : constant String := "SUPER";
   opt_ID     : constant String := "ID";
   opt_MMU18  : constant String := "MMU18";
   opt_MMU22  : constant String := "MMU22";
   opt_MTFPS  : constant String := "MTFPS";
   opt_SWAB   : constant String := "SWAB-V";
   opt_PSW_T  : constant String := "TRACE";
   opt_rValue : constant String := "R-VALUE";
   opt_rBus   : constant String := "R-BUS";
   opt_JMPREG : constant String := "JMP-REG";
   opt_stack  : constant String := "STACK";
   opt_switch : constant String := "SR";
   --
end;
