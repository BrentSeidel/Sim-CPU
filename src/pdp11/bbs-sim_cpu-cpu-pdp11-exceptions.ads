--
--  Author: Brent Seidel
--  Date: 2-Dec-2025
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
--  Package for exception related processing
--
package BBS.Sim_CPU.CPU.pdp11.exceptions is
   --
   --  List of constants for the defined PDP-11 interrupt/exception vectors.  Note
   --  that the numbers are in octal.  Each exception vector consists of a word long
   --  PC value and a word long PSW value.  Thus the vector numbers must all be
   --  multiples of 4.
   --
   ex_000_reserved  : constant byte := 8#000#;  --  Reserved
   ex_004_assorted  : constant byte := 8#004#;  --  Assorted reasons
   ex_010_res_inst  : constant byte := 8#010#;  --  Reserved instruction
   ex_014_trace     : constant byte := 8#014#;  --  Trace or breakpoint
   ex_020_iot       : constant byte := 8#020#;  --  IOT instruction
   ex_024_pwr_fail  : constant byte := 8#024#;  --  Power fail
   ex_030_emt       : constant byte := 8#030#;  --  EMT instruction
   ex_034_trap      : constant byte := 8#034#;  --  TRAP instruction
   ex_114_parity    : constant byte := 8#114#;  --  Parity error
   ex_240_pirq      : constant byte := 8#240#;  --  Programmed interrupt request (not in PDP-11 handbook)
   ex_244_float     : constant byte := 8#244#;  --  Floating point error
   ex_250_mmu       : constant byte := 8#250#;  --  Memory management error

   procedure process_exception(self : in out pdp11; ex_num : byte)
     with pre => ((ex_num and 3) = 0);
   procedure perform_exception(self : in out pdp11);
end;
