--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
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
--  Package for exception related processing
--
package BBS.Sim_CPU.m68000.exceptions is
--
--  Not sure if it is better to represent these as an enum.  This will
--  probably be sparse list and the actual values are needed to compute
--  the offset to the vector in memory.
--
   ex_0_reset_ssp  : constant byte :=  0;
   ex_1_reset_pc   : constant byte :=  1;  --  Part of the reset exception
   ex_2_bus_err    : constant byte :=  2;
   ex_3_addr_err   : constant byte :=  3;  --  Odd address
   ex_4_ill_inst   : constant byte :=  4;
   ex_5_div0       : constant byte :=  5;
   ex_6_CHK        : constant byte :=  6;
   ex_7_TRAPV      : constant byte :=  7;
   ex_8_priv_viol  : constant byte :=  8;
   ex_9_trace      : constant byte :=  9;
   ex_10_line_1010 : constant byte := 10;
   ex_11_line_1111 : constant byte := 11;
   ex_14_format    : constant byte := 14;  --  68010 only (and later?)
   ex_24_spurious  : constant byte := 24;
   ex_32_TRAP_base : constant byte := 32;  --  Add trap number to get ex

   procedure process_exception(self : in out m68000; ex_num : byte; prio : byte := 255);
   procedure perform_exception(self : in out m68000);
end;
