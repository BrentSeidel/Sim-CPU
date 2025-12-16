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
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
--
--  Package for exception related processing.
--
package body BBS.Sim_CPU.CPU.pdp11.exceptions is
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   --
   --  If an exception occurs, set the appropriate flag in the queue and
   --  set the flag to show that an exception has occurred.
   --
   procedure process_exception(self : in out pdp11; ex_num : byte; prio : byte := 255) is
   begin
      null;
   end;
   --
   --  Creates an exception stack frame for 68000/68008 processors.  The
   --  stack frame for later processors is more complicated and includes
   --  more information.
   --
   --  This should only be called when the except_occur flag is set.
   --
   procedure perform_exception(self : in out pdp11) is
      temp_psw : constant status_word := self.psw;
      new_psw  : status_word;
   begin
      null;
   end;
end;
