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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
--
--  Package for exception related processing.
--
package body BBS.Sim_CPU.CPU.m68000.exceptions is
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   --
   --  If an exception occurs, set the appropriate flag in the queue and
   --  set the flag to show that an exception has occurred.
   --
   procedure process_exception(self : in out m68000; ex_num : byte; prio : byte := 255) is
   begin
      self.except_pend(ex_num) := True;
      self.check_except := True;
      if ex_num = ex_2_bus_err then
         self.bus_error := True;
      end if;
      if (ex_num >= 25 and ex_num <= 31) or (ex_num >= 64 and ex_num <= 255) then
         self.except_prio(ex_num) := prio;
         Ada.Text_IO.Put_Line("CPU: Posting exception " & byte'Image(ex_num) &
            " with priority " & byte'Image(prio));
      else
         self.except_prio(ex_num) := 255;  --  Exceptions get the highest priority
      end if;
   end;
   --
   --  Creates an exception stack frame for 68000/68008 processors.  The
   --  stack frame for later processors is more complicated and includes
   --  more information.
   --
   --  This should only be called when the except_occur flag is set.
   --
   procedure perform_exception(self : in out m68000) is
      temp_psw : constant status_word := self.psw;
      new_psw  : status_word;
   begin
      Ada.Text_IO.Put_Line("CPU: Checking for exceptions.");
      new_psw := temp_psw;
      new_psw.trace0 := False;
      new_psw.trace1 := False;
      new_psw.super := True;
      self.bus_error := False;
      if self.except_pend(ex_0_reset_ssp) then
         --
         --  Don't bother pushing anything onto the stack for reset and
         --  clear all other pending exceptions.
         --
         self.lr_ctl.atype := ADDR_INST;
         self.ssp := self.memory(addr_bus(ex_0_reset_ssp) * 4);
         self.pc  := self.memory(addr_bus(ex_1_reset_pc) * 4);
         self.except_pend := (others => False);
         self.psw.mask := 7;
      else
         --
         --  Reset is exception 0 (handled above).  Exception 1 is used
         --  by reset, so start checking the rest at exception 2.
         for i in 2 .. self.except_pend'Last loop
            if self.except_pend(i) then
               if (byte(temp_psw.mask) = 0) or (self.except_prio(i) > byte(temp_psw.mask)) then
                  Ada.Text_IO.Put_Line("CPU: Taking exception " & byte'Image(i) &
                     " with priority " & byte'Image(self.except_prio(i)));
                  self.lr_ctl.atype := ADDR_DATA;
                  if i = ex_4_ill_inst then
                     --
                     --  For illegal instruction (and possibly others to be added
                     --  later), the PC points to the instruction causing the exception
                     --
                     self.push(True, self.inst_pc);
                     self.push(True, psw_to_word(temp_psw));
                     self.psw := new_psw;
                     self.pc := self.memory(addr_bus(i) * 4);
                  else
                     --
                     --  For other exceptions (privilege violation is one), the PC
                     --  points to the instruction after the one causing the exception.
                     --
                     self.push(True, self.pc);
                     self.push(True, psw_to_word(temp_psw));
                     self.psw := new_psw;
                     self.pc := self.memory(addr_bus(i) * 4);
                  end if;
                  --
                  --  Only set the mask for interrupts.
                  --
                  if self.except_prio(i) <= 7 then
                     self.psw.mask := interrupt_mask(self.except_prio(i) and 7);
                  end if;
                  --
                  --  Having built the stack frame, clear the exception and return.
                  --
                  self.except_pend(i) := False;
                  --
                  --  Check for double bus error.  If processing a bus error exception
                  --  and a bus error occurs, halt the processor.
                  --
                  if i = ex_2_bus_err and self.bus_error then
                     self.cpu_halt := True;
                  end if;
                  return;
               end if;
            end if;
         end loop;
      end if;
      --
      --  If we get to this point, there should be no more exceptions
      --  left in the queue, so clear this flag.
      --
      self.check_except := False;
   end;
end;
