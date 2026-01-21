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
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                           target => status_word);
   --
   --  If an exception occurs, set the appropriate flag in the queue and
   --  set the flag to show that an exception has occurred.
   --
   procedure process_exception(self : in out pdp11; ex_num : byte) is
   begin
      self.except_pend(ex_num) := True;
      self.check_except := True;
   end;
   --
   --  Creates an exception stack frame for PDP-11 processors.
   --
   --  This should only be called when the check_except flag is true.
   --
   procedure perform_exception(self : in out pdp11) is
      old_psw : constant status_word := self.psw;
      old_pc  : word := self.pc;
      new_psw : status_word;
      new_pc  : word;
      temp    : bus_stat;
      temp_sp : word;
   begin
      for i in self.except_pend'Range loop
         if self.except_pend(i) then
--            Ada.Text_IO.Put_Line("Processing exception " & Integer'Image(Integer(i)));
            if self.waiting then        --  Check if waiting for interrupt
               self.waiting := False;   --  Clear wait flag
               old_pc := old_pc + 2;    --  Update PC to point to next instruction
--               Ada.Text_IO.Put_Line("Exception while waiting.  Updated PC to " & toOct(old_pc));
            end if;
            new_pc  := self.bus.readl16l(addr_bus(i), PROC_KERN, ADDR_DATA, temp);
            new_psw := word_to_psw(self.bus.readl16l(addr_bus(i + 2), PROC_KERN, ADDR_DATA, temp));
            self.psw := new_psw;
            self.psw.prev_mode := old_psw.curr_mode;
            temp_sp := self.get_regw(6) - 2;
            self.memory(addr_bus(temp_sp), psw_to_word(old_psw));
            temp_sp := temp_sp - 2;
            self.memory(addr_bus(temp_sp), old_pc);
            self.set_regw(6, temp_sp);
            self.pc := new_pc;
            self.except_pend(i) := False;
            return;
         end if;
      end loop;
      self.check_except := False;
   end;
end;
