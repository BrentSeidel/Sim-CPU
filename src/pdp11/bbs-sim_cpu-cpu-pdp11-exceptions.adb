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
with Ada.Containers;
use type Ada.Containers.Count_Type;
--
--  Package for exception related processing.
--
package body BBS.Sim_CPU.CPU.pdp11.exceptions is
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                           target => status_word);
   --
   --  For external interrupts, add an exception record to a queue.  This is
   --  because Vectors are not thread save.  Items are then pulled from the queue
   --  and added to the vector during the perform_exception call that happens at
   --  the end of instruction processing.
   --
   procedure process_exception(self : in out pdp11; ex_num : word; priority : byte; instr_count : byte) is
   begin
      self.check_except := True;
      self.except_pend.Enqueue((vector => ex_num,
                                   priority => priority,
                                   timeout => instr_count));
      if self.trace.except then
         Ada.Text_IO.Put_Line("CPU: Adding vector " & toOct(ex_num) & ", priority " & toOct(priority) &
                                ", timeout " & toOct(instr_count) & " to interrupt queue.");
      end if;
      self.check_except := True;
   end;
   --
   --  This is only to be called synchronously.
   --
   procedure process_exception(self : in out pdp11; except : ex_info) is
   begin
      self.check_except := True;
      self.except_pend.Enqueue(except);
      if self.trace.except then
         Ada.Text_IO.Put_Line("CPU: Adding vector " & toOct(except.vector) & ", priority " & toOct(except.priority) &
                                ", timeout " & toOct(except.timeout) & " to interrupt vector.");
      end if;
   end;
   --
   --  Creates an exception stack frame for PDP-11 processors.
   --
   --  This should only be called when the check_except flag is true.
   --
   procedure perform_exception(self : in out pdp11) is
      first    : int_queue.Extended_Index;
      max_prio : byte := 0;
      max_idx  : int_queue.Extended_Index;
      temp     : ex_info;
      found    : Boolean := False;
   begin
      --
      --  First check the pending interrupt queue and add any elements from that
      --  to the self.intr vector.
      --
      while self.except_pend.Current_Use > 0 loop
         self.except_pend.Dequeue(temp);
         self.intr.append(temp);
      end loop;
      --
      --  Loop through self.intr vector to search for pending interrupts.
      --  Check for priority and timeout = 0.  If timeout /= 0 then decrement timeout.
      --  Remove interrupt from vector when it is processed.
      --
      if self.intr.length = 0 then
         if self.trace.except then
            Ada.Text_IO.Put_Line("CPU: Exception queue empty when checking for exceptions.  Should not happen");
         end if;
         self.check_except := False;
         return;
      end if;
      first    := self.intr.first_index;
      max_idx  := first;
      if self.intr(first).timeout = 0 then
         max_prio := self.intr(first).priority;
         found := True;
      end if;
      if max_prio < byte(self.psw.priority) then
         max_prio := byte(self.psw.priority);
         found := False;
      end if;
      for i in first .. self.intr.last_index loop
         if self.intr(i).priority > max_prio and self.intr(i).timeout = 0 then
            max_prio := self.intr(i).priority;
            max_idx  := i;
            found := True;
         end if;
         if self.intr(i).timeout > 0 then
            self.intr(i).timeout := self.intr(i).timeout - 1;
         end if;
      end loop;
      if found then
         if self.trace.except then
            Ada.Text_IO.Put_Line("CPU: Taking exception " & toOct(self.intr(max_idx).vector) & " from " & toOct(self.pc));
         end if;
         take_vector(self, addr_bus(self.intr(max_idx).vector));
         self.intr.delete(max_idx);
      end if;
      if self.intr.length = 0 then
         self.check_except := False;
      end if;
   end;
   --
   --  Common code for taking an exception vector
   --
   procedure take_vector(self : in out pdp11; vect : addr_bus) is
      old_psw : constant status_word := self.psw;
      old_pc  : word := self.pc;
      new_psw : status_word;
      new_pc  : word;
      temp_sp : word;
      temp    : bus_stat;
   begin
      new_pc  := self.bus.readl16l(vect, PROC_KERN, ADDR_DATA, temp);
      new_psw := word_to_psw(self.bus.readl16l(vect + 2, PROC_KERN, ADDR_DATA, temp));
      self.psw := new_psw;
      self.psw.prev_mode := old_psw.curr_mode;
      temp_sp := self.get_regw(6) - 2;
      self.memory(addr_bus(temp_sp), psw_to_word(old_psw));
      temp_sp := temp_sp - 2;
      self.memory(addr_bus(temp_sp), old_pc);
      self.set_regw(6, temp_sp);
      self.pc := new_pc;
   end;
   --
   procedure flush_exceptions(self : in out pdp11) is
      temp     : ex_info;
   begin
      --
      --  First clear out the vector
      --
      while self.intr.length > 0 loop
         self.intr.delete(self.intr.first_index);
      end loop;
      --
      --  Then clear out the queue
      --
      while self.except_pend.Current_Use > 0 loop
         self.except_pend.Dequeue(temp);
      end loop;
   end;
   --
end;
