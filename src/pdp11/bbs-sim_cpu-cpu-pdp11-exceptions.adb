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
   --  Purge an exception entry from the interrupt queue
   --
   procedure purge_exception(self : in out pdp11; ex_num : word; priority : byte) is
      function find(v : word; p : byte) return int_queue.Extended_Index is
      begin
         for i in self.intr.first_index .. self.intr.last_index loop
            if (self.intr(i).priority = priority) and (self.intr(i).vector = ex_num) then
               return i;
            end if;
         end loop;
         return int_queue.No_Index;
      end;

      c : int_queue.Extended_Index;
   begin
      loop
         c := find(ex_num, priority);
         exit when c = int_queue.No_Index;
         self.intr.delete(c);
      end loop;
--      for i in self.intr.first_index .. self.intr.last_index loop
--         if (self.intr(i).priority = priority) and (self.intr(i).vector = ex_num) then
--            self.intr.delete(i);
--         end if;
--      end loop;
   end;
   --
   --  Creates an exception stack frame for PDP-11 processors.
   --
   --  This should only be called when the check_except flag is true.
   --
   procedure perform_exception(self : in out pdp11) is
      temp_be : constant Boolean := self.bus_error;
      first    : int_queue.Extended_Index;
      max_prio : byte := 0;
      max_idx  : int_queue.Extended_Index;
      temp     : ex_info;
      prev     : ex_info;
      found    : Boolean := False;
   begin
      self.bus_error := False;  --  So that bus errors during processing can be detected
      --
      --  First check the pending interrupt queue and add any elements from that
      --  to the self.intr vector.
      --
      if self.except_pend.Current_Use > 0 then
         self.except_pend.Dequeue(prev);
         self.intr.append(prev);
      end if;
      while self.except_pend.Current_Use > 0 loop
         self.except_pend.Dequeue(temp);
         --
         --  If temp is the same as the previous exception, we just ignore it.
         --  This will eliminate some duplicate interrupts (such as when the
         --  simulation is paused while clock interrupts keep occuring).
         --
         if temp /= prev then
            self.intr.append(temp);
            prev := temp;
         end if;
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
      if max_prio <= byte(self.psw.priority) then
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
         if self.bus_error then
            Ada.Text_IO.Put_Line("CPU: Bus error during exception processing.");
         end if;
         --
         --  Check for stack overflow during exception processing.
         --
         if (self.ksp <= self.config.stack_limit) and (self.ksp >= 0) and (self.intr(max_idx).vector /= 8#004#) then
            Ada.Text_IO.Put_Line("CPU: Stack overflow when processing exception.");
            if not self.bus_error then
               take_vector(self, addr_bus(ex_004_assorted.vector));
            end if;
         end if;
         self.intr.delete(max_idx);
      end if;
      if self.intr.length = 0 then
         self.check_except := False;
      end if;
      if self.bus_error then
         --  Bus error occured during exception processing.
         --  check the queue and take the first vector 004 (bus) or 250 (MMU)
         --  exception.
         self.bus_error := False;
         while self.except_pend.Current_Use > 0 loop
            self.except_pend.Dequeue(temp);
            if (temp.vector = 8#004#) or (temp.vector = 8#250#) then
               take_vector(self, addr_bus(temp.vector));
               exit;
            else
               self.intr.append(temp);
            end if;
         end loop;
      end if;
      if temp_be then
         self.bus_error := True;
      end if;
   end;
   --
   --  Common code for taking an exception vector.  Need to check for MMU aborts
   --  and process them
   --
   procedure take_vector(self : in out pdp11; vect : addr_bus) is
      temp_be : constant Boolean := self.bus_error;
      old_psw : constant status_word := self.psw;
      new_pc  : word;
      temp_sp : word;
      temp    : bus_stat;
   begin
      new_pc   := self.bus.readl16l(vect, PROC_KERN, ADDR_DATA, temp);
      self.psw := word_to_psw(self.bus.readl16l(vect + 2, PROC_KERN, ADDR_DATA, temp));
      self.psw.prev_mode := old_psw.curr_mode;
      temp_sp := self.get_regw(6) - 2;
      if self.trace.except then
         Ada.Text_IO.Put_Line("CPU: pushing PSW " & toOct(psw_to_word(old_psw)) & " to SP " & toOct(temp_sp));
      end if;
      self.memory(addr_bus(temp_sp), psw_to_word(old_psw));  --  Push original PSW
      if not self.bus_error then
         temp_sp := temp_sp - 2;
         if self.trace.except then
            Ada.Text_IO.Put_Line("     and PC " & toOct(self.pc) & " to SP " & toOct(temp_sp));
         end if;
         if self.waiting then
            self.waiting := False;
            self.pc := self.pc + 2;
         end if;
         self.memory(addr_bus(temp_sp), self.pc);                --  Push original PC
         if not self.bus_error then
            self.pc := new_pc;
            if self.trace.except then
               Ada.Text_IO.Put_Line("CPU: new PC " & toOct(new_pc) & ", new PSW " & toOct(psw_to_word(self.psw)));
            end if;
         end if;
      end if;
      self.set_regw(6, temp_sp);
      if temp_be then
         self.bus_error := True;
      end if;
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
