with Ada.Text_IO;
with Ada.Unchecked_Conversion;
--
--  Package for exception related processing.
--
package body BBS.Sim_CPU.m68000.exceptions is
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   --
   --  If an exception occurs, set the appropriate flag in the queue and
   --  set the flag to show that an exception has occurred.
   --
   procedure process_exception(self : in out m68000; ex_num : byte) is
   begin
      self.except_pend(ex_num) := True;
      self.except_occur := True;
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
   begin
      Ada.Text_IO.Put_Line("Exception processing");
      self.psw.trace0 := False;
      self.psw.trace1 := False;
      self.psw.super := True;
      if self.except_pend(ex_0_reset_ssp) then
         --
         --  Don't bother pushing anything onto the stack for reset and
         --  clear all other pending exceptions.
         --
         self.lr_ctl.atype := ADDR_INST;
         self.ssp := self.memory(addr_bus(ex_0_reset_ssp) * 4);
         self.pc  := self.memory(addr_bus(ex_1_reset_pc) * 4);
         self.except_pend := (others => False);
      else
         --
         --  Reset is exception 0 (handled above).  Exception 1 is used
         --  by reset, so start checking the rest at exception 2.
         for i in 2 .. self.except_pend'Last loop
            if self.except_pend(i) then
               if i = ex_4_ill_inst then
                  --
                  --  For illegal instruction (and possibly others to be added
                  --  later), the PC points to the instruction causing the exception
                  --
                  self.push(True, self.inst_pc);
                  self.push(True, psw_to_word(temp_psw));
                  self.pc := self.memory(addr_bus(i) * 4);
               else
                  --
                  --  For other exceptions (privilege violation is one), the PC
                  --  points to the instruction after the one causing the exception.
                  --
                  self.lr_ctl.atype := ADDR_DATA;
                  self.push(True, self.pc);
                  self.push(True, psw_to_word(temp_psw));
                  self.pc := self.memory(addr_bus(i) * 4);
               end if;
               --
               --  Having built the stack frame, clear the exception and return.
               --
               self.except_pend(i) := False;
               return;
            end if;
         end loop;
      end if;
      --
      --  If we get to this point, there should be no more exceptions
      --  left in the queue, so clear this flag.
      --
      self.except_occur := False;
   end;
end;

