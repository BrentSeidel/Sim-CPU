with Ada.Text_IO;
with Ada.Unchecked_Conversion;
package body BBS.Sim_CPU.m68000.exceptions is
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   --
   --  Package for exception related processing.
   --
   procedure process_exception(self : in out m68000; ex_num : byte) is
      addr : long := self.memory(addr_bus(ex_num) * 4);
   begin
      Ada.Text_IO.Put_Line("Processing exception " & toHex(ex_num));
      Ada.Text_IO.Put_Line("  Vector address is " & toHex(addr));
      self.except_occur := True;
      self.except_pend  := ex_num;
   end;
   --
   --  Creates an exception stack frame for 68000/68008 processors.  The
   --  stack frame for later processors is more complicated and includes
   --  more information.
   --
   procedure perform_exception(self : in out m68000) is
   begin
      Ada.Text_IO.Put_Line("Exception transfer");
      if self.except_pend = ex_0_reset_ssp then
         --
         --  Don't bother pushing anything onto the stack for reset.
         --
         self.ssp := self.memory(addr_bus(ex_0_reset_ssp) * 4);
         self.pc  := self.memory(addr_bus(ex_1_reset_pc) * 4);
      elsif self.except_pend = ex_1_reset_pc then
         Ada.Text_IO.Put_Line("Internal error: Missing exception number.");
      elsif self.except_pend = ex_4_ill_inst then
         --
         --  For illegal instruction (and possibly others to be added
         --  later), the PC points to the instruction causing the exception
         --
         self.push(True, self.inst_pc);
         self.push(True, psw_to_word(self.psw));
      else
         --
         --  For other exceptions (privilege violation is one), the PC
         --  points to the instruction after the one causing the exception.
         --
         self.push(True, self.pc);
         self.push(True, psw_to_word(self.psw));
      end if;
      self.psw.trace0 := False;
      self.psw.trace1 := False;
      self.psw.super := True;
      self.pc := self.memory(addr_bus(self.except_pend) * 4);
      self.except_occur := False;
      self.except_pend := 1;  --  This is the only number that is not a valid exception
   end;
end;

