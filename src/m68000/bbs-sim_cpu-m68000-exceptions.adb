with Ada.Text_IO;
package body BBS.Sim_CPU.m68000.exceptions is
   --
   --  Package for exception related processing.
   --
   --  This is a stub for now.  Once the RTE instruction is implemented,
   --  this can be replaced with code that does the exception handling.
   --
   procedure process_exception(self : in out m68000; ex_num : byte) is
      addr : long := self.memory(addr_bus(ex_num) * 4);
   begin
      Ada.Text_IO.Put_Line("Processing exception " & toHex(ex_num));
      Ada.Text_IO.Put_Line("  Vector address is " & toHex(addr));

   end;
end;

