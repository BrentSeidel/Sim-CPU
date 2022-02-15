with BBS.embed;
use type BBS.embed.uint32;
with Ada.Text_IO;
package body BBS.Sim_CPU.con8 is
   --  ----------------------------------------------------------------------
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out con; addr : addr_bus; data : data_bus) is
   begin
      if addr = self.base then
         Ada.Text_IO.Put(Character'Val(Integer(data and 16#FF#)));
      end if;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out con; addr : addr_bus) return data_bus is
   begin
      if addr = self.base then
         if self.ready then
            self.ready := False;
         end if;
         return data_bus(Character'Pos(self.char));
      elsif addr = (self.base + 1) then
         if self.ready then
            return 1;
         end if;
         Ada.Text_IO.Get_Immediate(self.char, self.ready);
         if self.ready then
            return 1;
         else
            return 0;
         end if;
      end if;
      return 0;
   end;
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out con) return addr_bus is
   begin
      return self.base;
   end;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out con; base : addr_bus) is
   begin
      self.base := base;
   end;
end;
