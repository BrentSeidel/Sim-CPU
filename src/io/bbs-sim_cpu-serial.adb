with BBS.embed;
use type BBS.embed.uint32;
with Ada.Text_IO;
package body BBS.Sim_CPU.serial is
   --  ----------------------------------------------------------------------
   --  8 bit console device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out con8; addr : addr_bus; data : data_bus) is
   begin
      if addr = self.base then
         Ada.Text_IO.Put(Character'Val(Integer(data and 16#FF#)));
      end if;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out con8; addr : addr_bus) return data_bus is
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
   function getBase(self : in out con8) return addr_bus is
   begin
      return self.base;
   end;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out con8; base : addr_bus) is
   begin
      self.base := base;
   end;
   --  ----------------------------------------------------------------------
   --
   --  Printer device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out print8; addr : addr_bus; data : data_bus) is
   begin
      if self.ready then
         Ada.Text_IO.Put(Character'Val(Integer(data and 16#FF#)));
      end if;
   end;
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out print8) return addr_bus is
   begin
      return self.base;
   end;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out print8; base : addr_bus) is
   begin
      self.base := base;
   end;
   --
   --  Open the attached file
   --
   procedure open(self : in out print8; name : String) is
   begin
      if self.ready then
         Ada.Text_IO.Close(self.file);
      end if;
      Ada.Text_IO.Open(self.file, Ada.Text_IO.Out_File, name);
      self.ready := True;
   end;
   --
   --  Close the attached file
   --
   procedure close(self : in out print8) is
   begin
      if self.ready then
         Ada.Text_IO.Close(self.file);
         self.ready := False;
      end if;
   end;
   --
end;
