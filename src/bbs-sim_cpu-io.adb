package body BBS.Sim_CPU.io is
   --
   --  ----------------------------------------------------------------------
   --  I/O device actions
   --
   function getBase(self : in out io_device) return addr_bus is (self.base);
   procedure setBase(self : in out io_device; base : addr_bus) is
   begin
      self.base := base;
   end;
   --
   --  Set the owning CPU
   --
   procedure setOwner(self : in out io_device; owner : BBS.Sim_CPU.CPU.sim_access) is
   begin
      self.host := owner;
   end;
end;
