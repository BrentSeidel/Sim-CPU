with BBS.Sim_CPU.bus;
package body BBS.Sim_CPU.CPU is
   --
   --  Set/Get trace level
   --
   procedure trace(self : in out simulator; l : Natural) is
   begin
      self.trace := l;
   end;
   --
   function trace(self : in out simulator) return Natural is
   begin
      return self.trace;
   end;
   --
   --  Attach CPU to a bus.  Index is provided for use in mult-cpu systems to
   --  identify the CPU on the bus.
   --
   procedure attach_bus(self : in out simulator; bus : BBS.Sim_CPU.bus.bus_access;
                        index : Natural) is
   begin
      self.bus := bus;
      bus.attach_cpu(self'Access, index);
   end;
end;
