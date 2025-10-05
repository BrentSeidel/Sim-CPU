with BBS.Sim_CPU.bus;
package body BBS.Sim_CPU.CPU is
   --
   --  Simulator switches and lights
   --
   function get_lr_data(self : in out simulator) return data_bus is
   begin
      return self.lr_data;
   end;
   --
   function get_lr_addr(self : in out simulator) return addr_bus is
   begin
      return self.lr_addr;
   end;
   --
   function get_lr_ctrl(self : in out simulator) return ctrl_mode is
   begin
      return self.lr_ctl;
   end;
   --
   procedure set_sr_ad(self : in out simulator; value : ad_bus) is
   begin
      self.sr_ad := value;
   end;
   --
   procedure set_sr_ctrl(self : in out simulator; value : ctrl_mode) is
   begin
      self.sr_ctl := value;
   end;
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
