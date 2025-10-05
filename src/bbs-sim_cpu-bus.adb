package body BBS.Sim_CPU.bus is
   --
   --  Simulator switches and lights
   --
   function get_lr_data(self : in out bus) return data_bus is
   begin
      return self.lr_data;
   end;
   --
   function get_lr_addr(self : in out bus) return addr_bus is
   begin
      return self.lr_addr;
   end;
   --
   function get_lr_ctrl(self : in out bus) return ctrl_mode is
   begin
      return self.lr_ctl;
   end;
   --
   procedure set_sr_ad(self : in out bus; value : ad_bus) is
   begin
      self.sr_ad := value;
   end;
   --
   procedure set_sr_ctrl(self : in out bus; value : ctrl_mode) is
   begin
      self.sr_ctl := value;
   end;
end;
