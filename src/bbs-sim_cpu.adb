package body BBS.Sim_CPU is
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
      return self.lr_data;
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
end BBS.Sim_CPU;
