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
   --  Utility functions
   --
   hex_digit : String := "0123456789ABCDEF";
   --
   function toHex(value : byte) return String is
   begin
      return hex_digit(Integer(value/16#10#) + 1) & hex_digit(Integer(value and 16#0F#) + 1);
   end;
   --
   function toHex(value : word) return String is
   begin
      return hex_digit(Integer(value/16#1000#) + 1) &
        hex_digit(Integer((value/16#100#) and 16#0F#) + 1) &
        hex_digit(Integer((value/16#10#) and 16#0F#) + 1) &
        hex_digit(Integer(value and 16#0F#) + 1);
   end;
   --  ----------------------------------------------------------------------
   --  I/O device actions
   --
   function getSize(self : in out io_device) return addr_bus is (self.size);
   function getBase(self : in out io_device) return addr_bus is (self.base);
   --
end BBS.Sim_CPU;
