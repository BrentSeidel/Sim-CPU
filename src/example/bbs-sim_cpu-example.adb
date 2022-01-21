with BBS.embed;
use type BBS.embed.uint16;
use type BBS.embed.uint32;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
package body BBS.Sim_CPU.example is
   --
   function uint16_to_ctrl is new Ada.Unchecked_Conversion(source => BBS.embed.uint16,
                                                           target => ctrl_mode);
   --
   package data_io is new Ada.Text_IO.Modular_IO(data_bus);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out simple) is
   begin
      self.reg(pattern) := 0;
      self.reg(ad_counter) := 0;
      self.reg(ctl_counter) := 0;
      self.reg(ad_bouncer) := 0;
      self.ad_bounce_dir := left;
      self.reg(ctl_bouncer) := 0;
      self.ctl_bounce_dir := left;
      self.reg(ad_scanner) := 0;
      self.reg(ctl_scanner) := 0;
      self.reg(ad_fib1) := 1;
      self.reg(ad_fib2) := 1;
      self.reg(ctl_fib1) := 1;
      self.reg(ctl_fib2) := 2;
   end;
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out simple) is
      d : Duration := 0.05;
   begin
      self.lr_addr := self.reg(pattern);
      case self.reg(pattern) is
         when 1 =>
            self.count;
         when 2 =>
            self.scan16;
         when 3 =>
            self.bounce16;
         when 4 =>
            self.fibonacci;
         when 9 =>
            self.count;
         when 10 =>
            self.scan32;
         when 11 =>
            self.bounce32;
         when 12 =>
            self.fibonacci;
         when others =>
            self.copy_sw;
            d := 0.01;
      end case;
      delay d;
   end;
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out simple) is
   begin
      if self.sr_ctl.addr then
         self.reg(addr) := self.sr_ad;
      else
         self.reg(pattern) := self.sr_ad;
         self.lr_data := self.reg(pattern);
         self.reg(addr) := self.reg(addr) + 1;
      end if;
      self.lr_addr := self.reg(addr);
   end;
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out simple) is
   begin
      self.lr_addr := self.reg(addr);
      self.lr_data := self.reg(pattern);
      if not self.sr_ctl.addr then
         self.reg(addr) := self.reg(addr) + 1;
      end if;
   end;
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out simple) return BBS.embed.uint32 is
      pragma Unreferenced(self);
   begin
      return reg_id'Pos(reg_id'Last) + 1;
   end;
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out simple; mem_addr : addr_bus;
                     data : data_bus) is
      pragma Unreferenced(mem_addr);
   begin
      self.reg(pattern) := data;
   end;
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out simple; mem_addr : addr_bus) return
     data_bus is
      pragma Unreferenced(mem_addr);
   begin
      return self.reg(pattern);
   end;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out simple; num : BBS.embed.uint32)
                     return String is
      pragma Unreferenced(self);
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         return reg_id'Image(reg_id'Val(num));
      else
         return "*invalid*";
      end if;
   end;
   --
   --  Called to get register value
   --
   overriding
   function read_reg(self : in out simple; num : BBS.embed.uint32)
                     return data_bus is
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         return self.reg(reg_id'Val(num));
      else
         return 0;
      end if;
   end;
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   function read_reg(self : in out simple; num : BBS.embed.uint32)
                     return String is
      value : String(1 .. 13);
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         data_io.Put(value, self.reg(reg_id'Val(num)), 16);
         return value;
      else
         return "*invalid*";
      end if;
   end;
   --
   --  Called to set register value
   --
--   overriding
--   procedure set_reg(self : in out simple; num : BBS.embed.uint32;
--                     data : BBS.embed.uint32) is null;
   --  --------------------------------------------------------------------
   --
   --  Code for the various patterns.
   --
   procedure count(self : in out simple) is
   begin
      self.reg(ad_counter) := self.reg(ad_counter) + 1;
      self.reg(ctl_counter) := self.reg(ctl_counter) + 2;
      self.lr_data := self.reg(ad_counter);
      self.lr_ctl := uint16_to_ctrl(BBS.embed.uint16(self.reg(ctl_counter) and 16#FFFF#));
   end;
   --
   procedure bounce16(self : in out simple) is
   begin
      if self.ad_bounce_dir = left then
         if (self.reg(ad_bouncer) and 16#FFFF#) = 0 then
            self.ad_bounce_dir := right;
            self.reg(ad_bouncer) := 16#8000#;
         else
            self.reg(ad_bouncer) := self.reg(ad_bouncer) * 2;
         end if;
      else
         if (self.reg(ad_bouncer) and 16#FFFF#) = 0 then
            self.ad_bounce_dir := left;
            self.reg(ad_bouncer) := 16#0001#;
         else
            self.reg(ad_bouncer) := self.reg(ad_bouncer) / 2;
         end if;
      end if;
      if self.ctl_bounce_dir = left then
         if self.reg(ctl_bouncer) = 0 then
            self.ctl_bounce_dir := right;
            self.reg(ctl_bouncer) := 16#8000#;
         else
            self.reg(ctl_bouncer) := self.reg(ctl_bouncer) * 2;
         end if;
      else
         if self.reg(ctl_bouncer) = 1 then
            self.ctl_bounce_dir := left;
            self.reg(ctl_bouncer) := 16#0002#;
         else
            self.reg(ctl_bouncer) := self.reg(ctl_bouncer) / 2;
         end if;
      end if;
      self.lr_data := self.reg(ad_bouncer);
      self.lr_ctl := uint16_to_ctrl(BBS.embed.uint16(self.reg(ctl_bouncer) and 16#FFFF#));
   end;
   --
   procedure bounce32(self : in out simple) is
   begin
      if self.ad_bounce_dir = left then
         if self.reg(ad_bouncer) = 0 then
            self.ad_bounce_dir := right;
            self.reg(ad_bouncer) := 16#8000_0000#;
         else
            self.reg(ad_bouncer) := self.reg(ad_bouncer) * 2;
         end if;
      else
         if self.reg(ad_bouncer) = 0 then
            self.ad_bounce_dir := left;
            self.reg(ad_bouncer) := 16#0000_0001#;
         else
            self.reg(ad_bouncer) := self.reg(ad_bouncer) / 2;
         end if;
      end if;
      if self.ctl_bounce_dir = left then
         if self.reg(ctl_bouncer) = 0 then
            self.ctl_bounce_dir := right;
            self.reg(ctl_bouncer) := 16#8000#;
         else
            self.reg(ctl_bouncer) := self.reg(ctl_bouncer) * 2;
         end if;
      else
         if self.reg(ctl_bouncer) = 1 then
            self.ctl_bounce_dir := left;
            self.reg(ctl_bouncer) := 16#0002#;
         else
            self.reg(ctl_bouncer) := self.reg(ctl_bouncer) / 2;
         end if;
      end if;
      self.lr_data := self.reg(ad_bouncer);
      self.lr_ctl := uint16_to_ctrl(BBS.embed.uint16(self.reg(ctl_bouncer) and 16#FFFF#));
   end;
   --
   procedure scan16(self : in out simple) is
   begin
      if (self.reg(ad_scanner) and 16#FFFF#) = 0 then
         self.reg(ad_scanner) := 1;
      else
         self.reg(ad_scanner) := self.reg(ad_scanner) * 2;
      end if;
      if self.reg(ctl_scanner) = 0 then
         self.reg(ctl_scanner) := 2;
      else
         self.reg(ctl_scanner) := self.reg(ctl_scanner) * 2;
      end if;
      self.lr_data := self.reg(ad_scanner);
      self.lr_ctl := uint16_to_ctrl(BBS.embed.uint16(self.reg(ctl_scanner) and 16#FFFF#));
   end;
   --
   procedure scan32(self : in out simple) is
   begin
      if self.reg(ad_scanner) = 0 then
         self.reg(ad_scanner) := 1;
      else
         self.reg(ad_scanner) := self.reg(ad_scanner) * 2;
      end if;
      if self.reg(ctl_scanner) = 0 then
         self.reg(ctl_scanner) := 2;
      else
         self.reg(ctl_scanner) := self.reg(ctl_scanner) * 2;
      end if;
      self.lr_data := self.reg(ad_scanner);
      self.lr_ctl := uint16_to_ctrl(BBS.embed.uint16(self.reg(ctl_scanner) and 16#FFFF#));
   end;
   --
   procedure fibonacci(self : in out simple) is
      ad_temp : constant BBS.embed.uint32 := self.reg(ad_fib1) + self.reg(ad_fib2);
      ctl_temp : constant BBS.embed.uint16 := BBS.embed.uint16((self.reg(ctl_fib1) + self.reg(ctl_fib2)) and 16#FFFF#);
   begin
      self.lr_data := ad_temp;
      self.reg(ad_fib2) := self.reg(ad_fib1);
      self.reg(ad_fib1) := ad_temp;
      self.lr_ctl := uint16_to_ctrl(ctl_temp);
      self.reg(ctl_fib2) := self.reg(ctl_fib1);
      self.reg(ctl_fib1) := BBS.embed.uint32(ctl_temp);
      if (self.reg(ad_fib1) = 0) and (self.reg(ad_fib2) = 0) then
         self.reg(ad_fib1) := 1;
         self.reg(ad_fib2) := 1;
      end if;
      if (self.reg(ctl_fib1) = 0) and (self.reg(ctl_fib2) = 0) then
         self.reg(ctl_fib1) := 1;
         self.reg(ctl_fib2) := 2;
      end if;
   end;
   --
   --
   procedure copy_sw(self : in out simple) is
   begin
      self.lr_data := self.sr_ad;
      self.lr_addr := self.sr_ad;
      self.lr_ctl := self.sr_ctl;
   end;

end BBS.Sim_CPU.example;
