with BBS.embed;
use type BBS.embed.uint8;
use type BBS.embed.uint32;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
package body BBS.Sim_CPU.i8080 is
   --
   function uint16_to_ctrl is new Ada.Unchecked_Conversion(source => BBS.embed.uint16,
                                                           target => ctrl_mode);
   function psw_to_byte is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => byte);
   --
   package data_io is new Ada.Text_IO.Modular_IO(data_bus);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out i8080) is
   begin
      self.a  := 0;
      self.b  := 0;
      self.c  := 0;
      self.d  := 0;
      self.e  := 0;
      self.h  := 0;
      self.l  := 0;
      self.sp := 0;
      self.pc := 0;
      self.psw.carry := False;
      self.psw.zero := True;
      self.psw.parity := False;
      self.psw.aux_carry := False;
      self.psw.unused0 := True;
      self.psw.unused1 := False;
      self.psw.unused2 := False;
      self.lr_ctl.mode := PROC_KERN;
   end;
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out i8080) is
   begin
      self.decode;
   end;
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out i8080) is
   begin
      if self.sr_ctl.addr then
         self.addr := word(self.sr_ad and 16#FFFF#);
      else
         self.lr_data := data_bus(self.mem(self.addr) and 16#FF#);
         self.addr := self.addr + 1;
      end if;
      self.lr_addr := addr_bus(self.addr);
   end;
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out i8080) is
   begin
      self.lr_addr := addr_bus(self.addr);
      self.lr_data := data_bus(self.mem(self.addr));
      if not self.sr_ctl.addr then
         self.addr := self.addr + 1;
      end if;
   end;
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out i8080) return BBS.embed.uint32 is
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
   procedure set_mem(self : in out i8080; mem_addr : addr_bus;
                     data : data_bus) is
   begin
      self.mem(word(mem_addr and 16#FFFF#)) := byte(data and 16#FF#);
   end;
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out i8080; mem_addr : addr_bus) return
     data_bus is
   begin
      return data_bus(self.mem(word(mem_addr and 16#FFFF#)));
   end;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out i8080; num : BBS.embed.uint32)
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
   function read_reg(self : in out i8080; num : BBS.embed.uint32)
                     return data_bus is
      reg : reg_id;
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         reg := reg_id'Val(num);
         case reg is
            when reg_a =>
               return data_bus(self.a);
            when reg_psw =>
               return data_bus(psw_to_byte(self.psw));
            when reg_b =>
               return data_bus(self.b);
            when reg_c =>
               return data_bus(self.c);
            when reg_bc =>
               return data_bus(word(self.b)*16#100# + word(self.c));
            when reg_d =>
               return data_bus(self.d);
            when reg_e =>
               return data_bus(self.e);
            when reg_de =>
               return data_bus(word(self.d)*16#100# + word(self.e));
            when reg_h =>
               return data_bus(self.h);
            when reg_l =>
               return data_bus(self.l);
            when reg_hl =>
               return data_bus(word(self.h)*16#100# + word(self.l));
            when reg_sp =>
               return data_bus(self.sp);
            when reg_pc =>
               return data_bus(self.pc);
            when others =>
               return 0;
         end case;
      else
         return 0;
      end if;
   end;
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   overriding
   function read_reg(self : in out i8080; num : BBS.embed.uint32)
                     return String is
      value : String(1 .. 13);
      reg : reg_id;
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         reg := reg_id'Val(num);
         case reg is
            when reg_a =>
               data_io.Put(value, data_bus(self.a), 16);
            when reg_psw =>
               data_io.Put(value, data_bus(psw_to_byte(self.psw)), 16);
            when reg_b =>
               data_io.Put(value, data_bus(self.b), 16);
            when reg_c =>
               data_io.Put(value, data_bus(self.c), 16);
            when reg_bc =>
               data_io.Put(value, data_bus(word(self.b)*16#100# + word(self.c)), 16);
            when reg_d =>
               data_io.Put(value, data_bus(self.d), 16);
            when reg_e =>
               data_io.Put(value, data_bus(self.e), 16);
            when reg_de =>
               data_io.Put(value, data_bus(word(self.d)*16#100# + word(self.e)), 16);
            when reg_h =>
               data_io.Put(value, data_bus(self.h));
            when reg_l =>
               data_io.Put(value, data_bus(self.l));
            when reg_hl =>
               data_io.Put(value, data_bus(word(self.h)*16#100# + word(self.l)), 16);
            when reg_sp =>
               data_io.Put(value, data_bus(self.sp));
            when reg_pc =>
               data_io.Put(value, data_bus(self.pc));
            when others =>
               return "*invalid*";
         end case;
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
   --  Code for the instruction processing.
   --
   procedure decode(self : in out i8080) is
      inst : byte;
   begin
      self.intr := False;  --  Currently interrupts are not implemented
      self.incpc := True;  --  Increment the PC
      inst := self.get_next(ADDR_INST);
      --
      --  Do instruction processing
      --
      case inst is
         when 16#76# =>  -- HLT (halt)
            self.incpc := False;
         when others =>
            null;
      end case;
      if self.incpc then
         self.pc := self.pc + 1;
      end if;
   end;
   --
   function get_next(self : in out i8080; mode : addr_type) return byte is
   begin
      self.lr_ctl.atype := mode;
      if self.intr then
         self.lr_ctl.atype := ADDR_INTR;
         return 0;  -- Currently interrupts are not implemented
      else
         self.lr_addr := addr_bus(self.pc);
         self.lr_data := data_bus(self.mem(self.pc));
         return self.mem(self.pc);
      end if;
   end;
   --
   --  Code for the various patterns.
   --
   procedure copy_sw(self : in out i8080) is
   begin
      self.lr_data := self.sr_ad;
      self.lr_addr := self.sr_ad;
      self.lr_ctl := self.sr_ctl;
   end;

end BBS.Sim_CPU.i8080;
