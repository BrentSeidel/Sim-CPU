with BBS.embed;
use type BBS.embed.uint8;
use type BBS.embed.uint32;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
package body BBS.Sim_CPU.m68000 is
   --
   function uint16_to_ctrl is new Ada.Unchecked_Conversion(source => BBS.embed.uint16,
                                                           target => ctrl_mode);
   function psw_to_byte is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => byte);
   function byte_to_psw is new Ada.Unchecked_Conversion(source => byte,
                                                        target => status_word);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out m68000) is
   begin
      self.pc := self.addr;
      self.cpu_halt := False;
      self.lr_ctl.mode := PROC_KERN;
   end;
   --
   --  Called to start simulator execution at a specific address.
   --
   overriding
   procedure start(self : in out m68000; addr : addr_bus) is
   begin
      self.start;
      self.pc := addr;
   end;
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out m68000) is
   begin
      if not self.halted then
         self.decode;
      end if;
   end;
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out m68000) is
   begin
      if self.sr_ctl.addr then
         self.addr := addr_bus(self.sr_ad);
      else
         self.mem(self.addr) := byte(self.sr_ad and 16#FF#);
         self.addr := self.addr + 1;
      end if;
      self.lr_addr := addr_bus(self.addr);
      self.lr_data := data_bus(self.mem(self.addr));
   end;
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out m68000) is
   begin
      self.lr_addr := addr_bus(self.addr);
      self.lr_data := data_bus(self.mem(self.addr));
      self.addr := self.addr + 1;
   end;
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out m68000) return BBS.embed.uint32 is
      pragma Unreferenced(self);
   begin
      return reg_id'Pos(reg_id'Last) + 1;
   end;
   --
   --  Called to get variant name
   --
--   overriding
--   function variant(self : in out m68000; v : natural) return String is
--   begin
--      return variants_m68000'Image(self.variant);
--   end;
   --
   --  Called to get current variant index
   --
   overriding
   function variant(self : in out m68000) return Natural is
   begin
    return variants_m68000'pos(self.cpu_model);
   end;
   --
   --  Called to set variant
   --
   overriding
   procedure variant(self : in out m68000; v : natural) is
   begin
      self.cpu_model := variants_m68000'Val(v);
   end;
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out m68000; mem_addr : addr_bus;
                     data : data_bus) is
   begin
      self.mem(mem_addr) := byte(data and 16#FF#);
   end;
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out m68000; mem_addr : addr_bus) return
     data_bus is
   begin
      return data_bus(self.mem(mem_addr));
   end;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out m68000; num : BBS.embed.uint32)
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
   function read_reg(self : in out m68000; num : BBS.embed.uint32)
                     return data_bus is
      reg : reg_id;
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         reg := reg_id'Val(num);
         case reg is
            when reg_d0 =>
               return data_bus(self.d0);
            when reg_d1 =>
               return data_bus(self.d1);
            when reg_d2 =>
               return data_bus(self.d2);
            when reg_d3 =>
               return data_bus(self.d3);
            when reg_d4 =>
               return data_bus(self.d4);
            when reg_d5 =>
               return data_bus(self.d5);
            when reg_d6 =>
               return data_bus(self.d6);
            when reg_d7 =>
               return data_bus(self.d7);
            when reg_a0 =>
               return data_bus(self.a0);
            when reg_a1 =>
               return data_bus(self.a1);
            when reg_a2 =>
               return data_bus(self.a2);
            when reg_a3 =>
               return data_bus(self.a3);
            when reg_a4 =>
               return data_bus(self.a4);
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
   function read_reg(self : in out m68000; num : BBS.embed.uint32)
                     return String is
      reg : reg_id;
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         reg := reg_id'Val(num);
         case reg is
            when reg_d0 =>
               return toHex(self.d0);
            when reg_d1 =>
               return toHex(self.d1);
            when reg_d2 =>
               return toHex(self.d2);
            when reg_d3 =>
               return toHex(self.d3);
            when reg_d4 =>
               return toHex(self.d4);
            when reg_d5 =>
               return toHex(self.d5);
            when reg_d6 =>
               return toHex(self.d6);
            when reg_d7 =>
               return toHex(self.d7);
            when reg_a0 =>
               return toHex(self.a0);
            when reg_a1 =>
               return toHex(self.a1);
            when reg_a2 =>
               return toHex(self.a2);
            when reg_a3 =>
               return toHex(self.a3);
            when reg_a4 =>
               return toHex(self.a4);
            when others =>
               return "*invalid*";
         end case;
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
   --
   --  This loads data from a file specified by "name" into the simulator memory.
   --  Currently, only Intel Hex format is supported.
   --
   overriding
   procedure load(self : in out m68000; name : String) is
      inp   : Ada.Text_IO.File_Type;
      line  : Ada.Strings.Unbounded.Unbounded_String;
      count : byte;
      addr  : addr_bus;
      rec   : byte;
      data  : page;
      valid : Boolean;
   begin
      Ada.Text_IO.Open(inp, Ada.Text_IO.In_File, name);
      while not Ada.Text_IO.End_Of_File(inp) loop
         Ada.Text_IO.Unbounded_IO.Get_Line(inp, line);
         S_Record(Ada.Strings.Unbounded.To_String(line), count, addr, rec, data,
                  valid);
         exit when rec = 1;  --  End of file record type
         if rec = 0 and valid then  --  Process a data record
            for i in 0 .. count - 1 loop
               self.memory(addr + addr_bus(i), data_bus(data(Integer(i))), ADDR_DATA);
            end loop;
         else
            Ada.Text_IO.Put_Line("Ignoring record: " & Ada.Strings.Unbounded.To_String(line));
         end if;
      end loop;
      Ada.Text_IO.Close(inp);
   exception
      when Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line("Error in file name: " & name);
      when others =>
         Ada.Text_IO.Put_Line("Error occured processing " & name);
         Ada.Text_IO.Close(inp);
   end;
   --
   --  Called to check if the CPU is halted
   --
   overriding
   function halted(self : in out m68000) return Boolean is
   begin
      return self.cpu_halt;
   end;
   --
   --  This clears the halted flag allowing processing to continue.
   --
   overriding
   procedure continue_proc(self : in out m68000) is
   begin
      self.cpu_halt := False;
   end;
   --
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out m68000; addr : addr_bus) is
   begin
      self.break_enable := True;
      self.break_point  := addr;
   end;
   --
   procedure clearBreak(self : in out m68000; addr : addr_bus) is
   begin
      self.break_enable := False;
   end;
   --
   --
   --  Unimplemented instruction response
   --
   --
   --  Right now just print a message for unrecognized opcodes.
   --  At some point, may want to do something different here.
   --
   procedure unimplemented(self : in out m68000; addr : data_bus; data : word) is
   begin
      Ada.Text_IO.Put_Line("Illegal instruction at " & ToHex(addr) &
         " code " & ToHex(data));
   end;
--  --------------------------------------------------------------------
--
--  Code for the instruction processing.
--
   procedure decode(self : in out m68000) is
      inst    : byte;
   begin
      self.intr := False;  --  Currently interrupts are not implemented
      --
      --  Check for breakpoint
      --
      if self.break_enable then
         if self.break_point = self.pc then
            self.cpu_halt := True;
            if (word(self.trace) and 1) = 1 then
               Ada.Text_IO.Put_Line("TRACE: Breakpoint at " & toHex(self.pc));
            end if;
         end if;
      end if;
      inst := self.get_next;
      if (word(self.trace) and 1) = 1 then
         Ada.Text_IO.Put_Line("TRACE: Address: " & toHex(self.pc - 1) & " instruction " &
                           toHex(inst));
      end if;
      --
      --  Do instruction processing
      --
   end;
   --
   --  Utility code for instruction decoder
   --
   function get_next(self : in out m68000) return byte is
      t : byte;
   begin
      self.lr_ctl.atype := ADDR_INST;
      if self.intr then
         self.lr_ctl.atype := ADDR_INTR;
         return 0;  -- Currently interrupts are not implemented
      else
         t := byte(self.memory(self.pc, ADDR_INST));
         self.pc := self.pc + 1;
         return t;
      end if;
   end;
   --
   --  Set flags based on value (zero, sign, parity)
   --
   procedure setf(self : in out m68000; value : byte) is
      p : byte := 0;  --  Bit counter
   begin
      self.psw.zero := (value = 0);
      self.psw.sign := ((value and 16#80#) = 16#80#);
      p := p + (if ((value and 16#01#) = 16#01#) then 1 else 0);
      p := p + (if ((value and 16#02#) = 16#02#) then 1 else 0);
      p := p + (if ((value and 16#04#) = 16#04#) then 1 else 0);
      p := p + (if ((value and 16#08#) = 16#08#) then 1 else 0);
      p := p + (if ((value and 16#10#) = 16#10#) then 1 else 0);
      p := p + (if ((value and 16#20#) = 16#20#) then 1 else 0);
      p := p + (if ((value and 16#40#) = 16#40#) then 1 else 0);
      p := p + (if ((value and 16#80#) = 16#80#) then 1 else 0);
      self.psw.parity := not ((p and 16#01#) = 16#01#);  --  True is even parity
   end;
   --
   --  All memory accesses should be routed through these functions so that they
   --  can do checks for memory-mapped I/O or shared memory.
   --
   procedure memory(self : in out m68000; addr : addr_bus; value : data_bus; mode : addr_type) is
   begin
      --
      --  Set LED register values
      --
      self.lr_addr := addr;
      self.lr_data := data_bus(value);
      self.lr_ctl.atype := mode;
      --
      --  Set memory.  Optionally, checks for memory mapped I/O or shared memory
      --  or other special stuff can bed added here.
      --
      self.mem(addr) := byte(value);
   end;
   --
   function memory(self : in out m68000; addr : addr_bus; mode : addr_type) return data_bus is
   begin
      --
      --  Set LED register values
      --
      self.lr_addr := addr;
      self.lr_data := data_bus(self.mem(addr));
      self.lr_ctl.atype := mode;
      --
      --  Read memory.  Optionally, checks for memory mapped I/O or shared memory
      --  or other special stuff can bed added here.
      --
      return data_bus(self.mem(addr));
   end;
   --
   --  Called to attach an I/O device to a simulator at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --  For bus:
   --    0 - I/O space
   --    1 - Memory space (currently unimplemented)
   --
   overriding
   procedure attach_io(self : in out m68000; io_dev : io_access;
                       base_addr : addr_bus; bus : bus_type) is
      size : addr_bus := io_dev.all.getSize;
      valid : Boolean := True;
   begin
      if bus = BUS_IO then
         Ada.Text_IO.Put_Line("I/O mapped I/O not yet implemented");
         --
         --  Check for port conflicts
         --
--         for i in BBS.embed.uint8(base_addr) .. BBS.embed.uint8(base_addr + size - 1) loop
--            if self.io_ports(i) /= null then
--               valid := False;
--               Ada.Text_IO.Put_Line("Port conflict detected attching device to port " & toHex(i));
--            end if;
--            exit when not valid;
--         end loop;
--         if valid then
--            for i in BBS.embed.uint8(base_addr) .. BBS.embed.uint8(base_addr + size - 1) loop
--               self.io_ports(i) := io_dev;
--               Ada.Text_IO.Put_Line("Attaching " & io_dev.name & " to I/O port " & toHex(i));
--            end loop;
--            io_dev.setBase(base_addr);
--         end if;
      elsif bus = BUS_MEMORY then
         Ada.Text_IO.Put_Line("Memory mapped I/O not yet implemented");
      else
         Ada.Text_IO.Put_Line("Unknown I/O bus type");
      end if;
   end;
   --
   --  Common code for Jump, Call, and Return
   --
   procedure jump(self : in out m68000; go : Boolean) is
      temp_pc : addr_bus;
   begin
      if go then
         temp_pc := addr_bus(self.get_next);
         temp_pc := temp_pc + addr_bus(self.get_next)*16#100#;
         self.pc := temp_pc;
      else
         self.pc := self.pc + 2;
      end if;
   end;
   --
   procedure call(self : in out m68000; go : Boolean) is
      temp_pc : addr_bus;
   begin
      if go then
         temp_pc := addr_bus(self.get_next);
         temp_pc := temp_pc + addr_bus(self.get_next)*16#100#;
         --
         self.sp := self.sp - 1;
         self.memory(self.sp, data_bus(self.pc/16#100#), ADDR_DATA);
         self.sp := self.sp - 1;
         self.memory(self.sp, data_bus(self.pc and 16#FF#), ADDR_DATA);
         self.pc := temp_pc;
      else
         self.pc := self.pc + 2;
      end if;
   end;
   --
   procedure ret(self : in out m68000; go : Boolean) is
      temp_pc : addr_bus;
   begin
      if go then
         temp_pc := addr_bus(self.memory(self.sp, ADDR_DATA));
         self.sp := self.sp + 1;
         temp_pc := temp_pc + addr_bus(self.memory(self.sp, ADDR_DATA))*16#100#;
         self.sp := self.sp + 1;
         self.pc := temp_pc;
      end if;
   end;
   --
end BBS.Sim_CPU.m68000;
