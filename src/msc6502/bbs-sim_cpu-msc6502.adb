--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of SimCPU.
--  SimCPU is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  SimCPU is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.--
--
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
package body BBS.Sim_CPU.msc6502 is
   --
   function uint16_to_ctrl is new Ada.Unchecked_Conversion(source => uint16,
                                                           target => ctrl_mode);
   function psw_to_byte is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => byte);
   function byte_to_psw is new Ada.Unchecked_Conversion(source => byte,
                                                        target => status_word);
--   function byte_to_op is new Ada.Unchecked_Conversion(source => byte,
--                                                       target => opcode);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out msc6502) is
   begin
      self.pc := self.addr;
      self.cpu_halt := False;
      self.lr_ctl.mode := PROC_KERN;
   end;
   --
   --  Called to start simulator execution at a specific address.
   --
   overriding
   procedure start(self : in out msc6502; addr : addr_bus) is
   begin
      self.start;
      self.pc := word(addr and 16#FFFF#);
   end;
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out msc6502) is
   begin
      if not self.halted then
         self.decode;
      end if;
   end;
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out msc6502) is
   begin
      if self.sr_ctl.addr then
         self.addr := word(self.sr_ad and 16#FFFF#);
      else
         self.mem(self.addr) := byte(self.sr_ad and 16#FF#);
         self.addr := self.addr + 1;
      end if;
      self.lr_addr := addr_bus(self.addr);
      self.lr_data := data_bus(self.mem(self.addr) and 16#FF#);
   end;
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out msc6502) is
   begin
      self.lr_addr := addr_bus(self.addr);
      self.lr_data := data_bus(self.mem(self.addr));
      self.addr := self.addr + 1;
   end;
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out msc6502) is
   begin
      self.addr := 0;
      self.temp_addr := 0;
      self.a   := 0;
      self.ix  := 0;
      self.iy  := 0;
      self.sp  := 0;
      self.pc  := 0;
      self.f.carry   := False;
      self.f.zero    := False;
      self.f.intdis  := False;
      self.f.decmode := False;
      self.f.break   := False;
      self.f.over    := False;
      self.f.sign    := False;
      self.ptr  := use_hl;
      self.int_mode := 0;
   end;
   --
   --  Called to get number of registers
   --  The Z-80 variant has additional registers defined.
   --
   overriding
   function registers(self : in out msc6502) return uint32 is
   begin
      return reg_id'Pos(reg_pc) + 1;
   end;
   --
   --  Called to get current variant index
   --
   overriding
   function variant(self : in out msc6502) return Natural is
   begin
    return variants_msc6502'pos(self.cpu_model);
   end;
   --
   --  Called to set variant
   --
   overriding
   procedure variant(self : in out msc6502; v : Natural) is
   begin
      self.cpu_model := variants_msc6502'Val(v);
   end;
   --
   --  Called to get variant name
   --
   overriding
   function variant(self : in out msc6502; v : Natural) return String is
   begin
      case v is
         when 0 =>
            return "msc6502";
         when 1 =>
            return "other";
         when others =>
            return "*Unknown variant*";
      end case;
   end;
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out msc6502; mem_addr : addr_bus;
                     data : data_bus) is
   begin
      self.mem(word(mem_addr and 16#FFFF#)) := byte(data and 16#FF#);
   end;
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out msc6502; mem_addr : addr_bus) return
     data_bus is
   begin
      return data_bus(self.mem(word(mem_addr and 16#FFFF#)));
   end;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out msc6502; num : uint32)
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
   function read_reg(self : in out msc6502; num : uint32)
                     return data_bus is
      reg : reg_id;
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         reg := reg_id'Val(num);
         case reg is
            when reg_a =>
               return data_bus(self.a);
            when reg_psw =>
               return data_bus(psw_to_byte(self.f));
            when reg_ix =>
               return data_bus(self.ix);
            when reg_iy =>
               return data_bus(self.iy);
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
   function read_reg(self : in out msc6502; num : uint32)
                     return String is
      reg : reg_id;
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         reg := reg_id'Val(num);
         case reg is
            when reg_a =>
               return toHex(self.a);
            when reg_psw =>
               return (if self.f.sign then "S" else "-") &
                      (if self.f.over then "O" else "-") & "*" &
                      (if self.f.break then "B" else "-") &
                      (if self.f.decmode then "D" else "-") &
                      (if self.f.intdis then "E" else "-") &
                      (if self.f.zero then "Z" else "-") &
                      (if self.f.carry then "C" else "-");
            when reg_ix =>
               return toHex(self.ix);
            when reg_iy =>
               return toHex(self.iy);
            when reg_sp =>
               return toHex(self.sp);
            when reg_pc =>
               return toHex(self.pc);
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
   --   procedure set_reg(self : in out simple; num : uint32;
   --                     data : uint32) is null;
   --
   --  This loads data from a file specified by "name" into the simulator memory.
   --  Currently, only Intel Hex format is supported.
   --
   overriding
   procedure load(self : in out msc6502; name : String) is
      inp   : Ada.Text_IO.File_Type;
      line  : Ada.Strings.Unbounded.Unbounded_String;
      count : byte;
      addr  : word;
      rec   : byte;
      data  : page;
      valid : Boolean;
   begin
      Ada.Text_IO.Open(inp, Ada.Text_IO.In_File, name);
      while not Ada.Text_IO.End_Of_File(inp) loop
         Ada.Text_IO.Unbounded_IO.Get_Line(inp, line);
         IntelHex(Ada.Strings.Unbounded.To_String(line), count, addr, rec, data,
                  valid);
         exit when rec = 1;  --  End of file record type
         if rec = 0 and valid then  --  Process a data record
            for i in 0 .. count - 1 loop
               self.memory(addr + word(i), data(Integer(i)), ADDR_DATA);
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
   function halted(self : in out msc6502) return Boolean is
   begin
      return self.cpu_halt;
   end;
   --
   --  This clears the halted flag allowing processing to continue.
   --
   overriding
   procedure continue_proc(self : in out msc6502) is
   begin
      self.cpu_halt := False;
   end;
   --
   --  Interrupt status.  Returns simulator dependent status of interrupts
   --
   overriding
   function intStatus(self : in out msc6502) return int32 is
   begin
      if self.int_enable then
         return 1;
      else
         return 0;
      end if;
   end;
   --
   --  Input/Output debugging
   --
   overriding
   function lastOutAddr(self : in out msc6502) return addr_bus is
   begin
      return self.last_out_addr;
   end;
   --
   overriding
   function lastOutData(self : in out msc6502) return data_bus is
   begin
      return self.last_out_data;
   end;
   --
   overriding
   procedure overrideIn(self : in out msc6502; addr : in addr_bus; data : in data_bus) is
   begin
      self.in_override  := True;
      self.in_over_addr := addr;
      self.in_over_data := data;
   end;
   --
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out msc6502; addr : addr_bus) is
   begin
      self.break_enable := True;
      self.break_point  := word(addr and 16#FFFF#);
   end;
   --
   procedure clearBreak(self : in out msc6502; addr : addr_bus) is
   begin
      self.break_enable := False;
   end;
   --
   --  Unimplemented instruction response
   --
   --  Right now just print a message for unrecognized opcodes.
   --  At some point, may want to do something different here.
   --
   procedure unimplemented(self : in out msc6502; addr : word; data : byte) is
   begin
      Ada.Text_IO.Put_Line("Illegal instruction at " & ToHex(addr) &
         " code " & ToHex(data));
   end;
--  --------------------------------------------------------------------
--
--  Code for the instruction processing.
--
   procedure decode(self : in out msc6502) is
      inst    : byte;
      temp_addr : word;
      temp16  : word;
      temp8   : byte;
      temppsw : status_word;
   begin
      --
      --  Interrupt check should go here
      --
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
      case inst is
         when 0 =>  --  BRK Forced interrupt
            self.f.break := True;
            self.pc := self.pc + 1;
            self.sp := self.sp - 1;
            self.memory(word(self.sp), byte(self.pc/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(word(self.sp), byte(self.pc and 16#FF#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(word(self.sp), psw_to_byte(self.f), ADDR_DATA);
            self.f.intdis := True;
            self.pc := word(self.memory(vect_IRQ, ADDR_DATA)) + word(self.memory(vect_IRQ + 1, ADDR_DATA))*16#100#;
         when 16#01# =>  --  ORA (indirect,X)
            temp16 := word(self.ix + self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            self.a := self.a or self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#02# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#03# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#04# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#05# =>  --  ORA zero page
            temp_addr := word(self.get_next);
            self.a := self.a or self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#06# =>  --  ASL zero page
            temp_addr := word(self.get_next);
            temp16 := word(self.memory(temp_addr, ADDR_DATA))*2;
            temp8  := byte(temp16 and 16#FF#);
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.carry := ((temp16 and 16#0100#) /= 0);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#07# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#08# =>  --  PHP
            self.push(psw_to_byte(self.f));
         when 16#09# =>  --  ORA immediate
            self.a := self.a or self.get_next;
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#0A# =>  --  ASL accumulator
            temp16 := word(self.a)*2;
            self.f.carry := ((temp16 and 16#0100#) /= 0);
            self.a := byte(temp16 and 16#FF#);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#0B# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#0C# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#0D# =>  --  ORA absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.a := self.a or self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#0E# =>  --  ASL absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp16 := word(self.memory(temp_addr, ADDR_DATA))*2;
            temp8  := byte(temp16 and 16#FF#);
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.carry := ((temp16 and 16#0100#) /= 0);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#0F# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#10# =>  --  BPL
            temp_addr := sign_extend(self.get_next);
            if not self.f.sign then
               self.pc := self.pc + temp_addr;
            end if;
         when 16#11# =>  --  ORA (indirect),Y
            temp16 := word(self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.a := self.a or self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#12# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#13# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#14# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#15# =>  --  ORA zero page,X
            temp_addr := word(self.get_next + self.ix);
            self.a := self.a or self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#16# =>  --  ASL zero page,X
            temp_addr := word(self.get_next + self.ix);
            temp16 := word(self.memory(temp_addr, ADDR_DATA))*2;
            temp8  := byte(temp16 and 16#FF#);
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.carry := ((temp16 and 16#0100#) /= 0);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#17# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#18# =>  --  CLC
            self.f.carry := False;
         when 16#19# =>  --  ORA absolute,Y
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.a := self.a or self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#1A# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#1B# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#1C# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#1D# =>  --  ORA absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            self.a := self.a or self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#1E# =>  --  ASL absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            temp16 := word(self.memory(temp_addr, ADDR_DATA))*2;
            temp8  := byte(temp16 and 16#FF#);
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.carry := ((temp16 and 16#0100#) /= 0);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#1F# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#20# =>
            self.unimplemented(self.pc, inst);
         when 16#21# =>  --  AND (indirect,X)
            temp16 := word(self.ix + self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            self.a := self.a and self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#22# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#23# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#24# =>  --  BIT zero page
            temp_addr := word(self.get_next);
            temp8 := self.memory(temp_addr, ADDR_DATA) and self.a;
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.f.over := (temp8 and 16#40#) /= 0;
            self.f.zero := (temp8 = 0);
         when 16#25# =>  --  AND zero page
            temp_addr := word(self.get_next);
            self.a := self.a and self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#26# =>  --  ROL zero page
            temp_addr := word(self.get_next);
            temp16 := word(self.memory(temp_addr, ADDR_DATA))*2;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            self.f.carry := (temp16 and 16#100#) /= 0;
            temp8 := byte(temp16 and 16#FF#);
            self.f.zero := (temp8 = 0);
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#27# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#28# =>  --  PLP
            self.f := byte_to_psw(self.pull);
         when 16#29# =>  --  AND immediate
            self.a := self.a and self.get_next;
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#2A# =>  --  ROL A
            temp16 := word(self.a)*2;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            self.f.carry := (temp16 and 16#100#) /= 0;
            temp8 := byte(temp16 and 16#FF#);
            self.f.zero := (temp8 = 0);
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.a := temp8;
         when 16#2B# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#2C# =>  --  BIT absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp8 := self.memory(temp_addr, ADDR_DATA) and self.a;
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.f.over := (temp8 and 16#40#) /= 0;
            self.f.zero := (temp8 = 0);
         when 16#2D# =>  --  AND absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.a := self.a and self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#2E# =>  --  ROL absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp16 := word(self.memory(temp_addr, ADDR_DATA))*2;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            self.f.carry := (temp16 and 16#100#) /= 0;
            temp8 := byte(temp16 and 16#FF#);
            self.f.zero := (temp8 = 0);
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#2F# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#30# =>  --  BMI
            temp_addr := sign_extend(self.get_next);
            if self.f.sign then
               self.pc := self.pc + temp_addr;
            end if;
         when 16#31# =>  --  AND (indirect),Y
            temp16 := word(self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.a := self.a and self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#32# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#33# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#34# =>
            self.unimplemented(self.pc, inst);
         when 16#35# =>  --  AND zero page,X
            temp_addr := word(self.get_next + self.ix);
            self.a := self.a and self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#36# =>  --  ROL zero page,X
            temp_addr := word(self.get_next + self.ix);
            temp16 := word(self.memory(temp_addr, ADDR_DATA))*2;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            self.f.carry := (temp16 and 16#100#) /= 0;
            temp8 := byte(temp16 and 16#FF#);
            self.f.zero := (temp8 = 0);
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#37# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#38# =>  --  SEC
            self.f.carry := True;
         when 16#39# =>  --  AND absolute,Y
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.a := self.a and self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#3A# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#3B# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#3C# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#3D# =>  --  AND absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            self.a := self.a and self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#3E# =>  --  ROL absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            temp16 := word(self.memory(temp_addr, ADDR_DATA))*2;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            self.f.carry := (temp16 and 16#100#) /= 0;
            temp8 := byte(temp16 and 16#FF#);
            self.f.zero := (temp8 = 0);
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#3F# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#40# =>
            self.unimplemented(self.pc, inst);
         when 16#41# =>  --  EOR (indirect,X)
            temp16 := word(self.ix + self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            self.a := self.a xor self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#42# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#43# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#44# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#45# =>  --  EOR zero page
            temp_addr := word(self.get_next);
            self.a := self.a xor self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#46# =>  --  LSR zero page
            temp_addr := word(self.get_next);
            temp8 := self.memory(temp_addr, ADDR_DATA);
            self.f.carry := (temp8 and 16#01#) /= 0;
            temp8 := temp8/2;
            self.f.zero := (temp8 = 0);
            self.f.sign := False;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#47# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#48# =>  --  PHA
            self.push(self.a);
         when 16#49# =>  --  EOR immediate
            self.a := self.a xor self.get_next;
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#4a# =>  --  LSR accumulator
            temp8 := self.a;
            self.f.carry := (temp8 and 16#01#) /= 0;
            temp8 := temp8/2;
            self.f.zero := (temp8 = 0);
            self.f.sign := False;
            self.a := temp8;
         when 16#4b# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#4c# =>  --  JMP absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.pc := temp_addr;
         when 16#4d# =>  --  EOR absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.a := self.a xor self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#4e# =>  --  LSR absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp8 := self.memory(temp_addr, ADDR_DATA);
            self.f.carry := (temp8 and 16#01#) /= 0;
            temp8 := temp8/2;
            self.f.zero := (temp8 = 0);
            self.f.sign := False;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#4f# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#50# =>  --  BVC
            temp_addr := sign_extend(self.get_next);
            if not self.f.over then
               self.pc := self.pc + temp_addr;
            end if;
         when 16#51# =>  --  EOR (indirect),Y
            temp16 := word(self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.a := self.a xor self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#52# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#53# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#54# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#55# =>  --  EOR zero page,X
            temp_addr := word(self.get_next + self.ix);
            self.a := self.a xor self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#56# =>  --  LSR zero page,X
            temp_addr := word(self.get_next + self.ix);
            temp8 := self.memory(temp_addr, ADDR_DATA);
            self.f.carry := (temp8 and 16#01#) /= 0;
            temp8 := temp8/2;
            self.f.zero := (temp8 = 0);
            self.f.sign := False;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#57# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#58# =>  --  CLI
            self.f.intdis := False;
         when 16#59# =>  --  EOR absolute,Y
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.a := self.a xor self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#5a# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#5b# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#5c# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#5d# =>  --  EOR absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            self.a := self.a xor self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#5e# =>  --  LSR absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            temp8 := self.memory(temp_addr, ADDR_DATA);
            self.f.carry := (temp8 and 16#01#) /= 0;
            temp8 := temp8/2;
            self.f.zero := (temp8 = 0);
            self.f.sign := False;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#5f# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#60# =>
            self.unimplemented(self.pc, inst);
         when 16#61# =>  --  ADC (indirect,X)
            temp16 := word(self.ix + self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            self.addf(self.memory(temp_addr, ADDR_DATA));
         when 16#62# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#63# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#64# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#65# =>  --  ADC zero page
            temp_addr := word(self.get_next);
            self.addf(self.memory(temp_addr, ADDR_DATA));
         when 16#66# =>  --  ROR zero page
            temp_addr := word(self.get_next);
            temp16 := word(self.memory(temp_addr, ADDR_DATA));
            if self.f.carry then
               temp16 := temp16 + 16#100#;
            end if;
            self.f.carry := (temp16 and 1) /= 0;
            temp8 := byte(temp16/2 and 16#FF#);
            self.f.zero := (temp8 = 0);
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#67# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#68# =>  --  PLA
            self.a := self.pull;
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#69# =>  --  ADC immediate
            self.addf(self.get_next);
         when 16#6a# =>  --  ROR accumulator
            temp16 := word(self.a);
            if self.f.carry then
               temp16 := temp16 + 16#100#;
            end if;
            self.f.carry := (temp16 and 1) /= 0;
            temp8 := byte(temp16/2 and 16#FF#);
            self.f.zero := (temp8 = 0);
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.a := temp8;
         when 16#6b# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#6c# =>  --  JMP (indirect)
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp16 := word(self.memory(temp_addr, ADDR_DATA));
            temp16 := temp16 + word(self.memory(temp_addr + 1, ADDR_DATA))*16#100#;
            self.pc := temp16;
         when 16#6d# =>  --  ADC absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.addf(self.memory(temp_addr, ADDR_DATA));
         when 16#6e# =>  --  ROR absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp16 := word(self.memory(temp_addr, ADDR_DATA));
            if self.f.carry then
               temp16 := temp16 + 16#100#;
            end if;
            self.f.carry := (temp16 and 1) /= 0;
            temp8 := byte(temp16/2 and 16#FF#);
            self.f.zero := (temp8 = 0);
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#6f# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#70# =>  --  BVS
            temp_addr := sign_extend(self.get_next);
            if self.f.over then
               self.pc := self.pc + temp_addr;
            end if;
         when 16#71# =>  --  ADC (indirect),Y
            temp16 := word(self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.addf(self.memory(temp_addr, ADDR_DATA));
         when 16#72# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#73# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#74# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#75# =>  --  ADC zero page,X
            temp_addr := word(self.get_next + self.ix);
            self.addf(self.memory(temp_addr, ADDR_DATA));
         when 16#76# =>  --  ROR zero page,X
            temp_addr := word(self.get_next + self.ix);
            temp16 := word(self.memory(temp_addr, ADDR_DATA));
            if self.f.carry then
               temp16 := temp16 + 16#100#;
            end if;
            self.f.carry := (temp16 and 1) /= 0;
            temp8 := byte(temp16/2 and 16#FF#);
            self.f.zero := (temp8 = 0);
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#77# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#78# =>  --  SEI
            self.f.intdis := True;
         when 16#79# =>  --  ADC absolute,Y
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.addf(self.memory(temp_addr, ADDR_DATA));
         when 16#7a# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#7b# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#7c# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#7d# =>  --  ADC absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            self.addf(self.memory(temp_addr, ADDR_DATA));
         when 16#7e# =>  --  ROR absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            temp16 := word(self.memory(temp_addr, ADDR_DATA));
            if self.f.carry then
               temp16 := temp16 + 16#100#;
            end if;
            self.f.carry := (temp16 and 1) /= 0;
            temp8 := byte(temp16/2 and 16#FF#);
            self.f.zero := (temp8 = 0);
            self.f.sign := (temp8 and 16#80#) /= 0;
            self.memory(temp_addr, temp8, ADDR_DATA);
         when 16#7f# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#80# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#81# =>  --  STA (indirect,X)
            temp16 := word(self.ix + self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#82# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#83# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#84# =>  --  STY zero page
            temp_addr := word(self.get_next);
            self.memory(temp_addr, self.iy, ADDR_DATA);
         when 16#85# =>  --  STA zero page
            temp_addr := word(self.get_next);
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#86# =>  --  STX zero page
            temp_addr := word(self.get_next);
            self.memory(temp_addr, self.ix, ADDR_DATA);
         when 16#87# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#88# =>  --  DEY
            self.iy := self.iy - 1;
            self.f.zero := (self.iy = 0);
            self.f.sign := ((self.iy and 16#80#) /= 0);
         when 16#89# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#8A# =>  --  TXA
            self.a := self.ix;
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#8B# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#8C# =>  --  STY absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.memory(temp_addr, self.iy, ADDR_DATA);
         when 16#8D# =>  --  STA absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#8E# =>  --  STX absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.memory(temp_addr, self.ix, ADDR_DATA);
         when 16#8F# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#90# =>  --  BCC
            temp_addr := sign_extend(self.get_next);
            if not self.f.carry then
               self.pc := self.pc + temp_addr;
            end if;
         when 16#91# =>  --  STA (indirect),Y
            temp16 := word(self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#92# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#93# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#94# =>  --  STY zero page,X
            temp_addr := word(self.get_next + self.ix);
            self.memory(temp_addr, self.iy, ADDR_DATA);
         when 16#95# =>  --  STA zero page,X
            temp_addr := word(self.get_next + self.ix);
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#96# =>  --  STX zero page,Y
            temp_addr := word(self.get_next + self.iy);
            self.memory(temp_addr, self.ix, ADDR_DATA);
         when 16#97# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#98# =>  --  TYA
            self.a := self.iy;
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#99# =>  --  STA absolute,Y
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#9A# =>  --  TXS
            self.sp := self.ix;
            self.f.zero := (self.sp = 0);
            self.f.sign := ((self.sp and 16#80#) /= 0);
         when 16#9B# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#9C# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#9D# =>  --  STA absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#9E# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#9F# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#A0# =>  --  LDY imediate
            self.iy := self.get_next;
            self.f.zero := (self.iy = 0);
            self.f.sign := ((self.iy and 16#80#) /= 0);
         when 16#A1# =>  --  LDA (indirect,X)
            temp16 := word(self.ix + self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            self.a := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#A2# =>  --  LDX immediate
            self.ix := self.get_next;
            self.f.zero := (self.ix = 0);
            self.f.sign := ((self.ix and 16#80#) /= 0);
         when 16#A3# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#A4# =>  --  LDY zero page
            temp_addr := word(self.get_next);
            self.iy := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.iy = 0);
            self.f.sign := ((self.iy and 16#80#) /= 0);
         when 16#A5# =>  --  LDA zero page
            temp_addr := word(self.get_next);
            self.a := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#A6# =>  --  LDX zero page
            temp_addr := word(self.get_next);
            self.ix := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.ix = 0);
            self.f.sign := ((self.ix and 16#80#) /= 0);
         when 16#A7# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#A8# =>  --  TAY
            self.iy := self.a;
            self.f.zero := (self.iy = 0);
            self.f.sign := ((self.iy and 16#80#) /= 0);
         when 16#A9# =>  --  LDA immediate
            self.a := self.get_next;
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#AA# =>  --  TAX
            self.ix := self.a;
            self.f.zero := (self.ix = 0);
            self.f.sign := ((self.ix and 16#80#) /= 0);
         when 16#AB# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#AC# =>  --  LDY absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.iy := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.iy = 0);
            self.f.sign := ((self.iy and 16#80#) /= 0);
         when 16#AD# =>  --  LDA absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.a := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#AE# =>  --  LDX absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.ix := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.ix = 0);
            self.f.sign := ((self.ix and 16#80#) /= 0);
         when 16#AF# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#B0# =>  --  BCS
            temp_addr := sign_extend(self.get_next);
            if self.f.carry then
               self.pc := self.pc + temp_addr;
            end if;
         when 16#B1# =>  --  LDA (indirect),Y
            temp16 := word(self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.a := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#B2# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#B3# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#B4# =>  --  LDY zero page,X
            temp_addr := word(self.ix + self.get_next);
            self.iy := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.iy = 0);
            self.f.sign := ((self.iy and 16#80#) /= 0);
         when 16#B5# =>  --  LDA zero page,X
            temp_addr := word(self.ix + self.get_next);
            self.a := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#B6# =>  --  LDX zero page,Y
            temp_addr := word(self.iy + self.get_next);
            self.ix := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.ix = 0);
            self.f.sign := ((self.ix and 16#80#) /= 0);
         when 16#B7# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#B8# =>  --  CLV
            self.f.over := False;
         when 16#B9# =>  --  LDA absolute,Y
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.a := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#BA#=>  --  TSX
            self.ix := self.sp;
            self.f.zero := (self.ix = 0);
            self.f.sign := ((self.ix and 16#80#) /= 0);
         when 16#BB# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#BC# =>  --  LDY absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            self.iy := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.iy = 0);
            self.f.sign := ((self.iy and 16#80#) /= 0);
         when 16#BD# =>  --  LDA absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            self.a := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.a = 0);
            self.f.sign := ((self.a and 16#80#) /= 0);
         when 16#BE# =>  --  LDX absolute,Y
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.ix := self.memory(temp_addr, ADDR_DATA);
            self.f.zero := (self.ix = 0);
            self.f.sign := ((self.ix and 16#80#) /= 0);
         when 16#BF# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#C0# =>  --  CPY immediate
            temp16 := word(self.iy) - word(self.get_next);
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#C1# =>  --  CMP (indirect,X)
            temp16 := word(self.ix + self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            temp16 := word(self.a) - word(self.memory(temp_addr, ADDR_DATA));
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#C2# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#C3# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#C4# =>  --  CPY zero page
            temp_addr := word(self.get_next);
            temp16 := word(self.iy) - word(self.memory(temp_addr, ADDR_DATA));
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#C5# =>  --  CMP zero page
            temp_addr := word(self.get_next);
            temp16 := word(self.a) - word(self.memory(temp_addr, ADDR_DATA));
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#C6# =>  --  DEC zero page
            temp_addr := word(self.get_next);
            temp8 := self.memory(temp_addr, ADDR_DATA) - 1;
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#C7# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#C8# =>  --  INY
            self.iy := self.iy + 1;
            self.f.zero := (self.iy = 0);
            self.f.sign := ((self.iy and 16#80#) /= 0);
         when 16#C9# =>  --  CMP immediate
            temp16 := word(self.a) - word(self.get_next);
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#CA# =>  --  DEX
            self.ix := self.ix - 1;
            self.f.zero := (self.ix = 0);
            self.f.sign := ((self.ix and 16#80#) /= 0);
         when 16#CB# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#CC# =>  --  CPY absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp16 := word(self.iy) - word(self.memory(temp_addr, ADDR_DATA));
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#CD# =>  --  CMP absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp16 := word(self.a) - word(self.memory(temp_addr, ADDR_DATA));
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#CE# =>  --  DEC absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp8 := self.memory(temp_addr, ADDR_DATA) - 1;
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#CF# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#D0# =>  --  BNE
            temp_addr := sign_extend(self.get_next);
            if not self.f.zero then
               self.pc := self.pc + temp_addr;
            end if;
         when 16#D1# =>  --  CMP (indirect),Y
            temp16 := word(self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            temp_addr := temp_addr + word(self.iy);
            temp16 := word(self.a) - word(self.memory(temp_addr, ADDR_DATA));
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#D2# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#D3# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#D4# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#D5# =>  --  CMP zero page,X
            temp_addr := word(self.ix + self.get_next);
            temp16 := word(self.a) - word(self.memory(temp_addr, ADDR_DATA));
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#D6# =>  --  DEC zero page,X
            temp_addr := word(self.ix + self.get_next);
            temp8 := self.memory(temp_addr, ADDR_DATA) - 1;
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#D7# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#D8# =>  --  CLD
            self.f.decmode := False;
         when 16#D9# =>  --  CMP absolute,Y
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.iy);
            temp16 := word(self.a) - word(self.memory(temp_addr, ADDR_DATA));
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#DA# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#DB# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#DC# =>
            self.unimplemented(self.pc, inst);
         when 16#DD# =>  --  CMP absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            temp16 := word(self.a) - word(self.memory(temp_addr, ADDR_DATA));
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#DE# =>  --  DEC absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            temp8 := self.memory(temp_addr, ADDR_DATA) - 1;
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#DF# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#E0# =>  --  CPX immediate
            temp16 := word(self.ix) - word(self.get_next);
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#E1# =>  --  SBC (indirect,X)
            temp16 := word(self.ix + self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            self.subf(self.memory(temp_addr, ADDR_DATA));
         when 16#E2# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#E3# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#E4# =>  --  CPX zero page
            temp_addr := word(self.get_next);
            temp16 := word(self.ix) - word(self.memory(temp_addr, ADDR_DATA));
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#E5# =>  --  SBC zero page
            temp_addr := word(self.get_next);
            self.subf(self.memory(temp_addr, ADDR_DATA));
         when 16#E6# =>  --  INC zero page
            temp_addr := word(self.get_next);
            temp8 := self.memory(temp_addr, ADDR_DATA) + 1;
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#E7# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#E8# =>  --  INX
            self.ix := self.ix + 1;
            self.f.zero := (self.ix = 0);
            self.f.sign := ((self.ix and 16#80#) /= 0);
         when 16#E9# =>  --  SBC immediate
            self.subf(self.get_next);
         when 16#EA# =>  --  NOP
            null;
         when 16#EB# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#EC# =>  --  CPX absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp16 := word(self.ix) - word(self.memory(temp_addr, ADDR_DATA));
            self.f.carry := (temp16 and 16#100#) /= 0;
            self.f.sign  := (temp16 and 16#80#) /= 0;
            self.f.zero  := (temp16 and 16#ff#) = 0;
         when 16#ED# =>  --  SBC absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.subf(self.memory(temp_addr, ADDR_DATA));
         when 16#EE# =>  --  INC absolute
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp8 := self.memory(temp_addr, ADDR_DATA) + 1;
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#EF# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#F0# =>  --  BEQ
            temp_addr := sign_extend(self.get_next);
            if self.f.zero then
               self.pc := self.pc + temp_addr;
            end if;
         when 16#F1# =>  --  SBC (indirect),Y
            temp16 := word(self.get_next);
            temp_addr := word(self.memory(temp16, ADDR_DATA));
            temp_addr := temp_addr + word(self.memory(temp16 + 1, ADDR_DATA))*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.subf(self.memory(temp_addr, ADDR_DATA));
         when 16#F2# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#F3# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#F4# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#F5# =>  --  SBC zero page,X
            temp_addr := word(self.get_next + self.ix);
            self.subf(self.memory(temp_addr, ADDR_DATA));
         when 16#F6# =>  --  INC zero page,X
            temp_addr := word(self.ix + self.get_next);
            temp8 := self.memory(temp_addr, ADDR_DATA) + 1;
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#F7# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#F8# =>  --  SED
            self.f.decmode := True;
         when 16#F9# =>  --  SBC absolute,Y
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.iy);
            self.subf(self.memory(temp_addr, ADDR_DATA));
         when 16#FA# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#FB# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#FC# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
         when 16#FD# =>  --  SBC absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            self.subf(self.memory(temp_addr, ADDR_DATA));
         when 16#FE# =>  --  INC absolute,X
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            temp_addr := temp_addr + word(self.ix);
            temp8 := self.memory(temp_addr, ADDR_DATA) + 1;
            self.memory(temp_addr, temp8, ADDR_DATA);
            self.f.zero := (temp8 = 0);
            self.f.sign := ((temp8 and 16#80#) /= 0);
         when 16#FF# =>  --  Future expansion
            self.unimplemented(self.pc, inst);
      end case;
      self.ptr := use_hl;
   end;
   --
   --  Utility code for instruction decoder
   --
   function get_next(self : in out msc6502) return byte is
      t : byte;
   begin
      self.lr_ctl.atype := ADDR_INST;
      if self.intr then
         self.lr_ctl.atype := ADDR_INTR;
         return 0;  -- Currently interrupts are not implemented
      else
         t := self.memory(self.pc, ADDR_INST);
         self.pc := self.pc + 1;
         return t;
      end if;
   end;
   --
   --  Perform addition and set flags including carry and aux carry
   --
   procedure addf(self : in out msc6502; v1 : byte) is
      sum  : word;  --  Full sum so that carry can be extracted
      sumb : byte;  --  Byte sized result
   begin
      if self.f.decmode then  --  Decimal mode
         declare
            dig1a : constant byte := (v1/16#10#) and 16#0F#;
            dig1b : constant byte := v1 and 16#0F#;
            dig2a : constant byte := (self.a/16#10#) and 16#0F#;
            dig2b : constant byte := self.a and 16#0F#;
         begin
            sumb := dig1b + dig2b;
            if self.f.carry then
               sumb := sumb + 1;
            end if;
            sum  := word(dig1a) + word(dig2a);
            if sumb > 9 then
               sum := sum + 1;
               sumb := sumb - 10;
            end if;
            self.f.carry := (sum > 10);
            if sum > 10 then
               sum := sum - 10;
            end if;
            sumb := byte(sum)*16#10# + sumb;
         end;
         self.f.zero := ((self.a + v1) = 0);
      else  --  Binary mode
         sum := word(self.a) + word(v1);
         if self.f.carry then
            sum := sum + 1;
         end if;
         sumb := byte(sum and 16#FF#);
         self.f.carry := (sum > 16#FF#);
         self.f.zero := (sumb = 0);
      end if;
      if ((self.a and 16#80#) = (v1 and 16#80#)) then
         self.f.over := (v1 and 16#80#) /= (sumb and 16#80#);
      else
         self.f.over := False;
      end if;
      self.f.sign := ((sumb and 16#80#) = 16#80#);
      self.a := sumb;
   end;
   --
   --  Perform subtraction and set flags including carry and aux carry
   --
   procedure subf(self : in out msc6502; v1 : byte) is
      diff  : word;  --  Full difference so that carry can be extracted
      diffb : byte;  --  Byte sized result
   begin
      if self.f.decmode then  --  Decimal mode
         declare
            dig1a : constant byte := (v1/16#10#) and 16#0F#;
            dig1b : constant byte := v1 and 16#0F#;
            dig2a : constant byte := (self.a/16#10#) and 16#0F#;
            dig2b : constant byte := self.a and 16#0F#;
         begin
            diffb := dig2b - dig1b;  --  LSD calculation
            if self.f.carry then
               diffb := diffb - 1;
            end if;
            diff  := word(dig2a) - word(dig1a);  --  MSD calculation
            if diffb > 9 then  --  LSD adjustment
               diff := diff - 1;
               diffb := diffb + 10;
            end if;
            self.f.carry := (diff > 10);
            if diff > 9 then
               diff := diff + 10;
            end if;
            diffb := byte(diff)*16#10# + diffb;
         end;
         self.f.zero := ((self.a + v1) = 0);
      else  --  Binary mode
         diff := word(self.a) - word(v1);
         if self.f.carry then
            diff := diff - 1;
         end if;
         diffb := byte(diff and 16#FF#);
         self.f.carry := (diff > 16#FF#);
         self.f.zero := (diffb = 0);
      end if;
      if ((self.a and 16#80#) = (v1 and 16#80#)) then
         self.f.over := (v1 and 16#80#) /= (diffb and 16#80#);
      else
         self.f.over := False;
      end if;
      self.f.sign := ((diffb and 16#80#) = 16#80#);
      self.a := diffb;
   end;
   --
   --  All memory accesses should be routed through these functions so that they
   --  can do checks for memory-mapped I/O or shared memory.
   --
   procedure memory(self : in out msc6502; addr : word; value : byte; mode : addr_type) is
   begin
      --
      --  Set LED register values
      --
      self.lr_addr := addr_bus(addr);
      self.lr_data := data_bus(value);
      self.lr_ctl.atype := mode;
      --
      --  Set memory.  Optionally, checks for memory mapped I/O or shared memory
      --  or other special stuff can be added here.
      --
      self.mem(addr) := value;
   end;
   --
   function memory(self : in out msc6502; addr : word; mode : addr_type) return byte is
   begin
      --
      --  Set LED register values
      --
      self.lr_addr := addr_bus(addr);
      self.lr_data := data_bus(self.mem(addr));
      self.lr_ctl.atype := mode;
      --
      --  Read memory.  Optionally, checks for memory mapped I/O or shared memory
      --  or other special stuff can be added here.
      --
      return self.mem(addr);
   end;
   --
   --  Called to attach an I/O device to a simulator at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --  For bus:
   --    0 - I/O space (currently unimplemented)
   --    1 - Memory space (currently unimplemented)
   --
   overriding
   procedure attach_io(self : in out msc6502; io_dev : io_access;
                       base_addr : addr_bus; bus : bus_type) is
      size : addr_bus := io_dev.all.getSize;
      valid : Boolean := True;
   begin
      if bus = BUS_IO then
         Ada.Text_IO.Put_Line("I/O bus not yet implemented");
      elsif bus = BUS_MEMORY then
         Ada.Text_IO.Put_Line("Memory mapped I/O not yet implemented");
      else
         Ada.Text_IO.Put_Line("Unknown I/O bus type");
      end if;
   end;
   --
   --  Handle I/O port accesses
   --
   procedure port(self : in out msc6502; addr : byte; value : byte) is
   begin
      Ada.Text_IO.Put_Line("I/O Not yet implemented.");
   end;
   --
   function port(self : in out msc6502; addr : byte) return byte is
   begin
      Ada.Text_IO.Put_Line("I/O Not yet implemented.");
      return 0;
   end;
   --
   --  Common code for Jump, Call, and Return
   --
   procedure jump(self : in out msc6502; go : Boolean) is
      temp_pc : word;
   begin
      if go then
         temp_pc := word(self.get_next);
         temp_pc := temp_pc + word(self.get_next)*16#100#;
         self.pc := temp_pc;
      else
         self.pc := self.pc + 2;
      end if;
   end;
   --
   procedure call(self : in out msc6502; go : Boolean) is
      temp_pc : word;
   begin
      if go then
         temp_pc := word(self.get_next);
         temp_pc := temp_pc + word(self.get_next)*16#100#;
         --
         self.push(byte(self.pc/16#100#));
         self.push(byte(self.pc and 16#FF#));
         self.pc := temp_pc;
      else
         self.pc := self.pc + 2;
      end if;
   end;
   --
   procedure ret(self : in out msc6502; go : Boolean) is
      temp_pc : word;
   begin
      if go then
         temp_pc := word(self.pull);
         temp_pc := temp_pc + word(self.pull)*16#100#;
         self.pc := temp_pc;
      end if;
   end;
   --
   --  Stack instructions
   --  Note that the stack is in page 2 and SP is only 8 bits, so a 2#10# in bits
   --  9 and 8 is implied.  Running all stack operations through here makes sure
   --  that this is uniformly applied.
   --
   procedure push(self : in out msc6502; value : byte) is
   begin
      self.memory(word(self.sp) + stack_page, value, ADDR_DATA);
      self.sp := self.sp - 1;
   end;
   --
   function pull(self : in out msc6502) return byte is
      t : byte;
   begin
      self.sp := self.sp + 1;
      t := self.memory(word(self.sp) + stack_page, ADDR_DATA);
      return t;
   end;
   --
   --  Other utility functions
   --
   function sign_extend(t8 : byte) return word is
   begin
      if (t8 and 16#80#) = 16#80# then
         return word(t8) + 16#FF00#;
      else
         return word(t8);
      end if;
   end;
   --
end BBS.Sim_CPU.msc6502;
