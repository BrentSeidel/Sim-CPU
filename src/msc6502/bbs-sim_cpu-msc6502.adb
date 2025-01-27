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
      self.b   := 0;
      self.c   := 0;
      self.d   := 0;
      self.e   := 0;
      self.h   := 0;
      self.l   := 0;
      self.sp  := 0;
      self.pc  := 0;
      self.f.carry   := False;
      self.f.zero    := False;
      self.f.intdis  := False;
      self.f.decmode := False;
      self.f.break   := True;
      self.f.over    := True;
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
                      (if self.f.intdis then "E" else "D") &
                      (if self.f.zero then "Z" else "D") &
                      (if self.f.carry then "C" else "-");
            when reg_b =>
               return toHex(self.b);
            when reg_c =>
               return toHex(self.c);
            when reg_bc =>
               return toHex(word(self.b)*16#100# + word(self.c));
            when reg_d =>
               return toHex(self.d);
            when reg_e =>
               return toHex(self.e);
            when reg_de =>
               return toHex(word(self.d)*16#100# + word(self.e));
            when reg_h =>
               return toHex(self.h);
            when reg_l =>
               return toHex(self.l);
            when reg_hl =>
               return toHex(word(self.h)*16#100# + word(self.l));
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
--      op_inst : opcode;
--      temp_addr : word;
--      temp16  : word;
--      temp8   : byte;
      temppsw : status_word;
   begin
      --
      --  Interrupt check should go here
      --
      self.intr := False;  --  Currently interrupts are not implemented
      --
      --  Check to see if interrupts are to be enabled
      --
      if self.ie_pending then
         self.int_enable := True;
         self.ie_pending := False;
      end if;
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
--      op_inst := byte_to_op(inst);
      if (word(self.trace) and 1) = 1 then
         Ada.Text_IO.Put_Line("TRACE: Address: " & toHex(self.pc - 1) & " instruction " &
                           toHex(inst));
      end if;
      --
      --  Do instruction processing
      --
      case inst is
         when 0 =>
            self.unimplemented(self.pc, inst);
         when 16#01# =>
            self.unimplemented(self.pc, inst);
         when 16#02# =>
            self.unimplemented(self.pc, inst);
         when 16#03# =>
            self.unimplemented(self.pc, inst);
         when 16#04# =>
            self.unimplemented(self.pc, inst);
         when 16#05# =>
            self.unimplemented(self.pc, inst);
         when 16#06# =>
            self.unimplemented(self.pc, inst);
         when 16#07# =>
            self.unimplemented(self.pc, inst);
         when 16#08# =>
            self.unimplemented(self.pc, inst);
         when 16#09# =>
            self.unimplemented(self.pc, inst);
         when 16#0A# =>
            self.unimplemented(self.pc, inst);
         when 16#0B# =>
            self.unimplemented(self.pc, inst);
         when 16#1B# =>
            self.unimplemented(self.pc, inst);
         when 16#2B# =>
            self.unimplemented(self.pc, inst);
         when 16#3B# =>
            self.unimplemented(self.pc, inst);
         when 16#0C# =>
            self.unimplemented(self.pc, inst);
         when 16#0D# =>
            self.unimplemented(self.pc, inst);
         when 16#0E# =>
            self.unimplemented(self.pc, inst);
         when 16#0F# =>
            self.unimplemented(self.pc, inst);
         when 16#10# =>
            self.unimplemented(self.pc, inst);
         when 16#11# =>
            self.unimplemented(self.pc, inst);
         when 16#12# =>
            self.unimplemented(self.pc, inst);
         when 16#13# =>
            self.unimplemented(self.pc, inst);
         when 16#14# =>
            self.unimplemented(self.pc, inst);
         when 16#15# =>
            self.unimplemented(self.pc, inst);
         when 16#16# =>
            self.unimplemented(self.pc, inst);
         when 16#17# =>
            self.unimplemented(self.pc, inst);
         when 16#18# =>
            self.unimplemented(self.pc, inst);
         when 16#19# =>
            self.unimplemented(self.pc, inst);
         when 16#1A# =>
            self.unimplemented(self.pc, inst);
         when 16#1C# =>
            self.unimplemented(self.pc, inst);
         when 16#1D# =>
            self.unimplemented(self.pc, inst);
         when 16#1E# =>
            self.unimplemented(self.pc, inst);
         when 16#1F# =>
            self.unimplemented(self.pc, inst);
         when 16#20# =>
            self.unimplemented(self.pc, inst);
         when 16#21# =>
            self.unimplemented(self.pc, inst);
         when 16#22# =>
            self.unimplemented(self.pc, inst);
         when 16#23# =>
            self.unimplemented(self.pc, inst);
         when 16#24# =>
            self.unimplemented(self.pc, inst);
         when 16#25# =>
            self.unimplemented(self.pc, inst);
         when 16#26# =>
            self.unimplemented(self.pc, inst);
         when 16#27# =>
            self.unimplemented(self.pc, inst);
         when 16#28# =>
            self.unimplemented(self.pc, inst);
         when 16#29# =>
            self.unimplemented(self.pc, inst);
         when 16#2A# =>
            self.unimplemented(self.pc, inst);
         when 16#2C# =>
            self.unimplemented(self.pc, inst);
         when 16#2D# =>
            self.unimplemented(self.pc, inst);
         when 16#2E# =>
            self.unimplemented(self.pc, inst);
         when 16#2F# =>
            self.unimplemented(self.pc, inst);
         when 16#30# =>
            self.unimplemented(self.pc, inst);
         when 16#31# =>
            self.unimplemented(self.pc, inst);
         when 16#32# =>
            self.unimplemented(self.pc, inst);
         when 16#33# =>
            self.unimplemented(self.pc, inst);
         when 16#34# =>
            self.unimplemented(self.pc, inst);
         when 16#35# =>
            self.unimplemented(self.pc, inst);
         when 16#36# =>
            self.unimplemented(self.pc, inst);
         when 16#37# =>
            self.unimplemented(self.pc, inst);
         when 16#38# =>
            self.unimplemented(self.pc, inst);
         when 16#39# =>
            self.unimplemented(self.pc, inst);
         when 16#3A# =>
            self.unimplemented(self.pc, inst);
         when 16#3C# =>
            self.unimplemented(self.pc, inst);
         when 16#3D# =>
            self.unimplemented(self.pc, inst);
         when 16#3E# =>
            self.unimplemented(self.pc, inst);
         when 16#3F# =>
            self.unimplemented(self.pc, inst);
         when 16#40# =>
            self.unimplemented(self.pc, inst);
         when 16#41# =>
            self.unimplemented(self.pc, inst);
         when 16#42# =>
            self.unimplemented(self.pc, inst);
         when 16#43# =>
            self.unimplemented(self.pc, inst);
         when 16#44# =>
            self.unimplemented(self.pc, inst);
         when 16#45# =>
            self.unimplemented(self.pc, inst);
         when 16#46# =>
            self.unimplemented(self.pc, inst);
         when 16#47# =>
            self.unimplemented(self.pc, inst);
         when 16#48# =>
            self.unimplemented(self.pc, inst);
         when 16#49# =>
            self.unimplemented(self.pc, inst);
         when 16#4a# =>
            self.unimplemented(self.pc, inst);
         when 16#4b# =>
            self.unimplemented(self.pc, inst);
         when 16#4c# =>
            self.unimplemented(self.pc, inst);
         when 16#4d# =>
            self.unimplemented(self.pc, inst);
         when 16#4e# =>
            self.unimplemented(self.pc, inst);
         when 16#4f# =>
            self.unimplemented(self.pc, inst);
         when 16#50# =>
            self.unimplemented(self.pc, inst);
         when 16#51# =>
            self.unimplemented(self.pc, inst);
         when 16#52# =>
            self.unimplemented(self.pc, inst);
         when 16#53# =>
            self.unimplemented(self.pc, inst);
         when 16#54# =>
            self.unimplemented(self.pc, inst);
         when 16#55# =>
            self.unimplemented(self.pc, inst);
         when 16#56# =>
            self.unimplemented(self.pc, inst);
         when 16#57# =>
            self.unimplemented(self.pc, inst);
         when 16#58# =>
            self.unimplemented(self.pc, inst);
         when 16#59# =>
            self.unimplemented(self.pc, inst);
         when 16#5a# =>
            self.unimplemented(self.pc, inst);
         when 16#5b# =>
            self.unimplemented(self.pc, inst);
         when 16#5c# =>
            self.unimplemented(self.pc, inst);
         when 16#5d# =>
            self.unimplemented(self.pc, inst);
         when 16#5e# =>
            self.unimplemented(self.pc, inst);
         when 16#5f# =>
            self.unimplemented(self.pc, inst);
         when 16#60# =>
            self.unimplemented(self.pc, inst);
         when 16#61# =>
            self.unimplemented(self.pc, inst);
         when 16#62# =>
            self.unimplemented(self.pc, inst);
         when 16#63# =>
            self.unimplemented(self.pc, inst);
         when 16#64# =>
            self.unimplemented(self.pc, inst);
         when 16#65# =>
            self.unimplemented(self.pc, inst);
         when 16#66# =>
            self.unimplemented(self.pc, inst);
         when 16#67# =>
            self.unimplemented(self.pc, inst);
         when 16#68# =>
            self.unimplemented(self.pc, inst);
         when 16#69# =>
            self.unimplemented(self.pc, inst);
         when 16#6a# =>
            self.unimplemented(self.pc, inst);
         when 16#6b# =>
            self.unimplemented(self.pc, inst);
         when 16#6c# =>
            self.unimplemented(self.pc, inst);
         when 16#6d# =>
            self.unimplemented(self.pc, inst);
         when 16#6e# =>
            self.unimplemented(self.pc, inst);
         when 16#6f# =>
            self.unimplemented(self.pc, inst);
         when 16#70# =>
            self.unimplemented(self.pc, inst);
         when 16#71# =>
            self.unimplemented(self.pc, inst);
         when 16#72# =>
            self.unimplemented(self.pc, inst);
         when 16#73# =>
            self.unimplemented(self.pc, inst);
         when 16#74# =>
            self.unimplemented(self.pc, inst);
         when 16#75# =>
            self.unimplemented(self.pc, inst);
         when 16#76# =>
            self.unimplemented(self.pc, inst);
         when 16#77# =>
            self.unimplemented(self.pc, inst);
         when 16#78# =>
            self.unimplemented(self.pc, inst);
         when 16#79# =>
            self.unimplemented(self.pc, inst);
         when 16#7a# =>
            self.unimplemented(self.pc, inst);
         when 16#7b# =>
            self.unimplemented(self.pc, inst);
         when 16#7c# =>
            self.unimplemented(self.pc, inst);
         when 16#7d# =>
            self.unimplemented(self.pc, inst);
         when 16#7e# =>
            self.unimplemented(self.pc, inst);
         when 16#7f# =>
            self.unimplemented(self.pc, inst);
         when 16#80# =>
            self.unimplemented(self.pc, inst);
         when 16#81# =>
            self.unimplemented(self.pc, inst);
         when 16#82# =>
            self.unimplemented(self.pc, inst);
         when 16#83# =>
            self.unimplemented(self.pc, inst);
         when 16#84# =>
            self.unimplemented(self.pc, inst);
         when 16#85# =>
            self.unimplemented(self.pc, inst);
         when 16#86# =>
            self.unimplemented(self.pc, inst);
         when 16#87# =>
            self.unimplemented(self.pc, inst);
         when 16#88# =>
            self.unimplemented(self.pc, inst);
         when 16#89# =>
            self.unimplemented(self.pc, inst);
         when 16#8A# =>
            self.unimplemented(self.pc, inst);
         when 16#8B# =>
            self.unimplemented(self.pc, inst);
         when 16#8C# =>
            self.unimplemented(self.pc, inst);
         when 16#8D# =>
            self.unimplemented(self.pc, inst);
         when 16#8E# =>
            self.unimplemented(self.pc, inst);
         when 16#8F# =>
            self.unimplemented(self.pc, inst);
         when 16#90# =>
            self.unimplemented(self.pc, inst);
         when 16#91# =>
            self.unimplemented(self.pc, inst);
         when 16#92# =>
            self.unimplemented(self.pc, inst);
         when 16#93# =>
            self.unimplemented(self.pc, inst);
         when 16#94# =>
            self.unimplemented(self.pc, inst);
         when 16#95# =>
            self.unimplemented(self.pc, inst);
         when 16#96# =>
            self.unimplemented(self.pc, inst);
         when 16#97# =>
            self.unimplemented(self.pc, inst);
         when 16#98# =>
            self.unimplemented(self.pc, inst);
         when 16#99# =>
            self.unimplemented(self.pc, inst);
         when 16#9A# =>
            self.unimplemented(self.pc, inst);
         when 16#9B# =>
            self.unimplemented(self.pc, inst);
         when 16#9C# =>
            self.unimplemented(self.pc, inst);
         when 16#9D# =>
            self.unimplemented(self.pc, inst);
         when 16#9E# =>
            self.unimplemented(self.pc, inst);
         when 16#9F# =>
            self.unimplemented(self.pc, inst);
         when 16#A0# =>
            self.unimplemented(self.pc, inst);
         when 16#A1# =>
            self.unimplemented(self.pc, inst);
         when 16#A2# =>
            self.unimplemented(self.pc, inst);
         when 16#A3# =>
            self.unimplemented(self.pc, inst);
         when 16#A4# =>
            self.unimplemented(self.pc, inst);
         when 16#A5# =>
            self.unimplemented(self.pc, inst);
         when 16#A6# =>
            self.unimplemented(self.pc, inst);
         when 16#A7# =>
            self.unimplemented(self.pc, inst);
         when 16#A8# =>
            self.unimplemented(self.pc, inst);
         when 16#A9# =>
            self.unimplemented(self.pc, inst);
         when 16#AA# =>
            self.unimplemented(self.pc, inst);
         when 16#AB# =>
            self.unimplemented(self.pc, inst);
         when 16#AC# =>
            self.unimplemented(self.pc, inst);
         when 16#AD# =>
            self.unimplemented(self.pc, inst);
         when 16#AE# =>
            self.unimplemented(self.pc, inst);
         when 16#AF# =>
            self.unimplemented(self.pc, inst);
         when 16#B0# =>
            self.unimplemented(self.pc, inst);
         when 16#B1# =>
            self.unimplemented(self.pc, inst);
         when 16#B2# =>
            self.unimplemented(self.pc, inst);
         when 16#B3# =>
            self.unimplemented(self.pc, inst);
         when 16#B4# =>
            self.unimplemented(self.pc, inst);
         when 16#B5# =>
            self.unimplemented(self.pc, inst);
         when 16#B6# =>
            self.unimplemented(self.pc, inst);
         when 16#B7# =>
            self.unimplemented(self.pc, inst);
         when 16#B8# =>
            self.unimplemented(self.pc, inst);
         when 16#B9# =>
            self.unimplemented(self.pc, inst);
         when 16#BA#=>
            self.unimplemented(self.pc, inst);
         when 16#BB# =>
            self.unimplemented(self.pc, inst);
         when 16#BC# =>
            self.unimplemented(self.pc, inst);
         when 16#BD# =>
            self.unimplemented(self.pc, inst);
         when 16#BE# =>
            self.unimplemented(self.pc, inst);
         when 16#BF# =>
            self.unimplemented(self.pc, inst);
         when 16#C0# =>
            self.unimplemented(self.pc, inst);
         when 16#C1# =>
            self.unimplemented(self.pc, inst);
         when 16#C2# =>
            self.unimplemented(self.pc, inst);
         when 16#C3# =>
            self.unimplemented(self.pc, inst);
         when 16#C4# =>
            self.unimplemented(self.pc, inst);
         when 16#C5# =>
            self.unimplemented(self.pc, inst);
         when 16#C6# =>
            self.unimplemented(self.pc, inst);
         when 16#C7# =>
            self.unimplemented(self.pc, inst);
         when 16#C8# =>
            self.unimplemented(self.pc, inst);
         when 16#C9# =>
            self.unimplemented(self.pc, inst);
         when 16#CA# =>
            self.unimplemented(self.pc, inst);
         when 16#CB# =>
            self.unimplemented(self.pc, inst);
         when 16#CC# =>
            self.unimplemented(self.pc, inst);
         when 16#CD# =>
            self.unimplemented(self.pc, inst);
         when 16#CE# =>
            self.unimplemented(self.pc, inst);
         when 16#CF# =>
            self.unimplemented(self.pc, inst);
         when 16#D0# =>
            self.unimplemented(self.pc, inst);
         when 16#D1# =>
            self.unimplemented(self.pc, inst);
         when 16#D2# =>
            self.unimplemented(self.pc, inst);
         when 16#D3# =>
            self.unimplemented(self.pc, inst);
         when 16#D4# =>
            self.unimplemented(self.pc, inst);
         when 16#D5# =>
            self.unimplemented(self.pc, inst);
         when 16#D6# =>
            self.unimplemented(self.pc, inst);
         when 16#D7# =>
            self.unimplemented(self.pc, inst);
         when 16#D8# =>
            self.unimplemented(self.pc, inst);
         when 16#D9# =>
            self.unimplemented(self.pc, inst);
         when 16#DA# =>
            self.unimplemented(self.pc, inst);
         when 16#DB# =>
            self.unimplemented(self.pc, inst);
         when 16#DC# =>
            self.unimplemented(self.pc, inst);
         when 16#DD# =>
            self.unimplemented(self.pc, inst);
         when 16#DE# =>
            self.unimplemented(self.pc, inst);
         when 16#DF# =>
            self.unimplemented(self.pc, inst);
         when 16#E0# =>
            self.unimplemented(self.pc, inst);
         when 16#E2# =>
            self.unimplemented(self.pc, inst);
         when 16#E1# =>
            self.unimplemented(self.pc, inst);
         when 16#E3# =>
            self.unimplemented(self.pc, inst);
         when 16#E4# =>
            self.unimplemented(self.pc, inst);
         when 16#E5# =>
            self.unimplemented(self.pc, inst);
         when 16#E6# =>
            self.unimplemented(self.pc, inst);
         when 16#E7# =>
            self.unimplemented(self.pc, inst);
         when 16#E8# =>
            self.unimplemented(self.pc, inst);
         when 16#E9# =>
            self.unimplemented(self.pc, inst);
         when 16#EA# =>
            self.unimplemented(self.pc, inst);
         when 16#EB# =>
            self.unimplemented(self.pc, inst);
         when 16#EC# =>
            self.unimplemented(self.pc, inst);
         when 16#ED# =>
            self.unimplemented(self.pc, inst);
         when 16#EE# =>
            self.unimplemented(self.pc, inst);
         when 16#EF# =>
            self.unimplemented(self.pc, inst);
         when 16#F0# =>
            self.unimplemented(self.pc, inst);
         when 16#F1# =>
            self.unimplemented(self.pc, inst);
         when 16#F2# =>
            self.unimplemented(self.pc, inst);
         when 16#F3# =>
            self.unimplemented(self.pc, inst);
         when 16#F4# =>
            self.unimplemented(self.pc, inst);
         when 16#F5# =>
            self.unimplemented(self.pc, inst);
         when 16#F6# =>
            self.unimplemented(self.pc, inst);
         when 16#F7# =>
            self.unimplemented(self.pc, inst);
         when 16#F8# =>
            self.unimplemented(self.pc, inst);
         when 16#F9# =>
            self.unimplemented(self.pc, inst);
         when 16#FA# =>
            self.unimplemented(self.pc, inst);
         when 16#FB# =>
            self.unimplemented(self.pc, inst);
         when 16#FC# =>
            self.unimplemented(self.pc, inst);
         when 16#FD# =>
            self.unimplemented(self.pc, inst);
         when 16#FE# =>
            self.unimplemented(self.pc, inst);
         when 16#FF# =>
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
   --  Set flags based on value (zero, sign, parity)
   --
   procedure setf(self : in out msc6502; value : byte) is
   begin
      self.f.zero := (value = 0);
      self.f.sign := ((value and 16#80#) = 16#80#);
   end;
   --
   --  Perform addition and set flags including carry and aux carry
   --
   function addf(self : in out msc6502; v1 : byte; v2 : byte; c : Boolean) return byte is
      sum  : word := word(v1) + word(v2);
      temp : byte;
   begin
      if c then
         sum := sum + 1;
      end if;
      self.f.carry := (sum > 16#FF#);
      temp := (v1 and 16#0F#) + (v2 and 16#0F#);
      if c then
         temp := temp + 1;
      end if;
      self.setf(byte(sum and 16#FF#));
      return byte(sum and 16#FF#);
   end;
   --
   --  Perform subtraction and set flags including carry and aux carry
   --
   function subf(self : in out msc6502; v1 : byte; v2 : byte; c : Boolean) return byte is
      diff : word := word(v1) - word(v2);
      temp : byte;
   begin
      if c then
         diff := diff - 1;
      end if;
      self.f.carry := (diff > 16#FF#);
      temp := (v1 and 16#0F#) - (v2 and 16#0F#);
      if c then
         temp := temp - 1;
      end if;
      self.setf(byte(diff and 16#FF#));
      return byte(diff and 16#FF#);
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
   --    0 - I/O space
   --    1 - Memory space (currently unimplemented)
   --
   overriding
   procedure attach_io(self : in out msc6502; io_dev : io_access;
                       base_addr : addr_bus; bus : bus_type) is
      size : addr_bus := io_dev.all.getSize;
      valid : Boolean := True;
   begin
      if bus = BUS_IO then
         --
         --  Check for port conflicts
         --
         for i in uint8(base_addr) .. uint8(base_addr + size - 1) loop
            if self.io_ports(i) /= null then
               valid := False;
               Ada.Text_IO.Put_Line("Port conflict detected attaching device to port " & toHex(i));
            end if;
            exit when not valid;
         end loop;
         if valid then
            for i in uint8(base_addr) .. uint8(base_addr + size - 1) loop
               self.io_ports(i) := io_dev;
               Ada.Text_IO.Put_Line("Attaching " & io_dev.name & " to I/O port " & toHex(i));
            end loop;
            io_dev.setBase(base_addr);
         end if;
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
      if self.io_ports(addr) /= null then
         self.io_ports(addr).all.write(addr_bus(addr), data_bus(value));
         if (word(self.trace) and 2) = 2 then
            Ada.Text_IO.Put_Line("Output " & toHex(value) & " to port " & toHex(addr));
         end if;
      else
         if (word(self.trace) and 2) = 2 then
            Ada.Text_IO.Put_Line("Output " & toHex(value) & " to unassigned port " & toHex(addr));
         end if;
      end if;
      self.last_out_addr := addr_bus(addr);
      self.last_out_data := data_bus(value);
   end;
   --
   function port(self : in out msc6502; addr : byte) return byte is
   begin
      if self.in_override and (addr_bus(addr) = self.in_over_addr) then
         self.in_override := False;
         return byte(self.in_over_data and 16#FF#);
      else
         if self.io_ports(addr) /= null then
            if (word(self.trace) and 2) = 2 then
               Ada.Text_IO.Put_Line("Input from port " & toHex(addr));
            end if;
            return byte(self.io_ports(addr).all.read(addr_bus(addr)) and 16#FF#);
         end if;
         if (word(self.trace) and 2) = 2 then
            Ada.Text_IO.Put_Line("Input from unassigned port " & toHex(addr));
         end if;
         return addr;
      end if;
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
         self.sp := self.sp - 1;
         self.memory(self.sp, byte(self.pc/16#100#), ADDR_DATA);
         self.sp := self.sp - 1;
         self.memory(self.sp, byte(self.pc and 16#FF#), ADDR_DATA);
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
         temp_pc := word(self.memory(self.sp, ADDR_DATA));
         self.sp := self.sp + 1;
         temp_pc := temp_pc + word(self.memory(self.sp, ADDR_DATA))*16#100#;
         self.sp := self.sp + 1;
         self.pc := temp_pc;
      end if;
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
