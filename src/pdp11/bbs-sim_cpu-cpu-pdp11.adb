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
with Ada.Exceptions;
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.CPU.pdp11.move;
--with BBS.Sim_CPU.CPU.pdp11.line_1;
--with BBS.Sim_CPU.CPU.pdp11.line_2;
--with BBS.Sim_CPU.CPU.pdp11.line_3;
--with BBS.Sim_CPU.CPU.pdp11.line_4;
--with BBS.Sim_CPU.CPU.pdp11.line_5;
--with BBS.Sim_CPU.CPU.pdp11.line_6;
--with BBS.Sim_CPU.CPU.pdp11.line_7;
--with BBS.Sim_CPU.CPU.pdp11.line_8;
--with BBS.Sim_CPU.CPU.pdp11.line_9;
--with BBS.Sim_CPU.CPU.pdp11.line_a;
--with BBS.Sim_CPU.CPU.pdp11.line_b;
--with BBS.Sim_CPU.CPU.pdp11.line_c;
--with BBS.Sim_CPU.CPU.pdp11.line_d;
--with BBS.Sim_CPU.CPU.pdp11.line_e;
--with BBS.Sim_CPU.CPU.pdp11.line_f;
with BBS.Sim_CPU.CPU.pdp11.exceptions;
package body BBS.Sim_CPU.CPU.pdp11 is
   --
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out pdp11) is
   begin
      self.r0 := 0;
      self.r1 := 0;
      self.r2 := 0;
      self.r3 := 0;
      self.r4 := 0;
      self.r5 := 0;
      self.usp := 0;
      self.ssp := 0;
      self.ksp := 0;
      self.pc := 0;
      self.psw.carry    := False;
      self.psw.overflow := False;
      self.psw.zero     := False;
      self.psw.negative := False;
      self.psw.trace    := False;
      self.psw.priority := 0;
      self.psw.unused0  := False;
      self.psw.unused1  := False;
      self.psw.unused2  := False;
      self.psw.reg_set  := false;
      self.psw.prev_mode := mode_kern;
      self.psw.curr_mode := mode_kern;
      self.cpu_halt := False;
   end;
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out pdp11) is
   begin
      self.pc := word(self.addr and 16#FFFF#);
      Ada.Text_IO.Put_Line("PC initialized to " & toHex(self.pc));
      self.cpu_halt := False;
   end;
   --
   --  Called to start simulator execution at a specific address.
   --
   overriding
   procedure start(self : in out pdp11; addr : addr_bus) is
   begin
      Ada.Text_IO.Put_Line("Starting with address " & toHex(addr));
      self.addr := addr;
      self.start;
   end;
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out pdp11) is
   begin
      if not self.halted then
         self.decode;
      end if;
   end;
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out pdp11) is
      sr_ad : ad_bus := self.bus.get_sr_ad;
      temp  : bus_stat;
   begin
      if self.bus.get_sr_ctrl.addr then
         self.addr := addr_bus(sr_ad);
      else
         self.bus.writep(self.addr, data_bus(sr_ad and 16#FF#), temp);
         self.addr := self.addr + 1;
      end if;
   end;
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out pdp11) is
   begin
      self.addr := self.addr + 1;
   end;
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out pdp11) return uint32 is
      pragma Unreferenced(self);
   begin
      return reg_id'Pos(reg_id'Last) + 1;
   end;
   --
   --  Called to get current variant index
   --
   overriding
   function variant(self : in out pdp11) return Natural is
   begin
    return variants_pdp11'pos(self.cpu_model);
   end;
   --
   --  Called to set variant
   --
   overriding
   procedure variant(self : in out pdp11; v : natural) is
   begin
      self.cpu_model := variants_pdp11'Val(v);
   end;
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out pdp11; mem_addr : addr_bus;
                     data : data_bus) is
      temp : bus_stat;
   begin
      self.bus.writep(mem_addr, data, temp);
   end;
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out pdp11; mem_addr : addr_bus) return
     data_bus is
      temp : bus_stat;
   begin
      return self.bus.readp(mem_addr, temp);
   end;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out pdp11; num : uint32)
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
   function read_reg(self : in out pdp11; num : uint32)
                     return data_bus is
      reg : reg_id;
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         reg := reg_id'Val(num);
         case reg is
            when reg_r0 =>
               return data_bus(self.r0);
            when reg_r1 =>
               return data_bus(self.r1);
            when reg_r2 =>
               return data_bus(self.r2);
            when reg_r3 =>
               return data_bus(self.r3);
            when reg_r4 =>
               return data_bus(self.r4);
            when reg_r5 =>
               return data_bus(self.r5);
            when reg_usp =>
               return data_bus(self.usp);
            when reg_ssp =>
               return data_bus(self.ssp);
            when reg_ksp =>
               return data_bus(self.ksp);
            when reg_pc =>
               return data_bus(self.pc);
            when reg_psw =>
               return data_bus(psw_to_word(self.psw));
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
   function read_reg(self : in out pdp11; num : uint32)
                     return String is
      reg : reg_id;
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         reg := reg_id'Val(num);
         case reg is
            when reg_r0 =>
               return toHex(self.r0);
            when reg_r1 =>
               return toHex(self.r1);
            when reg_r2 =>
               return toHex(self.r2);
            when reg_r3 =>
               return toHex(self.r3);
            when reg_r4 =>
               return toHex(self.r4);
            when reg_r5 =>
               return toHex(self.r5);
            when reg_usp =>
               return toHex(self.usp);
            when reg_ssp =>
               return toHex(self.ssp);
            when reg_ksp =>
               return toHex(self.ksp);
            when reg_pc =>
               return toHex(self.pc);
            when reg_psw =>
               return
                 cpu_mode'Image(self.psw.curr_mode) &
                 cpu_mode'Image(self.psw.prev_mode) &
               (if self.psw.reg_set then "R" else "-") &
               (if self.psw.unused2 then "*" else "+") &
               (if self.psw.unused1 then "*" else "+") &
               (if self.psw.unused0 then "*" else "+") &
                 uint3'Image(self.psw.priority) &
               (if self.psw.trace then "T" else "-") &
               (if self.psw.negative then "N" else "-") &
               (if self.psw.zero then "Z" else "-") &
               (if self.psw.overflow then "V" else "-") &
               (if self.psw.carry then "C" else "-");
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
   --
   overriding
   procedure load(self : in out pdp11; name : String) is
      inp   : Ada.Text_IO.File_Type;
      line  : Ada.Strings.Unbounded.Unbounded_String;
      count : byte;
      addr  : addr_bus;
      rec   : byte;
      data  : page;
      valid : Boolean;
      temp  : bus_stat;
   begin
      Ada.Text_IO.Open(inp, Ada.Text_IO.In_File, name);
      Ada.Text_IO.Put_Line("CPU: Loading S-Record file " & name);
      while not Ada.Text_IO.End_Of_File(inp) loop
         Ada.Text_IO.Unbounded_IO.Get_Line(inp, line);
         S_Record(Ada.Strings.Unbounded.To_String(line), count, addr, rec, data,
                  valid);
         if ((rec = 1) or (rec = 2) or (rec = 3)) and valid then  --  Process a data record
            for i in 0 .. count - 1 loop
               self.bus.writep(addr + addr_bus(i), data_bus(data(Integer(i))), temp);
            end loop;
         elsif (rec = 7) and valid then
            Ada.Text_IO.Put_Line("Starting address (32 bit) is " & toHex(addr));
            self.pc := word(addr and 16#FFFF#);
         elsif (rec = 8) and valid then
            Ada.Text_IO.Put_Line("Starting address (24 bit) is " & toHex(addr));
            self.pc := word(addr and 16#FFFF#);
         elsif (rec = 9) and valid then
            Ada.Text_IO.Put_Line("Starting address (16 bit) is " & toHex(addr));
            self.pc := word(addr and 16#FFFF#);
         elsif (rec = 0) and valid then
            Ada.Text_IO.Put("Header: ");
            for i in 0 .. count - 1 loop
               Ada.Text_IO.Put("" & Character'Val(data(Integer(i))));
            end loop;
            Ada.Text_IO.New_line;
         else
            Ada.Text_IO.Put_Line("Ignoring record: " & Ada.Strings.Unbounded.To_String(line));
         end if;
      end loop;
      Ada.Text_IO.Close(inp);
      self.cpu_halt := False;
   exception
      when Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line("Error in file name: " & name);
      when error : others =>
         Ada.Text_IO.Put_Line("Error occured processing " & name);
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(error));
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(error));
         Ada.Text_IO.Put_Line("Input line <" & Ada.Strings.Unbounded.To_String(line) & ">");
         Ada.Text_IO.Close(inp);
   end;
   --
   --  Called to check if the CPU is halted
   --
   overriding
   function halted(self : in out pdp11) return Boolean is
   begin
      return self.cpu_halt;
   end;
   --
   --  This clears the halted flag allowing processing to continue.
   --
   overriding
   procedure continue_proc(self : in out pdp11) is
   begin
      self.cpu_halt := False;
   end;
   --
   --  Interrupt status.  Returns simulator dependent status of interrupts
   --
   overriding
   function intStatus(self : in out pdp11) return int32 is
   begin
      return int32(self.psw.priority);
   end;
   --
   --  Post a reset exception request
   --
   overriding
   procedure reset(self : in out pdp11) is
   begin
      null;
      --
      --  PDP-11 doesn't have a reset vector.  Different processing will be needed.
      --
--      self.except_pend(BBS.Sim_CPU.CPU.pdp11.exceptions.ex_0_reset_ssp) := True;
--      self.check_except := True;
   end;
   --
   --  Enable/disable interrupt processing (ususally for debuggin purposes)
   --  Also clears pending interrupts if set to False.
   --
   overriding
   procedure interrupts(self : in out pdp11; state : Boolean) is
   begin
      self.int_enable := state;
      if state then
         Ada.Text_IO.Put_Line("CPU: Interrupt processing enabled.");
      else
         Ada.Text_IO.Put_Line("CPU: Interrupt processing disabled.");
         for i in 25 .. 31 loop
            self.except_pend(byte(i)) := False;
            self.except_prio(byte(i)) := 0;
         end loop;
         for i in 64 .. 255 loop
            self.except_pend(byte(i)) := False;
            self.except_prio(byte(i)) := 0;
         end loop;
      end if;
   end;
   --
   --  Post an interrupt exception
   --
   overriding
   procedure interrupt(self : in out pdp11; data : long) is
      inter : constant byte := byte(data and 16#FF#);
      prio  : constant byte := byte(data/16#100# and 16#FF#);
   begin
      --
      --  Allowed interrupt numbers are 25-31 for autovectors and 64-255.
      --  Other requests are ignored.  They could be turned into 15 for
      --  an uninitialied interrupt vector.
      --
      if self.int_enable then
         if (inter >= 25 and inter <= 31) or (inter >= 64 and inter <= 255) then
            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self, inter, prio);
         end if;
      end if;
   end;
   --
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out pdp11; addr : addr_bus) is
   begin
      self.break_enable := True;
      self.break_point  := word(addr and 16#FFFF#);
   end;
   --
   procedure clearBreak(self : in out pdp11; addr : addr_bus) is
   begin
      self.break_enable := False;
   end;
--  --------------------------------------------------------------------
--
--  Code for the instruction processing.
--
   procedure decode(self : in out pdp11) is
   begin
      --
      --  Check for odd PC value
      --
      if (self.pc and 1) = 1 then
         Ada.Text_IO.Put_Line("CPU:  PC set to odd address " & toHex(self.pc));
         Ada.Text_IO.Put_Line("   :  Previous PC is " & toHex(self.inst_pc));
         self.cpu_halt := True;
         return;
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
            return;
         end if;
      end if;
      self.inst_pc := self.pc;
      if (word(self.trace) and 1) = 1 then
         Ada.Text_IO.Put("TRACE: Address: " & toHex(self.pc));
      end if;
      instr := self.get_next;
      if (word(self.trace) and 1) = 1 then
         Ada.Text_IO.Put_Line(", instruction " & toHex(instr));
      end if;
      Ada.Text_IO.Put_Line("Processing instruction " & toOct(instr) & " " & toHex(instr));
      case instr1.pre is
         when 16#0# =>  --  Group 0
            null;
         when 16#1# =>  --  Group 1
            BBS.Sim_CPU.CPU.pdp11.move.MOV(self);
         when 16#2# =>  --  Group 2
            null;
--            BBS.Sim_CPU.CPU.pdp11.line_2.decode_2(self);
         when 16#3# =>  --  Group 3
            null;
--            BBS.Sim_CPU.CPU.pdp11.line_3.decode_3(self);
         when 16#4# =>  --  Group 4
            null;
--            BBS.Sim_CPU.CPU.pdp11.line_4.decode_4(self);
         when 16#5# =>  --  Group 5
            null;
--            BBS.Sim_CPU.CPU.pdp11.line_5.decode_5(self);
         when 16#6# =>  --  Group 6
            null;
--            BBS.Sim_CPU.CPU.pdp11.line_6.decode_6(self);
         when 16#7# =>  --  Group 7
            null;
--            BBS.Sim_CPU.CPU.pdp11.line_7.decode_7(self);
         when 16#8# =>  --  Group 8
            null;
--            BBS.Sim_CPU.CPU.pdp11.line_8.decode_8(self);
         when 16#9# =>  --  Group 9
            BBS.Sim_CPU.CPU.pdp11.move.MOVB(self);
         when 16#a# =>  --  Group 10
            null;
--            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
--                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_10_line_1010);
         when 16#b# =>  --  Group 11
            null;
--            BBS.Sim_CPU.CPU.pdp11.line_b.decode_b(self);
         when 16#c# =>  --  Group 12
            null;
--            BBS.Sim_CPU.CPU.pdp11.line_c.decode_c(self);
         when 16#d# =>  --  Group 13
            null;
--            BBS.Sim_CPU.CPU.pdp11.line_d.decode_d(self);
         when 16#e# =>  --  Group 14
            null;
--            BBS.Sim_CPU.CPU.pdp11.line_e.decode_e(self);
         when 16#f# =>  --  Group 15
            null;
--            BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
--                                                               BBS.Sim_CPU.CPU.pdp11.exceptions.ex_11_line_1111);
      end case;
      --
      --  Check for exceptions.  Note that trace exceptions will need to
      --  be added here.
      --
      if self.check_except then
         BBS.Sim_CPU.CPU.pdp11.exceptions.perform_exception(self);
      end if;
   end;
   --
   --  Utility code for instruction decoder
   --
   --  Get next instruction
   --
   function get_next(self : in out pdp11) return word is
      value  : word;
      temp : bus_stat;
   begin
      if lsb(self.pc) then
         Ada.Text_IO.Put_Line("CPU: Word read from odd address " & toHex(self.pc));
         Ada.Text_IO.Put_Line("   : Instruction " & toHex(instr) & " at " &
            toHex(self.inst_pc));
         BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
            BBS.Sim_CPU.CPU.pdp11.exceptions.ex_004_assorted);
      end if;
      if self.psw.curr_mode = mode_kern then
         value := self.bus.readl16l(addr_bus(self.pc), PROC_KERN, ADDR_INST, temp);
      elsif self.psw.curr_mode = mode_super then
         value := self.bus.readl16l(addr_bus(self.pc), PROC_SUP, ADDR_INST, temp);
      else
         value := self.bus.readl16l(addr_bus(self.pc), PROC_USER, ADDR_INST, temp);
      end if;
      if temp /= BUS_SUCC then
         BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                             BBS.Sim_CPU.CPU.pdp11.exceptions.ex_004_assorted);
      end if;
      self.pc := self.pc + 2;
      return value;
   end;
   --
   --  Get EA.  Decode the register and addressing mode to get the effective
   --  address.  Also does pre-decrement, as appropriate.
   --
   --  Mode  Addressing mode
   --    0   Register  direct
   --    1   Register indirect
   --    2   Register indirect with post increment
   --    3   Register indirect with post increment deferred
   --    4   Register indirect with pre decrement
   --    5   Register indirect with pre decrement deferred
   --    6   Indexed
   --    7   Indexed deferred
   --
   --  Note that there may be some limitations on which modes are allowed with R6
   --  (SP).  These are not yet implemented.
   --
   --  Note that modes 2, 3, 6, and 7 are the only modes valid for PC.
   --
   function get_EA(self : in out pdp11; reg : reg_num; mode : mode_code;
                   size : data_size) return operand is
      temp : word;
   begin
      case mode is
         when 0 =>  --  Register <Rx>
            if reg = 7 then  --  Not allowed for PC
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
            return (reg => reg, mode => mode, size => size, kind => register);
         when 1 =>  --  Register indirect <(Rx)>
            if reg = 7 then  --  Not allowed for PC
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
            return (reg => reg, mode => mode, size => size, kind => memory, address => self.get_regw(reg));
         when 2 =>  --  Register indirect with post increment <(Rx)+>
            return (reg => reg, mode => mode, size => size, kind => memory, address => self.get_regw(reg));
         when 3 =>  --  Register post incrememnt deferred <@(Rx)+>
            temp := self.get_regw(reg);
            temp := self.memory(addr_bus(temp));
            return (reg => reg, mode => mode, size => size, kind => memory, address => temp);
         when 4 =>  --  Register indirect with pre decrement <-(Rx)>
            if reg = 7 then  --  Not allowed for PC
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
            if size = data_byte then
               self.set_regw(reg, self.get_regw(reg) - 1);
            else
               self.set_regw(reg, self.get_regw(reg) - 2);
            end if;
            return (reg => reg, mode => mode, size => size, kind => memory, address => self.get_regw(reg));
         when 5 =>  --  Register indirect with pre decrement deferred <@-(Rx)>
            if reg = 7 then  --  Not allowed for PC
               BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                                  BBS.Sim_CPU.CPU.pdp11.exceptions.ex_010_res_inst);
            end if;
            self.set_regw(reg, self.get_regw(reg) - 2);
            temp := self.get_regw(reg);
            temp := self.memory(addr_bus(temp));
            return (reg => reg, mode => mode, size => size, kind => memory, address => temp);
         when 6 =>  --  Indexed <X(Rx)>
            temp := self.get_next;  --  Get extension word
            return (reg => reg, mode => mode, size => size, kind => memory, address =>
                      self.get_regw(reg) + temp);
         when 7 =>  --  Indexed deferred <@X(Rx)>
            temp := self.get_next;  --  Get extension word
            temp := self.get_regw(reg) + temp;
            temp := self.memory(addr_bus(temp));
            return (reg => reg, mode => mode, size => size, kind => memory, address => temp);
      end case;
   end;
   --
   --  Do post-processing, namely post-increment, if needed.
   --
   procedure post_EA(self : in out pdp11; ea : operand) is
   begin
      if ea.mode = 2 then  --  Register auto increment (Rn)+
         if ea.size = data_byte then
            self.set_regw(ea.reg, self.get_regw(ea.reg) + 1);
         else
            self.set_regw(ea.reg, self.get_regw(ea.reg) + 2);
         end if;
      elsif ea.mode = 3 then  --  Register auto increment deferred @(Rn)+
         self.set_regw(ea.reg, self.get_regw(ea.reg) + 2);
      end if;
   end;
   --
   --  Get and set value at the effective address.  Note that some effective
   --  addresses cannot be set.
   --
   function get_ea(self : in out pdp11; ea : operand) return word is
     b : byte;
     w : word;
   begin
      case ea.kind is
         when register =>
            w := self.get_regw(ea.reg);
         when memory =>
            if ea.size = data_byte then
               b := self.memory(addr_bus(ea.address));
               w := word(b);
            elsif ea.size = data_word then
               w := self.memory(addr_bus(ea.address));
            end if;
      end case;
      return w;
   end;
   --
   procedure set_ea(self : in out pdp11; ea : operand; val : word) is
   begin
      case ea.kind is
         when register =>
            if ea.size = data_byte then
               self.set_regb(ea.reg, byte(val and 16#FF#));
            elsif ea.size = data_word then
               self.set_regw(ea.reg, word(val and 16#FFFF#));
            end if;
         when memory =>
            Ada.Text_IO.Put_Line("Setting EA memory address " & toHex(ea.address));
            if ea.size = data_byte then
               self.memory(addr_bus(ea.address), byte(val and 16#FF#));
            elsif ea.size = data_word then
               self.memory(addr_bus(ea.address), word(val and 16#FFFF#));
            end if;
      end case;
   end;
   --
   --  Sign extension
   --
   function sign_extend(d : byte) return word is
   begin
      if (d and 16#80#) = 16#80# then
         return word(d) or 16#FF00#;
      else
         return word(d);
      end if;
   end;
   --
   --  Register opertions
   --
   function get_regb(self : in out pdp11; reg_index : reg_num) return byte is
   begin
      case reg_index is
         when 0 =>
            return byte(self.r0 and 16#ff#);
         when 1 =>
            return byte(self.r1 and 16#ff#);
         when 2 =>
            return byte(self.r2 and 16#ff#);
         when 3 =>
            return byte(self.r3 and 16#ff#);
         when 4 =>
            return byte(self.r4 and 16#ff#);
         when 5 =>
            return byte(self.r5 and 16#ff#);
         when 6 =>
            if self.psw.curr_mode = mode_kern then
               return byte(self.ksp and 16#ff#);
            elsif self.psw.curr_mode = mode_super then
               return byte(self.ssp and 16#ff#);
            else
               return byte(self.usp and 16#ff#);
            end if;
         when 7 =>
            return byte(self.pc and 16#ff#);
      end case;
   end;
   function get_regw(self : in out pdp11; reg_index : reg_num) return word is
   begin
      case reg_index is
         when 0 =>
            return self.r0;
         when 1 =>
            return self.r1;
         when 2 =>
            return self.r2;
         when 3 =>
            return self.r3;
         when 4 =>
            return self.r4;
         when 5 =>
            return self.r5;
         when 6 =>
            if self.psw.curr_mode = mode_kern then
               return self.ksp;
            elsif self.psw.curr_mode = mode_super then
               return self.ssp;
            else
               return self.usp;
            end if;
         when 7 =>
            return self.pc;
      end case;
   end;
   --
   procedure set_regb(self : in out pdp11; reg_index : reg_num; value : byte) is
      l : constant word := word(value);
   begin
      case reg_index is
         when 0 =>
            self.r0 := (self.r0 and 16#FF00#) or l;
         when 1 =>
            self.r1 := (self.r1 and 16#FF00#) or l;
         when 2 =>
            self.r2 := (self.r2 and 16#FF00#) or l;
         when 3 =>
            self.r3 := (self.r3 and 16#FF00#) or l;
         when 4 =>
            self.r4 := (self.r4 and 16#FF00#) or l;
         when 5 =>
            self.r5 := (self.r5 and 16#FF00#) or l;
         when 6 =>
            if self.psw.curr_mode = mode_kern then
               self.ksp := (self.ksp and 16#FF00#) or l;
            elsif self.psw.curr_mode = mode_super then
               self.ssp := (self.ksp and 16#FF00#) or l;
            else
               self.usp := (self.ksp and 16#FF00#) or l;
            end if;
         when 7 =>
            self.pc := (self.pc and 16#FF00#) or l;
      end case;
   end;

   procedure set_regw(self : in out pdp11; reg_index : reg_num; value : word) is
   begin
      case reg_index is
         when 0 =>
            self.r0 := value;
         when 1 =>
            self.r1 := value;
         when 2 =>
            self.r2 := value;
         when 3 =>
            self.r3 := value;
         when 4 =>
            self.r4 := value;
         when 5 =>
            self.r5 := value;
         when 6 =>
            if self.psw.curr_mode = mode_kern then
               self.ksp := value;
            elsif self.psw.curr_mode = mode_super then
               self.ssp := value;
            else
               self.usp := value;
            end if;
         when 7 =>
            self.pc := value;
      end case;
   end;
   --
   --  Set flags based on value (zero, sign, parity)
   --
   procedure setf(self : in out pdp11; value : data_bus) is
   begin
      self.psw.zero := (value = 0);
   end;
   --
   --  Set memory.
   --
   --
   procedure memory(self : in out pdp11; addr : addr_bus; value : word) is
      temp : bus_stat;
   begin
      --
      --  Set memory.  Optionally, checks for memory mapped I/O or shared memory
      --  or other special stuff can be added here.
      --
      if lsb(addr) then
         Ada.Text_IO.Put_Line("CPU: Word write to odd address " & toHex(addr));
         Ada.Text_IO.Put_Line("   : Instruction " & toHex(instr) & " at " &
            toHex(self.inst_pc));
         BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
            BBS.Sim_CPU.CPU.pdp11.exceptions.ex_004_assorted);
      end if;
      if self.psw.curr_mode = mode_kern then
         self.bus.writel16l(addr, value, PROC_KERN, ADDR_DATA, temp);
      elsif self.psw.curr_mode = mode_super then
         self.bus.writel16l(addr, value, PROC_SUP, ADDR_DATA, temp);
      else
         self.bus.writel16l(addr, value, PROC_USER, ADDR_DATA, temp);
      end if;
      if temp /= BUS_SUCC then
         BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                             BBS.Sim_CPU.CPU.pdp11.exceptions.ex_004_assorted);
      end if;
   end;
   --
   procedure memory(self : in out pdp11; addr : addr_bus; value : byte) is
      temp : bus_stat;
   begin
      if self.psw.curr_mode = mode_kern then
         self.bus.writel8l(addr, value, PROC_KERN, ADDR_DATA, temp);
      elsif self.psw.curr_mode = mode_super then
         self.bus.writel8l(addr, value, PROC_SUP, ADDR_DATA, temp);
      else
         self.bus.writel8l(addr, value, PROC_USER, ADDR_DATA, temp);
      end if;
      if temp /= BUS_SUCC then
         BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                             BBS.Sim_CPU.CPU.pdp11.exceptions.ex_004_assorted);
      end if;
   end;
   --
   --  Read memory.
   --
   function memory(self : in out pdp11; addr : addr_bus) return word is
      value  : word;
      temp : bus_stat;
   begin
      if lsb(addr) then
         Ada.Text_IO.Put_Line("CPU: Word read from odd address " & toHex(addr));
         Ada.Text_IO.Put_Line("   : Instruction " & toHex(instr) & " at " &
            toHex(self.inst_pc));
         BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
            BBS.Sim_CPU.CPU.pdp11.exceptions.ex_004_assorted);
      end if;
      if self.psw.curr_mode = mode_kern then
         value := self.bus.readl16l(addr, PROC_KERN, ADDR_DATA, temp);
      elsif self.psw.curr_mode = mode_super then
         value := self.bus.readl16l(addr, PROC_SUP, ADDR_DATA, temp);
      else
         value := self.bus.readl16l(addr, PROC_USER, ADDR_DATA, temp);
      end if;
      if temp /= BUS_SUCC then
         BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                             BBS.Sim_CPU.CPU.pdp11.exceptions.ex_004_assorted);
      end if;
      return value;
   end;
   --
   function memory(self : in out pdp11; addr : addr_bus) return byte is
      value  : byte;
      temp : bus_stat;
   begin
      if self.psw.curr_mode = mode_kern then
         value := self.bus.readl8l(addr, PROC_KERN, ADDR_DATA, temp);
      elsif self.psw.curr_mode = mode_super then
         value := self.bus.readl8l(addr, PROC_SUP, ADDR_DATA, temp);
      else
         value := self.bus.readl8l(addr, PROC_USER, ADDR_DATA, temp);
      end if;
      if temp /= BUS_SUCC then
         BBS.Sim_CPU.CPU.pdp11.exceptions.process_exception(self,
                                                             BBS.Sim_CPU.CPU.pdp11.exceptions.ex_004_assorted);
      end if;
      return value;
   end;
   --
end BBS.Sim_CPU.CPU.pdp11;
