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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
with BBS.Sim_CPU.CPU.i8080.z80;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package body BBS.Sim_CPU.CPU.i8080 is
   --
   function uint16_to_ctrl is new Ada.Unchecked_Conversion(source => uint16,
                                                           target => ctrl_mode);
   function psw_to_byte is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => byte);
   function byte_to_psw is new Ada.Unchecked_Conversion(source => byte,
                                                        target => status_word);
   function byte_to_op is new Ada.Unchecked_Conversion(source => byte,
                                                       target => opcode);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out i8080) is
   begin
      self.pc := self.addr;
      self.cpu_halt := False;
      self.lr_ctl.mode := PROC_KERN;
   end;
   --
   --  Called to start simulator execution at a specific address.
   --
   overriding
   procedure start(self : in out i8080; addr : addr_bus) is
   begin
      self.start;
      self.pc := word(addr and 16#FFFF#);
   end;
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out i8080) is
   begin
      if not self.halted then
         self.decode;
      end if;
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
   procedure examine(self : in out i8080) is
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
   procedure init(self : in out i8080) is
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
      self.f.carry     := False;
      if self.cpu_model /= var_z80 then
         self.f.addsub    := True;
      else
         self.f.addsub    := False;
      end if;
      self.f.parity    := False;
      self.f.aux_carry := False;
      self.f.zero      := False;
      self.f.sign      := False;
      self.f.unused1   := True;
      self.f.unused2   := True;
      self.ap   := 0;  --  Z-80 registers
      self.bp   := 0;
      self.cp   := 0;
      self.dp   := 0;
      self.ep   := 0;
      self.hp   := 0;
      self.lp   := 0;
      self.fp.carry     := False;
      self.fp.addsub    := False;
      self.fp.parity    := False;
      self.fp.aux_carry := False;
      self.fp.zero      := False;
      self.fp.sign      := False;
      self.ix   := 0;
      self.iy   := 0;
      self.i    := 0;
      self.r    := 0;
      self.ptr  := use_hl;
      --
      --  Clear any pending interrupts and initialize modes
      self.int_mode := 0;
      self.intr     := False;
      self.iff2      := False;
      self.int_enable := False;
      self.ie_pending := False;
      self.int_posted := 0;
   end;
   --
   --  Called to get number of registers
   --  The Z-80 variant has additional registers defined.
   --
   overriding
   function registers(self : in out i8080) return uint32 is
   begin
      if self.cpu_model = var_z80 then
         return reg_id'Pos(reg_id'Last) + 1;
      else
         return reg_id'Pos(reg_pc) + 1;
      end if;
   end;
   --
   --  Called to get current variant index
   --
   overriding
   function variant(self : in out i8080) return Natural is
   begin
    return variants_i8080'pos(self.cpu_model);
   end;
   --
   --  Called to set variant
   --
   overriding
   procedure variant(self : in out i8080; v : natural) is
   begin
      self.cpu_model := variants_i8080'Val(v);
   end;
   --
   --  Called to get variant name
   --
   overriding
   function variant(self : in out i8080; v : natural) return String is
   begin
      case v is
         when 0 =>
            return "i8080";
         when 1 =>
            return "i8085";
         when 2 =>
            return "Z-80";
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
   function reg_name(self : in out i8080; num : uint32)
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
   function read_reg(self : in out i8080; num : uint32)
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
            when reg_ap =>    --  Accumulator' (start of Z-80 only registers)
               return data_bus(self.ap);
            when reg_pswp =>  --  Status word'
               return data_bus(psw_to_byte(self.fp));
            when reg_bp =>     --  B' register (8 bits)
               return data_bus(self.bp);
            when reg_cp =>     --  C' register (8 bits)
               return data_bus(self.cp);
            when reg_bcp =>    --  B' & C' registers (16 bits)
               return data_bus(word(self.bp)*16#100# + word(self.cp));
            when reg_dp =>     --  D' register (8 bits)
               return data_bus(self.dp);
            when reg_ep =>     --  E' register (8 bits)
               return data_bus(self.ep);
            when reg_dep =>    --  D' & E' registers (16 bits)
               return data_bus(word(self.dp)*16#100# + word(self.ep));
            when reg_hp =>     --  H' register (8 bits)
               return data_bus(self.hp);
            when reg_lp =>     --  L' register (8 bits)
               return data_bus(self.lp);
            when reg_hlp =>    --  H' & L' register (16 bits)
               return data_bus(word(self.hp)*16#100# + word(self.lp));
            when reg_ix =>     --  Index register X (16 bits)
               return data_bus(self.ix);
            when reg_iy =>     --  Index register Y (16 bits)
               return data_bus(self.iy);
            when reg_i =>      --  Interrupt base register (8 bits)
               return data_bus(self.i);
            when reg_r =>      --  Memory refresh register (8 bits)
               return data_bus(self.r);
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
   function read_reg(self : in out i8080; num : uint32)
                     return String is
      reg : reg_id;
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         reg := reg_id'Val(num);
         case reg is
            when reg_a =>
               return toHex(self.a);
            when reg_psw =>
               if self.cpu_model = var_z80 then
                  return BBS.Sim_CPU.CPU.i8080.z80.flags(self.f) & "(" &
                        (if self.int_enable then "E" else "D") & ")";
               else
                  return (if self.f.sign then "S" else "-") &
                        (if self.f.zero then "Z" else "-") & "*" &
                        (if self.f.aux_carry then "A" else "-") & "*" &
                        (if self.f.parity then "P" else "-") & "*" &
                        (if self.f.carry then "C" else "-") & "(" &
                        (if self.int_enable then "E" else "D") & ")";
               end if;
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
            when reg_ap =>    --  Accumulator' (start of Z-80 only registers)
               return toHex(self.ap);
            when reg_pswp =>  --  Status word'
               return BBS.Sim_CPU.CPU.i8080.z80.flags(self.fp) & "(" &
                     (if self.int_enable then "E" else "D") & ")";
            when reg_bp =>     --  B' register (8 bits)
               return toHex(self.bp);
            when reg_cp =>     --  C' register (8 bits)
               return toHex(self.cp);
            when reg_bcp =>    --  B' & C' registers (16 bits)
               return toHex(word(self.bp)*16#100# + word(self.cp));
            when reg_dp =>     --  D' register (8 bits)
               return toHex(self.dp);
            when reg_ep =>     --  E' register (8 bits)
               return toHex(self.ep);
            when reg_dep =>    --  D' & E' registers (16 bits)
               return toHex(word(self.dp)*16#100# + word(self.ep));
            when reg_hp =>     --  H' register (8 bits)
               return toHex(self.hp);
            when reg_lp =>     --  L' register (8 bits)
               return toHex(self.lp);
            when reg_hlp =>    --  H' & L' register (16 bits)
               return toHex(word(self.hp)*16#100# + word(self.lp));
            when reg_ix =>     --  Index register X (16 bits)
               return toHex(self.ix);
            when reg_iy =>     --  Index register Y (16 bits)
               return toHex(self.iy);
            when reg_i =>      --  Interrupt base register (8 bits)
               return toHex(self.i);
            when reg_r =>      --  Memory refresh register (8 bits)
               return toHex(self.r);
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
   procedure load(self : in out i8080; name : String) is
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
   --  Force a simulator to enter a halt state (if implemented).  Can be used for
   --  some error conditions.
   --
   overriding
   procedure halt(self : in out i8080) is
   begin
      self.cpu_halt := True;
   end;
   --
   --  Called to check if the CPU is halted
   --
   overriding
   function halted(self : in out i8080) return Boolean is
   begin
      return self.cpu_halt;
   end;
   --
   --  This clears the halted flag allowing processing to continue.
   --
   overriding
   procedure continue_proc(self : in out i8080) is
   begin
      self.cpu_halt := False;
   end;
   --
   --  Interrupt status.  Returns simulator dependent status of interrupts
   --
   overriding
   function intStatus(self : in out i8080) return int32 is
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
   function lastOutAddr(self : in out i8080) return addr_bus is
   begin
      return self.last_out_addr;
   end;
   --
   overriding
   function lastOutData(self : in out i8080) return data_bus is
   begin
      return self.last_out_data;
   end;
   --
   overriding
   procedure overrideIn(self : in out i8080; addr : in addr_bus; data : in data_bus) is
   begin
      self.in_override  := True;
      self.in_over_addr := addr;
      self.in_over_data := data;
   end;
   --
   --  Post an interrupt exception.  The interrupt data is a single byte representing
   --  a one byte instruction.  Typically a RST # instruction.  Note that if multiple
   --  interrupts occur, only the last one will be recognized.
   --
   overriding
   procedure interrupt(self : in out i8080; data : long) is
   begin
      self.int_posted := data;
      self.intr := True;
   end;
   --
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out i8080; addr : addr_bus) is
   begin
      self.break_enable := True;
      self.break_point  := word(addr and 16#FFFF#);
   end;
   --
   procedure clearBreak(self : in out i8080; addr : addr_bus) is
   begin
      self.break_enable := False;
   end;
   --
   --  Unimplemented instruction response
   --
   --  Right now just print a message for unrecognized opcodes.
   --  At some point, may want to do something different here.
   --
   procedure unimplemented(self : in out i8080; addr : word; data : byte) is
   begin
      Ada.Text_IO.Put_Line("Illegal instruction at " & ToHex(addr) &
         " code " & ToHex(data));
   end;
--  --------------------------------------------------------------------
--
--  Code for the instruction processing.
--
   procedure decode(self : in out i8080) is
      inst    : byte;
      op_inst : opcode;
      temp_addr : word;
      temp16  : word;
      temp8   : byte;
      temppsw : status_word;
   begin
      --
      --  Interrupt check for Z80 NMI, mode 1, and mode 2.  8080/8085 and Z80 mode 0
      --  are handled by self.get_next.
      --
      if (self.cpu_model = var_z80) and self.intr and (not self.prefix) then
         --
         --  NMI processing is not masked.  Next instruction is from 16#0066#.
         --
         if self.int_posted = Z80_NMI then
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc and 16#FF#), ADDR_DATA);
            self.pc := 16#0066#;
            self.iff2 := self.int_enable;
            self.int_enable := False;
            self.ie_pending := False;  --  Override any pending enable of interrupts
            self.int_posted := 0;
            self.intr := False;
            return;
         end if;
         --
         --  In mode 1, all maskable interrupts go to location 16#0038#
         --
         if (self.int_mode = 1) and self.int_enable then
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc and 16#FF#), ADDR_DATA);
            self.pc := 16#0038#;
            self.iff2 := False;
            self.int_enable := False;
            self.ie_pending := False;  --  Override any pending enable of interrupts
            self.int_posted := 0;
            self.intr := False;
            return;
         end if;
         --
         --  Mode 2 uses a vector table with the high 8 bits from self.i.  The next
         --  7 bits come from the vector number.  The LSB is 0.
         --
         if (self.int_mode = 2) and self.int_enable then
            temp_addr := word(self.i)*16#0100# + word(self.int_posted and 16#00fe#);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc and 16#FF#), ADDR_DATA);
            self.pc := word(self.memory(temp_addr, ADDR_DATA)) +
              word(self.memory(temp_addr + 1, ADDR_DATA))*16#0100#;
            self.iff2 := False;
            self.int_enable := False;
            self.ie_pending := False;  --  Override any pending enable of interrupts
            self.int_posted := 0;
            self.intr := False;
            return;
         end if;
      end if;
      --
      --  Check for 8085 interrupt lines
      --
      if (self.cpu_model = var_8085) and self.intr then
         --
         --  Non-maskable TRAP input
         --
         if self.int_posted = i85_TRAP then
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc and 16#FF#), ADDR_DATA);
            self.pc := 16#0024#;
            self.int_enable := False;
            self.ie_pending := False;  --  Override any pending enable of interrupts
            self.int_posted := 0;
            self.intr := False;
            return;
         end if;
         --
         --  Check for RST inputs
         --
         if (self.int_posted = i85_7_5) and self.int_enable and not self.m7_5 then
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc and 16#FF#), ADDR_DATA);
            self.pc := 16#003c#;
            self.int_enable := False;
            self.ie_pending := False;  --  Override any pending enable of interrupts
            self.int_posted := 0;
            self.intr := False;
            return;
         end if;
         if (self.int_posted = i85_6_5) and self.int_enable and not self.m6_5 then
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc and 16#FF#), ADDR_DATA);
            self.pc := 16#0034#;
            self.int_enable := False;
            self.ie_pending := False;  --  Override any pending enable of interrupts
            self.int_posted := 0;
            self.intr := False;
            return;
         end if;
         if (self.int_posted = i85_5_5) and self.int_enable and not self.m5_5 then
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(self.pc and 16#FF#), ADDR_DATA);
            self.pc := 16#002c#;
            self.int_enable := False;
            self.ie_pending := False;  --  Override any pending enable of interrupts
            self.int_posted := 0;
            self.intr := False;
            return;
         end if;
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
      if (word(self.trace) and 1) = 1 then
         op_inst := byte_to_op(inst);
         Ada.Text_IO.Put_Line("TRACE: Address: " & toHex(self.pc - 1) & " instruction " &
                           toHex(inst) & " (" & opcode'Image(op_inst) & ")");
      end if;
      --
      --  Check to see if interrupts are to be enabled
      --
      if self.ie_pending then
         self.int_enable := True;
         self.ie_pending := False;
      end if;
      --
      --  Increment the R register (only used for Z-80)
      --
      self.r := (self.r and 16#80#) or (((self.r and 16#7F#) + 1) and 16#7F#);
      --
      --  Do instruction processing
      --
      case inst is
         when 0 =>  --  NOP (No operation)
            null;
         when 16#01# =>  --  LXI B (load BC register pair)
            self.c := self.get_next;
            self.b := self.get_next;
         when 16#02# =>  --  STAX B (Store accumulator at address)
            temp_addr := word(self.b)*16#100# + word(self.c);
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#03# =>  --  INX r (increment double BC)
            self.mod16(REG16_BC, 1);
         when 16#04#  =>  --  INR B (Increment register)
            self.b := self.mod8(self.b, 1);
            self.f.addsub := False;
         when 16#05#=>  --  DCR B (Decrement register)
            self.b := self.mod8(self.b, -1);
            self.f.addsub := True;
         when 16#06# =>  --  MVI B (Move immediate to register)
            self.b := self.get_next;
         when 16#07# =>  --  RLC (Rotate accumulator left)
            temp16 := word(self.a)*2;
            if temp16 > 16#FF# then
               self.f.carry := True;
               temp16 := temp16 + 1;
            else
               self.f.carry := False;
            end if;
            self.a := byte(temp16 and 16#FF#);
            if self.cpu_model = var_z80 then
               self.f.aux_carry := False;
               self.f.addsub    := False;
            end if;
         when 16#08# =>  --  Z80: EX AF,AF'
            if self.cpu_model = var_z80 then
               temp8 := self.a;
               temppsw := self.f;
               self.a := self.ap;
               self.f := self.fp;
               self.ap := temp8;
               self.fp := temppsw;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#09# =>  --  DAD B (double add)
            temp16 := self.dad(self.reg16(REG16_HL, True), self.reg16(REG16_BC, True));
            self.reg16(REG16_HL, temp16, True);
            self.f.addsub := False;
         when 16#0A# =>  --  LDAX B (Load accumulator from address)
            temp_addr := word(self.b)*16#100# + word(self.c);
            self.a := self.memory(temp_addr, ADDR_DATA);
         when 16#0B# =>  --  DCX B (decrement double)
            self.mod16(REG16_BC, -1);
         when 16#1B# =>  --  DCX D (decrement double)
            self.mod16(REG16_DE, -1);
         when 16#2B# =>  --  DCX H (decrement double)
            self.mod16(REG16_HL, -1);
         when 16#3B# =>  --  DCX SP (decrement double)
            self.mod16(REG16_SP, -1);
         when 16#0C# =>  --  INR C (Increment register)
            self.c := self.mod8(self.c, 1);
            self.f.addsub := False;
         when 16#0D# =>  --  DCR r (Decrement register)
            self.c := self.mod8(self.c, -1);
            self.f.addsub := True;
         when 16#0E# =>  --  MVI C (Move immediate to register)
            self.c := self.get_next;
         when 16#0F# =>  --  RRC (Rotate accumulator right)
            self.f.carry := ((self.a and 16#01#) = 1);
            self.a := self.a/2;
            if self.f.carry then
               self.a := self.a + 16#80#;
            end if;
            if self.cpu_model = var_z80 then
               self.f.aux_carry := False;
               self.f.addsub    := False;
            end if;
         when 16#10# =>  --  Z80 DJNZ
            if self.cpu_model = var_z80 then
               temp8 := self.get_next;
               self.b := self.b - 1;
               if self.b /= 0 then
                  self.pc := self.pc + sign_extend(temp8);
               end if;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#11# =>  --  LXI D (load DE register pair)
            self.e := self.get_next;
            self.d := self.get_next;
         when 16#12# =>  --  STAX D (Store accumulator at address)
            temp_addr := word(self.d)*16#100# + word(self.e);
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#13# =>  --  INX r (increment double DE)
            self.mod16(REG16_DE, 1);
         when 16#14# =>  --  INR D (Increment register)
            self.d := self.mod8(self.d, 1);
            self.f.addsub := False;
         when 16#15# =>  --  DCR D (Decrement register)
            self.d := self.mod8(self.d, -1);
            self.f.addsub := True;
         when 16#16# =>  --  MVI D (Move immediate to register)
            self.d := self.get_next;
         when 16#17# =>  --  RAL (Rotate left through carry)
            temp16 := word(self.a)*2;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            self.f.carry := (temp16 > 16#FF#);
            self.a := byte(temp16 and 16#FF#);
            if self.cpu_model = var_z80 then
               self.f.aux_carry := False;
               self.f.addsub    := False;
            end if;
         when 16#18# =>  --  Z80 JR offset
            if self.cpu_model = var_z80 then
               temp8 := self.get_next;
               self.pc := self.pc + sign_extend(temp8);
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#19# =>  --  DAD D (double add)
            temp16 := self.dad(self.reg16(REG16_HL, True), self.reg16(REG16_DE, True));
            self.reg16(REG16_HL, temp16, True);
            self.f.addsub := False;
         when 16#1A# =>  --  LDAX D (Load accumulator from address)
            temp_addr := word(self.d)*16#100# + word(self.e);
            self.a := self.memory(temp_addr, ADDR_DATA);
         when 16#1C# =>  --  INR E (Increment register)
            self.e := self.mod8(self.e, 1);
            self.f.addsub := False;
         when 16#1D# =>  --  DCR E (Decrement register)
            self.e := self.mod8(self.e, -1);
            self.f.addsub := True;
         when 16#1E# =>  --  MVI E (Move immediate to register)
            self.e := self.get_next;
         when 16#1F# =>  --  RAR (Rotate right through carry)
            temp16 := word(self.a);
            if self.f.carry then
               temp16 := temp16 + 16#100#;
            end if;
            self.f.carry := ((temp16 and 16#01#) = 1);
            self.a := byte(temp16/2);
            if self.cpu_model = var_z80 then
               self.f.aux_carry := False;
               self.f.addsub    := False;
            end if;
         when 16#20# =>  --  RIM (Read interrupt mask, 8085 only)
         --
         --  This will need to be updated should the serial input ever be
         --  implemented.
         --
         --  Note that for the Z80, this is a JR NZ,offset instruction
         --
            if self.cpu_model = var_8085 then
               self.a := 0;
               if self.intr then
                  if self.int_posted = i85_7_5 then
                     self.a := self.a or 16#40#;
                  end if;
                  if self.int_posted = i85_6_5 then
                     self.a := self.a or 16#20#;
                  end if;
                  if self.int_posted = i85_5_5 then
                     self.a := self.a or 16#10#;
                  end if;
               end if;
               if self.int_enable then
                  self.a := self.a or 16#08#;
               end if;
               if self.m7_5 then
                  self.a := self.a or 16#04#;
               end if;
               if self.m6_5 then
                  self.a := self.a or 16#02#;
               end if;
               if self.m5_5 then
                  self.a := self.a or 16#01#;
               end if;
            elsif self.cpu_model = var_z80 then
               temp8 := self.get_next;
               if not self.f.zero then
                  self.pc := self.pc + sign_extend(temp8);
               end if;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#21# =>  --  LXI H (Load HL register pair)
            temp16 := word(self.get_next);
            temp16 := temp16 + word(self.get_next)*16#100#;
            self.reg16(REG16_HL, temp16, True);
         when 16#22# =>  --  SHLD addr (Store HL direct)
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.memory(temp_addr, self.l, ADDR_DATA);
            temp_addr := temp_addr + 1;
            self.memory(temp_addr, self.h, ADDR_DATA);
         when 16#23# =>  --  INX r (increment double HL)
            self.mod16(REG16_HL, 1);
         when 16#24# =>  --  INR H (Increment register)
            self.mod8(REG8_H, 1);
            self.f.addsub := False;
         when 16#25# =>  --  DCR H (Decrement register)
            self.mod8(REG8_H, -1);
            self.f.addsub := True;
         when 16#26# =>  --  MVI H (Move immediate to register)
            self.h := self.get_next;
         when 16#27# =>  --  DAA (Decimal adjust accumulator)
            if self.cpu_model = var_z80 then
               self.a := BBS.Sim_CPU.CPU.i8080.z80.daa(self.a, self.f);
            else
               temp8 := self.a;
               if ((temp8 and 16#0F#) > 6) or self.f.aux_carry then
                  self.f.aux_carry := (((temp8 and 16#0F#) + 6) > 16#0F#);
                  temp8 := temp8 + 6;
               end if;
               if ((temp8/16#10# and 16#0F#) > 6) or self.f.carry then
                  self.f.carry := (((temp8/16#10# and 16#0F#) + 6) > 16#0F#);
                  temp8 := temp8 + 16#60#;
               end if;
               self.a := temp8;
            end if;
         when 16#28# =>  --  Z80 JR Z,offset
            if self.cpu_model = var_z80 then
               temp8 := self.get_next;
               if self.f.zero then
                  self.pc := self.pc + sign_extend(temp8);
               end if;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#29# =>  --  DAD H (double add)
            temp16 := self.dad(self.reg16(REG16_HL, True), self.reg16(REG16_HL, True));
            self.reg16(REG16_HL, temp16, True);
            self.f.addsub := False;
         when 16#2A# =>  --  LHLD addr (Load HL direct)
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.l := self.memory(temp_addr, ADDR_DATA);
            temp_addr := temp_addr + 1;
            self.h := self.memory(temp_addr, ADDR_DATA);
         when 16#2C# =>  --  INR L (Increment register)
            self.mod8(REG8_L, 1);
            self.f.addsub := False;
         when 16#2D# =>  --  DCR L (Decrement register)
            self.mod8(REG8_L, -1);
            self.f.addsub := True;
         when 16#2E# =>  --  MVI L (Move immediate to register)
            self.l := self.get_next;
         when 16#2F# =>  --  CMA (Complement accumulator)
            self.a := not self.a;
            self.f.addsub := True;
         when 16#30# =>  --  SIM (Set interrupt mask, 8085 only)
         --
         --  This will need to be updated should the serial input ever be
         --  implemented.
         --
         --  Note that for the Z80, this is a JR NC,offset instruction
         --
            if self.cpu_model = var_8085 then
               if (self.a and 16#08#) /= 0 then
                  self.m7_5 := (self.a and 16#04#) /= 0;
                  self.m6_5 := (self.a and 16#02#) /= 0;
                  self.m5_5 := (self.a and 16#01#) /= 0;
               end if;
               if (self.a and 16#10#) /= 0 then
                  if self.intr and (self.int_posted = i85_7_5) then
                     Ada.Text_IO.Put_Line("SIM: Clearing posted 7.5 interrupt");
                     self.intr := False;
                     self.int_posted := 0;
                  end if;
               end if;
            elsif self.cpu_model = var_z80 then
               temp8 := self.get_next;
               if not self.f.carry then
                  self.pc := self.pc + sign_extend(temp8);
               end if;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#31# =>  --  LXI SP (Load register pair)
            temp16 := word(self.get_next);
            temp16 := temp16 + word(self.get_next)*16#100#;
            self.reg16(REG16_SP, temp16, True);
         when 16#32# =>  --  STA addr (Store accumulator)
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#33# =>  --  INX r (increment double  SP)
            self.mod16(REG16_SP, 1);
         when 16#34# =>  --  INR M (Increment register)
            self.mod8(REG8_M, 1);
            self.f.addsub := False;
         when 16#35# =>  --  DCR M (Decrement register)
            self.mod8(REG8_M, -1);
            self.f.addsub := True;
         when 16#36# =>  --  MVI M (Move immediate to memory)
            temp8 := self.get_next;
            self.reg8(REG8_M, temp8, False);
         when 16#37# =>  --  STC (Set carry)
            self.f.carry := True;
            self.f.addsub := False;
         when 16#38# =>  --  Z80 JR C,offset
            if self.cpu_model = var_z80 then
               temp8 := self.get_next;
               if self.f.carry then
                  self.pc := self.pc + sign_extend(temp8);
               end if;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#39# =>  --  DAD SP (double add)
            temp16 := self.dad(self.reg16(REG16_HL, True), self.sp);
            self.reg16(REG16_HL, temp16, True);
            self.f.addsub := False;
         when 16#3A# =>  --  LDA addr (Load accumulator)
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.a := self.memory(temp_addr, ADDR_DATA);
         when 16#3C# =>  --  INR A (Increment register)
            self.a := self.mod8(self.a, 1);
            self.f.addsub := False;
         when 16#3D# =>  --  DCR A (Decrement register)
            self.a := self.mod8(self.a, -1);
            self.f.addsub := True;
         when 16#3E# =>  --  MVI A (Move immediate to register)
            self.a := self.get_next;
         when 16#3F# =>  --  CMC (Complement carry)
            self.f.carry := not self.f.carry;
            self.f.addsub := False;
         --
         --  Note that 63 of the 256 instruction codes are occupied by various MOV
         --  instructions.  The MOV M,M instruction is illegal and that code is used
         --  for the HLT instruction.
         --
         when 16#40# =>  --  MOV B,B
            null;  --  No operation
         when 16#41# =>  --  MOV B,C
            self.b := self.c;
         when 16#42# =>  --  MOV B,D
            self.b := self.d;
         when 16#43# =>  --  MOV B,E
            self.b := self.e;
         when 16#44# =>  --  MOV B,H
            self.b := self.h;
         when 16#45# =>  --  MOV B,L
            self.b := self.l;
         when 16#46# =>  --  MOV B,M
            self.b := self.reg8(REG8_M, True);
         when 16#47# =>  --  MOV B,A
            self.b := self.a;
         when 16#48# =>  --  MOV C,B
            self.c := self.b;
         when 16#49# =>  --  MOV C,C
            null;  --  No operation
         when 16#4a# =>  --  MOV C,D
            self.c := self.d;
         when 16#4b# =>  --  MOV C,E
            self.c := self.e;
         when 16#4c# =>  --  MOV C,H
            self.c := self.h;
         when 16#4d# =>  --  MOV C,L
            self.c := self.l;
         when 16#4e# =>  --  MOV C,M
            self.c := self.reg8(REG8_M, True);
         when 16#4f# =>  --  MOV C,A
            self.c := self.a;
         when 16#50# =>  --  MOV D,B
            self.d := self.b;
         when 16#51# =>  --  MOV D,C
            self.d := self.c;
         when 16#52# =>  --  MOV D,D
            null;  --  No operation
         when 16#53# =>  --  MOV D,E
            self.d := self.e;
         when 16#54# =>  --  MOV D,H
            self.d := self.h;
         when 16#55# =>  --  MOV D,L
            self.d := self.l;
         when 16#56# =>  --  MOV D,M
            self.d := self.reg8(REG8_M, True);
         when 16#57# =>  --  MOV D,A
            self.d := self.a;
         when 16#58# =>  --  MOV E,B
            self.e := self.b;
         when 16#59# =>  --  MOV E,C
            self.e := self.c;
         when 16#5a# =>  --  MOV E,D
            self.e := self.d;
         when 16#5b# =>  --  MOV E,E
            null;  --  No operation
         when 16#5c# =>  --  MOV E,H
            self.e := self.h;
         when 16#5d# =>  --  MOV E,L
            self.e := self.l;
         when 16#5e# =>  --  MOV E,M
            self.e := self.reg8(REG8_M, True);
         when 16#5f# =>  --  MOV E,A
            self.e := self.a;
         when 16#60# =>  --  MOV H,B
            self.h := self.b;
         when 16#61# =>  --  MOV H,C
            self.h := self.c;
         when 16#62# =>  --  MOV H,D
            self.h := self.d;
         when 16#63# =>  --  MOV H,E
            self.h := self.e;
         when 16#64# =>  --  MOV H,H
            null;  --  No operation
         when 16#65# =>  --  MOV H,L
            self.h := self.l;
         when 16#66# =>  --  MOV H,M
            self.h := self.reg8(REG8_M, True);
         when 16#67# =>  --  MOV H,A
            self.h := self.a;
         when 16#68# =>  --  MOV L,B
            self.l := self.b;
         when 16#69# =>  --  MOV L,C
            self.l := self.c;
         when 16#6a# =>  --  MOV L,D
            self.l := self.d;
         when 16#6b# =>  --  MOV L,E
            self.l := self.e;
         when 16#6c# =>  --  MOV L,H
            self.l := self.h;
         when 16#6d# =>  --  MOV L,L
            null;  --  No operation
         when 16#6e# =>  --  MOV L,M
            self.l := self.reg8(REG8_M, True);
         when 16#6f# =>  --  MOV L,A
            self.l := self.a;
         when 16#70# =>  --  MOV M,B
            self.reg8(REG8_M, self.b, True);
         when 16#71# =>  --  MOV M,C
            self.reg8(REG8_M, self.c, True);
         when 16#72# =>  --  MOV M,D
            self.reg8(REG8_M, self.d, True);
         when 16#73# =>  --  MOV M,E
            self.reg8(REG8_M, self.e, True);
         when 16#74# =>  --  MOV M,H
            self.reg8(REG8_M, self.h, True);
         when 16#75# =>  --  MOV M,L
            self.reg8(REG8_M, self.l, True);
         when 16#76# =>  --  HLT (halt, this would be code for MOV M,M)
            self.cpu_halt := True;
         when 16#77# =>  --  MOV M,A
            self.reg8(REG8_M, self.a, True);
         when 16#78# =>  --  MOV A,B
            self.a := self.b;
         when 16#79# =>  --  MOV A,C
            self.a := self.c;
         when 16#7a# =>  --  MOV A,D
            self.a := self.d;
         when 16#7b# =>  --  MOV A,E
            self.a := self.e;
         when 16#7c# =>  --  MOV A,H
            self.a := self.h;
         when 16#7d# =>  --  MOV A,L
            self.a := self.l;
         when 16#7e# =>  --  MOV A,M
            self.a := self.reg8(REG8_M, True);
         when 16#7f# =>  --  MOV A,A
            null;  --  No operation
         when 16#80# =>  -- ADD B (ADD register to accumulator)
            self.a := self.addf(self.a, self.b, False);
            self.f.addsub := False;
         when 16#81# =>  -- ADD C (ADD register to accumulator)
            self.a := self.addf(self.a, self.c, False);
            self.f.addsub := False;
         when 16#82# =>  -- ADD D (ADD register to accumulator)
            self.a := self.addf(self.a, self.d, False);
            self.f.addsub := False;
         when 16#83# =>  -- ADD E (ADD register to accumulator)
            self.a := self.addf(self.a, self.e, False);
            self.f.addsub := False;
         when 16#84# =>  -- ADD H (ADD register to accumulator)
            self.a := self.addf(self.a, self.reg8(REG8_H, False), False);
            self.f.addsub := False;
         when 16#85# =>  -- ADD L (ADD register to accumulator)
            self.a := self.addf(self.a, self.reg8(REG8_L, False), False);
            self.f.addsub := False;
         when 16#86# =>  -- ADD M (ADD register to accumulator)
            self.a := self.addf(self.a, self.reg8(REG8_M, False), False);
            self.f.addsub := False;
         when 16#87# =>  -- ADD A (ADD register to accumulator)
            self.a := self.addf(self.a, self.a, False);
            self.f.addsub := False;
         when 16#88# =>  -- ADC B (ADD register to accumulator with carry)
            self.a := self.addf(self.a, self.b, self.f.carry);
            self.f.addsub := False;
         when 16#89# =>  -- ADC C (ADD register to accumulator with carry)
            self.a := self.addf(self.a, self.c, self.f.carry);
            self.f.addsub := False;
         when 16#8A# =>  -- ADC D (ADD register to accumulator with carry)
            self.a := self.addf(self.a, self.d, self.f.carry);
            self.f.addsub := False;
         when 16#8B# =>  -- ADC E (ADD register to accumulator with carry)
            self.a := self.addf(self.a, self.e, self.f.carry);
            self.f.addsub := False;
         when 16#8C# =>  -- ADC H (ADD register to accumulator with carry)
            self.a := self.addf(self.a, self.reg8(REG8_H, False), self.f.carry);
            self.f.addsub := False;
         when 16#8D# =>  -- ADC L (ADD register to accumulator with carry)
            self.a := self.addf(self.a, self.reg8(REG8_L, False), self.f.carry);
            self.f.addsub := False;
         when 16#8E# =>  -- ADC M (ADD register to accumulator with carry)
            self.a := self.addf(self.a, self.reg8(REG8_M, False), self.f.carry);
            self.f.addsub := False;
         when 16#8F# =>  -- ADC r (ADD register to accumulator with carry)
            self.a := self.addf(self.a, self.a, self.f.carry);
            self.f.addsub := False;
         when 16#90# =>  -- SUB B (SUB register from accumulator)
            self.a := self.subf(self.a, self.b, False);
            self.f.addsub := True;
         when 16#91# =>  -- SUB C (SUB register from accumulator)
            self.a := self.subf(self.a, self.c, False);
            self.f.addsub := True;
         when 16#92# =>  -- SUB D (SUB register from accumulator)
            self.a := self.subf(self.a, self.d, False);
            self.f.addsub := True;
         when 16#93# =>  -- SUB E (SUB register from accumulator)
            self.a := self.subf(self.a, self.e, False);
            self.f.addsub := True;
         when 16#94# =>  -- SUB H (SUB register from accumulator)
            self.a := self.subf(self.a, self.reg8(REG8_H, False), False);
            self.f.addsub := True;
         when 16#95# =>  -- SUB L (SUB register from accumulator)
            self.a := self.subf(self.a, self.reg8(REG8_L, False), False);
            self.f.addsub := True;
         when 16#96# =>  -- SUB M (SUB register from accumulator)
            self.a := self.subf(self.a, self.reg8(REG8_M, False), False);
            self.f.addsub := True;
         when 16#97# =>  -- SUB A (SUB register from accumulator)
            self.a := self.subf(self.a, self.a, False);
            self.f.addsub := True;
         when 16#98# =>  -- SBB B (SUB register from accumulator with borrow)
            self.a := self.subf(self.a, self.b, self.f.carry);
            self.f.addsub := True;
         when 16#99# =>  -- SBB C (SUB register from accumulator with borrow)
            self.a := self.subf(self.a, self.c, self.f.carry);
            self.f.addsub := True;
         when 16#9A# =>  -- SBB D (SUB register from accumulator with borrow)
            self.a := self.subf(self.a, self.D, self.f.carry);
            self.f.addsub := True;
         when 16#9B# =>  -- SBB E (SUB register from accumulator with borrow)
            self.a := self.subf(self.a, self.e, self.f.carry);
            self.f.addsub := True;
         when 16#9C# =>  -- SBB H (SUB register from accumulator with borrow)
            self.a := self.subf(self.a, self.reg8(REG8_H, False), self.f.carry);
            self.f.addsub := True;
         when 16#9D# =>  -- SBB L (SUB register from accumulator with borrow)
            self.a := self.subf(self.a, self.reg8(REG8_L, False), self.f.carry);
            self.f.addsub := True;
         when 16#9E# =>  -- SBB M (SUB register from accumulator with borrow)
            self.a := self.subf(self.a, self.reg8(REG8_M, False), self.f.carry);
            self.f.addsub := True;
         when 16#9F# =>  -- SBB A (SUB register from accumulator with borrow)
            self.a := self.subf(self.a, self.a, self.f.carry);
            self.f.addsub := True;
         when 16#A0# =>  -- ANA B (AND accumulator with register)
            self.a := self.a and self.b;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#A1# =>  -- ANA C (AND accumulator with register)
            self.a := self.a and self.c;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#A2# =>  -- ANA D (AND accumulator with register)
            self.a := self.a and self.d;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#A3# =>  -- ANA E (AND accumulator with register)
            self.a := self.a and self.e;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#A4# =>  -- ANA H (AND accumulator with register)
            self.a := self.a and self.reg8(REG8_H, False);
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#A5# =>  -- ANA L (AND accumulator with register)
            self.a := self.a and self.reg8(REG8_L, False);
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#A6# =>  -- ANA M (AND accumulator with register)
            self.a := self.a and self.reg8(REG8_M, False);
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#A7# =>  -- ANA r (AND accumulator with register)
--            self.a := self.a and self.a;  --  A and A is equal to A.
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#A8# =>  -- XRA B (XOR accumulator with register)
            self.a := self.a xor self.b;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#A9# =>  -- XRA C (XOR accumulator with register)
            self.a := self.a xor self.c;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#AA# =>  -- XRA D (XOR accumulator with register)
            self.a := self.a xor self.d;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#AB# =>  -- XRA E (XOR accumulator with register)
            self.a := self.a xor self.e;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#AC# =>  -- XRA H (XOR accumulator with register)
            self.a := self.a xor self.reg8(REG8_H, False);
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#AD# =>  -- XRA L (XOR accumulator with register)
            self.a := self.a xor self.reg8(REG8_L, False);
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#AE# =>  -- XRA M (XOR accumulator with register)
            self.a := self.a xor self.reg8(REG8_M, False);
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#AF# =>  -- XRA A (XOR accumulator with register)
            self.a := 0;  --  A xor A is zero.
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#B0# =>  -- ORA B (OR accumulator with register)
            self.a := self.a or self.b;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#B1# =>  -- ORA C (OR accumulator with register)
            self.a := self.a or self.c;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#B2# =>  -- ORA D (OR accumulator with register)
            self.a := self.a or self.d;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#B3# =>  -- ORA E (OR accumulator with register)
            self.a := self.a or self.e;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#B4# =>  -- ORA H (OR accumulator with register)
            self.a := self.a or self.reg8(REG8_H, False);
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#B5# =>  -- ORA L (OR accumulator with register)
            self.a := self.a or self.reg8(REG8_L, False);
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#B6# =>  -- ORA M (OR accumulator with register)
            self.a := self.a or self.reg8(REG8_M, False);
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#B7# =>  -- ORA r (OR accumulator with register)
--            self.a := self.a or self.a;  --  A or A is equal to A
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         --  For CMP we are only intersted in flags.  Ignore the actual result.
         when 16#B8# =>  -- CMP B (CMP register with accumulator)
            temp8 := self.subf(self.a, self.b, False);
            self.f.addsub := True;
         when 16#B9# =>  -- CMP C (CMP register with accumulator)
            temp8 := self.subf(self.a, self.c, False);
            self.f.addsub := True;
         when 16#BA#=>  -- CMP D (CMP register with accumulator)
            temp8 := self.subf(self.a, self.d, False);
            self.f.addsub := True;
         when 16#BB# =>  -- CMP E (CMP register with accumulator)
            temp8 := self.subf(self.a, self.e, False);
            self.f.addsub := True;
         when 16#BC# =>  -- CMP H (CMP register with accumulator)
            temp8 := self.subf(self.a, self.reg8(REG8_H, False), False);
            self.f.addsub := True;
         when 16#BD# =>  -- CMP L (CMP register with accumulator)
            temp8 := self.subf(self.a, self.reg8(REG8_L, False), False);
            self.f.addsub := True;
         when 16#BE# =>  -- CMP M (CMP register with accumulator)
            temp8 := self.subf(self.a, self.reg8(REG8_M, False), False);
            self.f.addsub := True;
         when 16#BF# =>  -- CMP A (CMP register with accumulator)
            temp8 := self.subf(self.a, self.a, False);  --  This could be impoved for just setting the flags for equals
            self.f.addsub := True;
         when 16#C0# =>  --  RNZ (Return if not zero)
            self.ret(not self.f.zero);
         when 16#C1# =>  --  POP B (Pop from stack)
            self.c := self.memory(self.sp, ADDR_DATA);
            self.sp := self.sp + 1;
            self.b := self.memory(self.sp, ADDR_DATA);
            self.sp := self.sp + 1;
         when 16#C2# =>  -- JNZ (Jump if not zero)
            self.jump(not self.f.zero);
         when 16#C3# =>  --  JMP (Jump unconditional)
            self.jump(true);
         when 16#C4# =>  --  CNZ (Call if not zero)
            self.call(not self.f.zero);
         when 16#C5# =>  --  PUSH B (Push to stack)
            self.sp := self.sp - 1;
            self.memory(self.sp, self.b, ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, self.c, ADDR_DATA);
         when 16#C6# =>  --  ADI (ADD immediate with accumulator)
            self.a := self.addf(self.a, self.get_next, False);
            self.f.addsub := False;
         when 16#C7# =>  --  RST 0 (Restart)
            temp16 := self.pc;
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
            self.pc := 0;
         when 16#C8# =>  --  RZ (Return if zero)
            self.ret(self.f.zero);
         when 16#C9# =>  --  RET (Return unconditional)
            self.ret(True);
         when 16#CA# =>  -- JZ (Jump if zero)
            self.jump(self.f.zero);
         when 16#CB# =>  --  Z80 CB instruction prefix
            if self.cpu_model = var_z80 then
               BBS.Sim_CPU.CPU.i8080.z80.prefix_cb(self);
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#CC# =>  --  CZ (Call if zero)
            self.call(self.f.zero);
         when 16#CD# =>  --  CALL (Call unconditional)
            self.call(True);
         when 16#CE# =>  --  ACI (ADD immediate with accumulator and carry)
            self.a := self.addf(self.a, self.get_next, self.f.carry);
            self.f.addsub := False;
         when 16#CF# =>  --  RST 1 (Restart)
            temp16 := self.pc;
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
            self.pc := 16#08#;
         when 16#D0# =>  --  RNC (Return if not carry)
            self.ret(not self.f.carry);
         when 16#D1# =>  --  POP D (Pop from stack)
            self.e := self.memory(self.sp, ADDR_DATA);
            self.sp := self.sp + 1;
            self.d := self.memory(self.sp, ADDR_DATA);
            self.sp := self.sp + 1;
         when 16#D2# =>  --  JNC (Jump if not carry)
            self.jump(not self.f.carry);
         when 16#D3# =>  --  OUT (Output to port)
            temp8 := self.get_next;
            self.port(temp8, self.a);
         when 16#D4# =>  --  CNC (Call if not carry)
            self.call(not self.f.carry);
         when 16#D5# =>  --  PUSH D (Push to stack)
            self.sp := self.sp - 1;
            self.memory(self.sp, self.d, ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, self.e, ADDR_DATA);
         when 16#D6# =>  --  SUI (Subtract immediate)
            self.a := self.subf(self.a, self.get_next, False);
            self.f.addsub := True;
         when 16#D7# =>  --  RST 2 (Restart)
            temp16 := self.pc;
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
            self.pc := 16#10#;
         when 16#D8# =>  --  RC (Return if carry)
            self.ret(self.f.carry);
         when 16#D9# =>  --  Z80 EXX
            if self.cpu_model = var_z80 then
               temp8 := self.b;
               self.b := self.bp;
               self.bp := temp8;
               --
               temp8 := self.c;
               self.c := self.cp;
               self.cp := temp8;
               --
               temp8 := self.d;
               self.d := self.dp;
               self.dp := temp8;
               --
               temp8 := self.e;
               self.e := self.ep;
               self.ep := temp8;
               --
               temp8 := self.h;
               self.h := self.hp;
               self.hp := temp8;
               --
               temp8 := self.l;
               self.l := self.lp;
               self.lp := temp8;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#DA# =>  --  JC (Jump if carry)
            self.jump(self.f.carry);
         when 16#DB# =>  --  IN (Input from port)
            temp8 := self.get_next;
            self.a := self.port(temp8);
         when 16#DC# =>  --  CC (Call if carry)
            self.call(self.f.carry);
         when 16#DD# =>  --  Z80 DD instruction prefix
            if self.cpu_model = var_z80 then
               self.ptr := use_ix;
               self.prefix := True;
               return;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#DE# =>  --  SUI (Subtract immediate)
            self.a := self.subf(self.a, self.get_next, self.f.carry);
            self.f.addsub := True;
         when 16#DF# =>  --  RST 3 (Restart)
            temp16 := self.pc;
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
            self.pc := 16#18#;
         when 16#E0# =>  --  RPO (Return if parity odd (parity flag false))
            self.ret(not self.f.parity);
         when 16#E2# =>  --  JPO (Jump if parity odd (parity flag false))
            self.jump(not self.f.parity);
         when 16#E1# =>  --  POP H (Pop from stack)
            temp16 := word(self.memory(self.sp, ADDR_DATA));
            self.sp := self.sp + 1;
            temp16 := temp16 + word(self.memory(self.sp, ADDR_DATA))*16#100#;
            self.sp := self.sp + 1;
            self.reg16(REG16_HL, temp16, False);
         when 16#E3# =>  --  XTHL (Exchange HL with top of stack)
            temp8 := self.memory(self.sp, ADDR_DATA);
            self.memory(self.sp, self.l, ADDR_DATA);
            self.l := temp8;
            temp8 := self.memory(self.sp + 1, ADDR_DATA);
            self.memory(self.sp + 1, self.h, ADDR_DATA);
            self.h := temp8;
         when 16#E4# =>  --  CPO (Call if parity odd (parity flag false))
            self.call(not self.f.parity);
         when 16#E5# =>  --  PUSH H (Push to stack)
            temp16 := self.reg16(REG16_HL, False);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
         when 16#E6# =>  --  ANI (AND immediate with accumulator)
            self.a := self.a and self.get_next;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#E7# =>  --  RST 4 (Restart)
            temp16 := self.pc;
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
            self.pc := 16#20#;
         when 16#E8# =>  --  RPE (Return if parity even (parity flag true))
            self.ret(self.f.parity);
         when 16#E9# =>  --  PCHL (Copies HL into PC)
            self.pc := word(self.h)*16#100# + word(self.l);
         when 16#EA# =>  --  JPE (Jump if parity even (parity flag true))
            self.jump(self.f.parity);
         when 16#EB# =>  -- XCHG (Exchange HL and DE registers)
            temp8 := self.d;
            self.d := self.h;
            self.h := temp8;
            temp8 := self.e;
            self.e := self.l;
            self.l := temp8;
         when 16#EC# =>  --  CPE (Call if parity even (parity flag true))
            self.call(self.f.parity);
         when 16#ED# =>  --  Z80 ED instruction prefix
            if self.cpu_model = var_z80 then
               BBS.Sim_CPU.CPU.i8080.z80.prefix_ed(self);
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#EE# =>  --  XRI (Exclusive OR immediate with accumulator)
            self.a := self.a xor self.get_next;
            self.f.carry := False;
            self.setf(self.a);
            self.f.addsub := False;
         when 16#EF# =>  --  RST 5 (Restart)
            temp16 := self.pc;
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
            self.pc := 16#28#;
         when 16#F0# =>  --  RP (Return if positive (sign flag false))
            self.ret(not self.f.sign);
         when 16#F1# =>  --  POP PSW (Pop from stack)
            self.f := byte_to_psw(self.memory(self.sp, ADDR_DATA));
            self.sp := self.sp + 1;
            self.a := (self.memory(self.sp, ADDR_DATA));
            self.sp := self.sp + 1;
         when 16#F2# =>  --  JP (Jump if positive (sign flag false))
            self.jump(not self.f.sign);
         when 16#F3# =>  --  DI  (disable interrupts)
            self.int_enable := False;
            self.iff2 := self.int_enable;
         when 16#F4# =>  --  CP (Call if positive (sign flag false))
            self.call(not self.f.sign);
         when 16#F5# =>  --  PUSH PSW (Push to stack)
            self.sp := self.sp - 1;
            self.memory(self.sp, self.a, ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, psw_to_byte(self.f), ADDR_DATA);
         when 16#F6# =>  --  ORI (OR immediate with accumulator)
            self.a := self.a or self.get_next;
            self.f.carry := False;
            self.setf(self.a);
         when 16#F7# =>  --  RST 6 (Restart)
            temp16 := self.pc;
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
            self.pc := 16#30#;
         when 16#F8# =>  --  RM (Return if minus (sign flag true))
            self.ret(self.f.sign);
         when 16#F9# =>  --  SPHL (Copies HL into SP)
            self.sp := word(self.h)*16#100# + word(self.l);
         when 16#FA# =>  --  JM (Jump if minus (sign flag true))
            self.jump(self.f.sign);
         when 16#FB# =>  -- EI (enable interrupts)
            --
            --  When enabling interrupts, they are actually be enabled after
            --  the next instruction.  This allows a service routine to end with
            --  EI and RET instructions with the interrupts begin enabled after
            --  the RET.
            --
            self.ie_pending := True;
            self.iff2 := True;
         when 16#FC# =>  --  CM (Call if minus (sign flag true))
            self.call(self.f.sign);
         when 16#FD# =>  --  Z80 FD instruction prefix
            if self.cpu_model = var_z80 then
               self.ptr := use_iy;
               self.prefix := True;
               return;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#FE# =>  --  CPI (Compare immediate)
            temp8 := self.subf(self.a, self.get_next, False);
            self.f.addsub := True;
         when 16#FF# =>  --  RST 7 (Restart)
            temp16 := self.pc;
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
            self.pc := 16#38#;
      end case;
      self.ptr := use_hl;
      self.prefix := False;
   end;
   --
   --  Utility code for instruction decoder
   --
   function get_next(self : in out i8080) return byte is
      t : byte;
   begin
      if self.intr and self.int_enable and (not self.prefix) then
         self.lr_ctl.atype := ADDR_INTR;
         --
         --  For 8080/8085 and Z80 interrupt mode 0, read an instruction.
         --  Unless it's an 8085 vectored interrupt.  If so and processing gets
         --  here, the interrupt was masked, so we do normal processing.
         --
         if (self.cpu_model = var_8085) and ((self.int_posted = i85_5_5) or (self.int_posted = i85_6_5)
                                             or (self.int_posted = i85_7_5)) then
            self.lr_ctl.atype := ADDR_INST;
            t := self.memory(self.pc, ADDR_INST);
            self.pc := self.pc + 1;
            return t;
         elsif (self.cpu_model /= var_z80) or ((self.cpu_model = var_z80) and (self.int_mode = 0)) then
            self.intr := False;
            self.int_enable := False;
            return byte(self.int_posted and 16#FF#);
         end if;
         --
         --  Any other interrupt codes that get here should have been handled by
         --  the vector interrupt processing earlier.  If processing gets here
         --  then those interrupts have been masked and the next instruction should
         --  be fetched.
         --
      end if;
      self.lr_ctl.atype := ADDR_INST;
      t := self.memory(self.pc, ADDR_INST);
      self.pc := self.pc + 1;
      return t;
   end;
   --
   procedure reg8(self : in out i8080; reg : reg8_index; value : byte; override : Boolean) is
   begin
      case reg is
         when REG8_B =>
            self.b := value;
         when REG8_C =>
            self.c := value;
         when REG8_D =>
            self.d := value;
         when REG8_E =>
            self.e := value;
         when REG8_H =>
            case self.ptr is
               when use_hl =>
                  self.h := value;
               when use_ix =>  --  Z-80 only (undocumented)
                  if override then
                     self.h := value;
                  else
                     self.ix := (self.ix and 16#FF#) or (word(value)*16#100#);
                  end if;
               when use_iy =>  --  Z-80 only (undocumented)
                  if override then
                     self.h := value;
                  else
                     self.iy := (self.iy and 16#FF#) or (word(value)*16#100#);
                  end if;
            end case;
         when REG8_L =>
            case self.ptr is
               when use_hl =>
                  self.l := value;
               when use_ix =>  --  Z-80 only (undocumented)
                  if override then
                     self.l := value;
                  else
                     self.ix := (self.ix and 16#FF00#) or word(value);
                  end if;
               when use_iy =>  --  Z-80 only (undocumented)
                  if override then
                     self.l := value;
                  else
                     self.iy := (self.iy and 16#FF00#) or word(value);
                  end if;
            end case;
         when REG8_M =>  --  Memory
            case self.ptr is
               when use_hl =>
                  self.memory(word(self.h)*16#100# + word(self.l), value, ADDR_DATA);
               when use_ix =>  --  Z-80 only
                  self.memory(self.ix + sign_extend(self.get_next), value, ADDR_DATA);
               when use_iy =>  --  Z-80 only
                  self.memory(self.iy + sign_extend(self.get_next), value, ADDR_DATA);
            end case;
         when REG8_A =>
            self.a := value;
         when others =>
            Ada.Text_IO.Put_Line("REG8: Register other referenced");
            null;
      end case;
   end;
   --
   function reg8(self : in out i8080; reg : reg8_index; override : Boolean) return byte is
   begin
      case reg is
         when REG8_B =>
            return self.b;
         when REG8_C =>
            return self.c;
         when REG8_D =>
            return self.d;
         when REG8_E =>
            return self.e;
         when REG8_H =>
            case self.ptr is
               when use_hl =>
                  return self.h;
               when use_ix =>  --  Z-80 only (undocumented)
                  if override then
                     return self.h;
                  else
                     return byte((self.ix and 16#FF00#)/16#100#);
                  end if;
               when use_iy =>  --  Z-80 only (undocumented)
                  if override then
                     return self.h;
                  else
                     return byte((self.iy and 16#FF00#)/16#100#);
                  end if;
            end case;
         when REG8_L =>
            case self.ptr is
               when use_hl =>
                  return self.l;
               when use_ix =>  --  Z-80 only (undocumented)
                  if override then
                     return self.h;
                  else
                     return byte(self.ix and 16#FF#);
                  end if;
               when use_iy =>  --  Z-80 only (undocumented)
                  if override then
                     return self.l;
                  else
                     return byte(self.iy and 16#FF#);
                  end if;
            end case;
         when REG8_M =>
            case self.ptr is
               when use_hl =>
                  return self.memory(word(self.h)*16#100# + word(self.l), ADDR_DATA);
               when use_ix =>  --  Z-80 only
                  return self.memory(self.ix + sign_extend(self.get_next), ADDR_DATA);
               when use_iy =>  --  Z-80 only
                  return self.memory(self.iy + sign_extend(self.get_next), ADDR_DATA);
            end case;
         when REG8_A =>
            return self.a;
         when others =>
            Ada.Text_IO.Put_Line("REG8: Register other referenced");
            return 0;
      end case;
   end;
   --
   procedure reg16(self : in out i8080; reg : reg16_index; value : word; use_sp : Boolean) is
   begin
      case reg is
         when REG16_BC =>  --  Register pair BC
            self.b := byte(value/16#100#);
            self.c := byte(value and 16#FF#);
         when REG16_DE =>  --  Register pair DE
            self.d := byte(value/16#100#);
            self.e := byte(value and 16#FF#);
         when REG16_HL =>  --  Register pair HL
            case self.ptr is
               when use_hl =>
                  self.h := byte(value/16#100#);
                  self.l := byte(value and 16#FF#);
               when use_ix =>  --  Z-80 only
                  self.ix := value;
               when use_iy =>  --  Z-80 only
                  self.iy := value;
            end case;
         when REG16_SP =>  -- Register pair A and PSW or SP
            if use_sp then
               self.sp := value;
            else
               self.a := byte(value/16#100#);
               self.f := byte_to_psw(byte(value and 16#FF#));
            end if;
         when others =>
            null;
      end case;
   end;
   --
   function reg16(self : in out i8080; reg : reg16_index; use_sp : Boolean) return word is
   begin
      case reg is
         when REG16_BC =>  --  Register pair BC
            return word(self.b)*16#100# + word(self.c);
         when REG16_DE =>  --  Register pair DE
            return word(self.d)*16#100# + word(self.e);
         when REG16_HL =>  --  Register pair HL
            case self.ptr is
               when use_hl =>
                  return word(self.h)*16#100# + word(self.l);
               when use_ix =>  --  Z-80 only
                  return self.ix;
               when use_iy =>  --  Z-80 only
                  return self.iy;
            end case;
         when REG16_SP =>  -- Register pair A and PSW or SP
            if use_sp then
               return self.sp;
            else
               return word(self.a)*16#100# + word(psw_to_byte(self.f));
            end if;
         when others =>
            return 0;
      end case;
   end;
   --
   --  Set flags based on value (zero, sign, parity)
   --
   procedure setf(self : in out i8080; value : byte) is
      p : byte := 0;  --  Bit counter
   begin
      self.f.zero := (value = 0);
      self.f.sign := ((value and 16#80#) = 16#80#);
      p := p + (if ((value and 16#01#) = 16#01#) then 1 else 0);
      p := p + (if ((value and 16#02#) = 16#02#) then 1 else 0);
      p := p + (if ((value and 16#04#) = 16#04#) then 1 else 0);
      p := p + (if ((value and 16#08#) = 16#08#) then 1 else 0);
      p := p + (if ((value and 16#10#) = 16#10#) then 1 else 0);
      p := p + (if ((value and 16#20#) = 16#20#) then 1 else 0);
      p := p + (if ((value and 16#40#) = 16#40#) then 1 else 0);
      p := p + (if ((value and 16#80#) = 16#80#) then 1 else 0);
      self.f.parity := ((p and 16#01#) /= 16#01#);  --  True is even parity
   end;
   --
   --  Perform addition and set flags including carry and aux carry
   --
   function addf(self : in out i8080; v1 : byte; v2 : byte; c : Boolean) return byte is
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
      self.f.aux_carry := (temp > 16#0F#);
      self.setf(byte(sum and 16#FF#));
      --
      --  Compute overflow for Z-80
      --
      if self.cpu_model = var_z80 then
         self.f.parity := (((v1 and 16#80#) = (v2 and 16#80#)) and
                            (byte(sum and 16#80#) /= byte(v1 and 16#80#)));
      end if;
      return byte(sum and 16#FF#);
   end;
   --
   --  Perform subtraction and set flags including carry and aux carry
   --
   function subf(self : in out i8080; v1 : byte; v2 : byte; c : Boolean) return byte is
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
      self.f.aux_carry := (temp > 16#0F#);
      self.setf(byte(diff and 16#FF#));
      --
      --  Compute overflow for Z-80
      --
      if self.cpu_model = var_z80 then
         self.f.parity := (((v1 and 16#80#) /= (v2 and 16#80#)) and
                            (byte(diff and 16#80#) /= byte(v1 and 16#80#)));
      end if;
      return byte(diff and 16#FF#);
   end;
   --
   --  Modify a single 8 bit value.  This is used for both incrememnt and decrement.
   --  Flags are affected.
   --
   procedure mod8(self  : in out i8080; reg : reg8_index; dir : Integer) is
      value : byte;
   begin
      value := self.reg8(reg, False);
      if dir < 0 then
         self.f.aux_carry := ((value and 16#0F#) - 1 > 16#0F#);
         value := value - 1;
      else
         self.f.aux_carry := ((value and 16#0F#) + 1 > 16#0F#);
         value := value + 1;
      end if;
      self.setf(value);
      self.reg8(reg, value, False);
   end;
   --
   --  Modify a single 8 bit value.  This is used for both incrememnt and decrement.
   --  Flags are affected.
   --
   function mod8(self : in out i8080; value : byte; dir : Integer) return byte is
      t : byte := value;
   begin
      if dir < 0 then
         self.f.aux_carry := ((value and 16#0F#) - 1 > 16#0F#);
         t := t - 1;
      else
         self.f.aux_carry := ((value and 16#0F#) + 1 > 16#0F#);
         t := value + 1;
      end if;
      self.setf(t);
      return t;
   end;
   --
   --  Perform addition of register pairs.  The carry flag is affected
   --
   function dad(self  : in out i8080; v1 : word; v2 : word) return word is
      sum : uint32;
   begin
      sum := uint32(v1) + uint32(v2);
      self.f.carry := ((sum and 16#FFFF0000#) /= 0);
      return word(sum and 16#FFFF#);
   end;
   --
   --  Modify a 16 bit register pair.  This is used for both increment and decrement.
   --  No flags are affected.  Dir is positive for increment and negative for
   --  decrement.
   --
   procedure mod16(self  : in out i8080; reg : reg16_index; dir : Integer) is
      value : word;
   begin
      case reg is
         when REG16_BC =>  --  Register pair BC
            value := word(self.b)*16#100# + word(self.c);
         when REG16_DE =>  --  Register pair DE
            value := word(self.d)*16#100# + word(self.e);
         when REG16_HL =>  --  Register pair HL
            case self.ptr is
               when use_hl =>
                  value := word(self.h)*16#100# + word(self.l);
               when use_ix =>  --  Z-80 only
                  value := self.ix;
               when use_iy =>  --  Z-80 only
                  value := self.iy;
            end case;
         when REG16_SP =>  -- Register pair A and PSW
            value := self.sp;
         when others =>
            value := 0;
      end case;
      if dir < 0 then
         value := value - 1;
      else
         value := value + 1;
      end if;
      case reg is
         when REG16_BC =>  --  Register pair BC
            self.b := byte(value/16#100#);
            self.c := byte(value and 16#FF#);
         when REG16_DE =>  --  Register pair DE
            self.d := byte(value/16#100#);
            self.e := byte(value and 16#FF#);
         when REG16_HL =>  --  Register pair HL
            case self.ptr is
               when use_hl =>
                  self.h := byte(value/16#100#);
                  self.l := byte(value and 16#FF#);
               when use_ix =>  --  Z-80 only
                  self.ix := value;
               when use_iy =>  --  Z-80 only
                  self.iy := value;
            end case;
         when REG16_SP =>  -- Register pair A and PSW
            self.sp := value;
         when others =>
            null;
      end case;
   end;
   --
   --  All memory accesses should be routed through these functions so that they
   --  can do checks for memory-mapped I/O or shared memory.
   --
   procedure memory(self : in out i8080; addr : word; value : byte; mode : addr_type) is
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
   function memory(self : in out i8080; addr : word; mode : addr_type) return byte is
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
   procedure attach_io(self : in out i8080; io_dev : BBS.Sim_CPU.io.io_access;
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
   procedure port(self : in out i8080; addr : byte; value : byte) is
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
   function port(self : in out i8080; addr : byte) return byte is
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
   procedure jump(self : in out i8080; go : Boolean) is
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
   procedure call(self : in out i8080; go : Boolean) is
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
   procedure ret(self : in out i8080; go : Boolean) is
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
end BBS.Sim_CPU.CPU.i8080;
