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
with BBS.Sim_CPU.i8080.z80;
package body BBS.Sim_CPU.i8080 is
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
      self.int_mode := 0;
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
                  return BBS.Sim_CPU.i8080.z80.flags(self.f) & "(" &
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
               return BBS.Sim_CPU.i8080.z80.flags(self.fp) & "(" &
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
--  Implementation matrix
--   \ 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
--  00  V  V  V  V  V  V  V  V  Z  V  V  V  V  V  V  V
--  10  Z  V  V  V  V  V  V  V  Z  V  V  V  V  V  V  V
--  20  Z  V  V  V  V  V  V  V  Z  V  V  V  V  V  V  V
--  30  Z  V  V  V  V  V  V  V  Z  V  V  V  V  V  V  V
--  40  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  50  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  60  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  70  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  80  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  90  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  A0  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  B0  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  C0  V  V  V  V  V  V  V  V  V  V  V  Z  V  V  V  V
--  D0  V  V  V  V  V  V  V  V  V  Z  V  V  V  Z  V  V
--  E0  V  V  V  V  V  V  V  V  V  V  V  V  V  Z  V  V
--  F0  V  V  V  V  V  V  V  V  V  V  V  V  V  Z  V  V
--
--  * represents alternate opcodes that should not be used.
--  X represents opcodes implemented.
--  V represents opcodes implemented and tested.
--  Z represents Z-80 opcodes that are not used by the 8080.
--  . or blank represent opcodes not yet implemented.
   procedure decode(self : in out i8080) is
      inst    : byte;
      op_inst : opcode;
      reg1    : reg8_index;
      reg2    : reg8_index;
      reg16   : reg16_index;
      temp_addr : word;
      temp16  : word;
      temp8   : byte;
      temppsw  : status_word;
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
      op_inst := byte_to_op(inst);
      if (word(self.trace) and 1) = 1 then
         Ada.Text_IO.Put_Line("TRACE: Address: " & toHex(self.pc - 1) & " instruction " &
                           toHex(inst) & " (" & opcode'Image(op_inst) & ")");
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
         when 16#01# |16#11# | 16#21# | 16#31# =>  --  LXI r (Load register pair)
            temp16 := word(self.get_next);
            temp16 := temp16 + word(self.get_next)*16#100#;
            self.reg16(reg16_index((inst and 16#30#)/16#10#), temp16, True);
         when 16#02# | 16#12# =>  --  STAX B/D (Store accumulator at address)
            if inst = 16#02# then  --  STAX B
               temp_addr := word(self.b)*16#100# + word(self.c);
            else
               temp_addr := word(self.d)*16#100# + word(self.e);
            end if;
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#03# | 16#13# | 16#23# | 16#33# =>  --  INX r (increment double)
            reg16 := reg16_index((inst/16#10#) and 3);
            self.mod16(reg16, 1);
         when 16#04# |16#14# | 16#24# | 16#34# | 16#0C#| 16#1C# |
               16#2C# | 16#3C# =>  --  INR r (Increment register)
            reg1 := (inst/8) and 7;
            self.mod8(reg1, 1);
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#05# | 16#15# | 16#25# | 16#35# | 16#0D# | 16#1D# |
               16#2D# | 16#3D# =>  --  DCR r (Decrement register)
            reg1 := (inst/8) and 7;
            self.mod8(reg1, -1);
            if self.cpu_model = var_z80 then
               self.f.addsub := True;
            end if;
         when 16#06# | 16#0E# | 16#16# | 16#1E# | 16#26# | 16#2E# |
               16#36# | 16#3E# =>  --  MVI r (Move immediate to register)
            temp8 := self.get_next;
            self.reg8((inst and 16#38#)/16#08#, temp8, False);
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
         when 16#09# | 16#19# | 16#29# | 16#39# =>  --  DAD r (double add)
            reg16 := reg16_index((inst/16#10#) and 3);
            temp16 := self.dad(self.reg16(reg16_index(REG16_HL), True), self.reg16(reg16, True));
            self.reg16(reg16_index(REG16_HL), temp16, True);
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#0A# | 16#1A# =>  --  LDAX B/D (Load accumulator from address)
            if inst = 16#0A# then  --  LDAX B
               temp_addr := word(self.b)*16#100# + word(self.c);
            else  --  LDAX D
               temp_addr := word(self.d)*16#100# + word(self.e);
            end if;
            self.a := self.memory(temp_addr, ADDR_DATA);
         when 16#0B# | 16#1B# | 16#2B# | 16#3B# =>  --  DCX r (decrement double)
            reg16 := reg16_index((inst/16#10#) and 3);
            self.mod16(reg16, -1);
         when 16#0F# =>  --  RRC (Rotate accumulator right)
            if (self.a and 16#01#) = 1 then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
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
         when 16#17# =>  --  RAL (Rotate left through carry)
            temp16 := word(self.a)*2;
            if self.f.carry then
               temp16 := temp16 + 1;
            end if;
            if temp16 > 16#FF# then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
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
         when 16#1F# =>  --  RAR (Rotate right through carry)
            temp16 := word(self.a);
            if self.f.carry then
               temp16 := temp16 + 16#100#;
            end if;
            if (temp16 and 16#01#) = 1 then
               self.f.carry := True;
            else
               self.f.carry := False;
            end if;
            self.a := byte(temp16/2);
            if self.cpu_model = var_z80 then
               self.f.aux_carry := False;
               self.f.addsub    := False;
            end if;
         when 16#20# =>  --  RIM (Read interrupt mask, 8085 only)
         --
         --  This will need to be updated once interrupts are
         --  implemented.  It will also need to be updated should
         --  the serial input ever be implemented.  Right now, all
         --  it does is return the status of the interrupt enable
         --  flag.
         --
         --  Note that for the Z80, this is a JR NZ,offset instruction
         --
            if self.cpu_model = var_8085 then
               if self.int_enable then
                  self.a := 16#08#;
               else
                  self.a := 16#00#;
               end if;
            elsif self.cpu_model = var_z80 then
               temp8 := self.get_next;
               if not self.f.zero then
                  self.pc := self.pc + sign_extend(temp8);
               end if;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#22# =>  --  SHLD addr (Store HL direct)
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.memory(temp_addr, self.l, ADDR_DATA);
            temp_addr := temp_addr + 1;
            self.memory(temp_addr, self.h, ADDR_DATA);
         when 16#27# =>  --  DAA (Decimal adjust accumulator)
            if self.cpu_model = var_z80 then
               self.a := BBS.Sim_CPU.i8080.z80.daa(self.a, self.f);
            else
               temp8 := self.a;
               if ((temp8 and 16#0F#) > 6) or self.f.aux_carry then
                  if ((temp8 and 16#0F#) + 6) > 16#0F# then
                     self.f.aux_carry := True;
                  else
                     self.f.aux_carry := False;
                  end if;
                  temp8 := temp8 + 6;
               end if;
               if ((temp8/16#10# and 16#0F#) > 6) or self.f.carry then
                  if ((temp8/16#10# and 16#0F#) + 6) > 16#0F# then
                     self.f.carry := True;
                  else
                     self.f.carry := False;
                  end if;
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
         when 16#2A# =>  --  LHLD addr (Load HL direct)
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.l := self.memory(temp_addr, ADDR_DATA);
            temp_addr := temp_addr + 1;
            self.h := self.memory(temp_addr, ADDR_DATA);
         when 16#2F# =>  --  CMA (Complement accumulator)
            self.a := not self.a;
            if self.cpu_model = var_z80 then
               self.f.addsub := True;
            end if;
         when 16#30# =>  --  SIM (Set interrupt mask, 8085 only)
         --
         --  This will need to be updated once interrupts are
         --  implemented.  It will also need to be updated should
         --  the serial input ever be implemented.  Right now,
         --  this instruction does nothing.
         --
         --  Note that for the Z80, this is a JR NC,offset instruction
         --
            if self.cpu_model = var_8085 then
               null;
            elsif self.cpu_model = var_z80 then
               temp8 := self.get_next;
               if not self.f.carry then
                  self.pc := self.pc + sign_extend(temp8);
               end if;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#32# =>  --  STA addr (Store accumulator)
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.memory(temp_addr, self.a, ADDR_DATA);
         when 16#37# =>  --  STC (Set carry)
            self.f.carry := True;
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#38# =>  --  Z80 JR C,offset
            if self.cpu_model = var_z80 then
               temp8 := self.get_next;
               if self.f.carry then
                  self.pc := self.pc + sign_extend(temp8);
               end if;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#3A# =>  --  LDA addr (Load accumulator)
            temp_addr := word(self.get_next);
            temp_addr := temp_addr + word(self.get_next)*16#100#;
            self.a := self.memory(temp_addr, ADDR_DATA);
         when 16#3F# =>  --  CMC (Complement carry)
            self.f.carry := not self.f.carry;
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#40# .. 16#75# | 16#77# .. 16#7f# =>  --  MOV instructions
         --
         --  Note that 63 of the 256 instruction codes are occupied by various MOV
         --  instructions.  The MOV M,M instruction is illegal and that code is used
         --  for the HLT instruction, hence the "inst /= 16#76#" test below.
         --
            reg1 := inst and 16#07#;
            reg2 := (inst/8) and 16#07#;
            if (reg1 = 6) or (reg2 = 6) then  --  Check for Z-80 overrides
               self.reg8(reg2, self.reg8(reg1, True), True);
            else
               self.reg8(reg2, self.reg8(reg1, False), False);
            end if;
         when 16#76# =>  --  HLT (Halt)
            self.cpu_halt := True;
         when 16#80# .. 16#87# =>  -- ADD r (ADD register to accumulator)
            reg1 := inst and 16#07#;
            self.a := self.addf(self.a, self.reg8(reg1, False), False);
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#88# .. 16#8F# =>  -- ADC r (ADD register to accumulator with carry)
            reg1 := inst and 16#07#;
            self.a := self.addf(self.a, self.reg8(reg1, False), self.f.carry);
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#90# .. 16#97# =>  -- SUB r (SUB register from accumulator)
            reg1 := inst and 16#07#;
            self.a := self.subf(self.a, self.reg8(reg1, False), False);
            if self.cpu_model = var_z80 then
               self.f.addsub := True;
            end if;
         when 16#98# .. 16#9F# =>  -- SBB r (SUB register from accumulator with borrow)
            reg1 := inst and 16#07#;
            self.a := self.subf(self.a, self.reg8(reg1, False), self.f.carry);
            if self.cpu_model = var_z80 then
               self.f.addsub := True;
            end if;
         when 16#A0# .. 16#A7# =>  -- ANA r (AND accumulator with register)
            reg1 := inst and 16#07#;
            self.a := self.a and self.reg8(reg1, False);
            self.f.carry := False;
            self.setf(self.a);
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#A8# .. 16#AF# =>  -- XRA r (XOR accumulator with register)
            reg1 := inst and 16#07#;
            self.a := self.a xor self.reg8(reg1, False);
            self.f.carry := False;
            self.setf(self.a);
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#B0# .. 16#B7# =>  -- ORA r (OR accumulator with register)
            reg1 := inst and 16#07#;
            self.a := self.a or self.reg8(reg1, False);
            self.f.carry := False;
            self.setf(self.a);
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#B8# .. 16#BF# =>  -- CMP r (CMP register with accumulator)
            reg1 := inst and 16#07#;
            --  Only intersted in flags.  Ignore the actual result.
            temp8 := self.subf(self.a, self.reg8(reg1, False), False);
            if self.cpu_model = var_z80 then
               self.f.addsub := True;
            end if;
         when 16#C0# =>  --  RNZ (Return if not zero)
            self.ret(not self.f.zero);
         when 16#C1# | 16#D1# | 16#E1# | 16#F1# =>  --  POP r (Pop from stack)
            temp16 := word(self.memory(self.sp, ADDR_DATA));
            self.sp := self.sp + 1;
            temp16 := temp16 + word(self.memory(self.sp, ADDR_DATA))*16#100#;
            self.sp := self.sp + 1;
            self.reg16(reg16_index((inst and 16#30#)/16#10#), temp16, False);
         when 16#C2# =>  -- JNZ (Jump if not zero)
            self.jump(not self.f.zero);
         when 16#C3# =>  --  JMP (Jump unconditional)
            self.jump(true);
         when 16#C4# =>  --  CNZ (Call if not zero)
            self.call(not self.f.zero);
         when 16#C5# | 16#D5# | 16#E5# | 16#F5# =>  --  PUSH r (Push to stack)
            temp16 := self.reg16(reg16_index((inst and 16#30#)/16#10#), False);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
         when 16#C6# =>  --  ADI (ADD immediate with accumulator)
            reg1 := inst and 16#07#;
            self.a := self.addf(self.a, self.get_next, False);
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#C7# | 16#CF# | 16#D7# | 16#DF# | 16#E7# | 16#EF# |
               16#F7# | 16#FF# =>  --  RST n (Restart)
            temp8 := (inst/16#8#) and 7;
            temp16 := self.pc;
            Ada.Text_IO.Put_Line("SIM: RST instruction " & toHex(temp8));
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
            self.sp := self.sp - 1;
            self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
            self.pc := word(temp8*16#08#);
         when 16#C8# =>  --  RZ (Return if zero)
            self.ret(self.f.zero);
         when 16#C9# =>  --  RET (Return unconditional)
            self.ret(True);
         when 16#CA# =>  -- JZ (Jump if zero)
            self.jump(self.f.zero);
         when 16#CB# =>  --  Z80 CB instruction prefix
            if self.cpu_model = var_z80 then
               BBS.Sim_CPU.i8080.z80.prefix_cb(self);
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#CC# =>  --  CZ (Call if zero)
            self.call(self.f.zero);
         when 16#CD# =>  --  CALL (Call unconditional)
            self.call(True);
         when 16#CE# =>  --  ACI (ADD immediate with accumulator and carry)
            reg1 := inst and 16#07#;
            self.a := self.addf(self.a, self.get_next, self.f.carry);
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#D0# =>  --  RNC (Return if not carry)
            self.ret(not self.f.carry);
         when 16#D2# =>  --  JNC (Jump if not carry)
            self.jump(not self.f.carry);
         when 16#D3# =>  --  OUT (Output to port)
            temp8 := self.get_next;
            self.port(temp8, self.a);
         when 16#D4# =>  --  CNC (Call if not carry)
            self.call(not self.f.carry);
         when 16#D6# =>  --  SUI (Subtract immediate)
            self.a := self.subf(self.a, self.get_next, False);
            if self.cpu_model = var_z80 then
               self.f.addsub := True;
            end if;
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
               return;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#DE# =>  --  SUI (Subtract immediate)
            self.a := self.subf(self.a, self.get_next, self.f.carry);
            if self.cpu_model = var_z80 then
               self.f.addsub := True;
            end if;
         when 16#E0# =>  --  RPO (Return if parity odd (parity flag false))
            self.ret(not self.f.parity);
         when 16#E2# =>  --  JPO (Jump if parity odd (parity flag false))
            self.jump(not self.f.parity);
         when 16#E3# =>  --  XTHL (Exchange HL with top of stack)
            temp8 := self.memory(self.sp, ADDR_DATA);
            self.memory(self.sp, self.l, ADDR_DATA);
            self.l := temp8;
            temp8 := self.memory(self.sp + 1, ADDR_DATA);
            self.memory(self.sp + 1, self.h, ADDR_DATA);
            self.h := temp8;
         when 16#E4# =>  --  CPO (Call if parity odd (parity flag false))
            self.call(not self.f.parity);
         when 16#E6# =>  --  ANI (AND immediate with accumulator)
            self.a := self.a and self.get_next;
            self.f.carry := False;
            self.setf(self.a);
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
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
               BBS.Sim_CPU.i8080.z80.prefix_ed(self);
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#EE# =>  --  XRI (Exclusive OR immediate with accumulator)
            self.a := self.a xor self.get_next;
            self.f.carry := False;
            self.setf(self.a);
            if self.cpu_model = var_z80 then
               self.f.addsub := False;
            end if;
         when 16#F0# =>  --  RP (Return if positive (sign flag false))
            self.ret(not self.f.sign);
         when 16#F2# =>  --  JP (Jump if positive (sign flag false))
            self.jump(not self.f.sign);
         when 16#F3# | 16#FB# =>  --  DI and EI (disable/enable interrupts)
            --
            --  When enabling interrupts, they should actually be enabled after
            --  the next instruction.  This allows a service routine to end with
            --  EI and RET instructions with the interrupts begin enabled after
            --  the RET.  This will need to be done as part of implementing
            --  interrupts.
            --
            self.int_enable := (inst = 16#FB#);
            self.iff2 := self.int_enable;
         when 16#F4# =>  --  CP (Call if positive (sign flag false))
            self.call(not self.f.sign);
         when 16#F6# =>  --  ORI (OR immediate with accumulator)
            self.a := self.a or self.get_next;
            self.f.carry := False;
            self.setf(self.a);
         when 16#F8# =>  --  RM (Return if minus (sign flag true))
            self.ret(self.f.sign);
         when 16#F9# =>  --  SPHL (Copies HL into SP)
            self.sp := word(self.h)*16#100# + word(self.l);
         when 16#FA# =>  --  JM (Jump if minus (sign flag true))
            self.jump(self.f.sign);
         when 16#FC# =>  --  CM (Call if minus (sign flag true))
            self.call(self.f.sign);
         when 16#FD# =>  --  Z80 FD instruction prefix
            if self.cpu_model = var_z80 then
               self.ptr := use_iy;
               return;
            else
               self.unimplemented(self.pc, inst);
            end if;
         when 16#FE# =>  --  CPI (Compare immediate)
            temp8 := self.subf(self.a, self.get_next, False);
            if self.cpu_model = var_z80 then
               self.f.addsub := True;
            end if;
         when others =>
               self.unimplemented(self.pc, inst);
      end case;
      self.ptr := use_hl;
   end;
   --
   --  Utility code for instruction decoder
   --
   function get_next(self : in out i8080) return byte is
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
      self.f.parity := not ((p and 16#01#) = 16#01#);  --  True is even parity
   end;
   --
   --  Perform addition and set flags including carry and aux carry
   --
   function addf(self : in out i8080; v1 : byte; v2 : byte; c : Boolean) return byte is
      sum : word;
      temp : byte;
   begin
      sum := word(v1) + word(v2);
      if c then
         sum := sum + 1;
      end if;
      if sum > 16#FF# then
         self.f.carry := True;
      else
         self.f.carry := False;
      end if;
      temp := (v1 and 16#0F#) + (v2 and 16#0F#);
      if c then
         temp := temp + 1;
      end if;
      if temp > 16#0F# then
         self.f.aux_carry := True;
      else
         self.f.aux_carry := False;
      end if;
      self.setf(byte(sum and 16#FF#));
      --
      --  Compute overflow for Z-80
      --
      if self.cpu_model = var_z80 then
         if ((v1 and 16#80#) = (v2 and 16#80#)) and
            (byte(sum and 16#80#) /= byte(v1 and 16#80#))then
            self.f.parity := True;
         else
            self.f.parity := False;
         end if;
      end if;
      return byte(sum and 16#FF#);
   end;
   --
   --  Perform subtraction and set flags including carry and aux carry
   --
   function subf(self : in out i8080; v1 : byte; v2 : byte; c : Boolean) return byte is
      diff : word;
      temp : byte;
   begin
      diff := word(v1) - word(v2);
      if c then
         diff := diff - 1;
      end if;
      if diff > 16#FF# then
         self.f.carry := True;
      else
         self.f.carry := False;
      end if;
      temp := (v1 and 16#0F#) - (v2 and 16#0F#);
      if c then
         temp := temp - 1;
      end if;
      if temp > 16#0F# then
         self.f.aux_carry := True;
      else
         self.f.aux_carry := False;
      end if;
      self.setf(byte(diff and 16#FF#));
      --
      --  Compute overflow for Z-80
      --
      if self.cpu_model = var_z80 then
         if ((v1 and 16#80#) /= (v2 and 16#80#)) and
            (byte(diff and 16#80#) /= byte(v1 and 16#80#))then
            self.f.parity := True;
         else
            self.f.parity := False;
         end if;
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
         if (value and 16#0F#) - 1 > 16#0F# then
            self.f.aux_carry := True;
         else
            self.f.aux_carry := False;
         end if;
         value := value - 1;
      else
         if (value and 16#0F#) + 1 > 16#0F# then
            self.f.aux_carry := True;
         else
            self.f.aux_carry := False;
         end if;
         value := value + 1;
      end if;
      self.setf(value);
      self.reg8(reg, value, False);
   end;
   --
   --  Perform addition of register pairs.  The carry flag is affected
   --
   function dad(self  : in out i8080; v1 : word; v2 : word) return word is
      sum : uint32;
   begin
      sum := uint32(v1) + uint32(v2);
      if (sum and 16#FFFF0000#) /= 0 then
         self.f.carry := True;
      else
         self.f.carry := False;
      end if;
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
   procedure attach_io(self : in out i8080; io_dev : io_access;
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
end BBS.Sim_CPU.i8080;
