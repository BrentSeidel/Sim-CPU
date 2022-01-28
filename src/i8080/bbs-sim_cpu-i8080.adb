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
   function byte_to_psw is new Ada.Unchecked_Conversion(source => byte,
                                                           target => status_word);
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
      reg : reg_id;
   begin
      if num <= reg_id'Pos(reg_id'Last) then
         reg := reg_id'Val(num);
         case reg is
            when reg_a =>
               return toHex(self.a);
            when reg_psw =>
               return (if self.psw.sign then "S" else "-") &
               (if self.psw.zero then "Z" else "-") & "*" &
               (if self.psw.aux_carry then "A" else "-") & "*" &
               (if self.psw.parity then "P" else "-") & "*" &
               (if self.psw.carry then "C" else "-");
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
--   procedure set_reg(self : in out simple; num : BBS.embed.uint32;
--                     data : BBS.embed.uint32) is null;
   --
   --  Called to check if the CPU is halted
   --
   overriding
   function halted(self : in out i8080) return Boolean is
   begin
      return self.cpu_halt;
   end;
--  --------------------------------------------------------------------
--
--  Code for the instruction processing.
--
--  Implementation matrix
--   \ 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
--  00  V  V  V  .  .  .  V  .  *  .  V  .  .  .  V  .
--  10  *  V  V  .  .  .  V  .  *  .  V  .  .  .  V  .
--  20  *  V  .  .  .  .  V  .  *  .  .  .  .  .  V  V
--  30  *  V  .  .  .  .  V  V  *  .  .  .  .  .  V  V
--  40  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  50  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  60  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  70  V  V  V  V  V  V  X  V  V  V  V  V  V  V  V  V
--  80  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
--  90  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
--  A0  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V  V
--  B0  V  V  V  V  V  V  V  V  .  .  .  .  .  .  .  .
--  C0  .  V  X  X  .  V  .  .  .  .  X  *  .  .  .  .
--  D0  .  V  X  .  .  V  .  .  .  *  X  .  .  *  .  .
--  E0  .  V  X  .  .  V  V  .  .  .  X  .  X  *  V  .
--  F0  .  V  X  X  .  V  V  .  .  .  X  X  .  *  .  .
--
--  * represents alternate opcodes that should not be used.
--  X represents opcodes implemented.
--  V represents opcodes implemented and tested.
--  . or blank represent opcodes not yet implemented.
   procedure decode(self : in out i8080) is
      inst : byte;
      reg1 : reg8_index;
      reg2 : reg8_index;
      temp_addr : word;
      temp16 : word;
      temp8 : byte;
   begin
      self.intr := False;  --  Currently interrupts are not implemented
      inst := self.get_next(ADDR_INST);
      --
      --  Do instruction processing
      --
      --  Note that 63 of the 256 instruction codes are occupied by simple MOV
      --  instructions.  The MOV M,M instruction is illegal and that code is used
      --  for the HLT instruction, hence the "inst /= 16#76#" test below.
      --
      if ((inst and 16#C0#) = 16#40#) and (inst /= 16#76#) then  --  An assortment of move instructions
         reg1 := inst and 16#07#;
         reg2 := (inst/8) and 16#07#;
         self.reg8(reg2, self.reg8(reg1, 1), 1);
      else
         case inst is
            when 16#00# =>  --  NOP (No operation)
               null;
            when 16#01# | 16#11# | 16#21# | 16#31# =>  --  LXI r (Load register pair)
               temp16 := word(self.get_next(ADDR_DATA));
               temp16 := temp16 + word(self.get_next(ADDR_DATA))*16#100#;
               self.reg16(reg16_index((inst and 16#30#)/16#10#), temp16, 0);
            when 16#06# | 16#0E# | 16#16# | 16#1E# | 16#26# | 16#2E# |
                 16#36# | 16#3E# =>  --  MVI r (Move immediate to register)
               temp8 := self.get_next(ADDR_DATA);
               self.reg8((inst and 16#38#)/16#08#, temp8, 0);
            when 16#02# | 16#12# =>  --  STAX B/D (Store accumulator at address)
               if inst = 16#02# then  --  STAX B
                  temp_addr := word(self.b)*16#100# + word(self.c);
               else
                  temp_addr := word(self.d)*16#100# + word(self.e);
               end if;
               self.memory(temp_addr, self.a, ADDR_DATA);
            when 16#0A# | 16#1A# =>  --  LDAX B/D (Load accumulator from address)
               if inst = 16#0A# then  --  LDAX B
                  temp_addr := word(self.b)*16#100# + word(self.c);
               else  --  LDAX D
                  temp_addr := word(self.d)*16#100# + word(self.e);
               end if;
               self.a := self.memory(temp_addr, ADDR_DATA);
            when 16#2F# =>  --  CMA (Complement accumulator)
               self.a := not self.a;
            when 16#37# =>  --  STC (Set carry)
               self.psw.carry := True;
            when 16#3F# =>  --  CMC (Complement carry)
               self.psw.carry := not self.psw.carry;
            when 16#76# =>  --  HLT (Halt)
               self.cpu_halt := True;
            when 16#A0# | 16#A1# | 16#A2# | 16#A3# |16#A4# | 16#A5# |
                 16#A6# | 16#A7# =>  -- ANA r (AND accumulator with register)
               reg1 := inst and 16#07#;
               self.a := self.a and self.reg8(reg1, 0);
               self.psw.carry := False;
               self.setf(self.a);
            when 16#A8# | 16#A9# | 16#AA# | 16#AB# |16#AC# | 16#AD# |
                 16#AE# | 16#AF# =>  -- XRA r (XOR accumulator with register)
               reg1 := inst and 16#07#;
               self.a := self.a xor self.reg8(reg1, 0);
               self.psw.carry := False;
               self.setf(self.a);
            when 16#B0# | 16#B1# | 16#B2# | 16#B3# |16#B4# | 16#B5# |
                 16#B6# | 16#B7# =>  -- ORA r (OR accumulator with register)
               reg1 := inst and 16#07#;
               self.a := self.a or self.reg8(reg1, 0);
               self.psw.carry := False;
               self.setf(self.a);
            when 16#C1# | 16#D1# | 16#E1# | 16#F1# =>  --  POP r (Pop from stack)
               temp16 := word(self.memory(self.sp, ADDR_DATA));
               self.sp := self.sp + 1;
               temp16 := temp16 + word(self.memory(self.sp, ADDR_DATA))*16#100#;
               self.sp := self.sp + 1;
               self.reg16(reg16_index((inst and 16#30#)/16#10#), temp16, 1);
            when 16#C2# =>  -- JNZ (Jump if not zero)
               self.jump(not self.psw.zero);
            when 16#C3# =>  --  JMP (Jump)
               self.jump(true);
            when 16#C5# | 16#D5# | 16#E5# | 16#F5# =>  --  PUSH r (Push to stack)
               temp16 := self.reg16(reg16_index((inst and 16#30#)/16#10#));
               self.sp := self.sp - 1;
               self.memory(self.sp, byte(temp16/16#100#), ADDR_DATA);
               self.sp := self.sp - 1;
               self.memory(self.sp, byte(temp16 and 16#FF#), ADDR_DATA);
            when 16#CA# =>  -- JZ (Jump if zero)
               self.jump(self.psw.zero);
            when 16#D2# =>  --  JNC (Jump if not carry)
               self.jump(not self.psw.carry);
            when 16#DA# =>  --  JC (Jump if carry)
               self.jump(self.psw.carry);
            when 16#E2# =>  --  JPO (Jump if parity odd (parity flag false))
               self.jump(not self.psw.parity);
            when 16#E6# =>  --  ANI (AND immediate with accumulator)
               self.a := self.a and self.get_next(ADDR_DATA);
               self.psw.carry := False;
               self.setf(self.a);
            when 16#EA# =>  --  JPE (Jump if parity even (parity flag true))
               self.jump(self.psw.parity);
            when 16#EB# =>  -- XCHG (Exchange HL and DE registers)
               temp8 := self.d;
               self.d := self.h;
               self.h := temp8;
               temp8 := self.e;
               self.e := self.l;
               self.l := temp8;
            when 16#EE# =>  --  XRI (Exclusive OR immediate with accumulator)
               self.a := self.a xor self.get_next(ADDR_DATA);
               self.psw.carry := False;
               self.setf(self.a);
            when 16#F2# =>  --  JP (Jump if positive (sign flag false))
               self.jump(not self.psw.sign);
            when 16#F3# | 16#FB# =>  --  DI and EI (disable/enable interrupts)
               self.int_enable := (inst = 16#FB#);
            when 16#F6# =>  --  OR (OR immediate with accumulator)
               self.a := self.a or self.get_next(ADDR_DATA);
               self.psw.carry := False;
               self.setf(self.a);
            when 16#FA# =>  --  JM (Jump if minus (sign flag true))
               self.jump(self.psw.sign);
            when others =>
               null;
         end case;
      end if;
   end;
   --
   --  Utility code for instruction decoder
   --
   function get_next(self : in out i8080; mode : addr_type) return byte is
      t : byte;
   begin
      self.lr_ctl.atype := mode;
      if self.intr then
         self.lr_ctl.atype := ADDR_INTR;
         return 0;  -- Currently interrupts are not implemented
      else
         t := self.memory(self.pc, mode);
         self.pc := self.pc + 1;
         return t;
      end if;
   end;
   --
   procedure reg8(self : in out i8080; reg : reg8_index; value : byte; v : Natural) is
   begin
      case reg is
         when 0 =>
            self.b := value;
         when 1 =>
            self.c := value;
         when 2 =>
            self.d := value;
         when 3 =>
            self.e := value;
         when 4 =>
            self.h := value;
         when 5 =>
            self.l := value;
         when 6 =>  --  Memory
            if v = 1 then  -- Memory address is immediate
               self.temp_addr := word(self.get_next(ADDR_DATA));
               self.temp_addr := self.temp_addr + word(self.get_next(ADDR_DATA))*16#100#;
               self.memory(self.temp_addr,  value, ADDR_DATA);
            else  --  Memory address is in HL register pair
               self.memory(word(self.h)*16#100# + word(self.l), value, ADDR_DATA);
            end if;
         when 7 =>
            self.a := value;
         when others =>
            null;
      end case;
   end;
   --
   function reg8(self : in out i8080; reg : reg8_index; v : Natural) return byte is
   begin
      case reg is
         when 0 =>
            return self.b;
         when 1 =>
            return self.c;
         when 2 =>
            return self.d;
         when 3 =>
            return self.e;
         when 4 =>
            return self.h;
         when 5 =>
            return self.l;
         when 6 =>
            if v = 1 then  -- Memory address is immediate
               self.temp_addr := word(self.get_next(ADDR_DATA));
               self.temp_addr := self.temp_addr + word(self.get_next(ADDR_DATA))*16#100#;
               return self.memory(self.temp_addr, ADDR_DATA);
            else  --  Memory address is in HL register pair
               return self.memory(word(self.h)*16#100# + word(self.l), ADDR_DATA);
            end if;
         when 7 =>
            return self.a;
         when others =>
            return 0;
      end case;
   end;
   --
   procedure reg16(self : in out i8080; reg : reg16_index; value : word; v : Natural) is
   begin
      case reg is
         when 0 =>  --  Register pair BC
            self.b := byte(value/16#100#);
            self.c := byte(value and 16#FF#);
         when 1 =>  --  Register pair DE
            self.d := byte(value/16#100#);
            self.e := byte(value and 16#FF#);
         when 2 =>  --  Register pair HL
            self.h := byte(value/16#100#);
            self.l := byte(value and 16#FF#);
         when 3 =>  -- Register pair A and PSW
            if v = 0 then
               self.sp := value;
            elsif v = 1 then
               self.a := byte(value/16#100#);
               self.psw := byte_to_psw(byte(value and 16#FF#));
            end if;
         when others =>
            null;
      end case;
   end;
   --
   function reg16(self : in out i8080; reg : reg16_index) return word is
   begin
      case reg is
         when 0 =>  --  Register pair BC
            return word(self.b)*16#100# + word(self.c);
         when 1 =>  --  Register pair DE
            return word(self.d)*16#100# + word(self.e);
         when 2 =>  --  Register pair HL
            return word(self.h)*16#100# + word(self.l);
         when 3 =>  -- Register pair A and PSW
            return word(self.a)*16#100# + word(psw_to_byte(self.psw));
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
   --  Perform addition and set flags including carry and aux carry
   --
   function addf(self : in out i8080; v1 : byte; v2 : byte) return byte is
   begin
      return 0;
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
      --  or other special stuff can bed added here.
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
      --  or other special stuff can bed added here.
      --
      return self.mem(addr);
   end;
   --
   --  Handle I/O port accesses
   --
   procedure port(self : in out i8080; addr : word; value : byte; mode : addr_type) is
   begin
      null;
   end;
   --
   function port(self : in out i8080; addr : word; mode : addr_type) return byte is
   begin
      return 0;
   end;
   --
   --  Common code for Jump, Call, and Return
   --
   procedure jump(self : in out i8080; go : Boolean) is
      temp_pc : word;
   begin
      if go then
         temp_pc := word(self.get_next(ADDR_DATA));
         temp_pc := temp_pc + word(self.get_next(ADDR_DATA))*16#100#;
         self.pc := temp_pc;
      else
         self.sp := self.sp + 2;
      end if;
   end;
   --
   procedure call(self : in out i8080; go : Boolean) is
      temp_pc : word;
   begin
      if go then
         temp_pc := self.pc;
         self.sp := self.sp - 1;
         self.memory(self.sp, byte(temp_pc/16#100#), ADDR_DATA);
         self.sp := self.sp - 1;
         self.memory(self.sp, byte(temp_pc and 16#FF#), ADDR_DATA);
         --
         temp_pc := word(self.get_next(ADDR_DATA));
         temp_pc := temp_pc + word(self.get_next(ADDR_DATA))*16#100#;
         self.pc := temp_pc;
      else
         self.sp := self.sp + 2;
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
end BBS.Sim_CPU.i8080;
