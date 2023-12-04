with BBS.embed;
use type BBS.embed.uint8;
use type BBS.embed.uint32;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
with BBS.Sim_CPU.m68000.line_0;
--with BBS.Sim_CPU.m68000.line_1;
--with BBS.Sim_CPU.m68000.line_2;
--with BBS.Sim_CPU.m68000.line_3;
--with BBS.Sim_CPU.m68000.line_4;
with BBS.Sim_CPU.m68000.line_5;
--with BBS.Sim_CPU.m68000.line_6;
--with BBS.Sim_CPU.m68000.line_7;
--with BBS.Sim_CPU.m68000.line_8;
--with BBS.Sim_CPU.m68000.line_9;
--with BBS.Sim_CPU.m68000.line_a;
--with BBS.Sim_CPU.m68000.line_b;
with BBS.Sim_CPU.m68000.line_c;
with BBS.Sim_CPU.m68000.line_d;
--with BBS.Sim_CPU.m68000.line_e;
--with BBS.Sim_CPU.m68000.line_f;
package body BBS.Sim_CPU.m68000 is
   --
   function uint16_to_ctrl is new Ada.Unchecked_Conversion(source => BBS.embed.uint16,
                                                           target => ctrl_mode);
   function psw_to_word is new Ada.Unchecked_Conversion(source => status_word,
                                                           target => word);
   function word_to_psw is new Ada.Unchecked_Conversion(source => word,
                                                        target => status_word);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out m68000) is
   begin
      self.d0 := 0;
      self.d1 := 0;
      self.d2 := 0;
      self.d3 := 0;
      self.d4 := 0;
      self.d5 := 0;
      self.d6 := 0;
      self.d7 := 0;
      self.a0 := 0;
      self.a1 := 0;
      self.a2 := 0;
      self.a3 := 0;
      self.a4 := 0;
      self.a5 := 0;
      self.a6 := 0;
      self.usp := 0;
      self.ssp := 0;
      self.pc := 0;
      self.psw.carry    := False;
      self.psw.overflow := False;
      self.psw.zero     := False;
      self.psw.negative := False;
      self.psw.extend   := False;
      self.psw.mask     := 0;
      self.psw.super    := True;
      self.psw.trace0   := False;
      self.psw.trace1   := False;
   end;
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
            when reg_a5 =>
               return data_bus(self.a5);
            when reg_a6 =>
               return data_bus(self.a6);
            when reg_usp =>
               return data_bus(self.usp);
            when reg_ssp =>
               return data_bus(self.ssp);
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
            when reg_a5 =>
               return toHex(self.a5);
            when reg_a6 =>
               return toHex(self.a6);
            when reg_usp =>
               return toHex(self.usp);
            when reg_ssp =>
               return toHex(self.ssp);
            when reg_pc =>
               return toHex(self.pc);
            when reg_stat =>
               return (if self.psw.carry then "C" else "-") &
                  (if self.psw.overflow then "V" else "-") &
                  (if self.psw.zero then "Z" else "-") &
                  (if self.psw.negative then "N" else "-") &
                  (if self.psw.extend then "X" else "-") &
                  "***" &
                  (interrupt_mask'Image(self.psw.mask)) &
                  "**" &
                  (if self.psw.super then "S" else "-") &
                  (if self.psw.trace0 then "t" else "-") &
                  (if self.psw.trace1 then "T" else "-");
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
               self.memory(addr + addr_bus(i), data(Integer(i)));
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
   --  Unimplemented instruction response
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
      instr := self.get_next;
      if (word(self.trace) and 1) = 1 then
         Ada.Text_IO.Put_Line("TRACE: Address: " & toHex(self.pc - 1) & " instruction " &
                           toHex(instr));
      end if;
      Ada.Text_IO.Put_Line("Executing group " & toHex(byte(instr1.pre)) &
         " instruction.");
      case instr1.pre is
        when 16#0# =>  --  Group 0 - Bit manipulation/MOVEP/Immediate
           BBS.Sim_CPU.m68000.line_0.decode_0(self);
        when 16#1# =>  --  Group 1 - Move byte
           null;
        when 16#2# =>  --  Group 2 - Move long
           null;
        when 16#3# =>  --  Group 3 - Move word
           null;
        when 16#4# =>  --  Group 4 - Miscellaneous
           null;
        when 16#5# =>  --  Group 5 - ADDQ/SUBQ/Scc/DBcc/TRAPcc
           BBS.Sim_CPU.m68000.line_5.decode_5(self);
        when 16#6# =>  --  Group 6 - Bcc/BSR/BRA
           null;
        when 16#7# =>  --  Group 7 - MOVEQ
           null;
        when 16#8# =>  --  Group 8 - OR/DIV/SBCD
           null;
        when 16#9# =>  --  Group 9 - SUB/SUBX
           null;
        when 16#a# =>  --  Group 10 - Unassigned/Reserved (A-Line)
           null;
        when 16#b# =>  --  Group 11 - CMP/EOR
           null;
        when 16#c# =>  --  Group 12 - AND/MUL/ABCD/EXG
           BBS.Sim_CPU.m68000.line_c.decode_c(self);
        when 16#d# =>  --  Group 13 - ADD/ADDX
           BBS.Sim_CPU.m68000.line_d.decode_d(self);
        when 16#e# =>  --  Group 14 - Shift/Rotate/Bit Field
           null;
        when 16#f# =>  --  Group 15 - Unassigned/Reserved (F-Line) (table lookup and interpolation)
           null;
      end case;
   end;
   --
   --  Utility code for instruction decoder
   --
   --  Get next instruction
   --
   function get_next(self : in out m68000) return word is
      t : word;
   begin
      self.lr_ctl.atype := ADDR_INST;
      if self.intr then
         self.lr_ctl.atype := ADDR_INTR;
         return 0;  -- Currently interrupts are not implemented
      else
         t := self.memory(self.pc);
         self.pc := self.pc + 2;
         return t;
      end if;
   end;
   --
   --  Get extension word
   --
   function get_ext(self : in out m68000) return word is
      t : word;
   begin
      self.lr_ctl.atype := ADDR_INST;
      t := self.memory(self.pc);
      self.pc := self.pc + 2;
      return t;
   end;
   --
   --  BCD Conversions
   --
   function bcd_to_byte(b : byte) return byte is
   begin
      return ((b/16#10# and 16#F#)*10) + (b and 16#F#);
   end;
   --
   function byte_to_bcd(b : byte) return byte is
   begin
      return (b mod 10) + (b/10)*16;
   end;
   --
   --  Sign extension
   --
   function sign_extend(d : byte) return long is
   begin
      if (d and 16#80#) = 16#80# then
         return long(d) or 16#FFFF_FF00#;
      else
         return long(d);
      end if;
   end;
   --
   function sign_extend(d : word) return long is
   begin
      if (d and 16#8000#) = 16#8000# then
         return long(d) or 16#FFFF_0000#;
      else
         return long(d);
      end if;
   end;
   --
   --  Register opertions
   --
   function get_reg(self : in out m68000; data_addr : reg_type; reg_index : uint3) return byte is
   begin
     if data_addr = data then
       case reg_index is
         when 0 =>
           return byte(self.d0 and 16#ff#);
         when 1 =>
           return byte(self.d1 and 16#ff#);
         when 2 =>
           return byte(self.d2 and 16#ff#);
         when 3 =>
           return byte(self.d3 and 16#ff#);
         when 4 =>
           return byte(self.d4 and 16#ff#);
         when 5 =>
           return byte(self.d5 and 16#ff#);
         when 6 =>
           return byte(self.d6 and 16#ff#);
         when 7 =>
           return byte(self.d7 and 16#ff#);
       end case;
     else
       case reg_index is
         when 0 =>
           return byte(self.a0 and 16#ff#);
         when 1 =>
           return byte(self.a1 and 16#ff#);
         when 2 =>
           return byte(self.a2 and 16#ff#);
         when 3 =>
           return byte(self.a3 and 16#ff#);
         when 4 =>
           return byte(self.a4 and 16#ff#);
         when 5 =>
           return byte(self.a5 and 16#ff#);
         when 6 =>
           return byte(self.a6 and 16#ff#);
         when 7 =>
            if self.psw.super then
               return byte(self.ssp and 16#ff#);
            else
               return byte(self.usp and 16#ff#);
            end if;
         end case;
     end if;
   end;
   function get_reg(self : in out m68000; data_addr : reg_type; reg_index : uint3) return word is
   begin
     if data_addr = data then
       case reg_index is
         when 0 =>
           return word(self.d0 and 16#ffff#);
         when 1 =>
           return word(self.d1 and 16#ffff#);
         when 2 =>
           return word(self.d2 and 16#ffff#);
         when 3 =>
           return word(self.d3 and 16#ffff#);
         when 4 =>
           return word(self.d4 and 16#ffff#);
         when 5 =>
           return word(self.d5 and 16#ffff#);
         when 6 =>
           return word(self.d6 and 16#ffff#);
         when 7 =>
           return word(self.d7 and 16#ffff#);
       end case;
     else
       case reg_index is
         when 0 =>
           return word(self.a0 and 16#ffff#);
         when 1 =>
           return word(self.a1 and 16#ffff#);
         when 2 =>
           return word(self.a2 and 16#ffff#);
         when 3 =>
           return word(self.a3 and 16#ffff#);
         when 4 =>
           return word(self.a4 and 16#ffff#);
         when 5 =>
           return word(self.a5 and 16#ffff#);
         when 6 =>
           return word(self.a6 and 16#ffff#);
         when 7 =>
            if self.psw.super then
               return word(self.ssp and 16#ffff#);
            else
               return word(self.usp and 16#ffff#);
            end if;
         end case;
     end if;
   end;
   function get_reg(self : in out m68000; data_addr : reg_type; reg_index : uint3) return long is
   begin
     if data_addr = data then
       case reg_index is
         when 0 =>
           return self.d0;
         when 1 =>
           return self.d1;
         when 2 =>
           return self.d2;
         when 3 =>
           return self.d3;
         when 4 =>
           return self.d4;
         when 5 =>
           return self.d5;
         when 6 =>
           return self.d6;
         when 7 =>
           return self.d7;
       end case;
     else
       case reg_index is
         when 0 =>
           return self.a0;
         when 1 =>
           return self.a1;
         when 2 =>
           return self.a2;
         when 3 =>
           return self.a3;
         when 4 =>
           return self.a4;
         when 5 =>
           return self.a5;
         when 6 =>
           return self.a6;
         when 7 =>
            if self.psw.super then
               return self.ssp;
            else
               return self.usp;
            end if;
         end case;
     end if;
   end;
   procedure set_reg(self : in out m68000; data_addr : reg_type; reg_index : uint3; value : long) is
   begin
     if data_addr = data then
       case reg_index is
         when 0 =>
           self.d0 := value;
         when 1 =>
           self.d1 := value;
         when 2 =>
           self.d2 := value;
         when 3 =>
           self.d3 := value;
         when 4 =>
           self.d4 := value;
         when 5 =>
           self.d5 := value;
         when 6 =>
           self.d6 := value;
         when 7 =>
           self.d7 := value;
       end case;
     else
       case reg_index is
         when 0 =>
           self.a0 := value;
         when 1 =>
           self.a1 := value;
         when 2 =>
           self.a2 := value;
         when 3 =>
           self.a3 := value;
         when 4 =>
           self.a4 := value;
         when 5 =>
           self.a5 := value;
         when 6 =>
           self.a6 := value;
         when 7 =>
           if self.psw.super then
              self.ssp := value;
           else
              self.usp := value;
           end if;
         end case;
     end if;
   end;
   --
   --  Get EA.  Decode the register, addressing modes, and extension
   --  words to get the effective address.  Also does any pre-processing,
   --  namely pre-decrement, as appropriate.
   --
   --  Mode  Addressing mode
   --    0   Data register direct
   --    1   Address register direct
   --    2   Address register indirect
   --    3   Address register indirect with post increment
   --    4   Address register indirect with pre decrement
   --    5   Address register indirect with 16 bit displacement
   --    6   Extension word modes
   --    7   Special modes
   --
   function get_EA(self : in out m68000; reg : uint3; mode : uint3;
      size : data_size) return operand is
   begin
      Ada.Text_IO.Put_Line("Decoding EA mode " & uint3'Image(mode) &
         " and register " & uint3'Image(reg));
      case mode is
        when 0 =>  --  Data register <Dx>
           return (kind => data_register, data_reg => reg);
        when 1 =>  --  Address register <Ax>
           return (kind => address_register, addr_reg => reg);
        when 2 =>  --  Address register indirect <(Ax)>
           return (kind => memory_address, address => self.get_reg(Address, reg));
        when 3 =>  --  Address register indirect with post increment <(Ax)+>
           return (kind => memory_address, address => self.get_reg(Address, reg));
        when 4 =>  --  Address register indirect with pre decrement <-(Ax)>
           case size is
             when data_byte =>
                if reg = 7 then --  Stack pointer needs to stay even
                   self.set_reg(Address, reg, self.get_reg(Address, reg) - 2);
                else
                   self.set_reg(Address, reg, self.get_reg(Address, reg) - 1);
                end if;
             when data_word =>
                self.set_reg(Address, reg, self.get_reg(Address, reg) - 2);
             when data_long =>
                self.set_reg(Address, reg, self.get_reg(Address, reg) - 4);
             when data_long_long =>
                self.set_reg(Address, reg, self.get_reg(Address, reg) - 8);
           end case;
           return (kind => memory_address, address => self.get_reg(Address, reg));
        when 5 =>  --  Address register indirect with displacement <(d16,Ax)>
           ext := self.get_ext;  --  Get extension word
           return (kind => memory_address, address =>
              self.get_reg(Address, reg) + sign_extend(ext));
        when 6 =>  --  Extension word modes
           return self.decode_ext(reg);
        when 7 =>  --  Special modes
           return self.decode_special(reg, size);
      end case;
   end;
   --
   --  Do post-processing, namely post-increment, if needed.
   --
   procedure post_EA(self : in out m68000; reg : uint3; mode : uint3;
      size : data_size) is
   begin
      if mode = 3 then  --  Address register indirect with post increment<(Ax)+>
        case size is
          when data_byte =>
             if reg = 7 then  --  Stack pointer needs to stay even
                self.set_reg(Address, reg, self.get_reg(Address, reg) + 2);
             else
                self.set_reg(Address, reg, self.get_reg(Address, reg) + 1);
             end if;
          when data_word =>
             self.set_reg(Address, reg, self.get_reg(Address, reg) + 2);
          when data_long =>
             self.set_reg(Address, reg, self.get_reg(Address, reg) + 4);
          when data_long_long =>
             self.set_reg(Address, reg, self.get_reg(Address, reg) + 8);
        end case;
      end if;
   end;
   --
   --  Decode extension word and return effective address
   --
   function decode_ext(self : in out m68000; reg : uint3) return operand is
     ea    : addr_bus := self.get_reg(Address, reg);
     scale : addr_bus := 1;
     temp  : word;
   begin
      ext := self.get_ext;
      if ext_brief.br_full then
         --
         --  Full extension word is only supported by CPU32 and M68020 or
         --  higher processors.
         --
         Ada.Text_IO.Put_Line("Full extension word is not supported yet.");
      else
         --
         --  Brief extension word is supported by the full M68000 family.
         --
         ea := ea + sign_extend(ext_brief.displacement);
         --
         --  Scale is used only on later processors
         --
         if ext_brief.word_long then
            ea := ea + self.get_reg(ext_brief.reg_mem, ext_brief.reg)*scale;
         else
            temp := self.get_reg(ext_brief.reg_mem, ext_brief.reg);
            ea := ea + sign_extend(temp)*scale;
         end if;
         return (kind => memory_address, address => ea);
      end if;
      return (kind => value, value => 0);
   end;
   --
   --  Decode group 7 (special addressing modes
   --  Note that depending on the mode, this may be an effective address
   --  or a value.  If <value> is true, then a value is returned in <data>,
   --  otherwise an address is returned in <ea>.
   --
   --  Reg  Addressing mode
   --   0   Absolute short address
   --   1   Absolute long address
   --   2   Program counter with displacement
   --   3   Program counter with index
   --   4   Immediate data (byte, word, or long)
   --  5-7  unused in 68000
   --
   function decode_special(self : in out m68000; reg : uint3; size : data_size)
         return operand is
      ea    : addr_bus := self.pc;
      scale : addr_bus := 1;
      ext1  : word;
      ext2  : word;
      ret_value : long;
   begin
      case reg is
         when 0 =>  --  Absolute short address
            return (kind => memory_address, address => sign_extend(self.get_ext));
         when 1 =>  --  Absolute long address
            ext1 := self.get_ext;
            ext2 := self.get_ext;
            return (kind => memory_address, address => long(ext1)*16#0001_0000# +
                                                       long(ext2));
         when 2 =>  --  Program counter with displacement
                    --  *** This will require some testing to ensure that
                    --  the PC value here matches the actual hardware PC.
                    --  Some adjustment may be needed.
            return (kind => memory_address, address => sign_extend(self.get_ext) + self.pc);
         when 3 =>  --  Program counter with index
                    --  *** This will require some testing to ensure that
                    --  the PC value here matches the actual hardware PC.
                    --  Some adjustment may be needed.
            ext := self.get_ext;
            if ext_brief.br_full then
               --
               --  Full extension word is only supported by CPU32 and M68020 or
               --  higher processors.
               --
               Ada.Text_IO.Put_Line("Full extension word is not supported yet.");
            else
               --
               --  Brief extension word is supported by the full M68000 family.
               --
               ea := ea + sign_extend(ext_brief.displacement);
               --
               --  Scale is used only on later processors
               --
               if ext_brief.word_long then
                  ea := ea + self.get_reg(ext_brief.reg_mem, ext_brief.reg)*scale;
               else
                  ext1 := self.get_reg(ext_brief.reg_mem, ext_brief.reg);
                  ea := ea + sign_extend(ext1)*scale;
               end if;
            end if;
         when 4 =>  --  Immediate data (byte, word, or long)
            ext1 := self.get_ext;
            case size is
              when data_byte =>
                 ret_value := long(ext1 and 16#00FF#);
              when data_word =>
                 ret_value := long(ext1);
              when data_long =>
                 ext2 := self.get_ext;
                 ret_value := long(ext1)*16#0001_0000# + long(ext2);
              when others =>
                 Ada.Text_IO.Put_Line("Unrecognized immediate data size");
                 ret_value := 0;
            end case;
            return (kind => value, value => ret_value);
         when others =>
            Ada.Text_IO.Put_Line("Unrecognized special mode register " & uint3'Image(reg));
           null;
      end case;
      return (kind => value, value => 0);
   end;
   --
   --  Get and set value at the effective address.  Note that some effective
   --  addresses cannot be set.
   --
   function get_ea(self : in out m68000; ea : operand; size : data_size) return long is
     b : byte;
     w : word;
   begin
      case ea.kind is
         when value =>
            return ea.value;
         when data_register =>
            return self.get_reg(Data, ea.data_reg);
         when address_register =>
            return self.get_reg(Address, ea.addr_reg);
         when memory_address =>
            Ada.Text_IO.Put_Line("Getting EA data from memory at " & toHex(ea.address));
            if size = data_byte then
               b := self.memory(ea.address);
               return long(b);
            elsif size = data_word then
               w := self.memory(ea.address);
               return long(w);
            else
               return self.memory(ea.address);
            end if;
      end case;
   end;
   --
   procedure set_ea(self : in out m68000; ea : operand; val : long;
      size : data_size) is
   begin
      case ea.kind is
         when value =>
            null;
         when data_register =>
            self.set_reg(Data, ea.data_reg, val);
         when address_register =>
            self.set_reg(Address, ea.addr_reg, val);
         when memory_address =>
            if size = data_byte then
               self.memory(ea.address, byte(val and 16#FF#));
            elsif size = data_word then
               self.memory(ea.address, word(val and 16#FFFF#));
            else
               self.memory(ea.address, val);
            end if;
      end case;
   end;
   --
   --  Set flags based on value (zero, sign, parity)
   --
   procedure setf(self : in out m68000; value : data_bus) is
   begin
      self.psw.zero := (value = 0);
   end;
   --
   --  All memory accesses should be routed through these functions so that they
   --  can do checks for memory-mapped I/O or shared memory.
   --
   --  Trim the address depending on the CPU model.  The 68008 has 20 bits,
   --  the 68000 has 24 bits, some others have the full 32 bits.  This will
   --  change as more models are implemented.
   --
   function trim_addr(addr : addr_bus; model : variants_m68000) return addr_bus is
   begin
     if model = var_68000 then
        return (addr and 16#00FF_FFFF#);
     elsif model = var_68008 then
        return (addr and 16#000F_FFFF#);
     else
        return addr;
     end if;
   end;
   --
   --  Set memory (need to add checks for odd access traps for some variants).
   --
   procedure memory(self : in out m68000; addr : addr_bus; value : long) is
      t_addr : addr_bus := trim_addr(addr, self.cpu_model);
   begin
      --
      --  Set LED register values
      --
      self.lr_addr := t_addr;
      self.lr_data := data_bus(value);
      --
      --  Set memory.  Optionally, checks for memory mapped I/O or shared memory
      --  or other special stuff can be added here.
      --
      self.mem(t_addr) := byte(value/16#0100_0000#);
      self.mem(t_addr + 1) := byte((value/16#0001_0000#) and 16#FF#);
      self.mem(t_addr + 2) := byte((value/16#0000_0100#) and 16#FF#);
      self.mem(t_addr + 3) := byte(value and 16#FF#);
   end;
   --
   procedure memory(self : in out m68000; addr : addr_bus; value : word) is
      t_addr : addr_bus := trim_addr(addr, self.cpu_model);
   begin
      --
      --  Set LED register values
      --
      self.lr_addr := t_addr;
      self.lr_data := data_bus(value);
      --
      --  Set memory.  Optionally, checks for memory mapped I/O or shared memory
      --  or other special stuff can be added here.
      --
      self.mem(t_addr) := byte((value/16#0000_0100#) and 16#FF#);
      self.mem(t_addr + 1) := byte(value and 16#FF#);
   end;
   --
   procedure memory(self : in out m68000; addr : addr_bus; value : byte) is
      t_addr : addr_bus := trim_addr(addr, self.cpu_model);
   begin
      --
      --  Set LED register values
      --
      self.lr_addr := t_addr;
      self.lr_data := data_bus(value);
      --
      --  Set memory.  Optionally, checks for memory mapped I/O or shared memory
      --  or other special stuff can be added here.
      --
      self.mem(t_addr) := byte(value and 16#FF#);
   end;
   --
   --  Read memory (need to add checks for odd access traps for some variants).
   --
   function memory(self : in out m68000; addr : addr_bus) return long is
      t : data_bus;
      t_addr : addr_bus := trim_addr(addr, self.cpu_model);
   begin
      --
      --  Read memory.  Optionally, checks for memory mapped I/O or shared memory
      --  or other special stuff can be added here.
      --
      t := data_bus(self.mem(t_addr))*16#0100_0000# +
           data_bus(self.mem(t_addr + 1))*16#0001_0000# +
           data_bus(self.mem(t_addr + 2))*16#0000_0100# +
           data_bus(self.mem(t_addr + 3));
      --
      --  Set LED register values
      --
      self.lr_addr := t_addr;
      self.lr_data := t;
      return long(t);
   end;
   --
   function memory(self : in out m68000; addr : addr_bus) return word is
      t : word;
      t_addr : addr_bus := trim_addr(addr, self.cpu_model);
   begin
      --
      --  Read memory.  Optionally, checks for memory mapped I/O or shared memory
      --  or other special stuff can be added here.
      --
      t := word(self.mem(t_addr))*16#0000_0100# +
           word(self.mem(t_addr + 1));
      --
      --  Set LED register values
      --
      self.lr_addr := t_addr;
      self.lr_data := data_bus(t);
      return t;
   end;
   --
   function memory(self : in out m68000; addr : addr_bus) return byte is
      t_addr : addr_bus := trim_addr(addr, self.cpu_model);
   begin
      --
      --  Set LED register values
      --
      self.lr_addr := t_addr;
      self.lr_data := data_bus(self.mem(t_addr));
      --
      --  Read memory.  Optionally, checks for memory mapped I/O or shared memory
      --  or other special stuff can be added here.
      --
      return self.mem(t_addr);
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
         Ada.Text_IO.Put_Line("I/O mapped I/O not used in 68000 family");
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
         if self.psw.super then
            self.ssp := self.ssp - 4;
            self.memory(self.ssp, data_bus(self.pc));
         else
            self.usp := self.usp - 4;
            self.memory(self.usp, data_bus(self.pc));
         end if;
         self.pc := temp_pc;
      else
         self.pc := self.pc + 2;
      end if;
   end;
   --
   procedure ret(self : in out m68000; go : Boolean) is
      temp_pc : data_bus;
   begin
      if go then
         if self.psw.super then
            temp_pc := self.memory(self.ssp);
            self.ssp := self.ssp + 4;
         else
            temp_pc := self.memory(self.usp);
            self.usp := self.usp + 4;
         end if;
         self.pc := addr_bus(temp_pc);
      end if;
   end;
   --
end BBS.Sim_CPU.m68000;