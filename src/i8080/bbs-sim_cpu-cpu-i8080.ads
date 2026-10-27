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
with BBS.Sim_CPU.io;
with BBS.Sim_CPU.bus;
package BBS.Sim_CPU.CPU.i8080 is
   --
   --  The simple Intel 8080 simulator inheriting from Sim.simulator.
   --
   type i8080 is new simulator with private;
   --
   --  The trace level is interpreted as follows for this simulator:
   --  Bit  Use
   --   0   List instructions being traced
   --   1   List I/O operations
   --   2   Unused
   --   3   Unused
   --   4   Unused
   --   5   Unused
   --   6   Unused
   --   7   Unused
   --
   --  Variants of processor
   --
   type variants_i8080 is (var_8080,
                           var_8085,
                           var_z80);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out i8080);
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out i8080);
   --
   --  Called to start simulator execution at a specific address.
   --
   overriding
   procedure start(self : in out i8080; addr : addr_bus);
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out i8080);
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out i8080);
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out i8080);
   --
   --  This loads data from a file specified by "name" into the simulator memory.
   --
   overriding
   procedure load(self : in out i8080; name : String);
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   overriding
   function name(self : in out i8080) return String is ("i8080");
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out i8080) return uint32;
   --
   --  Called to get number of variants
   --
   overriding
   function variants(self : in out i8080) return Natural is (3);
   --
   --  Called to get variant name
   --
   overriding
   function variant(self : in out i8080; v : natural) return String;
   --
   --  Called to get current variant index
   --
   overriding
   function variant(self : in out i8080) return Natural;
   --
   --  Called to set variant
   --
   overriding
   procedure variant(self : in out i8080; v : natural);
   --
   --  Interrupt status.  Returns simulator dependent status of interrupts
   --
   overriding
   function intStatus(self : in out i8080) return int32;
   --
   --  Input/Output debugging
   --
   overriding
   function lastOutAddr(self : in out i8080) return addr_bus;
   overriding
   function lastOutData(self : in out i8080) return data_bus;
   overriding
   procedure overrideIn(self : in out i8080; addr : in addr_bus; data : in data_bus);
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out i8080; mem_addr : addr_bus;
                     data : data_bus);
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out i8080; mem_addr : addr_bus)
      return data_bus;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out i8080; num : uint32)
      return String;
   --
   --  Called to get register value as a number
   --
   overriding
   function read_reg(self : in out i8080; num : uint32)
      return data_bus;
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   overriding
   function read_reg(self : in out i8080; num : uint32)
      return String;
   --
   --  Called to set register value
   --
   overriding
   procedure set_reg(self : in out i8080; num : uint32;
                     data : data_bus) is null;
   --
   --  Force a simulator to enter a halt state (if implemented).  Can be used for
   --  some error conditions.
   --
   overriding
   procedure halt(self : in out i8080);
   --
   --  Called to check if the CPU is halted
   --
   overriding
   function halted(self : in out i8080) return Boolean;
   --
   --  This clears the halted flag allowing processing to continue.
   --
   overriding
   procedure continue_proc(self : in out i8080);
   --
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out i8080; addr : addr_bus);
   procedure clearBreak(self : in out i8080; addr : addr_bus);
   --
   --  Unimplemented instruction response
   --
   procedure unimplemented(self : in out i8080; addr : word; data : byte);
   --
   --  Post an interrupt exception
   --  Constants are provided for certain hardware interrupts
   --
   Z80_NMI : constant long := 16#FFFF_FFFF#;
   i85_5_5 : constant long := 16#FFFF_FF55#;
   i85_6_5 : constant long := 16#FFFF_FF65#;
   i85_7_5 : constant long := 16#FFFF_FF75#;
   i85_TRAP : constant long := 16#FFFF_FFFF#;
   --
   overriding
   procedure interrupt(self : in out i8080; data : long);
   --
   type status_word is record
      carry   : Boolean := False;
      addsub  : Boolean := False;  --  Add/Subtract for Z-80, unused otherwise
      parity  : Boolean := False;  --  Z-80 also uses this for overflow
      unused1 : Boolean := True;
      aux_carry : Boolean := False;
      unused2 : Boolean := True;
      zero    : Boolean := False;
      sign    : Boolean := False;
   end record;
   --
   for status_word use record
      carry   at 0 range 0 .. 0;
      addsub  at 0 range 1 .. 1;
      parity  at 0 range 2 .. 2;
      unused1 at 0 range 3 .. 3;
      aux_carry at 0 range 4 .. 4;
      unused2 at 0 range 5 .. 5;
      zero    at 0 range 6 .. 6;
      sign    at 0 range 7 .. 7;
   end record;
   --
   for status_word'Size use 8;
   --
private
   --
   type reg_id is (reg_a,     --  Accumulator (8 bits)
                   reg_psw,   --  Status word
                   reg_b,     --  B register (8 bits)
                   reg_c,     --  C register (8 bits)
                   reg_bc,    --  B & C registers (16 bits)
                   reg_d,     --  D register (8 bits)
                   reg_e,     --  E register (8 bits)
                   reg_de,    --  D & E registers (16 bits)
                   reg_h,     --  H register (8 bits)
                   reg_l,     --  L register (8 bits)
                   reg_hl,    --  H & L register (16 bits)
                   reg_sp,    --  Stack pointer (16 bits)
                   reg_pc,    --  Program counter (16 bits)
                   --  Z-80 only registers below
                   reg_ap,    --  Accumulator'
                   reg_pswp,  --  Status word'
                   reg_bp,    --  B' register (8 bits)
                   reg_cp,    --  C' register (8 bits)
                   reg_bcp,   --  B' & C' registers (16 bits)
                   reg_dp,    --  D' register (8 bits)
                   reg_ep,    --  E' register (8 bits)
                   reg_dep,   --  D' & E' registers (16 bits)
                   reg_hp,    --  H' register (8 bits)
                   reg_lp,    --  L' register (8 bits)
                   reg_hlp,   --  H' & L' register (16 bits)
                   reg_ix,    --  Index register X (16 bits)
                   reg_iy,    --  Index register Y (16 bits)
                   reg_i,     --  Interrupt base register (8 bits)
                   reg_r      --  Memory refresh register (8 bits)
                   );
   --
   type pointer is (use_hl, use_ix, use_iy);
   --
   type i8080 is new simulator with record
      addr : word := 0;
      temp_addr : word := 0;
      a   : byte := 0;
      f   : status_word;  --  Flags (processor status word)
      b   : byte := 0;
      c   : byte := 0;
      d   : byte := 0;
      e   : byte := 0;
      h   : byte := 0;
      l   : byte := 0;
      sp  : word := 0;
      pc  : word := 0;
      ap  : byte := 0;    --  A', Z-80 specific
      fp  : status_word;  --  F', Z-80 specific
      bp  : byte := 0;    --  B', Z-80 specific
      cp  : byte := 0;    --  C', Z-80 specific
      dp  : byte := 0;    --  D', Z-80 specific
      ep  : byte := 0;    --  E', Z-80 specific
      hp  : byte := 0;    --  H', Z-80 specific
      lp  : byte := 0;    --  L', Z-80 specific
      ix  : word := 0;    --  IX, Z-80 specific
      iy  : word := 0;    --  IY, Z-80 specific
      i   : byte := 0;    --  Interrupt page register, Z-80 specific
      r   : byte := 0;    --  Refresh register, Z-80 specific
      ptr : pointer := use_hl;
      intr         : Boolean := False;  --  Interrupt is pending
      cpu_halt     : Boolean := False;
      int_enable   : Boolean := False;  --  IFF1 for Z-80
      ie_pending   : Boolean := False;  --  Is interrupt enable pending?
      iff2         : Boolean := False;
      prefix       : Boolean := False;  --  Z80 instruction prefix in progress
      int_mode     : byte := 0;  -- Z-80 interrupt mode
      int_posted   : data_bus;
      m7_5         : Boolean := False;  --  Mask 8085 rst7.5 interrupt
      m6_5         : Boolean := False;  --  Mask 8085 rst6.5 interrupt
      m5_5         : Boolean := False;  --  Mask 8085 rst5.5 interrupt
      break_enable : Boolean := False;
      break_point  : word;
      last_out_addr : addr_bus := 0;
      last_out_data : data_bus := 0;
      in_override  : Boolean := False;
      in_over_addr : addr_bus := 0;
      in_over_data : data_bus := 0;
      cpu_model    : variants_i8080 := var_8080;
   end record;
   --
   subtype reg8_index is byte range 0 .. 7;
   subtype reg16_index is byte range 0 .. 3;
   reg8_name  : constant array (reg8_index) of String(1 .. 1) := ("B", "C", "D", "E", "H", "L", "M", "A");
   reg16_name : constant array (reg16_index) of String (1 .. 2) := ("BC", "DE", "HL", "SP");
   --
   --  Code for the instruction processing.
   --
   procedure decode(self : in out i8080);
   function get_next(self : in out i8080) return byte;
   procedure check_intr(self : in out i8080) is null;
   --
   --  For Z80, the DD and FD instruction prefixes set a flag to use the
   --  IX or IY index registers in place of HL.  Except this doesn't happen
   --  in a move instruction when one of the arguments is index mode.  In
   --  this case, H or L is used.  This is actually undocumented.
   --
   --  The override flag is set True to indicate this condition.
   --
   procedure reg8(self : in out i8080; reg : reg8_index; value : byte; override : Boolean);
   function  reg8(self : in out i8080; reg : reg8_index; override : Boolean) return byte;
   procedure incr8(self : in out i8080; reg : reg8_index);
   procedure decr8(self : in out i8080; reg : reg8_index);
   --
   --  LXI and PUSH/POP have different reg16 indices.  V = 0 selects the LXI
   --  version and V = 1 selects the PUSH/POP version.  Override is used to
   --  prevent selection of IX/IY for Z-80 ED prefix instructions.
   --
   procedure reg16(self : in out i8080; reg : reg16_index; value : word; use_sp : Boolean);
   function reg16(self : in out i8080; reg : reg16_index; use_sp : Boolean) return word;
   procedure setf(self : in out i8080; value : byte);
   function addf(self : in out i8080; v1 : byte; v2 : byte; c : Boolean) return byte;
   function subf(self : in out i8080; v1 : byte; v2 : byte; c : Boolean) return byte;
   procedure mod16(self  : in out i8080; reg : reg16_index; dir : Integer);
   --
   --  Handle I/O port accesses
   --
   procedure port(self : in out i8080; addr : byte; value : byte);
   function port(self : in out i8080; addr : byte) return byte;
   --
   --  Common code for Jump, Call, and Return
   --
   procedure jump(self : in out i8080);
   procedure call(self : in out i8080; go : Boolean);
   procedure ret(self : in out i8080);
   --
   --  Other utility functions
   --
   function sign_extend(t8 : byte) return word;
   --
   --  Put next address for tracing
   --
   function nextAddr(self : in out i8080) return String;
   --
   --  Utilities to support instruction tracing
   --
   --  Put next byte for tracing
   --
   function nextByte(self : in out i8080) return String;
   --
   --  Print selected Z80 index register for tracing
   --
   function indexMode(self : in out i8080) return String;
   function indexRegW(self : in out i8080) return String;
   function indexRegBh(self : in out i8080) return String;
   function indexRegBl(self : in out i8080) return String;
   --
   --  Constants for instruction register decoding.
   --
   REG8_B : constant reg8_index := 0;
   REG8_C : constant reg8_index := 1;
   REG8_D : constant reg8_index := 2;
   REG8_E : constant reg8_index := 3;
   REG8_H : constant reg8_index := 4;
   REG8_L : constant reg8_index := 5;
   REG8_M : constant reg8_index := 6;
   REG8_A : constant reg8_index := 7;
   --
   REG16_BC : constant reg16_index := 0;
   REG16_DE : constant reg16_index := 1;
   REG16_HL : constant reg16_index := 2;
   REG16_SP : constant reg16_index := 3;
   --
   --  Strings for base name and variants
   --
   base : aliased constant String := "8080";
   var1 : aliased constant String := "8080";
   var2 : aliased constant String := "8085";
   var3 : aliased constant String := "Z80";
   --
end BBS.Sim_CPU.CPU.i8080;
