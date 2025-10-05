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
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package BBS.Sim_CPU.CPU.m68000 is
   --
   --  The simple Motorola 68000 simulator inheriting from Sim.simulator.
   --
   type m68000 is new simulator with private;
   --
   memory_size : constant addr_bus := 2**24;
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
   type variants_m68000 is (var_68000,
                            var_68008,
                            var_68010,
                            var_cpu32);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out m68000);
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out m68000);
   --
   --  Called to start simulator execution at a specific address.
   --
   overriding
   procedure start(self : in out m68000; addr : addr_bus);
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out m68000);
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out m68000);
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out m68000);
   --
   --  This loads data from a file specified by "name" into the simulator memory.
   --
   overriding
   procedure load(self : in out m68000; name : String);
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   overriding
   function name(self : in out m68000) return String is ("m68000");
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out m68000) return uint32;
   --
   --  Called to get number of variants
   --
   overriding
   function variants(self : in out m68000) return Natural is (2);
   --
   --  Called to get variant name
   --
   overriding
   function variant(self : in out m68000; v : natural) return String is
        (variants_m68000'Image(variants_m68000'Val(v)));
   --
   --  Called to get current variant index
   --
   overriding
   function variant(self : in out m68000) return Natural;
   --
   --  Called to set variant
   --
   overriding
   procedure variant(self : in out m68000; v : natural);
   --
   --  Interrupt status.  Returns simulator dependent status of interrupts
   --
   overriding
   function intStatus(self : in out m68000) return int32;
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out m68000; mem_addr : addr_bus;
                     data : data_bus);
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out m68000; mem_addr : addr_bus) return
     data_bus;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out m68000; num : uint32)
                     return String;
   --
   --  Called to get register value as a number
   --
   overriding
   function read_reg(self : in out m68000; num : uint32)
                     return data_bus;
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   overriding
   function read_reg(self : in out m68000; num : uint32)
                     return String;
   --
   --  Called to set register value
   --
   overriding
   procedure set_reg(self : in out m68000; num : uint32;
                     data : data_bus) is null;
   --
   --  Called to check if the CPU is halted
   --
   overriding
   function halted(self : in out m68000) return Boolean;
   --
   --  This clears the halted flag allowing processing to continue.
   --
   overriding
   procedure continue_proc(self : in out m68000);
   --
   --  Post a reset exception request
   --
   overriding
   procedure reset(self : in out m68000);
   --
   --  Post an interrupt exception
   --
   overriding
   procedure interrupt(self : in out m68000; data : long);
   --
   --  Enable/disable interrupt processing
   --
   overriding
   procedure interrupts(self : in out m68000; state : Boolean);
   --
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out m68000; addr : addr_bus);
   procedure clearBreak(self : in out m68000; addr : addr_bus);

private
   --
   --  Private definitions not for external use.
   --
   type reg_id is (reg_d0,
                   reg_d1,
                   reg_d2,
                   reg_d3,
                   reg_d4,
                   reg_d5,
                   reg_d6,
                   reg_d7,
                   reg_a0,
                   reg_a1,
                   reg_a2,
                   reg_a3,
                   reg_a4,
                   reg_a5,
                   reg_a6,
                   reg_usp,  --  User stack pointer
                   reg_ssp,  --  Supervisor stack pointer
                   reg_pc,
                   reg_psw);
   --
   type interrupt_mask is mod 2**3
      with size => 3;
   type status_word is record
      carry    : Boolean := False;
      overflow : Boolean := False;
      zero     : Boolean := False;
      negative : Boolean := False;
      extend   : Boolean := False;
      unused0  : Boolean := False;
      unused1  : Boolean := False;
      unused2  : Boolean := False;
      mask     : interrupt_mask := 0;
      unused3  : Boolean := False;
      unused4  : Boolean := False;
      super    : Boolean := False;
      trace0   : Boolean := False;
      trace1   : Boolean := False;
   end record;
   --
   for status_word use record
      carry    at 0 range  0 ..  0;
      overflow at 0 range  1 ..  1;
      zero     at 0 range  2 ..  2;
      negative at 0 range  3 ..  3;
      extend   at 0 range  4 ..  4;
      unused0  at 0 range  5 ..  5;
      unused1  at 0 range  6 ..  6;
      unused2  at 0 range  7 ..  7;
      mask     at 0 range  8 .. 10;
      unused3  at 0 range 11 .. 11;
      unused4  at 0 range 12 .. 12;
      super    at 0 range 13 .. 13;
      trace0   at 0 range 14 .. 14;
      trace1   at 0 range 15 .. 15;
   end record;
   --
   for status_word'Size use 16;
   --
   type mem_array is array (0 .. memory_size - 1) of byte;
   --
   type interrupt_queue is array (byte) of Boolean;
   type interrupt_priority is array (byte) of byte;
   --
   type m68000 is new simulator with record
      addr : addr_bus := 0;
      temp_addr : addr_bus := 0;
      d0  : long := 0;
      d1  : long := 0;
      d2  : long := 0;
      d3  : long := 0;
      d4  : long := 0;
      d5  : long := 0;
      d6  : long := 0;
      d7  : long := 0;
      a0  : long := 0;
      a1  : long := 0;
      a2  : long := 0;
      a3  : long := 0;
      a4  : long := 0;
      a5  : long := 0;
      a6  : long := 0;
      usp : long := 0;
      ssp : long := 0;
      pc  : long := 0;
      psw : status_word;
      check_except : Boolean := False;    --  Check for exceptions
      except_pend  : interrupt_queue;     --  Flags for each possible exception
      except_prio  : interrupt_priority;  --  Priority for each exception
      int_enable   : Boolean := True;     --  Enable/disable interrupt processing
      inst_pc      : long;  --  Address at start of instruction
      cpu_halt     : Boolean := False;
      break_enable : Boolean := False;
      bus_error    : Boolean := False;
      break_point  : addr_bus;
      cpu_model    : variants_m68000 := var_68000;
   end record;
   --
   --  Records and types for decoding various instruction formats.
   --  The records are all overlapped in memory to make is easier to get
   --  at the various fields for each instruction format.
   --
   --  Types for the various record fields
   --
   type uint2 is mod 2**2
      with size => 2;
   type reg_num is mod 2**3
      with size => 3;
   type mode_code is mod 2**3
      with size => 3;
   type uint3 is mod 2**3  --  Other 3 bit codes
      with size => 3;
   type uint4 is mod 2**4
      with size => 4;
   type prefix is mod 2**4
      with size => 4;
   type uint5 is mod 2**5    --  Five bit sub code
      with size => 5;
   type uint6 is mod 2**6
      with size => 6;
   type uint9 is mod 2**9
      with size => 9;
   type uint12 is mod 2**12
      with size => 12;
   type uint33 is mod 2**33
      with size => 33;
   type reg_type is (data, address)
      with size => 1;
   for reg_type use (data => 0, address => 1);
   type data_size is (data_byte, data_word, data_long, data_long_long)
      with size => 2;
   for data_size use (data_byte => 0, data_word => 1, data_long => 2,
        data_long_long => 3);
   --
   --  Record definitions for instruction decoding.  Most of the records
   --  and overlays have been moved to the package where they are used.
   --
   type step1 is record
       rest : uint12;
       pre  : prefix;  --  The prefix is used in the first stage of instruction decoding
   end record;
   for step1 use record
      rest at 0 range  0 .. 11;
      pre  at 0 range 12 .. 15;
   end record;
   --
   --  The instruction word is overlayed with various intruction formats
   --  to ease decoding.  The instruction formats are defined below.
   --
   instr  : aliased word;
   instr1 : step1  --  For first stage of instruction decoding
      with address => instr'Address;
   --
   --  Record definitions for extension words.  These are used for
   --  some of the addressing modes.
   --
   type extension_brief is record
      displacement : byte;
      br_full      : Boolean;    --  False for brief format
      scale        : data_size;  --  Used only for CPU32, M68020, M68030, M68040
      word_long    : Boolean;
      reg          : reg_num;
      reg_mem      : reg_type;
   end record;
   for extension_brief use record
      displacement at 0 range 0 .. 7;
      br_full      at 0 range 8 .. 8;
      scale        at 0 range 9 .. 10;
      word_long    at 0 range 11 .. 11;
      reg          at 0 range 12 .. 14;
      reg_mem      at 0 range 15 .. 15;
   end record;
   type extension_full is record  --  Used only for M68020, M68030, M68040
      index_sel : uint3;
      unused0   : Boolean;
      bd_size   : uint2;
      index_sup : Boolean;
      base_sub  : Boolean;
      br_full   : Boolean;    --  True for full format
      scale     : data_size;
      index_size : Boolean;
      reg       : reg_num;
      reg_mem   : reg_type;
   end record;
   for extension_full use record
      index_sel at 0 range 0 .. 2;
      unused0   at 0 range 3 .. 3;
      bd_size   at 0 range 4 .. 5;
      index_sup at 0 range 6 .. 6;
      base_sub  at 0 range 7 .. 7;
      br_full   at 0 range 8 .. 8;
      scale     at 0 range 9 .. 10;
      index_size at 0 range 11 .. 11;
      reg       at 0 range 12 .. 14;
      reg_mem   at 0 range 15 .. 15;
   end record;
   ext       : aliased word;
   ext_brief : extension_brief with address => ext'Address;
   ext_full  : extension_full  with address => ext'Address;
   --
   --  Operands.  They can be a data register, address register, memory
   --  address, or a value.
   --
   type operand_kind is (value, data_register, address_register, memory_address);
   type operand (kind : operand_kind) is record
      reg  : reg_num;
      mode : mode_code;
      size : data_size;
      case kind is
         when value =>
            value : long;
         when data_register =>
            null;
         when address_register =>
            null;
         when memory_address =>
            address : addr_bus;
      end case;
   end record;

   --
   --  Code for the instruction processing.
   --
   function get_next(self : in out m68000) return word;
   function get_ext(self : in out m68000) return word;
   procedure check_intr(self : in out m68000) is null;
   procedure decode(self : in out m68000);
   --
   --  Get EA.  Decode the register, addressing modes, and extension
   --  words to get the effective address.  Also does any pre-processing,
   --  namely pre-decrement, as appropriate.
   --
   function get_EA(self : in out m68000; reg : reg_num; mode : mode_code;
      size : data_size) return operand;
   --
   --  Do post-processing, namely post-increment, if needed.
   --
   procedure post_EA(self : in out m68000; ea : operand);
   --
   --  Decode extension word and return effective address
   --
   function decode_ext(self : in out m68000; reg : reg_num; size : data_size) return operand;
   --
   --  Decode group 7 (special) addressing modes
   --  Note that depending on the mode, this may be an effective address
   --  or a value.  If <value> is true, then a value is returned in <data>,
   --  otherwise an address is returned in <ea>.
   --
   function decode_special(self : in out m68000; reg : reg_num; size : data_size) return operand;
   --
   --  Get and set value at the effective address.  Note that some effective
   --  addresses cannot be set.
   --
   function get_ea(self : in out m68000; ea : operand) return long;
   procedure set_ea(self : in out m68000; ea : operand; val : long);
   --
   --  Sign extension
   --
   function sign_extend(d : byte) return long;
   function sign_extend(d : word) return long;
   --
   --  MSB and LSB
   --
   function msb(b : byte) return Boolean is ((b and 16#80#) = 16#80#);
   function msb(w : word) return Boolean is ((w and 16#8000#) = 16#8000#);
   function msb(l : long) return Boolean is ((l and 16#8000_0000#) = 16#8000_0000#);
   function lsb(b : byte) return Boolean is ((b and 1) = 1);
   function lsb(w : word) return Boolean is ((w and 1) = 1);
   function lsb(l : long) return Boolean is ((l and 1) = 1);
   --
   --  Register opertions
   --
   function get_regb(self : in out m68000; data_addr : reg_type; reg_index : reg_num) return byte;
   function get_regw(self : in out m68000; data_addr : reg_type; reg_index : reg_num) return word;
   function get_regl(self : in out m68000; data_addr : reg_type; reg_index : reg_num) return long;
   procedure set_regb(self : in out m68000; data_addr : reg_type; reg_index : reg_num; value : byte);
   procedure set_regw(self : in out m68000; data_addr : reg_type; reg_index : reg_num; value : word);
   procedure set_regl(self : in out m68000; data_addr : reg_type; reg_index : reg_num; value : long);
   --
   --  All memory accesses should be routed through these functions so that they
   --  can do checks for memory-mapped I/O or shared memory.
   --
   --  Update for access mode instruction/data/etc.
   --
   procedure memory(self : in out m68000; addr : addr_bus; value : byte);
   procedure memory(self : in out m68000; addr : addr_bus; value : word);
   procedure memory(self : in out m68000; addr : addr_bus; value : long);
   function memory(self : in out m68000; addr : addr_bus) return byte;
   function memory(self : in out m68000; addr : addr_bus) return word;
   function memory(self : in out m68000; addr : addr_bus) return long;
   procedure memb(self : in out m68000; addr : addr_bus; value : byte);
   function memb(self : in out m68000; addr : addr_bus) return byte;
   --
   --  Push and pop long or word to the user or system stack
   --
   procedure push(self : in out m68000; stack : Boolean; value : long);
   procedure push(self : in out m68000; stack : Boolean; value : word);
   function pop(self : in out m68000; stack : Boolean) return long;
   function pop(self : in out m68000; stack : Boolean) return word;
   --
   --  Records for instruction formats for decoding instructions.  They have been
   --  spread out among the various _line* packages.  This collects them all into
   --  one place so that reuse can be applied for common formats.
   --
   type fmt_move is record
      reg_y  : reg_num;
      mode_y : mode_code;
      mode_x : mode_code;
      reg_x  : reg_num;
      pre    : prefix;  --  1 for move byte, 2 for move long, 3 For move word
   end record;
   for fmt_move use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      mode_x at 0 range 6 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 ..15;
   end record;
   --
   instr_move : fmt_move  --  Decode MOVE instructions
     with address => instr'Address;
   --
   type fmt_2op_size is record
      reg_y   : reg_num;
      reg_mem : reg_type;
      code1   : uint2;    --  0 For ADDX/SUBX instruction
      size    : data_size;
      code2   : Boolean;  --  True for ADDX/SUBX instruction
      reg_x   : reg_num;
      pre     : prefix;  --  9 for SUBX, d for ADDX
   end record;
   for fmt_2op_size use record
      reg_y   at 0 range 0 .. 2;
      reg_mem at 0 range 3 .. 3;
      code1   at 0 range 4 .. 5;
      size    at 0 range 6 .. 7;
      code2   at 0 range 8 .. 8;
      reg_x   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   --
   instr_2op_size : fmt_2op_size  --  Decode SUBX/ADDX/SUBX instructions
     with address => instr'Address;
   --
   type fmt_cmpm is record
      reg_y : reg_num;
      code1 : uint3;
      size  : data_size;
      code2 : Boolean;
      reg_x : reg_num;
      pre   : prefix;  --  b
   end record;
   for fmt_cmpm use record
      reg_y at 0 range 0 .. 2;
      code1 at 0 range 3 .. 5;
      size  at 0 range 6 .. 7;
      code2 at 0 range 8 .. 8;
      reg_x at 0 range 9 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   instr_cmpm : fmt_cmpm  --  Decode CMPM instructions
     with address => instr'Address;
   --
   --  For prefix 0, code specifies which bit instruction
   --  For prefix 4, code 7 is for LEA
   --  For prefix 8, code 0, 1, 2, 4, 5, 6 are for OR, 3 and 7 are for DIVS/DIVU
   --  For prefix 9, all code are used for SUB
   --  For prefix b, all code are used for CMP
   --  For prefix c, code 0, 1, 2, 4, 5, and 6 are for AND, 3 and 7 are for MULS/MULU
   --  For prefix d, all code are used for ADD
   type fmt_2op is record
      reg_y  : reg_num;
      mode_y : mode_code;
      code   : uint3;  --  0, 1, 2, 4, 5, and 6 for AND instruction, 3 and 7 for MULS/MULU
      reg_x  : reg_num;
      pre    : prefix;  --  9 for SUB, b for CMP, c for AND & MULS/MULU, d for ADD
   end record;
   for fmt_2op use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      code   at 0 range 6 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 .. 15;
   end record;
   --
   instr_2op : fmt_2op  --  Decode 2 operand instructions
      with address => instr'Address;
   --
   type fmt_regy is record
      reg_y : reg_num;
      code  : uint9;  --  16#108# for SWAP, 16#1ca# for LINK 16#1cb# for UNLK
      pre   : prefix;  --  4
   end record;
   for fmt_regy use record
      reg_y at 0 range 0 .. 2;
      code  at 0 range 3 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   --
   instr_regy : fmt_regy
     with address => instr'Address;
   --
   --  For prefix 0, code 0 for ORI, 2 for ANDI, 4 for SUBI, 6 for ADDI, A for EORI, C for CMPI
   --  For prefix 4, code 2 for CLR, 4 for NEG, 0 for NEGX, 6 for NOT, A for TST
   type fmt_1op_size is record  --  Immediate instructions
     reg_y  : reg_num;
     mode_y : mode_code;
     size   : data_size;
     code   : uint4;  --  0 for ORI, 2 for ANDI, 4 for SUBI
                      --  6 for ADDI, A for EORI, C for CMPI
     pre    : prefix;  --  0
   end record;
   for fmt_1op_size use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      size   at 0 range 6 .. 7;
      code   at 0 range 8 .. 11;
      pre    at 0 range 12 ..15;
   end record;
   --
   instr_1op_size : fmt_1op_size
     with address => instr'Address;
   --
   type step_1ea is record  --  One effective address
      reg_y  : reg_num;
      mode_y : mode_code;
      code   : uint6;  --  16#3B# for JMP, 16#3A# for JSR,
                       --  16#13# for MOVE to CCR, 16#1b# for MOVE to SR,
                       --  16#03# for MOVE from SR, 16#0B for MOVE from CCR,
                       --  16#20# for NBCD, 16#21# for PEA, 16#2b# for TAS
      pre    : prefix;  --  4
   end record;
   for step_1ea use record
      reg_y   at 0 range 0 .. 2;
      mode_y  at 0 range 3 .. 5;
      code    at 0 range 6 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   --
   instr_1ea : step_1ea
     with address => instr'Address;
   --
   type fmt_bcd is record  --  BCD Subtract
      reg_y   : reg_num;
      reg_mem : reg_type;
      code    : uint5;  --  16#10# for SBCD instruction
      reg_x   : reg_num;
      pre     : prefix;  --  8
   end record;
   for fmt_bcd use record
      reg_y   at 0 range 0 .. 2;
      reg_mem at 0 range 3 .. 3;
      code    at 0 range 4 .. 8;
      reg_x   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   --
   instr_bcd : fmt_bcd
      with address => instr'Address;
   --
   --  From line 0
   bit_pos : array (long range 0 .. 31) of long := (
               16#0000_0001#,
               16#0000_0002#,
               16#0000_0004#,
               16#0000_0008#,
               16#0000_0010#,
               16#0000_0020#,
               16#0000_0040#,
               16#0000_0080#,
               16#0000_0100#,
               16#0000_0200#,
               16#0000_0400#,
               16#0000_0800#,
               16#0000_1000#,
               16#0000_2000#,
               16#0000_4000#,
               16#0000_8000#,
               16#0001_0000#,
               16#0002_0000#,
               16#0004_0000#,
               16#0008_0000#,
               16#0010_0000#,
               16#0020_0000#,
               16#0040_0000#,
               16#0080_0000#,
               16#0100_0000#,
               16#0200_0000#,
               16#0400_0000#,
               16#0800_0000#,
               16#1000_0000#,
               16#2000_0000#,
               16#4000_0000#,
               16#8000_0000#);
   --
   --
   type step_movep is record
      reg_y : reg_num;
      code  : uint3;
      mode  : uint3;
      reg_x : reg_num;
      pre   : prefix;  --  0
   end record;
   for step_movep use record
      reg_y at 0 range 0 .. 2;
      code  at 0 range 3 .. 5;
      mode  at 0 range 6 .. 8;
      reg_x at 0 range 9 ..11;
      pre   at 0 range 12 .. 15;
   end record;
   --  From line 1 (fmt_move)
   --  From line 2 (fmt_move)
   --  From line 3 (fmt_move)
   --  From line 4
   type step_chk is record
      reg_y   : reg_num;
      mode_y  : mode_code;
      code    : Boolean;
      size    : uint2;  --  Uses different coding from other size fields
      reg_x   : reg_num;
      pre     : prefix;  --  4
   end record;
   for step_chk use record
      reg_y   at 0 range 0 .. 2;
      mode_y  at 0 range 3 .. 5;
      code    at 0 range 6 .. 6;
      size    at 0 range 7 .. 8;
      reg_x   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   --
   type step_ext is record
      reg_y : reg_num;
      code0 : uint3;  --  0 for EXT
      mode  : uint3;  --  2 & 3 for EXT (later processors allow 7)
      code1 : uint3;  --  4 for EXT
      pre   : prefix;  --  4
   end record;
   for step_ext use record
      reg_y at 0 range 0 .. 2;
      code0 at 0 range 3 .. 5;
      mode  at 0 range 6 .. 8;
      code1 at 0 range 9 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   --
   type step_musp is record
      reg_y : reg_num;
      dir   : Boolean;
      code  : uint8;  --  16#e6# for MOVE to/from USP
      pre   : prefix;  --  4
   end record;
   for step_musp use record
      reg_y at 0 range 0 .. 2;
      dir   at 0 range 3 .. 3;
      code  at 0 range 4 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   --
   type step_movem is record
      reg_y  : reg_num;
      mode_y : mode_code;
      size   : Boolean;  --  True = long, False = word
      code0  : uint3;    --  1 for MOVEM
      dir    : Boolean;  --  True = mem to reg, False = reg to mem
      code1  : Boolean;  --  True for MOVEM
      pre    : prefix;  --  4
   end record;
   --
   for step_movem use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      size   at 0 range 6 .. 6;
      code0  at 0 range 7 .. 9;
      dir    at 0 range 10 .. 10;
      code1  at 0 range 11 .. 11;
      pre    at 0 range 12 .. 15;
   end record;
   --
   type step_trap is record
      vect : uint4;
      code : uint8;  --  16#e4# for TRAP
      pre  : prefix;  --  4
   end record;
   for step_trap use record
      vect at 0 range 0 .. 3;
      code at 0 range 4 .. 11;
      pre  at 0 range 12 .. 15;
   end record;
   --  From line 5
   type step_addq is record
     reg_y  : reg_num;
     mode_y : mode_code;
     size   : data_size;
     code   : Boolean;  --  False for ADDQ instruction, True for SUBQ
     data   : uint3;
     pre    : prefix;  --  5
   end record;
   for step_addq use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      size   at 0 range 6 .. 7;
      code   at 0 range 8 .. 8;
      data   at 0 range 9 .. 11;
      pre    at 0 range 12 ..15;
   end record;
   --
   type step_dbcc is record
     reg_y : reg_num;
     code  : uint5;  --  16#19# for DBcc instructions
     cond  : uint4;
     pre   : prefix;  --  5
   end record;
   for step_dbcc use record
      reg_y at 0 range 0 .. 2;
      code  at 0 range 3 .. 7;
      cond  at 0 range 8 .. 11;
      pre   at 0 range 12 ..15;
   end record;
   --
   type step_scc is record
     reg_y  : reg_num;
     mode_y : mode_code;
     code   : uint2;  --  3 for Scc instructions
     cond   : uint4;
     pre    : prefix;  --  5
   end record;
   for step_scc use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      code   at 0 range 6 .. 7;
      cond   at 0 range 8 .. 11;
      pre    at 0 range 12 ..15;
   end record;
   --  From line 6
   type step_bcc is record  --  For branch instructions
      disp : byte;
      cond : uint4;
      pre  : prefix;  --  6
   end record;
   for step_bcc use record
      disp at 0 range 0 .. 7;
      cond at 0 range 8 .. 11;
      pre  at 0 range 12 .. 15;
   end record;
   --  From line 7
   type step_moveq is record  --  For move quick instructions
      data : byte;
      code : Boolean;  --  False for MOVEQ
      reg  : reg_num;
      pre  : prefix;  --  7
   end record;
   for step_moveq use record
      data at 0 range 0 .. 7;
      code at 0 range 8 .. 8;
      reg  at 0 range 9 .. 11;
      pre  at 0 range 12 .. 15;
   end record;
   --  From line 8
   --  From line 9
   --  From line a (used by Apple in MacOS classic, much to the chagrin of Motorola)
   --  From line b
   --  From line c
   --
   type step_exg is record
      reg_y  : reg_num;
      code   : uint5;    --  8, 9, and 17 for EXG instruction
      code1  : Boolean;  --  True for EXG instruction
      reg_x  : reg_num;
      pre    : prefix;  --  c
   end record;
   for step_exg use record
      reg_y  at 0 range 0 .. 2;
      code   at 0 range 3 .. 7;
      code1  at 0 range 8 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 .. 15;
   end record;
   --  From line d
   --  From line e
   type step_aslr1 is record
      reg_y   : reg_num;
      mode_y  : mode_code;
      code1   : uint2;  --  3 for ASL/ASR/LSL/LSR, ROL/ROR, ROXL/ROXR 1 operand
      dir     : Boolean;
      code2   : uint3;  --  0 for ASL/ASR, 2 for ROXL/ROXR, 3 for ROL/ROR,
                        --  1 for LSL/LSR 1 operand
      pre     : prefix;  --  e
   end record;
   for step_aslr1 use record
      reg_y   at 0 range 0 .. 2;
      mode_y  at 0 range 3 .. 5;
      code1   at 0 range 6 .. 7;
      dir     at 0 range 8 .. 8;
      code2   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   --
   type step_aslr2 is record
      reg_y : reg_num;
      code  : uint2;  --  0 for ASL/ASR 2 operand, 1 for LSL/LSR,
                      --  2 for ROXL/ROXR, 3 for ROL/ROR
      reg   : Boolean;
      size  : data_size;
      dir   : Boolean;
      count : uint3;
      pre   : prefix;  --  e
   end record;
   for step_aslr2 use record
      reg_y at 0 range 0 .. 2;
      code  at 0 range 3 .. 4;
      reg   at 0 range 5 .. 5;
      size  at 0 range 6 .. 7;
      dir   at 0 range 8 .. 8;
      count at 0 range 9 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   --  From line f (used by Motorola for coprocessor and extensions)
   --
end BBS.Sim_CPU.CPU.m68000;
