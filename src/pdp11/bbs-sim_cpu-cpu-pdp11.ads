--
--  Author: Brent Seidel
--  Date: 28-Nov-2025
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
package BBS.Sim_CPU.CPU.PDP11 is
   --
   --  The simple Dec PDP-11 simulator inheriting from Sim.simulator.
   --
   type pdp11 is new simulator with private;
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
   --  Note that some model numbers are for OEM versions of the processor
   --  and are identical to another model number.  The OEM versions are not
   --  considered to be separate variants.  It is likely that only some models
   --  will be included in the simulation.
   --
   --    DEC         OEM      Processor
   --  PDP-11/20 = PDP-11/15  KA11/KC11
   --  PDP-11/10 = PDP-11/05  KD11B
   --  PDP-11/40 = PDP-11/35  KD11A
   --
   --  In addition, some CPUs are given different model number depending on whether
   --  they have a Unibus or QBus.
   --
   --  Unibus      QBus
   --  PDP-11/24 = PDP-11/23
   --
   type variants_pdp11 is (var_1110,
                           var_1104);
   --
   --  Features for each processor.  Eventually, an array of these may be created
   --  with entries for each supported processor.  This may also be extended to
   --  include information for other differences between various models.
   --
   --  Extra instructions (not EIS) are included on all but the earliest models
   --  (probably not available on PDP-11/04, /05, /10, /15, and /20).  Instructions in
   --  this set are:
   --  SXT, XOR, MARK, SOB, RTT, MFPI, MTPI, SPL (maybe specific to PDP-11/45)
   --
   --  Extended instruction set is an option for PDP-11/35, /40, and /03 CPUs and
   --  standard on later models (probably not available on PDP-11/04, /05, /10, /15,
   --  and /20).  Instuctions in this set are:
   --  MUL, DIV, ASH, and ASHC
   --
   --  FIS is available only on PDP-11/34 and /40.  Instructions in this set are:
   --  FADD, FSUB, FMUL, and FDIV
   --
   --  FPP is available on PDP-11/45, /70 and probably at least an option on all
   --  later models.  Instructions in this set are:
   --  CFCC, SETF, SETI, SETDF, SETL, LDFPS, STFPS, STST, CLR[FD], TST[FD],
   --  ABS[FD], NEG[FD], MUL[FD], MOD[FD], ADD[FD], LD[FD], SUB[FD], CMP[FD],
   --  ST[FD], DIV[FD], STEXP, STC[FD][IL], STCFD, STCDF, LDEXP, LDC[IL][FD],
   --  LDCDF, and LDCFD
   --
   --  CIS is available on PDP-11/23, /24, /44, and /74?.
   --
   type features is record
      has_extra : Boolean;  --  Has extra instructions (not EIS)
      has_EIS   : Boolean;  --  Has extended instruction set (EIS)
      has_FIS   : Boolean;  --  Has floating instruction set
      has_FPP   : Boolean;  --  Has floating point processor
      has_CIS   : Boolean;  --  Has commercial instruction set
   end record;
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out pdp11);
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out pdp11);
   --
   --  Called to start simulator execution at a specific address.
   --
   overriding
   procedure start(self : in out pdp11; addr : addr_bus);
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out pdp11);
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out pdp11);
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out pdp11);
   --
   --  This loads data from a file specified by "name" into the simulator memory.
   --
   overriding
   procedure load(self : in out pdp11; name : String);
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   overriding
   function name(self : in out pdp11) return String is ("PDP-11");
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out pdp11) return uint32;
   --
   --  Called to get number of variants
   --
   overriding
   function variants(self : in out pdp11) return Natural is (2);
   --
   --  Called to get variant name
   --
   overriding
   function variant(self : in out pdp11; v : natural) return String is
        (variants_pdp11'Image(variants_pdp11'Val(v)));
   --
   --  Called to get current variant index
   --
   overriding
   function variant(self : in out pdp11) return Natural;
   --
   --  Called to set variant
   --
   overriding
   procedure variant(self : in out pdp11; v : natural);
   --
   --  Interrupt status.  Returns simulator dependent status of interrupts
   --
   overriding
   function intStatus(self : in out pdp11) return int32;
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out pdp11; mem_addr : addr_bus;
                     data : data_bus);
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out pdp11; mem_addr : addr_bus) return
     data_bus;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out pdp11; num : uint32)
                     return String;
   --
   --  Called to get register value as a number
   --
   overriding
   function read_reg(self : in out pdp11; num : uint32)
                     return data_bus;
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   overriding
   function read_reg(self : in out pdp11; num : uint32)
                     return String;
   --
   --  Called to set register value
   --
   overriding
   procedure set_reg(self : in out pdp11; num : uint32;
                     data : data_bus) is null;
   --
   --  Called to check if the CPU is halted
   --
   overriding
   function halted(self : in out pdp11) return Boolean;
   --
   --  This clears the halted flag allowing processing to continue.
   --
   overriding
   procedure continue_proc(self : in out pdp11);
   --
   --  Post a reset exception request
   --
   overriding
   procedure reset(self : in out pdp11);
   --
   --  Post an interrupt exception
   --
   overriding
   procedure interrupt(self : in out pdp11; data : long);
   --
   --  Enable/disable interrupt processing
   --
   overriding
   procedure interrupts(self : in out pdp11; state : Boolean);
   --
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out pdp11; addr : addr_bus);
   procedure clearBreak(self : in out pdp11; addr : addr_bus);

private
   --
   --  Private definitions not for external use.
   --
   --
   --  Records and types for decoding various instruction formats.
   --  The records are all overlapped in memory to make is easier to get
   --  at the various fields for each instruction format.
   --
   --  Types for the various record fields
   --
   type reg_num is mod 2**3
      with size => 3;
   type mode_code is mod 2**3
      with size => 3;
   type prefix is mod 2**4
      with size => 4;
   type reg_type is (data, address)
      with size => 1;
   for reg_type use (data => 0, address => 1);
   type data_size is (data_byte, data_word, data_long, data_long_long)
      with size => 2;
   for data_size use (data_byte => 0, data_word => 1, data_long => 2,
        data_long_long => 3);
   type uint2 is mod 2**2
     with size => 2;
   type uint3 is mod 2**3  --  Other 3 bit codes
     with size => 3;
   type uint4 is mod 2**4
     with size => 4;
   type uint5 is mod 2**5    --  Five bit sub code
     with size => 5;
   type uint6 is mod 2**6
     with size => 6;
   type uint7 is mod 2**7
     with size => 7;
--   type uint9 is mod 2**9
--     with size => 9;
   type uint12 is mod 2**12
     with size => 12;
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
   type fmt_2op is record
      reg_dest  : reg_num;
      mode_dest : mode_code;
      reg_src   : reg_num;
      mode_src  : mode_code;
      pre       : prefix;
   end record;
   for fmt_2op use record
      reg_dest  at 0 range  0 ..  2;
      mode_dest at 0 range  3 ..  5;
      reg_src   at 0 range  6 ..  8;
      mode_src  at 0 range  9 .. 11;
      pre       at 0 range 12 .. 15;
   end record;
   --
   type fmt_1op is record  --  One effective address
      reg_y  : reg_num;
      mode_y : mode_code;
      code   : uint6;
      pre    : prefix;
   end record;
   for fmt_1op use record
      reg_y   at 0 range 0 .. 2;
      mode_y  at 0 range 3 .. 5;
      code    at 0 range 6 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   --
   type fmt_rop is record  -- Register plus one op instructions
      reg_dest  : reg_num;
      mode_dest : mode_code;
      reg_src   : reg_num;
      mode_src  : mode_code;
      pre       : prefix;
   end record;
   for fmt_rop use record
      reg_dest  at 0 range  0 ..  2;
      mode_dest at 0 range  3 ..  5;
      reg_src   at 0 range  6 ..  8;
      mode_src  at 0 range  9 .. 11;
      pre       at 0 range 12 .. 15;
   end record;
   --
   type fmt_br is record  --  Branch instructions, also EMT and TRAP instructions
      offset : byte;
      code   : uint4;
      pre    : prefix;
   end record;
   for fmt_br use record
      offset at 0 range  0 ..  7;
      code   at 0 range  8 .. 11;
      pre    at 0 range 12 .. 15;
   end record;
   --
   type fmt_cc is record  --  Set/clear condition codes
      carry    : Boolean;
      overflow : Boolean;
      zero     : Boolean;
      negative : Boolean;
      set      : Boolean;
      code     : uint7;
      pre      : prefix;
   end record;
   for fmt_cc use record
      carry    at 0 range  0 ..  0;
      overflow at 0 range  1 ..  1;
      zero     at 0 range  2 ..  2;
      negative at 0 range  3 ..  3;
      set      at 0 range  4 ..  4;
      code     at 0 range  5 .. 11;
      pre      at 0 range 12 .. 15;
   end record;
   --
   --  Setup types for the various intruction formats.
   --
   type instr_fmt is (blank, start, fmt2op, fmt1op, fmtrop, fmtbr, fmtcc);
   type instr_decode(fmt : instr_fmt) is record
      case fmt is
         when blank =>
            b : word;
         when start =>
            s : step1;
         when fmt2op =>
            f2 : fmt_2op;
         when fmt1op =>
            f1 : fmt_1op;
         when fmtrop =>
            fr : fmt_rop;
         when fmtbr =>
            fbr : fmt_br;
         when fmtcc =>
            fcc : fmt_cc;
      end case;
   end record;
   --
   --  An unchecked union is used to make it easier to read the instruction in the
   --  desired format.
   --
   type unchecked_decode (fmt : instr_fmt := blank) is new
     instr_decode(fmt)
     with unchecked_union;
   --
   --  The instruction word is overlayed with various intruction formats
   --  to ease decoding.
   --
   instr : unchecked_decode;
   --
   type reg_id is (reg_r0,
                   reg_r1,
                   reg_r2,
                   reg_r3,
                   reg_r4,
                   reg_r5,
                   reg_usp,  --  User stack pointer
                   reg_ksp,  --  Kernel stack pointer
                   reg_ssp,  --  Supervisor stack pointer
                   reg_pc,
                   reg_psw);
   --
   type interrupt_mask is mod 2**3
      with size => 3;
   type cpu_mode is (mode_kern, mode_super, mode_unused, mode_user)
      with size => 2;
   for cpu_mode use (mode_kern => 0, mode_super => 1, mode_unused => 2,
        mode_user => 3);
   type status_word is record
      carry    : Boolean := False;
      overflow : Boolean := False;
      zero     : Boolean := False;
      negative : Boolean := False;
      trace    : Boolean := False;
      priority : uint3 := 0;
      unused0  : Boolean := False;
      unused1  : Boolean := False;
      unused2  : Boolean := False;
      reg_set  : Boolean := False;
      prev_mode : cpu_mode := mode_kern;
      curr_mode : cpu_mode := mode_kern;
   end record;
   --
   for status_word use record
      carry    at 0 range  0 ..  0;
      overflow at 0 range  1 ..  1;
      zero     at 0 range  2 ..  2;
      negative at 0 range  3 ..  3;
      trace    at 0 range  4 ..  4;
      priority at 0 range  5 ..  7;
      unused0  at 0 range  8 ..  8;
      unused1  at 0 range  9 ..  9;
      unused2  at 0 range 10 .. 10;
      reg_set  at 0 range 11 .. 11;
      prev_mode at 0 range 12 .. 13;
      curr_mode at 0 range 14 .. 15;
   end record;
   --
   for status_word'Size use 16;
   --
   type interrupt_queue is array (byte) of Boolean;
   type interrupt_priority is array (byte) of byte;
   --
   type pdp11 is new simulator with record
      addr : addr_bus := 0;
      temp_addr : addr_bus := 0;
      r0  : word := 0;
      r1  : word := 0;
      r2  : word := 0;
      r3  : word := 0;
      r4  : word := 0;
      r5  : word := 0;
      pc  : word := 0;
      usp : word := 0;
      ksp : word := 0;
      ssp : word := 0;
      psw : status_word;
      check_except : Boolean := False;    --  Check for exceptions
      except_pend  : interrupt_queue;     --  Flags for each possible exception
      except_prio  : interrupt_priority;  --  Priority for each exception
      int_enable   : Boolean := True;     --  Enable/disable interrupt processing
      inst_pc      : word;  --  Address at start of instruction
      cpu_halt     : Boolean := False;
      break_enable : Boolean := False;
      bus_error    : Boolean := False;
      break_point  : word;
      cpu_model    : variants_pdp11 := var_1110;
      config       : features;
   end record;
   --
   --  Operands.  They can be a register number or memory address.
   --
   type operand_kind is (register, memory);
   type operand (kind : operand_kind) is record
      reg  : reg_num;
      mode : mode_code;
      size : data_size;
      case kind is
         when register =>
            null;
         when memory =>
            address : word;
      end case;
   end record;
   --
   --  Code for the instruction processing.
   --
   function get_next(self : in out pdp11) return word;
   procedure check_intr(self : in out pdp11) is null;
   procedure decode(self : in out pdp11);
   --
   --  Get EA.  Decode the register and addressing mode to get the effective
   --  address.  Also does any pre-processing, namely pre-decrement, as appropriate.
   --
   function get_EA(self : in out pdp11; reg : reg_num; mode : mode_code;
      size : data_size) return operand;
   --
   --  Do post-processing, namely post-increment, if needed.
   --
   procedure post_EA(self : in out pdp11; ea : operand);
   --
   --  Get and set value at the effective address.  Note that some effective
   --  addresses cannot be set.
   --
   function get_ea(self : in out pdp11; ea : operand) return word;
   procedure set_ea(self : in out pdp11; ea : operand; val : word);
   --
   --  Sign extension
   --
   function sign_extend(d : byte) return word;
   function sign_extend(d : word) return word is (d);
   --
   --  MSB and LSB
   --
   function msb(b : byte) return Boolean is ((b and 16#80#) = 16#80#);
   function msb(w : word) return Boolean is ((w and 16#8000#) = 16#8000#);
   function lsb(b : byte) return Boolean is ((b and 1) = 1);
   function lsb(w : word) return Boolean is ((w and 1) = 1);
   function lsb(a : addr_bus) return Boolean is ((a and 1) = 1);
   --
   --  Register opertions
   --
   function get_regb(self : in out pdp11; reg_index : reg_num) return byte;
   function get_regw(self : in out pdp11; reg_index : reg_num) return word;
   procedure set_regb(self : in out pdp11; reg_index : reg_num; value : byte);
   procedure set_regw(self : in out pdp11; reg_index : reg_num; value : word);
   --
   --  All data memory accesses should be routed through these functions.
   --
   --  Update for access mode instruction/data/etc.
   --
   procedure memory(self : in out pdp11; addr : addr_bus; value : byte);
   procedure memory(self : in out pdp11; addr : addr_bus; value : word);
   function memory(self : in out pdp11; addr : addr_bus) return byte;
   function memory(self : in out pdp11; addr : addr_bus) return word;
   --
   --  Records for instruction formats for decoding instructions.
   --
   --
--   type fmt_regy is record
--      reg_y : reg_num;
--      code  : uint9;  --  16#108# for SWAP, 16#1ca# for LINK 16#1cb# for UNLK
--      pre   : prefix;  --  4
--   end record;
--   for fmt_regy use record
--      reg_y at 0 range 0 .. 2;
--      code  at 0 range 3 .. 11;
--      pre   at 0 range 12 .. 15;
--   end record;
   --
--   instr_regy : fmt_regy
--     with address => instr'Address;
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
--   instr_1op_size : fmt_1op_size
--     with address => instr'Address;
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
--   instr_bcd : fmt_bcd
--      with address => instr'Address;
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
end BBS.Sim_CPU.CPU.pdp11;
