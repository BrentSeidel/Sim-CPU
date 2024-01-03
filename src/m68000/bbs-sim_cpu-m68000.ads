with Ada.Containers.Indefinite_Ordered_Maps;
with BBS.embed;
use type BBS.embed.uint16;
use type BBS.embed.uint32;
package BBS.Sim_CPU.m68000 is
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
   --  Called to attach an I/O device to a simulator at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --  For bus:
   --    0 - I/O space
   --    1 - Memory space (currently unimplemented)
   --
   overriding
   procedure attach_io(self : in out m68000; io_dev : io_access;
                       base_addr : addr_bus; bus : bus_type);
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   overriding
   function name(self : in out m68000) return String is ("m68000");
   --
   --  Called to get simulator memory size
   --
   overriding
   function mem_size(self : in out m68000) return addr_bus is (BBS.embed.uint32(memory_size));
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out m68000) return BBS.embed.uint32;
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
   function reg_name(self : in out m68000; num : BBS.embed.uint32)
                     return String;
   --
   --  Called to get register value as a number
   --
   overriding
   function read_reg(self : in out m68000; num : BBS.embed.uint32)
                     return data_bus;
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   overriding
   function read_reg(self : in out m68000; num : BBS.embed.uint32)
                     return String;
   --
   --  Called to set register value
   --
   overriding
   procedure set_reg(self : in out m68000; num : BBS.embed.uint32;
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
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out m68000; addr : addr_bus);
   procedure clearBreak(self : in out m68000; addr : addr_bus);
   --
   --  Unimplemented instruction response
   --
   procedure unimplemented(self : in out m68000; addr : addr_bus; data : word);

private
   --
   --  Private definitions not for external use.
   --
   --  For memory mapped I/O devices
   --
   package io_map_type is new Ada.Containers.Indefinite_Ordered_maps
         (key_type => addr_bus, element_type => io_access);
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
      overflow : Boolean := True;
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
      mem : mem_array := (others => 0);
      except_occur : Boolean := False;  --  Has an exception occured?
      intr         : Boolean := False;
      cpu_halt     : Boolean := False;
      int_enable   : Boolean := False;
      break_enable : Boolean := False;
      break_point  : addr_bus;
      cpu_model    : variants_m68000 := var_68000;
      io_ports     : io_map_type.Map;
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
   --  to ease decoding
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
   function decode_ext(self : in out m68000; reg : reg_num) return operand;
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
   --  BCD operations
   --
   function bcd_to_byte(b : byte) return byte;
   function byte_to_bcd(b : byte) return byte;
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
   procedure memory(self : in out m68000; addr : addr_bus; value : byte);
   procedure memory(self : in out m68000; addr : addr_bus; value : word);
   procedure memory(self : in out m68000; addr : addr_bus; value : long);
   function memory(self : in out m68000; addr : addr_bus) return byte;
   function memory(self : in out m68000; addr : addr_bus) return word;
   function memory(self : in out m68000; addr : addr_bus) return long;
   --
   --  Push and pop long or word to the user or system stack
   --
   procedure push(self : in out m68000; stack : Boolean; value : long);
   procedure push(self : in out m68000; stack : Boolean; value : word);
   function pop(self : in out m68000; stack : Boolean) return long;
   function pop(self : in out m68000; stack : Boolean) return word;

end BBS.Sim_CPU.m68000;

