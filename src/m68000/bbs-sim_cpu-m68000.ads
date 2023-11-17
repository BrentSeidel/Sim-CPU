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
                            var_cpu32);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out m68000) is null;
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
                   reg_stat);
   --
   type status_word is record
      carry   : Boolean := False;
      unused0 : Boolean := True;
      parity  : Boolean := False;
      unused1 : Boolean := False;
      aux_carry : Boolean := False;
      unused2 : Boolean := False;
      zero    : Boolean := False;
      sign    : Boolean := False;
   end record;
   --
   for status_word use record
      carry   at 0 range 0 .. 0;
      unused0 at 0 range 1 .. 1;
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
   type mem_array is array (0 .. memory_size - 1) of byte;
   --
   type m68000 is new simulator with record
      addr : addr_bus := 0;
      temp_addr : addr_bus := 0;
      d0 : byte := 0;
      d1 : data_bus := 0;
      d2 : data_bus := 0;
      d3 : data_bus := 0;
      d4 : data_bus := 0;
      d5 : data_bus := 0;
      d6 : data_bus := 0;
      d7 : data_bus := 0;
      a0 : addr_bus := 0;
      a1 : addr_bus := 0;
      a2 : addr_bus := 0;
      a3 : addr_bus := 0;
      a4 : addr_bus := 0;
      a5 : addr_bus := 0;
      a6 : addr_bus := 0;
      sp : addr_bus := 0;
      pc : addr_bus := 0;
      psw : status_word;
      mem : mem_array := (others => 0);
      intr         : Boolean := False;
      cpu_halt     : Boolean := False;
      int_enable   : Boolean := False;
      break_enable : Boolean := False;
      break_point  : addr_bus;
      cpu_model    : variants_m68000 := var_68000;
   end record;
   --
   subtype reg8_index is byte range 0 .. 7;
   subtype reg16_index is byte range 0 .. 3;
   --
   --  Code for the instruction processing.
   --
   procedure decode(self : in out m68000);
   function get_next(self : in out m68000) return byte;
   procedure check_intr(self : in out m68000) is null;
   --
   --  All memory accesses should be routed through these functions so that they
   --  can do checks for memory-mapped I/O or shared memory.
   --
   procedure memory(self : in out m68000; addr : addr_bus; value : data_bus; mode : addr_type);
   function memory(self : in out m68000; addr : addr_bus; mode : addr_type) return data_bus;
   --
   --  Common code for Jump, Call, and Return
   --
   procedure jump(self : in out m68000; go : Boolean);
   procedure call(self : in out m68000; go : Boolean);
   procedure ret(self : in out m68000; go : Boolean);

end BBS.Sim_CPU.m68000;

