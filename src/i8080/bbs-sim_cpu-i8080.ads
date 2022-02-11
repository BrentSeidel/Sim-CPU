with BBS.embed;
use type BBS.embed.uint16;
package BBS.Sim_CPU.i8080 is
   --
   --  The simple Intel 8080 simulator inheriting from Sim.simulator.
   --
   type i8080 is new simulator with private;
   --
   memory_size : constant word := 2**16;
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out i8080) is null;
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
   --  Called to attach an I/O device to a simulator at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --  For bus:
   --    0 - I/O space
   --    1 - Memory space (currently unimplemented)
   --
   overriding
   procedure attach_io(self : in out i8080; io_dev : io_access;
                       base_addr : addr_bus; bus : Natural);
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   overriding
   function name(self : in out i8080) return String is ("i8080");
   --
   --  Called to get simulator memory size
   --
   overriding
   function mem_size(self : in out i8080) return addr_bus is (BBS.embed.uint32(memory_size));
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out i8080) return BBS.embed.uint32;
   --
   --  Called to get number of variants
   --
   overriding
   function variants(self : in out i8080) return Natural is (1);
   --
   --  Called to get variant name
   --
   overriding
   function variant(self : in out i8080; v : natural) return String;
   --
   --  Called to get current variant index
   --
   overriding
   function variant(self : in out i8080; v : natural) return Natural is (1);
   --
   --  Called to set variant
   --
   overriding
   procedure variant(self : in out i8080; v : natural) is null;
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
   function read_mem(self : in out i8080; mem_addr : addr_bus) return
     data_bus;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out i8080; num : BBS.embed.uint32)
                     return String;
   --
   --  Called to get register value as a number
   --
   overriding
   function read_reg(self : in out i8080; num : BBS.embed.uint32)
                     return data_bus;
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   overriding
   function read_reg(self : in out i8080; num : BBS.embed.uint32)
                     return String;
   --
   --  Called to set register value
   --
   overriding
   procedure set_reg(self : in out i8080; num : BBS.embed.uint32;
                     data : data_bus) is null;
   --
   --  Called to check if the CPU is halted
   --
   overriding
   function halted(self : in out i8080) return Boolean;

private
   --
   type reg_id is (reg_a,    --  Accumulator (8 bits)
                   reg_psw,  --  Status word
                   reg_b,    --  B register (8 bits)
                   reg_c,    --  C register (8 bits)
                   reg_bc,   --  B & C registers (16 bits)
                   reg_d,    --  D register (8 bits)
                   reg_e,    --  E register (8 bits)
                   reg_de,   --  D & E registers (16 bits)
                   reg_h,    --  H register (8 bits)
                   reg_l,    --  L register (8 bits)
                   reg_hl,   --  H & L register (16 bits)
                   reg_sp,   --  Stack pointer (16 bits)
                   reg_pc);  --  Program counter (16 bits)
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
   type io_array is array (byte'Range) of io_access;
   --
   type i8080 is new simulator with record
      addr : word := 0;
      temp_addr : word := 0;
      a  : byte := 0;
      psw : status_word;
      b  : byte := 0;
      c  : byte := 0;
      d  : byte := 0;
      e  : byte := 0;
      h  : byte := 0;
      l  : byte := 0;
      sp : word := 0;
      pc : word := 0;
      mem : mem_array := (others => 0);
      io_ports : io_array := (others => null);
      intr : Boolean := False;
      cpu_halt : Boolean := False;
      int_enable : Boolean := False;
   end record;
   --
   subtype reg8_index is byte range 0 .. 7;
   subtype reg16_index is byte range 0 .. 3;
   --
   --  Code for the instruction processing.
   --
   procedure decode(self : in out i8080);
   function get_next(self : in out i8080) return byte;
   procedure check_intr(self : in out i8080) is null;
   --
   --
   procedure reg8(self : in out i8080; reg : reg8_index; value : byte);
   function reg8(self : in out i8080; reg : reg8_index) return byte;
   procedure mod8(self  : in out i8080; reg : reg8_index; dir : Integer);
   --
   --  LXI and PUSH/POP have different reg16 indices.  V = 0 selects the LXI
   --  version and V = 1 selects the PUSH/POP version.
   --
   procedure reg16(self : in out i8080; reg : reg16_index; value : word; v : Natural);
   function reg16(self : in out i8080; reg : reg16_index; v : Natural) return word;
   procedure setf(self : in out i8080; value : byte);
   function addf(self : in out i8080; v1 : byte; v2 : byte; c : Boolean) return byte;
   function subf(self : in out i8080; v1 : byte; v2 : byte; c : Boolean) return byte;
   function dad(self  : in out i8080; v1 : word; v2 : word) return word;
   procedure mod16(self  : in out i8080; reg : reg16_index; dir : Integer);
   --
   --  All memory accesses should be routed through these functions so that they
   --  can do checks for memory-mapped I/O or shared memory.
   --
   procedure memory(self : in out i8080; addr : word; value : byte; mode : addr_type);
   function memory(self : in out i8080; addr : word; mode : addr_type) return byte;
   --
   --  Handle I/O port accesses
   --
   procedure port(self : in out i8080; addr : byte; value : byte);
   function port(self : in out i8080; addr : byte) return byte;
   --
   --  Common code for Jump, Call, and Return
   --
   procedure jump(self : in out i8080; go : Boolean);
   procedure call(self : in out i8080; go : Boolean);
   procedure ret(self : in out i8080; go : Boolean);
end BBS.Sim_CPU.i8080;
