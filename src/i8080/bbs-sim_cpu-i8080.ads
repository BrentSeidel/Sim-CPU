with BBS.embed;
use type BBS.embed.uint16;
package BBS.Sim_CPU.i8080 is
   --
   --  The simple Intel 8080 simulator inheriting from Sim.simulator.
   --
   type i8080 is new simulator with private;
   --
   subtype byte is BBS.embed.uint8;
   subtype word is BBS.embed.uint16;
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
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   overriding
   function name(self : in out i8080) return String is ("Example simulator");
   --
   --  Called to get simulator memory size
   --
   overriding
   function mem_size(self : in out i8080) return addr_bus is (0);
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out i8080) return BBS.embed.uint32;
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
      carry   : Boolean;
      unused0 : Boolean;
      parity  : Boolean;
      unused1 : Boolean;
      aux_carry : Boolean;
      unused2 : Boolean;
      zero    : Boolean;
      sign    : Boolean;
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
   type i8080 is new simulator with record
      addr : word;
      a  : byte;
      psw : status_word;
      b  : byte;
      c  : byte;
      d  : byte;
      e  : byte;
      h  : byte;
      l  : byte;
      sp : word;
      pc : word;
      mem : mem_array;
      intr : Boolean;
      incpc: Boolean;
   end record;
   --
   --  Code for the instruction processing.
   --
   procedure decode(self : in out i8080);
   function get_next(self : in out i8080; mode : addr_type) return byte;
   procedure check_intr(self : in out i8080) is null;
end BBS.Sim_CPU.i8080;
