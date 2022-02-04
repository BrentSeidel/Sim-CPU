with BBS.embed;
package BBS.Sim_CPU.example is
   --
   --  The simple simulator object inheriting from Sim.simulator.
   --
   type simple is new simulator with private;
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out simple) is null;
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out simple);
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out simple);
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out simple);
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out simple);
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   overriding
   function name(self : in out simple) return String is ("Example simulator");
   --
   --  Called to get simulator memory size
   --
   overriding
   function mem_size(self : in out simple) return addr_bus is (0);
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out simple) return BBS.embed.uint32;
   --
   --  Called to get number of variants
   --
   overriding
   function variants(self : in out simple) return Natural is (6);
   --
   --  Called to get variant name
   --
   overriding
   function variant(self : in out simple; v : natural) return String;
   --
   --  Called to set variant
   --
   overriding
   procedure variant(self : in out simple; v : natural);
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out simple; mem_addr : addr_bus;
                     data : data_bus);
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out simple; mem_addr : addr_bus) return
     data_bus;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out simple; num : BBS.embed.uint32)
                     return String;
   --
   --  Called to get register value as a number
   --
   overriding
   function read_reg(self : in out simple; num : BBS.embed.uint32)
                     return data_bus;
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   overriding
   function read_reg(self : in out simple; num : BBS.embed.uint32)
                     return String;
   --
   --  Called to set register value
   --
   overriding
   procedure set_reg(self : in out simple; num : BBS.embed.uint32;
                     data : data_bus) is null;

private
   --
   --  Which pattern to select:
   --     0 - Copy switches
   --     1 - count
   --     2 - scan 16-bit
   --     3 - bounce 16-bit
   --     4 - fibbonacci
   --     9 - count
   --    10 - scan 32-bit
   --    11 - bounce 32-bit
   --    12 - fibbonacci
   --    others - Copy switches
   --
   type bounce_type is (left, right);
   type reg_id is (addr,          --  Address register
                   pattern,       --  Selected pattern
                   ad_counter,    --  Address/Data LED counter
                   ctl_counter,   --  Control LED counter
                   ad_bouncer,    --  Address/Data LED bouncer
                   ctl_bouncer,   --  Control LED bouncer
                   ad_scanner,    --  Address/Data LED scanner
                   ctl_scanner,   --  Control LED scanner
                   ad_fib1,       --  Address/Data LED Fibonacci value 1
                   ad_fib2,       --  Address/Data LED Fibonacci value 2
                   ctl_fib1,      --  Control LED Fibonacci value 1
                   ctl_fib2);     --  Control LED Fibonacci value 2
   type reg_array is array (reg_id) of data_bus;

   type simple is new simulator with record
      reg : reg_array := (ad_fib1 => 1, ad_fib2 => 1, ctl_fib1 => 1,
                          ctl_fib2 => 2, others => 0);
      --
      --  Data for the various patterns.
      --
      ad_bounce_dir : bounce_type := left;
      ctl_bounce_dir : bounce_type := left;
   end record;
   --
   --  Code for the various patterns.
   --
   procedure count(self : in out simple);
   procedure bounce16(self : in out simple);
   procedure bounce32(self : in out simple);
   procedure scan16(self : in out simple);
   procedure scan32(self : in out simple);
   procedure fibonacci(self : in out simple);
   procedure copy_sw(self : in out simple);
end BBS.Sim_CPU.example;
