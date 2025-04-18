package BBS.Sim_CPU is
   --
   --  This package describes an interaface that the panel can used to control
   --  a simulator.  Specifying this interface should allow easier implementation
   --  of multiple simulator.
   --
   --  Basic types for address and data bus.  Currently these support up to 32
   --  bits of address and data.  These will need to change if larger processors
   --  are to be supported.
   --
   subtype data_bus is uint32;  --  Data bus
   subtype addr_bus is uint32;  --  Address bus
   subtype ad_bus   is uint32;  --  Greater of address and data bus
   subtype byte     is uint8;
   subtype word     is uint16;
   subtype long     is uint32;
   subtype long_long is uint64;
   --
   --  Memory page for reading various hex formats
   --
   type page is array (0 .. 255) of byte;
   --
   --  Processor modes
   --
   type proc_mode is (PROC_NONE, PROC_KERN, PROC_EXEC, PROC_SUP, PROC_USER);
   --
   for proc_mode use (PROC_NONE => 0,
                      PROC_KERN => 16#01#,
                      PROC_EXEC => 16#02#,
                      PROC_SUP  => 16#04#,
                      PROC_USER => 16#08#);
   for proc_mode'Size use 4;
   --
   --  Address types
   --
   type addr_type is (ADDR_NONE, ADDR_INTR, ADDR_DATA, ADDR_INST, ADDR_IO);
   --
   for addr_type use (ADDR_NONE => 0,
                      ADDR_INTR => 16#01#,
                      ADDR_DATA => 16#02#,
                      ADDR_INST => 16#04#,
                      ADDR_IO   => 16#08#);
   for addr_type'Size use 4;
   --
   --  For processors with multiple bus types.  If only a single bus type is used,
   --  it should be BUS_MEMORY.
   --
   type bus_type is (BUS_MEMORY, BUS_IO);
   --
   --  Control and mode switches and LEDs
   --
   type ctrl_mode is record
      unused0 : Boolean;    --  LED/Switch 0 is hardwired to power
      ready   : Boolean;    --  LED only
      exam    : Boolean;    --  Examine
      dep     : Boolean;    --  Deposit
      addr    : Boolean;    --  Address/Data
      auto    : Boolean;    --  Auto/Man, enable remote control via web server
      start   : Boolean;    --  Start
      run     : Boolean;    --  Run
      atype   : addr_type;  --  LED only, address type
      mode    : proc_mode;  --  LED only, processor mode
   end record;
   --
   for ctrl_mode use record
      unused0 at 0 range  0 ..  0;
      ready   at 0 range  1 ..  1;
      exam    at 0 range  2 ..  2;
      dep     at 0 range  3 ..  3;
      addr    at 0 range  4 ..  4;
      auto    at 0 range  5 ..  5;
      start   at 0 range  6 ..  6;
      run     at 0 range  7 ..  7;
      atype   at 0 range  8 .. 11;
      mode    at 0 range 12 .. 15;
   end record;
   --
   for ctrl_mode'Size use 16;
   --
   --  The simulator object
   --
   type simulator is abstract tagged private;
   type sim_access is access all simulator'Class;
   --
   --  The I/O device object for simulated CPUs
   --
   type io_device is abstract tagged limited private;
   type io_access is access all io_device'Class;
   --
   --  Codes for various device types
   --  NL - Null device
   --  TT - Serial interface/terminal
   --  FD - Floppy disk
   --  HD - Hard disk
   --  CL - Clock
   --  PT - Paper tape
   --  MT - Magnetic tape
   --
   type dev_type is (NL, TT, FD, HD, CL, PT, MT);
   --
   --  The actual interface.  These are routines that are called under specific
   --  circumstances.  They can examine the switch register to further decide
   --  their actions and set the LED registers as desired.
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   procedure init(self : in out simulator) is abstract;
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   procedure start(self : in out simulator) is abstract;
   --
   --  Called to start simulator execution at a specific address.  This is made
   --  null rather than abstract so that simulators that don't use it don't need
   --  to override it.
   --
   procedure start(self : in out simulator; addr : addr_bus) is null;
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   procedure run(self : in out simulator) is abstract;
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   procedure deposit(self : in out simulator) is abstract;
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   procedure examine(self : in out simulator) is abstract;
   --
   --  Called to load data into the simulator.
   --
   procedure load(self : in out simulator; name : String) is null;
   --
   --  Called to attach an I/O device to a simulator at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --
   procedure attach_io(self : in out simulator; io_dev : io_access;
                       base_addr : addr_bus; bus : bus_type) is abstract;
   --
   --  Request a processor reset.
   --
   procedure reset(self : in out simulator) is null;
   --
   --  Signal an interrupt from a device.
   --
   procedure interrupt(self : in out simulator; data : long) is null;
   --
   --  Allow an external user to enable or disable processing of interrupts
   --
   procedure interrupts(self : in out simulator; state : Boolean) is null;
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   function name(self : in out simulator) return String is ("No simulator");
   --
   --  Called to get simulator memory size
   --
   function mem_size(self : in out simulator) return addr_bus is (0);
   --
   --  Called to get number of registers
   --
   function registers(self : in out simulator) return uint32 is (0);
   --
   --  Called to get number of variants
   --
   function variants(self : in out simulator) return Natural is (1);
   --
   --  Called to get variant name
   --
   function variant(self : in out simulator; v : Natural) return String is abstract;
   --
   --  Called to get current variant index
   --
   function variant(self : in out simulator) return Natural is abstract;
   --
   --  Called to set variant
   --
   procedure variant(self : in out simulator; v : Natural) is abstract;
   --
   --  Check if simulator is halted
   --
   function halted(self : in out simulator) return Boolean is (False);
   --
   --  Force a simulator to enter a halt state (if implemented).  Can be used for
   --  some error conditions.
   --
   procedure halt(self : in out simulator) is null;
   --
   --  This clears the halted flag allowing processing to continue.
   --
   procedure continue_proc(self : in out simulator) is null;
   --
   --  Set/Get trace level
   --
   procedure trace(self : in out simulator; l : Natural);
   function trace(self : in out simulator) return Natural;
   --
   --  Trace levels are handled by the implementation.  The only globally defined
   --  trace level is that 0 means to do no tracing.
   --
   TRACE_NONE : constant Natural := 0;
   --
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out simulator; addr : addr_bus) is null;
   procedure clearBreak(self : in out simulator; addr : addr_bus) is null;
   --
   --  Interrupt status.  Returns simulator dependent status of interrupts
   --
   function intStatus(self : in out simulator) return int32 is (0);
   --
   --  Input/Output debugging
   --
   function lastOutAddr(self : in out simulator) return addr_bus is (0);
   function lastOutData(self : in out simulator) return data_bus is (0);
   procedure overrideIn(self : in out simulator; addr : in addr_bus; data : in data_bus) is null;
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   procedure set_mem(self : in out simulator; mem_addr : addr_bus;
                     data : data_bus) is abstract;
   --
   --  Called to read a memory value
   --
   function read_mem(self : in out simulator; mem_addr : addr_bus) return
     data_bus is abstract;
   --
   --  Called to get register name
   --
   function reg_name(self : in out simulator; num : uint32)
                     return String is abstract;
   --
   --  Called to get register value as a number
   --
   function read_reg(self : in out simulator; num : uint32)
                     return data_bus is abstract;
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   function read_reg(self : in out simulator; num : uint32)
                     return String is abstract;
   --
   --  Called to set register value
   --
   procedure set_reg(self : in out simulator; num : uint32;
                     data : data_bus) is abstract;
   --
   --  Simulator switches and lights
   --
   function get_lr_data(self : in out simulator) return data_bus;
   function get_lr_addr(self : in out simulator) return addr_bus;
   function get_lr_ctrl(self : in out simulator) return ctrl_mode;
   procedure set_sr_ad(self : in out simulator; value : ad_bus);
   procedure set_sr_ctrl(self : in out simulator; value : ctrl_mode);
   --
   --  ----------------------------------------------------------------------
   --  I/O device actions
   --
   procedure write(self : in out io_device; addr : addr_bus; data : data_bus) is abstract;
   function read(self : in out io_device; addr : addr_bus) return data_bus is abstract;
   function getSize(self : in out io_device) return addr_bus is (0);
   function getBase(self : in out io_device) return addr_bus;
   procedure setBase(self : in out io_device; base : addr_bus);
   procedure setOwner(self : in out io_device; owner : sim_access);
   function name(self : in out io_device) return String is ("Unimplemented");
   function description(self : in out io_device) return String is ("Unimplemented");
   procedure setException(self : in out io_device; except : long) is abstract;
   function dev_class(self : in out io_device) return dev_type is (NL);
   procedure reset(self : in out io_device) is null;
   procedure shutdown(self : in out io_device) is null;
   --
   --  ----------------------------------------------------------------------
   --  Utility functions
   --
   function isHex(c : Character) return Boolean is
     ((c >= '0' and c <= '9') or (c >= 'A' and c <= 'F') or (c >= 'a' and c <= 'f'))
       with Global => Null;
   pragma Pure_Function(isHex);
   function hexDigit(c : Character) return uint32;
   function toHex(value : byte) return String;
   function toHex(value : word) return String;
   function toHex(value : uint32) return String;
   function toHex(s : String) return uint32;
   --
   --  Parse a line of an Intex Hex file
   --
   procedure IntelHex(s : String; count : out byte; addr : out word; rec : out byte;
                      data : out page; valid : out Boolean);
   --
   --  Parse a line of an S-Record file
   --
   procedure S_Record(s : String; count : out byte; addr : out ad_bus; rec : out byte;
                      data : out page; valid : out Boolean);
private
   --
   --  Simulator object.
   --
   --  This just contains the switch and light registers for interfacing with
   --  a control panel.  Each specific simulator will have to add its own data.
   --
   type simulator is abstract tagged record
      lr_addr : addr_bus;   --  LED register for address
      lr_data : data_bus;   --  LED register for data
      sr_ad   : ad_bus;     --  Switch register for address/data
      lr_ctl  : ctrl_mode;  --  LED registers for control/mode
      sr_ctl  : ctrl_mode;  --  Switch register for control/mode
      trace   : Natural;    --  Trace level
   end record;
   --
   --  These are the basic features that all I/O devices include.
   --
   type io_device is abstract tagged limited record
      base : addr_bus;  --  The base address
      host : BBS.Sim_CPU.sim_access;
   end record;
   --
end BBS.Sim_CPU;
