package BBS.Sim_CPU.CPU is
   --
   --  The simulator object
   --
   type simulator is abstract tagged private;
   type sim_access is access all simulator'Class;
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
   procedure attach_io(self : in out simulator; io_dev : BBS.Sim_CPU.io.io_access;
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
end;
