with Ada.Calendar;
package BBS.Sim_CPU.clock is
   --
   --  This contains a clock device that sends a periodic interrupt.  There
   --  are two ports for the device.  The first is control/status and the
   --  second is for the interval in 1/10 of a second.
   --
   type clock_device is new io_device with private;
   type clock_access is access all clock_device;
   --
   --  Once started, the clock server task periodically sends an interrupt
   --  to the simulator.
   --
   task type clock_server is
      entry start(self : clock_access; owner : BBS.Sim_CPU.sim_access);
      entry end_task;
   end clock_server;
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out clock_device; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out clock_device; addr : addr_bus) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out clock_device) return addr_bus is (2);
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out clock_device) return addr_bus;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out clock_device; base : addr_bus);
   --
   --  Set the owner (used mainly for DMA or interrupts)
   --
   overriding
   procedure setOwner(self : in out clock_device; owner : sim_access);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out clock_device) return string is ("Periodic Interrupt Generator");
   --
   --  This must be done before using the device.
   --
   procedure init(self : in out clock_device; ptr : clock_access);
   --
   --  Halt the tasks.
   --
   procedure shutdown(self : in out clock_device);
   --
   --  Set which exception to use
   --
   procedure setException(self : in out clock_device; except : long);
   --
private
   --
   --  The base number of ticks per second.
   --
   base_ticks : constant Duration := 10.0;

   type clock_device is new io_device with record
      int_code : long;
      enable   : Boolean := False;
      interval : Duration;
      host     : BBS.Sim_CPU.sim_access;
      T        : clock_server;
   end record;

end;
