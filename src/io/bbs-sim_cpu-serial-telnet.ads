with GNAT.Sockets;
with Ada.Characters.Latin_1;
--  ----------------------------------------------------------------------
--  This is an I/O device for a simple 8-bit console interface via network.
--  The user can telnet to the specified port to access the device.
--
--  Two addresses are used.
--  base + 0 - Data (R/W)
--  base + 1 - Status (R/W)
--    Status bits:
--      0 - Ready (RO)
--      1 - Connected (RO)
--      2 - Enable interrupt (R/W)
--      3 - Reset (WO)
--
--  Writes to the data port complete immediately as far as the simulator is concerned
--  Reads from the data port return the buffered read character and clear the ready
--  flag.
--
--  The status port is read only (writes are ignored).  The LSB is set if
--  data is available for reading.  The other bits are meaningless and are set to 0.
--
package BBS.Sim_CPU.serial.telnet is
   --
   --  The device object for a network based TTY.
   --
   type tel_tty is new io_device with private;
   type telnet_access is access all tel_tty;
   --
   --  Task type for telnet type server.
   --
   --  Note that the "Start" entry should only be called once.  Other
   --  calls are ignored.
   --
   task type telnet_server is
     entry start(self : telnet_access; port : GNAT.Sockets.Port_Type; owner : BBS.Sim_CPU.sim_access);
     entry write(char : Character);
     entry end_task;
   end telnet_server;
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out tel_tty; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out tel_tty; addr : addr_bus) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out tel_tty) return addr_bus is (2);
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out tel_tty) return addr_bus;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out tel_tty; base : addr_bus);
   --
   --  Set the owner (used mainly for DMA and interrupts)
   --
   overriding
   procedure setOwner(self : in out tel_tty; owner : sim_access);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out tel_tty) return string is ("8 Bit Telnet Port");
   --
   --  Set device port and do the network initialiation.  This must be
   --  done before using the device.
   --
   procedure init(self : in out tel_tty; ptr : telnet_access; port : GNAT.Sockets.Port_Type);
   --
   --  Close the network connection and halt the tasks.
   --
   procedure shutdown(self : in out tel_tty);
   --
   --  Set which exception to use
   --
   procedure setException(self : in out tel_tty; except : long);
   --
private
   CRLF : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
   --
   --  The definition of the 8 bit console object via telnet
   --
   type tel_tty is new io_device with record
      ready     : Boolean := False;  --  Data ready to read
      connected : Boolean := False;
      int_e     : Boolean := False;  --  Interrupt enable
      int_code  : long;
      char      : Character := Character'Val(0);
      host      : BBS.Sim_CPU.sim_access;
      T         : BBS.sim_cpu.serial.telnet.telnet_server;
   end record;
   --
   --  Task for telnet receiver
   --
   task type telnet_rx is
      entry start(self : telnet_access; sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.sim_access);
      entry end_task;
   end telnet_rx;

end;
