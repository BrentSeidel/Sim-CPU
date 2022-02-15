--
--  This is an I/O device for a simple 8-bit console interface.
--
--  Two addresses are used.
--  base + 0 - Data
--  base + 1 - Status
--
--  Writes to the data port complete immediately as far as the simulator is concerned
--  Reads from the data port return the buffered read character and clear the ready
--  flag.
--
--  The status port is read only (writes are ignored).  The LSB is set if
--  data is available for reading.  The other bits are meaningless and are set to 0.
--
with Ada.Text_IO;
package BBS.Sim_CPU.con8 is
   --
   --  The console device object
   --
   type con is new io_device with private;
   --
   --  ----------------------------------------------------------------------
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out con; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out con; addr : addr_bus) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out con) return addr_bus is (2);
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out con) return addr_bus;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out con; base : addr_bus);
   --
   --  Set the owner (used mainly for DMA)
   --
   overriding
   procedure setOwner(self : in out con; owner : sim_access) is null;
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out con) return string is ("8 Bit Console Port");
private

   type con is new io_device with record
      ready : Boolean := False;
      char  : Character := Character'Val(0);
   end record;
end;
