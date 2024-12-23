--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of SimCPU.
--  SimCPU is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  SimCPU is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.--
--
--  This package contains serial port devices.
--
with Ada.Text_IO;
with GNAT.Sockets;
package BBS.Sim_CPU.serial is
--
--  ----------------------------------------------------------------------
--  This is an I/O device for a simple 8-bit console interface.
--
--  Two addresses are used.
--  base + 0 - Data (R/W)
--  base + 1 - Status (RO)
--
--  Writes to the data port complete immediately as far as the simulator is concerned
--  Reads from the data port return the buffered read character and clear the ready
--  flag.
--
--  The status port is read only (writes are ignored).  The LSB is set if
--  data is available for reading.  The other bits are meaningless and are set to 0.
--
--  The console device object for an 8 bit system.
--
   type con8 is new io_device with private;
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out con8; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out con8; addr : addr_bus) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out con8) return addr_bus is (2);
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out con8) return addr_bus;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out con8; base : addr_bus);
   --
   --  Set the owner (used mainly for DMA)
   --
   overriding
   procedure setOwner(self : in out con8; owner : sim_access) is null;
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out con8) return string is ("CON");
   overriding
   function description(self : in out con8) return string is ("8 Bit Console Port");
   overriding
   function dev_class(self : in out con8) return dev_type is (TT);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out con8; except : long) is null;
   --  ----------------------------------------------------------------------
   --
   --  This is a simple printer style device.  It is output only and write to
   --  a file.  Only one address is used and it is write only.  Reads are
   --  undefined
   --
   --  The printer object for an 8 bit system
   --
   type print8 is new io_device with private;
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out print8; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out print8; addr : addr_bus) return data_bus is (0);
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out print8) return addr_bus is (1);
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out print8) return addr_bus;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out print8; base : addr_bus);
   --
   --  Set the owner (used mainly for DMA)
   --
   overriding
   procedure setOwner(self : in out print8; owner : sim_access) is null;
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out print8) return string is ("PRN");
   overriding
   function description(self : in out print8) return string is ("8 Bit Printer Port");
   overriding
   function dev_class(self : in out print8) return dev_type is (TT);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out print8; except : long) is null;
   --
   --  Open the attached file
   --
   procedure open(self : in out print8; name : String);
   --
   --  Close the attached file
   --
   procedure close(self : in out print8);
   --
private
   --
   --  The definition of the 8 bit console object
   --
   type con8 is new io_device with record
      ready : Boolean := False;
      char  : Character := Character'Val(0);
   end record;
   --
   --  The definition of the 8 bit printer object
   --
   type print8 is new io_device with record
      ready : Boolean := False;
      file : Ada.Text_IO.File_Type;
   end record;
end;
