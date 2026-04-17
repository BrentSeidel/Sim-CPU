--
--  Author: Brent Seidel
--  Date: 13-Apr-2026
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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
--  This package is the container for all tape devices.
--
with Ada.Sequential_IO;
--with Ada.Text_IO;
with BBS.Sim_CPU.CPU;
with BBS.Sim_CPU.io;
package BBS.Sim_CPU.io.tape is
--  ----------------------------------------------------------------------
--  This is an I/O device for a simple 8-bit paper tape interface.  It
--  may get expanded to be usable as a magnetic tape simulation.
--
--  Two addresses are used.
--  base + 0 - Data (R/W)
--  base + 1 - Status (RO)
--
--  Data read and write to the data port complete immediately as far as
--  the simulator is concerned
--
--  The status port is read only (writes are ignored) with the following
--  bits defined:
--  0 - Read file attached
--  1 - Write file attached
--  2 - Read file EOF
--  3-7 - unused
--
--  The paper tape reader/punch device object for an 8 bit system.
--
   type ptape is new io_device with private;
   type ptape_access is access all ptape'Class;
   --
   package tape_io is new Ada.Sequential_IO(byte);
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out ptape; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out ptape; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out ptape) return addr_bus is (2);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out ptape) return string is ("PTP");
   overriding
   function description(self : in out ptape) return string is ("8 Bit Paper Tape");
   overriding
   function dev_class(self : in out ptape) return dev_type is (PT);
   --
   --  Open attached file(s)
   --
   procedure openIn(self : in out ptape; name : String);
   procedure openOut(self : in out ptape; name : String);
   --
   --  Close the attached file
   --
   procedure closeIn(self : in out ptape);
   procedure closeOut(self : in out ptape);
   --
   --  Get the name of the attached file, if any.
   --
   function fnameIn(self : in out ptape) return String;
   function fnameOut(self : in out ptape) return String;
   --
   --  Get the presence of the attached file, if any.
   --
   function presentIn(self : in out ptape) return Boolean;
   function presentOut(self : in out ptape) return Boolean;
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out ptape; except : long) is null;
private
   --
   --  Ctrl-Z character
   --
   ctrl_z : constant data_bus := 26;
   --
   --  The definition of the 8 bit paper tape object
   --
   type ptape is new io_device with record
      inPresent  : Boolean := False;
      outPresent : Boolean := False;
      inFile     : tape_io.File_Type;
      outFile    : tape_io.File_Type;
   end record;

end;
