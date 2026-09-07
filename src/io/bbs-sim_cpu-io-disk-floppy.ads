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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
--  Contains I/O devices for various kinds of disk
--
with Ada.Direct_IO;
generic
  sector_size : Natural;
package BBS.Sim_CPU.io.disk.floppy is
   --
   --  The floppy disk device object for an 8 bit system.  This simulates an
   --  8 inch floppy with 128 byte sectors, but can be modified for others.
   --
   type fd_ctrl(max_num : byte) is new disk_ctrl with private;
   type fd_access is access all fd_ctrl'Class;
   --
   --  Port useage (base +)
   --    0 - Control port
   --    1 - Sector number LSB
   --    2 - Sector number MSB
   --    3 - Track number LSB
   --    4 - Track number MSB
   --    5 - DMA address LSB
   --    6 - DMA address MSB
   --    7 - Count (number of sectors to read)
   --    8 - Head number (not yet implemented)
   --
   --  Control port bits are:
   --  (write)
   --    7-6 - Action (0 - none, 1 - read, 2 - write, 3 - select disk)
   --    5-4 - Not used
   --    3-0 - Disk number (0-15)
   --  (read)
   --    7 - Offline (set true if drive unavailable or no file attached)
   --    6 - Out of range (track, sector, or head is out of range)
   --    5 - Disk changed
   --    4 - Readonly
   --    3-0 - Disk number (0-15)
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out fd_ctrl; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out fd_ctrl; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out fd_ctrl) return addr_bus is (8);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out fd_ctrl) return string is ("FD");
   overriding
   function description(self : in out fd_ctrl) return string is ("8 Bit Floppy Disk Controller");
   overriding
   function dev_class(self : in out fd_ctrl) return dev_type is (FD);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out fd_ctrl; except : long) is null;
   --
   --  Open the attached file
   --
   procedure open(self : in out fd_ctrl; drive : byte;
     geom : geometry; name : String);
   --
   --  Get/Set geometry for drive
   --
   function getGeometry(self : in out fd_ctrl; drive : byte) return geometry;
   procedure setGeometry(self : in out fd_ctrl; drive : byte; geom : geometry);
   --
   --  Get the name of the attached file, if any.
   --
   overriding
   function fname(self : in out fd_ctrl; drive : byte) return String;
   --
   --  Is a file attached to the specified drive?
   --
   overriding
   function present(self : in out fd_ctrl; drive : byte) return Boolean;
   --
   --  Is the specified drive read-only?
   --
   overriding
   function readonly(self : in out fd_ctrl; drive : byte) return Boolean;
   --
   --  Set the specified drive's read-only state?
   --
   overriding
   procedure readonly(self : in out fd_ctrl; drive : byte; state : Boolean);
   --
   --  Close the attached file
   --
   overriding
   procedure close(self : in out fd_ctrl; drive : byte);
   --
   --  Return maximum drive number
   --
   overriding
   function max_drive(self : in out fd_ctrl) return byte;
   -- =========================================================================
private
   --
   --  Constants for debugging
   --
   halt_on_io_error : constant Boolean := False;  --  Print message and halt CPU if track or sector out of range
   -- -------------------------------------------------------------------------
   --
   --  Types for mass storage device access
   --
   type disk_sector is array (0 .. sector_size - 1) of byte;
   package disk_io is new Ada.Direct_IO(disk_sector);
   -- -------------------------------------------------------------------------
   --
   --  Record for information specific to each floppy disk drive.
   --
   type disk_info is record
      present   : Boolean := False;
      writeable : Boolean := False;
      changed   : Boolean := False;
      geom      : geometry;
      image     : disk_io.File_Type;
   end record;
   type info_array is array (byte range <>) of disk_info;
   --
   --  Definition of the 8 bit floppy disk controller
   --
   type fd_ctrl(max_num : byte) is new disk_ctrl with record
      selected_drive : byte := 0;
      drive_info : info_array(0 .. max_num);
      sector : word := 1;
      track  : word := 0;
      count  : byte := 1;
      dma    : addr_bus;
   end record;
   --
   procedure extend(self : in out fd_ctrl; drive : byte;
                  geom : geometry; name : String);
   -- -------------------------------------------------------------------------
   --
   --  Read from the selected drive
   --
   procedure read(self : in out fd_ctrl);
   --
   --  write to the selected drive
   --
   procedure write(self : in out fd_ctrl);
   --
   --  Dump disk buffer
   --
   procedure dump_sect(buff : disk_sector);
end;
