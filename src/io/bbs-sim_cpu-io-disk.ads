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
--  Contains I/O devices for various kinds of disk
--
with Ada.Direct_IO;
generic
  sector_size : Natural;
package BBS.Sim_CPU.io.disk is
   --
   --  Subtype for drive number.  CP/M allows up to 16 drive (0 - 15).
   --
   subtype drive_num is Natural range 0 .. 15;
   --
   --  The floppy disk device object for an 8 bit system.  This simulates an
   --  8 inch floppy with 128 byte sectors, but can be modified for others.
   --
   type fd_ctrl(max_num : drive_num) is new io_device with private;
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
   --  Disk drive geometry
   --
   type geometry is record
      tracks  : word;     --  Number of tracks on disk
      sectors : word;     --  Number of sectors per track
      heads   : byte;     --  Number of heads per drive (currently unused)
   end record;
   --
   --  Geometry for 8 inch floppy disk for CP/M.
   --
   floppy8_geom : constant geometry := (77, 26, 0);
   --
   --  Geometry for experimental Hard Disk
   --
   hd_geom      : constant geometry := (200, 200, 0);
   --
   --  Null geometry, returned if no image file attached.
   --
   null_geom    : constant geometry := (0, 0, 0);
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out fd_ctrl; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out fd_ctrl; addr : addr_bus) return data_bus;
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
   procedure open(self : in out fd_ctrl; drive : drive_num;
     geom : geometry; name : String);
   --
   --  Get/Set geometry for drive
   --
   function getGeometry(self : in out fd_ctrl; drive : drive_num) return geometry;
   procedure setGeometry(self : in out fd_ctrl; drive : drive_num; geom : geometry);
   --
   --  Get the name of the attached file, if any.
   --
   function fname(self : in out fd_ctrl; drive : drive_num) return String;
   --
   --  Is a file attached to the specified drive?
   --
   function present(self : in out fd_ctrl; drive : drive_num) return Boolean;
   --
   --  Is the specified drive read-only?
   --
   function readonly(self : in out fd_ctrl; drive : drive_num) return Boolean;
   --
   --  Set the specified drive's read-only state?
   --
   procedure readonly(self : in out fd_ctrl; drive : drive_num; state : Boolean);
   --
   --  Close the attached file
   --
   procedure close(self : in out fd_ctrl; drive : drive_num);
   --
   --  Read from the selected drive
   --
   procedure read(self : in out fd_ctrl);
   --
   --  write to the selected drive
   --
   procedure write(self : in out fd_ctrl);
   -- -------------------------------------------------------------------------
   --
   --  Definitions for a hard disk controller with 32 bit addressing.
   --  The geomentry of this device is simplified into a simple linear
   --  sequence of blocks.
   --
   --  Port usage (base +)
   --  0 - Command
   --  1 - Status
   --  2 - Value MSB
   --  3 - Value
   --  4 - Value
   --  5 - Value LSB
   --
   --  Commands are:
   --  0 - Do nothing
   --  1 - Set drive
   --  2 - Set starting block
   --  3 - Set block count
   --  4 - Set DMA address
   --  5 - Read data
   --  6 - Write data
   --  7 - Read max drive number
   --  8 - Read max block number
   --  Status bits are
   --  7 - Unused
   --  6 - Unused
   --  5 - Unused
   --  4 - Bad command
   --  3 - Drive out of range
   --  2 - Count out of range
   --  1 - Starting block out of range
   --  0 - Drive out of range
   --
   --  In operation, write the value first and then issue the set command to
   --  set a value.  To read a value, issue a read command, then read the value.
   --
   type hd_ctrl(max_num : drive_num) is new io_device with private;
   type hd_access is access all hd_ctrl'Class;
   --
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out hd_ctrl; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out hd_ctrl; addr : addr_bus) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out hd_ctrl) return addr_bus is (6);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out hd_ctrl) return string is ("HD");
   overriding
   function description(self : in out hd_ctrl) return string is ("Mass Storage Controller");
   overriding
   function dev_class(self : in out hd_ctrl) return dev_type is (HD);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out hd_ctrl; except : long);
   --
   --  Open the attached file
   --
   procedure open(self : in out hd_ctrl; drive : drive_num;
     size : Natural; name : String);
   --
   --  Close the attached file
   --
   procedure close(self : in out hd_ctrl; drive : drive_num);
   --
   --  Read from the selected drive
   --
   procedure read(self : in out hd_ctrl) is null;
   --
   --  write to the selected drive
   --
   procedure write(self : in out hd_ctrl) is null;
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
   type info_array is array (drive_num range <>) of disk_info;
   --
   --  Definition of the 8 bit floppy disk controller
   --
   type fd_ctrl(max_num : drive_num) is new io_device with record
      selected_drive : drive_num := 0;
      drive_info : info_array(0 .. max_num);
      sector : word := 1;
      track  : word := 0;
      count  : byte := 1;
      dma    : addr_bus;
   end record;
   --
   procedure extend(self : in out fd_ctrl; drive : drive_num;
                  geom : geometry; name : String);
   -- -------------------------------------------------------------------------
   --
   --  Info for each mass storage device
   --
   type hd_info is record
      present : Boolean := False;
      size  : Natural;
      image : disk_io.File_Type;
   end record;
   --
   --  Array for HD info
   --
   type hd_array is array (drive_num range <>) of hd_info;
   --
   --  Definition for a hard disk controller with 32 bit addressing.
   --
   type hd_ctrl(max_num : drive_num) is new io_device with record
      int_code : long;    --  Code to send for interrupts
      t0     : byte;      --  Temp value 0
      t1     : byte;      --  Temp value 1
      t2     : byte;      --  Temp value 2
      t3     : byte;      --  Temp value 3
      status : byte;      --  Status code
      drive  : byte := 0; --  Which disk drive to access
      block  : addr_bus;  --  Which block to read/write
      count  : addr_bus;  --  How many blocks to read/write
      dma    : addr_bus;  --  Memory address to read/write to
      drive_info : hd_array(0 .. max_num);
   end record;
   -- -------------------------------------------------------------------------
   --
   --  Dump disk buffer
   --
   procedure dump_sect(buff : disk_sector);
end;
