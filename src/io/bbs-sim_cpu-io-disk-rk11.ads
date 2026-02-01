--
--  Author: Brent Seidel
--  Date: 22-Jan-2026
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
--  Contains I/O device for simulated RK11 disk controller
--
with Ada.Direct_IO;
with BBS.Sim_CPU.io;
package BBS.Sim_CPU.io.disk.rk11 is
   --
   type rk11 is new disk_ctrl with private;
   type rk11_access is access all rk11'Class;
   --
   --  Port useage (base +)
   --     0/ 1 - RKDS - Drive status register
   --     2/ 3 - RKER - Error register
   --     4/ 5 - RKCS - Control status register
   --     6/ 7 - RKWC - Word count register
   --     8/ 9 - RKBA - Bus address register (current memory address)
   --    10/11 - RKDA - Disk address register
   --    12/13 - Unused
   --    14/15 - RKDB - Data buffer register
   --
   --  Geometry for RK05 disk
   --
   rk05_geom : constant geometry := (
                                     tracks => 203,
                                     sectors => 12,
                                     heads => 2);
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out rk11; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out rk11; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out rk11) return addr_bus is (16);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out rk11) return string is ("FD");
   overriding
   function description(self : in out rk11) return string is ("RK11 Hard disk controller");
   overriding
   function dev_class(self : in out rk11) return dev_type is (FD);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out rk11; except : long) is null;
   --
   --  Open the attached file
   --
   procedure open(self : in out rk11; drive : Natural;
     geom : geometry; name : String);
   --
   --  Get/Set geometry for drive - RK05 geometry is fixed and can't change.
   --
   function getGeometry(self : in out rk11; drive : Natural) return geometry is (rk05_geom);
   procedure setGeometry(self : in out rk11; drive : Natural; geom : geometry) is null;
   --
   --  Get the name of the attached file, if any.
   --
   function fname(self : in out rk11; drive : Natural) return String;
   --
   --  Is a file attached to the specified drive?
   --
   function present(self : in out rk11; drive : Natural) return Boolean;
   --
   --  Is the specified drive read-only?
   --
   function readonly(self : in out rk11; drive : Natural) return Boolean;
   --
   --  Set the specified drive's read-only state?
   --
   procedure readonly(self : in out rk11; drive : Natural; state : Boolean);
   --
   --  Close the attached file
   --
   procedure close(self : in out rk11; drive : Natural);
   --
   --  Read from the selected drive
   --
   procedure read(self : in out rk11);
   --
   --  write to the selected drive
   --
   procedure write(self : in out rk11);
   -- =========================================================================
private
   --
   --  Constants for registers
   --
   RKDSlsb : constant byte :=  0;
   RKDSmsb : constant byte :=  1;
   RKERlsb : constant byte :=  2;
   RKERmsb : constant byte :=  3;
   RKCSlsb : constant byte :=  4;
   RKCSmsb : constant byte :=  5;
   RKWClsb : constant byte :=  6;
   RKWCmsb : constant byte :=  7;
   RKBAlsb : constant byte :=  8;
   RKBAmsb : constant byte :=  9;
   RKDAlsb : constant byte := 10;
   RKDAmsb : constant byte := 11;
   RKun1   : constant byte := 12;  --  Offset 12 is unused
   RKun2   : constant byte := 13;  --  Offset 13 is unused
   RKDBlsb : constant byte := 14;
   RKDBmsb : constant byte := 15;
   --
   --  Constants for debugging
   --
   halt_on_io_error : constant Boolean := False;  --  Print message and halt CPU if track or sector out of range
   -- -------------------------------------------------------------------------
   --
   --  Types for mass storage device access
   --
   sector_size : constant word := 512;  --  512 byte sectors is standard block size
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
      sector    : uint8;
      cylinder  : uint8;
      surface   : uint8;
      geom      : geometry;
      image     : disk_io.File_Type;
   end record;
   type info_array is array (Natural range <>) of disk_info;
   --
   --  Definition of the 8 bit floppy disk controller
   --
   type rk11 is new disk_ctrl with record
      selected_drive : Natural := 0;
      drive_info : info_array(0 .. 7);
      sector : word := 1;
      track  : word := 0;
      count  : byte := 1;
      dma    : addr_bus;
      last_offset : byte;  --  Last register offset read/write
   end record;
   --
   procedure extend(self : in out rk11; drive : Natural;
                  geom : geometry; name : String);
   -- -------------------------------------------------------------------------
   --
   --  Dump disk buffer
   --
   procedure dump_sect(buff : disk_sector);
end;
