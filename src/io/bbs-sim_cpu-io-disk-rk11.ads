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
--  Contains I/O device for simulated RK11-E disk controller
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
   --    12/13 - Unused (apparently this was a maintenance register in the RK11-C)
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
   --
   --  Reset/Initialize device
   --
   overriding
   procedure reset(self : in out rk11);
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
   function name(self : in out rk11) return string is ("DK");
   overriding
   function description(self : in out rk11) return string is ("RK11 Hard disk controller");
   overriding
   function dev_class(self : in out rk11) return dev_type is (FD);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out rk11; except : long);
   --
   --  Open the attached file
   --
   procedure open(self : in out rk11; drive : byte;
     geom : geometry; name : String);
   --
   --  Get/Set geometry for drive - RK05 geometry is fixed and can't change.
   --
   function getGeometry(self : in out rk11; drive : byte) return geometry is (rk05_geom);
   procedure setGeometry(self : in out rk11; drive : byte; geom : geometry) is null;
   --
   --  Get the name of the attached file, if any.
   --
   function fname(self : in out rk11; drive : byte) return String;
   --
   --  Is a file attached to the specified drive?
   --
   function present(self : in out rk11; drive : byte) return Boolean;
   --
   --  Is the specified drive read-only?
   --
   function readonly(self : in out rk11; drive : byte) return Boolean;
   --
   --  Set the specified drive's read-only state?
   --
   procedure readonly(self : in out rk11; drive : byte; state : Boolean);
   --
   --  Close the attached file
   --
   procedure close(self : in out rk11; drive : byte);
   --
   --  Return maximum drive number
   --
   function max_drive(self : in out rk11) return byte is (7);
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
   --  Locally needed types
   --
   type uint2 is mod 2**2
     with size => 2;
   type uint3 is mod 2**3
     with size => 3;
   type uint4 is mod 2**4
     with size => 4;
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
   RKun1   : constant byte := 12;  --  Offset 12 is unused  (apparently this was a maintenance)
   RKun2   : constant byte := 13;  --  Offset 13 is unused  (register in the RK11-C).
   RKDBlsb : constant byte := 14;
   RKDBmsb : constant byte := 15;
   --
   --  Constants for debugging
   --
   halt_on_io_error : constant Boolean := False;  --  Print message and halt CPU if track or sector out of range
   --
   --  RK11 register definitions
   --
   type tRKDS is record    --  Drive Status Register (read only)
      sector    : uint4;    --  Sector counter
      equal     : Boolean;  --  Sector count = sector address
      protect   : Boolean;  --  Write protected
      rws_ready : Boolean;  --  Drive head is not in motion
      drv_ready : Boolean;  --  Drive is ready to accept new function
      sect_ok   : Boolean;  --  Sector counter OK
      seek_inc  : Boolean;  --  An unusual conditions prevents seek from completing
      unsafe    : Boolean;  --  An unusual condition has occured
      rk05      : Boolean;  --  Identifies selected drive as an RK05
      pwr_low   : Boolean;  --  Drive has low power
      drv_id    : uint3;    --  Identification of drive causing interrupt
   end record;
   for tRKDS use record
      sector    at 0 range  0 ..  3;
      equal     at 0 range  4 ..  4;
      protect   at 0 range  5 ..  5;
      rws_ready at 0 range  6 ..  6;
      drv_ready at 0 range  7 ..  7;
      sect_ok   at 0 range  8 ..  8;
      seek_inc  at 0 range  9 ..  9;
      unsafe    at 0 range 10 .. 10;
      rk05      at 0 range 11 .. 11;
      pwr_low   at 0 range 12 .. 12;
      drv_id    at 0 range 13 .. 15;
   end record;
   for tRKDS'size use 16;
   --
   --  Some of these error bits indicate hardware errors and will never be set
   --  by the simulator.
   --
   type tRKER is record     --  Error Register (read only)
      write_chk : Boolean;  --  Write check error
      checksum  : Boolean;  --  Checksum
      unused    : uint3;    --  Unused bits
      bad_sect  : Boolean;  --  Nonexistent sector (number > 11)
      bad_cyl   : Boolean;  --  Nonexistent cylinder (number > 202)
      bad_disk  : Boolean;  --  Nonexistant disk/drive
      timing    : Boolean;  --  Timing error (loss of timing pulses)
      late      : Boolean;  --  Data late
      badram    : Boolean;  --  Nonexistant memory
      prog      : Boolean;  --  Attempt format wihtout read or write
      seek      : Boolean;  --  Seek error
      write_loc : Boolean;  --  Attempt to write to readonly drive
      overrun   : Boolean;  --  Read/write operations on last sector in a cylinder finished with data left to transfer
      drv_err   : Boolean;  --  Drive error
   end record;
   for tRKER use record
      write_chk at 0 range  0 ..  0;
      checksum  at 0 range  1 ..  1;
      unused    at 0 range  2 ..  4;
      bad_sect  at 0 range  5 ..  5;
      bad_cyl   at 0 range  6 ..  6;
      bad_disk  at 0 range  7 ..  7;
      timing    at 0 range  8 ..  8;
      late      at 0 range  9 ..  9;
      badram    at 0 range 10 .. 10;
      prog      at 0 range 11 .. 11;
      seek      at 0 range 12 .. 12;
      write_loc at 0 range 13 .. 13;
      overrun   at 0 range 14 .. 14;
      drv_err   at 0 range 15 .. 15;
   end record;
   for tRKER'size use 16;
   --
   --  Drive function codes are:
   --    0 - Control Reset
   --    1 - Write
   --    2 - Read
   --    3 - Write Check
   --    4 - Seek
   --    5 - Read Check
   --    6 - Drive Reset
   --    7 - Write lock
   --
   type tRKCS is record     --  Control Status Register
      go        : Boolean;  --  Set true to actually execute commands, cleared by controller when done.
      drv_func  : uint3;    --  Drive function
      ext_addr  : uint2;    --  Extended address bits (17 and 18)
      inte      : Boolean;  --  Interrupt on Done Enable
      ctrl_rdy  : Boolean;  --  Control Ready (read only)
      stop_soft : Boolean;  --  Stop on soft error
      extra     : Boolean;  --  Unused on RK11-D and RK11-E
      format    : Boolean;  --  Used with Read and Write to format a sector or disk
      not_incr  : Boolean;  --  Inhibit incrementing RKBA to do all transfers to the same memory location
      unused    : Boolean;  --  Unused
      search    : Boolean;  --  Search complete (interrupt is from seek or drive reset) (read only)
      hard_err  : Boolean;  --  Hard error (read only)
      error     : Boolean;  --  Any error (read only)
   end record;
   for tRKCS use record
      go        at 0 range  0 ..  0;
      drv_func  at 0 range  1 ..  3;
      ext_addr  at 0 range  4 ..  5;
      inte      at 0 range  6 ..  6;
      ctrl_rdy  at 0 range  7 ..  7;
      stop_soft at 0 range  8 ..  8;
      extra     at 0 range  9 ..  9;  --  Read/Write all in RK11-C
      format    at 0 range 10 .. 10;
      not_incr  at 0 range 11 .. 11;
      unused    at 0 range 12 .. 12;  --  Maint in RK11-C
      search    at 0 range 13 .. 13;
      hard_err  at 0 range 14 .. 14;
      error     at 0 range 15 .. 15;
   end record;
   for tRKCS'size use 16;
   --
   type tRKDA is record
      sector   : uint4;    --  Sector address
      surface  : Boolean;  --  Surface
      cylinder : uint8;    --  Cylinder address
      drive    : uint3;    --  Drive select
   end record;
   for tRKDA use record
      sector   at 0 range  0 ..  3;
      surface  at 0 range  4 ..  4;
      cylinder at 0 range  5 .. 12;
      drive    at 0 range 13 .. 15;
   end record;
   for tRKDA'size use 16;

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
      sector    : word;
      track     : word;
      surface   : Boolean;
--      geom      : geometry;
      image     : disk_io.File_Type;
   end record;
   type info_array is array (byte range <>) of disk_info;
   --
   --  Definition of the 8 bit floppy disk controller
   --
   type rk11 is new disk_ctrl with record
      vector    : byte;           --  Exception vector
      selected_drive : byte := 0;
      drive_info : info_array(0 .. 7);
      RKDS      : tRKDS;          --  Drive status register
      RKER      : tRKER;          --  Error register
      RKCS      : tRKCS;          --  Control status register
      RKWC      : word := 0;      --  Word count register
      RKBA      : addr_bus := 0;  --  Current bus address register
      RKDA      : tRKDA;          --  Disk address register
   end record;
   --
   procedure extend(self : in out rk11; drive : byte;
                    geom : geometry; name : String);
   --
   --  Process the command specified in RKCS
   --
   procedure process_command(self : in out rk11);
   --
   --  Seek a specific track, cylinder, and surface specified in RKDA
   --
   procedure seek(self : in out rk11);
   --
   --  Compute block number
   --
   function compute_block(sect : word; surf : Boolean; track : word) return Natural;
   -- -------------------------------------------------------------------------
   --
   --  Dump disk buffer
   --
   procedure dump_sect(buff : disk_sector);
end;
