--
--  Author: Brent Seidel
--  Date: 19-May-2026
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
--  Contains I/O device for simulated RK611 disk controller.
--
--  Note that many options, particularly error conditions which only apply to
--  real hardware, are not implemented.  It is also likely that only RK07 drives
--  will be implemented.
--
with Ada.Direct_IO;
with BBS.Sim_CPU.io;
package BBS.Sim_CPU.io.disk.rk611 is
   --
   type rk611 is new disk_ctrl with private;
   type rk611_access is access all rk611'Class;
   --
   --  Port useage (base +)
   --     0/ 1 - RKCS1 - Control/status register #1
   --     2/ 3 - RKWC  - Word count register
   --     4/ 5 - RKBA  - Bus address register (current memory address)
   --     6/ 7 - RKDA  - Disk address register
   --     8/ 9 - RKCS2 - Control/status register #2
   --    10/11 - RKDS  - Drive status register
   --    12/13 - RKER  - Error register
   --    14/15 - RKAS/OF - Attention Summary/Offset Register
   --    16/17 - RKDC  - Desired Cylinder Register
   --    18/19 - Unused
   --    20/21 - RKDB  - Data buffer register
   --    22/23 - RKMR1 - Maintenance Register 1
   --    24/25 - RKECPS - ECC Position Register
   --    26/27 - RKECPT - ECC Pattern Register
   --    28/29 - RKMR2 - Maintenance Register 2
   --    30/31 - RKMR3 - Maintenance Register 3
   --
   --  Geometry for RK06 and RK07 disks
   --
   rk06_geom : constant geometry := (
                                     tracks => 411,
                                     sectors => 22,
                                     heads => 3);
   rk07_geom : constant geometry := (
                                     tracks => 815,
                                     sectors => 22,
                                     heads => 3);
   --
   --  I/O device actions
   --
   --
   --  Reset/Initialize device
   --
   overriding
   procedure reset(self : in out rk611);
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out rk611; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out rk611; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out rk611) return addr_bus is (32);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out rk611) return string is ("DM");
   overriding
   function description(self : in out rk611) return string is ("RK611 Hard disk controller");
   overriding
   function dev_class(self : in out rk611) return dev_type is (FD);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out rk611; except : long);
   --
   --  Open the attached file
   --
   procedure open(self : in out rk611; drive : byte;
     geom : geometry; name : String);
   --
   --  Get/Set geometry for drive - RK05 geometry is fixed and can't change.
   --
   function getGeometry(self : in out rk611; drive : byte) return geometry is (rk07_geom);
   procedure setGeometry(self : in out rk611; drive : byte; geom : geometry) is null;
   --
   --  Get the name of the attached file, if any.
   --
   function fname(self : in out rk611; drive : byte) return String;
   --
   --  Is a file attached to the specified drive?
   --
   function present(self : in out rk611; drive : byte) return Boolean;
   --
   --  Is the specified drive read-only?
   --
   function readonly(self : in out rk611; drive : byte) return Boolean;
   --
   --  Set the specified drive's read-only state?
   --
   procedure readonly(self : in out rk611; drive : byte; state : Boolean);
   --
   --  Close the attached file
   --
   procedure close(self : in out rk611; drive : byte);
   --
   --  Return maximum drive number
   --
   function max_drive(self : in out rk611) return byte is (7);
   --
   --  Read from the selected drive
   --
   procedure read(self : in out rk611);
   --
   --  write to the selected drive
   --
   procedure write(self : in out rk611);
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
   type uint5 is mod 2**5
     with size => 5;
   --
   --  Constants for registers
   --  Port useage (base +)
   --     0/ 1 - RKCS1 - Control/status register #1
   --     2/ 3 - RKWC  - Word count register
   --     4/ 5 - RKBA  - Bus address register (current memory address)
   --     6/ 7 - RKDA  - Disk address register
   --     8/ 9 - RKCS2 - Control/status register #2
   --    10/11 - RKDS  - Drive status register
   --    12/13 - RKER  - Error register
   --    14/15 - RKAS/OF - Attention Summary/Offset Register
   --    16/17 - RKDC  - Desired Cylinder Register
   --    18/19 - Unused
   --    20/21 - RKDB  - Data buffer register
   --    22/23 - RKMR1 - Maintenance Register 1
   --    24/25 - RKECPS - ECC Position Register
   --    26/27 - RKECPT - ECC Pattern Register
   --    28/29 - RKMR2 - Maintenance Register 2
   --    30/31 - RKMR3 - Maintenance Register 3
   --
   RKCS1lsb : constant byte :=  0;
   RKCS1msb : constant byte :=  1;
   RKWClsb  : constant byte :=  2;
   RKWCmsb  : constant byte :=  3;
   RKBAlsb  : constant byte :=  4;
   RKBAmsb  : constant byte :=  5;
   RKDAlsb  : constant byte :=  6;
   RKDAmsb  : constant byte :=  7;
   RKCS2lsb : constant byte :=  8;
   RKCS2msb : constant byte :=  9;
   RKDSlsb  : constant byte := 10;
   RKDSmsb  : constant byte := 11;
   RKERlsb  : constant byte := 12;
   RKERmsb  : constant byte := 13;
   RKASlsb  : constant byte := 14;
   RKASmsb  : constant byte := 15;
   RKDClsb  : constant byte := 16;
   RKDCmsb  : constant byte := 17;
   RKunus1  : constant byte := 18;
   RKunus2  : constant byte := 19;
   RKDBlsb  : constant byte := 20;
   RKDBmsb  : constant byte := 21;
   RKMR1lsb : constant byte := 22;
   RKMR1msb : constant byte := 23;
   RKECPSlsb  : constant byte := 24;
   RKECPSmsb  : constant byte := 25;
   RKECPTlsb  : constant byte := 26;
   RKECPTmsb  : constant byte := 27;
   RKMR2lsb : constant byte := 28;
   RKMR2msb : constant byte := 29;
   RKMR3lsb : constant byte := 30;
   RKMR3msb : constant byte := 31;
   --
   --  Constants for debugging
   --
   halt_on_io_error : constant Boolean := False;  --  Print message and halt CPU if track or sector out of range
   --
   --  RK611 register definitions
   --
   --  Controller function codes are:
   --    0 - Select Drive
   --    1 - Pack Acknowledge
   --    2 - Drive Clear
   --    3 - Unload
   --    4 - Start Spindle
   --    5 - Recalibrate
   --    6 - Offset
   --    7 - Seek
   --    8 - Read Data
   --    9 - Write Data
   --   10 - Read Header
   --   11 - Write Header
   --   12 - Write Check
   --   13-15 Unused?
   --
   type tRKCS1 is record    --  Control Status Register #1
      go        : Boolean;  --  Set true to actually execute commands, cleared by controller when done.
      drv_func  : uint4;    --  Drive function
      spare     : Boolean;  --  Spare bit
      inte      : Boolean;  --  Interrupt on Done Enable
      ctrl_rdy  : Boolean;  --  Control Ready (read only)
      ext_addr  : uint2;    --  Extended address bits (17 and 18)
      drv_type  : Boolean;  --  Drive type (False - RK06, True - RK07)
      timeout   : Boolean;  --  Controller timed out (read only)
      format    : Boolean;  --  Selects number of sectors (False - 22 for 16 bit, True 20 for 18 bit)
      parity    : Boolean;  --  Drive to controller parity error (read only)
      drv_int   : Boolean;  --  Drive initiated interrupt (read only)
      error     : Boolean;  --  Combined error/Controller clear
   end record;
   for tRKCS1 use record
      go        at 0 range  0 ..  0;
      drv_func  at 0 range  1 ..  4;
      spare     at 0 range  5 ..  5;
      inte      at 0 range  6 ..  6;
      ctrl_rdy  at 0 range  7 ..  7;
      ext_addr  at 0 range  8 ..  9;
      drv_type  at 0 range 10 .. 10;
      timeout   at 0 range 11 .. 11;
      format    at 0 range 12 .. 12;
      parity    at 0 range 13 .. 13;
      drv_int   at 0 range 14 .. 14;
      error     at 0 range 15 .. 15;
   end record;
   for tRKCS1'size use 16;
   --
   type tRKDA is record
      sector   : uint5;    --  Sector address
      unused1  : uint3;    --  Spare/Unused bits
      surface  : uint3;    --  Surface
      unused2  : uint5;    --  Spare/Unused bits
   end record;
   for tRKDA use record
      sector   at 0 range  0 ..  4;
      unused1  at 0 range  5 ..  7;
      surface  at 0 range  8 .. 10;
      unused2  at 0 range 11 .. 15;
   end record;
   for tRKDA'size use 16;
   --
   type tRKCS2 is record    --  Control Status Register
      drive     : uint3;    --  Selected drive
      release   : Boolean;  --  Release drive (for dual ported drives)
      inc_inhib : Boolean;  --  Bus Address Increment Inhibit
      sub_clr   : Boolean;  --  Subsystem Clear
      inp_rdy   : Boolean;  --  Input Ready (read only)
      out_rdy   : Boolean;  --  Output Ready (read only)
      UFE       : Boolean;  --  Unit Field Error (read only)
      MDS       : Boolean;  --  Multiple Drive Select (read only)
      prog_err  : Boolean;  --  Programming Error (read only)
      nxmem     : Boolean;  --  Nonexistent Memory (read only)
      nxdrive   : Boolean;  --  Nonexistent Drive (read only)
      uni_par   : Boolean;  --  Unibus Parity Error (read only)
      WCE       : Boolean;  --  Write Check Error (read only)
      late_data : Boolean;  --  Data Late Error (read only)
   end record;
   for tRKCS2 use record
      drive     at 0 range  0 ..  2;
      release   at 0 range  3 ..  3;
      inc_inhib at 0 range  4 ..  4;
      sub_clr   at 0 range  5 ..  5;
      inp_rdy   at 0 range  6 ..  6;
      out_rdy   at 0 range  7 ..  7;
      UFE       at 0 range  8 ..  8;
      MDS       at 0 range  9 ..  9;
      prog_err  at 0 range 10 .. 10;
      nxmem     at 0 range 11 .. 11;
      nxdrive   at 0 range 12 .. 12;
      uni_par   at 0 range 13 .. 13;
      WCE       at 0 range 14 .. 14;
      late_data at 0 range 15 .. 15;
   end record;
   for tRKCS2'size use 16;
   --
   type tRKDS is record    --  Drive Status Register (read only)
      DRA       : Boolean;  --  Drive Available
      unused1   : Boolean;  --  Unused
      offset    : Boolean;  --  Drive is in offset mode
      ac_low    : Boolean;  --  Drive A/C power is low
      speed_los : Boolean;  --  Drive spindle speed is too low
      DROT      : Boolean;  --  Drive Off Track
      valid     : Boolean;  --  Volume Valid
      drv_rdy   : Boolean;  --  Drive Ready
      drv_type  : Boolean;  --  Drive Type (False - RK06, True - RK07)
      unused2   : Boolean;  --  Unused
      unused3   : Boolean;  --  Unused
      wrt_prot  : Boolean;  --  Write Lock (True - write protected)
      unused4   : Boolean;  --  Unused
      pos_prog  : Boolean;  --  Positioning in Progress
      drv_attn  : Boolean;  --  Current Drive Attention
      stat_val  : Boolean;  --  Status Valid
   end record;
   for tRKDS use record
      DRA       at 0 range  0 ..  0;
      unused1   at 0 range  1 ..  1;
      offset    at 0 range  2 ..  2;
      ac_low    at 0 range  3 ..  3;
      speed_los at 0 range  4 ..  4;
      DROT      at 0 range  5 ..  5;
      valid     at 0 range  6 ..  6;
      drv_rdy   at 0 range  7 ..  7;
      drv_type  at 0 range  8 ..  8;
      unused2   at 0 range  9 ..  9;
      unused3   at 0 range 10 .. 10;
      wrt_prot  at 0 range 11 .. 11;
      unused4   at 0 range 12 .. 12;
      pos_prog  at 0 range 13 .. 13;
      drv_attn  at 0 range 14 .. 14;
      stat_val  at 0 range 15 .. 15;
   end record;
   for tRKDS'size use 16;
   --
   type tRKER is record     --  Error Register (read only)
      bad_fun   : Boolean;  --  Illegal Function
      bad_seek  : Boolean;  --  Seek Incomplete
      non_fun   : Boolean;  --  Nonexecutable Function
      DRPAR     : Boolean;  --  Control-to-Drive Parity Error
      format    : Boolean;  --  Format Error
      bad_type  : Boolean;  --  Drive Type Error
      hard_err  : Boolean;  --  Error Correction Hard
      bad_sect  : Boolean;  --  Bad Sector Error
      HVRC      : Boolean;  --  Header Vertical Redundancy Check Error
      cyl_over  : Boolean;  --  Cylinder Overflow
      bad_daddr : Boolean;  --  Invalid Disk Address Error
      write_loc : Boolean;  --  Write Lock Error
      drv_time  : Boolean;  --  Drive Timing Error
      incompl   : Boolean;  --  Operation Incomplete
      unsafe    : Boolean;  --  Drive Unsave
      check     : Boolean;  --  Data Check
   end record;
   for tRKER use record
      bad_fun   at 0 range  0 ..  0;
      bad_seek  at 0 range  1 ..  1;
      non_fun   at 0 range  2 ..  2;
      DRPAR     at 0 range  3 ..  3;
      format    at 0 range  4 ..  4;
      bad_type  at 0 range  5 ..  5;
      hard_err  at 0 range  6 ..  6;
      bad_sect  at 0 range  7 ..  7;
      HVRC      at 0 range  8 ..  8;
      cyl_over  at 0 range  9 ..  9;
      bad_daddr at 0 range 10 .. 10;
      write_loc at 0 range 11 .. 11;
      drv_time  at 0 range 12 .. 12;
      incompl   at 0 range 13 .. 13;
      unsafe    at 0 range 14 .. 14;
      check     at 0 range 15 .. 15;
   end record;
   for tRKER'size use 16;
   --
   type tRKAS is record     --  Attention Summary/Offset Register
      offset    : uint8;    --  Illegal Function
      attention : uint8;    --  Attention (read only)
   end record;
   for tRKAS use record
      offset    at 0 range  0 ..  7;
      attention at 0 range  8 .. 15;
   end record;
   for tRKAS'size use 16;
   --
   type tRKMR1 is record    --  Maintenance Register #1
      msg_sel   : uint4;    --  Message Select
      parity    : Boolean;  --  Parity Test
      diag_mode : Boolean;  --  Diagnostic Mode
      mSect     : Boolean;  --  Maintenance Sector Pulse
      mIndex    : Boolean;  --  Maintenance Index
      mClock    : Boolean;  --  Maintenance Clock
      mRead     : Boolean;  --  Maintenance-Encoded Read Data (read only)
      mWrite    : Boolean;  --  Maintenance-Encoded Write Data (read only)
      precompA  : Boolean;  --  Precompensation Advance (read only)
      precompD  : Boolean;  --  Precompensation Delay (read only)
      ecc_word  : Boolean;  --  EEC Word (read only)
      gate_rd   : Boolean;  --  Read Gate (read only)
      gate_wr   : Boolean;  --  Write Gate (read only)
   end record;
   for tRKMR1 use record
      msg_sel   at 0 range  0 ..  3;
      parity    at 0 range  4 ..  4;
      diag_mode at 0 range  5 ..  5;
      mSect     at 0 range  6 ..  6;
      mIndex    at 0 range  7 ..  7;
      mClock    at 0 range  8 ..  8;
      mRead     at 0 range  9 ..  9;
      mWrite    at 0 range 10 .. 10;
      precompA  at 0 range 11 .. 11;
      precompD  at 0 range 12 .. 12;
      ecc_word  at 0 range 13 .. 13;
      gate_rd   at 0 range 14 .. 14;  --  RK611 doc text says this is order, but
      gate_wr   at 0 range 15 .. 15;  --  diagrams of bit layout in word shows reversed
   end record;
   for tRKMR1'size use 16;
   --
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
      writeable : Boolean := False;  --  Hardware write protect
      sw_prot   : Boolean := False;  --  Software write protect
      changed   : Boolean := False;
      track     : word;
      image     : disk_io.File_Type;
   end record;
   type info_array is array (byte range <>) of disk_info;
   --
   --  Definition of the RK611 disk controller
   --
   type rk611 is new disk_ctrl with record
      vector    : long;           --  Exception vector
      selected_drive : byte := 0;
      drive_info : info_array(0 .. 7);
      RKCS1     : tRKCS1;         --  Control/Status register #1
      RKWC      : word := 0;      --  Word count register
      RKBA      : addr_bus := 0;  --  Current bus address register
      RKDA      : tRKDA;          --  Disk address register
      RKCS2     : tRKCS2;         --  Control/Status register #2
      RKDS      : tRKDS;          --  Drive status register
      RKER      : tRKER;          --  Error register
      RKASOF    : tRKAS;          --  Attention Summary/Offset Register
      RKDC      : word;           --  Desired Cylinder Register
      RKDB      : word;           --  Data Buffer Register
      RKMR1     : tRKMR1;         --  Maintenance Register 1
      RKECPS    : word;           --  ECC Position Register
      RKECPT    : word;           --  ECC Pattern Register
      RKMR2     : word;           --  Maintenance Register 2 (read only)
      RKMR3     : word;           --  Maintenance Register 3 (read only)
   end record;
   --
   procedure extend(self : in out rk611; drive : byte;
                    geom : geometry; name : String);
   --
   --  Process the command specified in RKCS
   --
   procedure process_command(self : in out rk611);
   --
   --  Seek a specific track, cylinder, and surface specified in RKDA
   --
   procedure seek(self : in out rk611);
   --
   --  Compute block number
   --
   function compute_block(sect : word; surf : uint3; track : word) return Natural;
   --
   --  Other functions
   --
   procedure drive_select(self : in out rk611);
   procedure pack_acknowledge(self : in out rk611);
   procedure drive_clear(self : in out rk611);
   -- -------------------------------------------------------------------------
   --
   --  Dump disk buffer
   --
   procedure dump_sect(buff : disk_sector);
   --
   --  Internal seek to combine common code from read/write/seek
   --  Returns True if successful, False if error.
   --
   function internal_seek(self : in out rk611) return Boolean;
end;
