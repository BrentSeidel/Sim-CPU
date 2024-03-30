--
--  Contains I/O devices for various kinds of disk
--
with Ada.Direct_IO;
generic
  sector_size : Natural;
  max_drives  : Natural;
package BBS.Sim_CPU.disk is
   --
   --  The floppy disk device object for an 8 bit system.  This simulates an
   --  8 inch floppy with 128 byte sectors, but can be modified for others.
   --
   type disk_ctrl is new io_device with private;
   --
   --  Port useage (base +)
   --    0 - Control port
   --    1 - Sector number
   --    2 - Track number
   --    3 - DMA address LSB
   --    4 - DMA address MSB
   --    5 - Count (number of sectors to read)
   --    6 - Head number (not yet implemented)
   --
   --  Control port bits are:
   --  (write)
   --    7-6 - Action (0 - none, 1 - read, 2 - write, 3 - select disk)
   --    5-4 - Not used
   --    3-0 - Disk number (0-15)
   --  (read)
   --    7 - Offline (set true if drive unavailable or no file attached)
   --    6 - Out of range (track, sector, or head is out of range)
   --    5 - Other error
   --    4 - Unused
   --    3-0 - Disk number (0-15)
   --
   --  Controller configuration constants.  These may eventually move to
   --  be parameters for a generic.
   --
   subtype drive_num is Natural range 0 .. max_drives - 1;
   --
   --  Disk drive geometry
   --
   type geometry is record
      tracks  : byte;     --  Number of tracks on disk
      sectors : byte;     --  Number of sectors per track
      heads   : byte;     --  Number of heads per drive (currently unused)
   end record;
   --
   --  Geometry for 8 inch floppy disk for CP/M.
   --
   floppy8_geom : geometry := (77, 26, 0);
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out disk_ctrl; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out disk_ctrl; addr : addr_bus) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out disk_ctrl) return addr_bus is (6);
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out disk_ctrl) return addr_bus;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out disk_ctrl; base : addr_bus);
   --
   --  Set the owner (used mainly for DMA)
   --
   overriding
   procedure setOwner(self : in out disk_ctrl; owner : sim_access);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out disk_ctrl) return string is ("8 Bit Floppy Disk Controller");
   --
   --  Open the attached file
   --
   procedure open(self : in out disk_ctrl; drive : drive_num;
     geom : geometry; name : String);
   --
   --  Close the attached file
   --
   procedure close(self : in out disk_ctrl; drive : drive_num);
   --
   --  Read from the selected drive
   --
   procedure read(self : in out disk_ctrl);
   --
   --  write to the selected drive
   --
   procedure write(self : in out disk_ctrl);
   -- -------------------------------------------------------------------------
   --
   --  Definitions for a hard disk controller with 32 bit addressing.
   --  The geomentry if this device is simplified into a simple linear
   --  sequence of blocks.
   --
   type hd_ctrl is new io_device with private;
   --
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out hd_ctrl; addr : addr_bus; data : data_bus) is null;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out hd_ctrl; addr : addr_bus) return data_bus is (0);
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out hd_ctrl) return addr_bus is (8);
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out hd_ctrl) return addr_bus is (0);
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out hd_ctrl; base : addr_bus) is null;
   --
   --  Set the owner (used mainly for DMA)
   --
   overriding
   procedure setOwner(self : in out hd_ctrl; owner : sim_access) is null;
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out hd_ctrl) return string is ("Mass Storage Controller");
   --
   --  Open the attached file
   --
   procedure open(self : in out hd_ctrl; drive : drive_num;
     name : String) is null;
   --
   --  Close the attached file
   --
   procedure close(self : in out hd_ctrl; drive : drive_num) is null;
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
   --  Constants and type definitions for floppy disk controller
   --
   type disk_sector is array (0 .. sector_size - 1) of byte;
   package disk_io is new Ada.Direct_IO(disk_sector);
   --
   --  Record for information specific to each disk drive.
   --
   type disk_info is record
      present : Boolean := False;
      geom  : geometry;
      image : disk_io.File_Type;
   end record;
   type info_array is array (drive_num) of disk_info;
   --
   --  Definition of the 8 bit floppy disk controller
   --
   type disk_ctrl is new io_device with record
      selected_drive : drive_num := 0;
      drive_info : info_array;
      sector : byte;
      track  : byte;
      count  : byte := 1;
      dma    : addr_bus;
      host   : BBS.Sim_CPU.sim_access;
   end record;
   -- -------------------------------------------------------------------------
   --
   --  Definition for a hard disk controller with 32 bit addressing.
   --
   type hd_ctrl is new io_device with record
      target : byte;      --  Indicates which value to read/write
      drive  : byte;      --  Which disk drive to access
      block  : addr_bus;  --  Which block to read/write
      count  : addr_bus;  --  How many blocks to read/write
      dma    : addr_bus;  --  Memory address to read/write to
      host   : BBS.Sim_CPU.sim_access;
   end record;
end;
