--
--  Contains I/O devices for various kinds of disk
--
with Ada.Direct_IO;
package BBS.Sim_CPU.disk is
   --
   --  The floppy disk device object for an 8 bit system
   --
   type floppy8 is new io_device with private;
   --
   --  Port useage (base +)
   --    0 - Control port
   --    1 - Sector number
   --    2 - Track number
   --    3 - DMA address LSB
   --    4 - DMA address MSB
   --
   --  Control port bits are:
   --    7-6 - Action (0 - none, 1 - read, 2 - write, 3 - select disk)
   --    5-4 - Not used
   --    3-0 - Disk number (0-15)
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out floppy8; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out floppy8; addr : addr_bus) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out floppy8) return addr_bus is (5);
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out floppy8) return addr_bus;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out floppy8; base : addr_bus);
   --
   --  Set the owner (used mainly for DMA)
   --
   overriding
   procedure setOwner(self : in out floppy8; owner : sim_access);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out floppy8) return string is ("8 Bit Floppy Disk Controller");
   --
   --  Open the attached file
   --
   procedure open(self : in out floppy8; drive : Natural; name : String);
   --
   --  Close the attached file
   --
   procedure close(self : in out floppy8; drive : Natural);
   --
   --  Read from the selected drive
   --
   procedure read(self : in out floppy8);
   --
   --  write to the selected drive
   --
   procedure write(self : in out floppy8);
   --
private
   --
   --  Constants and type definitions for floppy disk controller
   --
   package byte_io is new Ada.Direct_IO(byte);
   --
   --  Geometry for a disk
   --
   type geometry is record
      tracks  : byte;     --  Number of tracks on disk
      sectors : byte;     --  Number of sectors per track
      size    : Natural;  --  Number of bytes per sector
   end record;
   floppy8_geom : geometry := (77, 26, 128);
   type floppy_sector is array (0 .. floppy8_geom.size - 1) of byte;
   package floppy_io is new Ada.Direct_IO(floppy_sector);
   --
   --  Number of driver per controller (must be in range 0-15).
   drives : constant Natural := 4;
   type floppy_info is record
      present : Boolean := False;
      image   : floppy_io.File_Type;
   end record;
   type info_array is array (0 .. drives - 1) of floppy_info;
   --
   --  Definition of the 8 bit floppy disk controller
   --
   type floppy8 is new io_device with record
      selected_drive : Natural := 0;
      drive_info : info_array;
      sector : byte;
      track  : byte;
      dma    : addr_bus;
      host   : BBS.Sim_CPU.sim_access;
   end record;
end;
