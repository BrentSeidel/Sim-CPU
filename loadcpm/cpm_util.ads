with BBS;
use type BBS.uint8;
use type BBS.uint32;
with BBS.Sim_CPU;
with BBS.Sim_CPU.i8080;
with Ada.Direct_IO;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
package cpm_util is
   --
   sector_size : constant BBS.uint8 := 128;
   type sector is array (0 .. sector_size - 1) of BBS.uint8;
   package image_file is new Ada.Direct_IO(sector);
   --
   --  Disk drive geometry
   --
   type geometry is record
      tracks  : BBS.Sim_CPU.byte;  --  Number of tracks on disk
      sectors : BBS.Sim_CPU.byte;  --  Number of sectors per track
      heads   : BBS.Sim_CPU.byte;  --  Number of heads per drive (currently unused)
   end record;
   --
   --  Geometry for 8 inch floppy disk for CP/M.
   --
   floppy8_geom : constant geometry := (77, 26, 0);
   disk_geom    : geometry := floppy8_geom;
   --
   --  Pull the next hexidecimal value off of a string
   --
   procedure nextValue(v : out BBS.uint32;
                       s : in out Ada.Strings.Unbounded.Unbounded_String);
   --
   --  Open or create a disk image file.
   --
   function open_image(img : in out image_file.File_Type; name : String)
         return Boolean;
   --
   --  Create a disk image file.
   --
   function create_image(img : in out image_file.File_Type; name : String)
         return Boolean;
   --
   --  Write the CP/M operating system to the first two tracks of a disk
   --  image.  Currently only 26 sector per track disks are supported.
   --  Perhaps in the future additional geometries will be added.  Note
   --  that BIOS changes will be needed to support this as well.
   --
   procedure write_os(start, finish : BBS.uint32);
   --
   --  Write a level 0 boot program.  This program is customized for the
   --  parameters for the CP/M written in write_os().
   --
   procedure write_boot(start : BBS.uint32);
end cpm_util;
