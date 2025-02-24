--
--  Author: Brent Seidel
--  Date: 24-Jan-2025
--
--  This file is part of LoadCPM.
--  LoadCPM is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  LoadCPM is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with LoadCPM. If not, see <https://www.gnu.org/licenses/>.
--
with BBS;
use type BBS.uint8;
use type BBS.uint16;
with Ada.Direct_IO;
with Ada.Strings.Fixed;
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
      tracks   : BBS.uint8;  --  Number of tracks on disk
      sectors  : BBS.uint8;  --  Number of sectors per track
      ostrack  : BBS.uint8;  --  Number of tracks reserved for O/S
   end record;
   --
   --  Geometry for 8 inch floppy disk for CP/M.
   --
   floppy8_geom : constant geometry := (77, 26, 2);
   hd_geom      : constant geometry := (200, 200, 1);
   disk_geom    : geometry := floppy8_geom;
   --
   --  Memory
   --
   memory : array (BBS.uint16'Range) of BBS.uint8;
   --
   --  Pull the next hexidecimal value off of a string
   --
   procedure nextValue(v : out BBS.uint16;
                       s : in out Ada.Strings.Unbounded.Unbounded_String);
   --
   -- Enter a hexidecimal value with a given default
   --
   function defaultHex(v : BBS.uint16) return BBS.uint16;
   --
   -- Enter a decimal integer value with a given default
   --
   function defaultInt(v : Integer) return Integer;
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
   --  Returns the number of sectors written.
   --
   function write_os(start, finish : BBS.uint16) return Positive;
   --
   --  Write a level 0 boot program.  This program is customized for the
   --  parameters for the CP/M written in write_os().
   --
   procedure write_boot(start : BBS.uint16; sects : Positive);
   --  ----------------------------------------------------------------------
   --  Utility functions
   --
   function isHex(c : Character) return Boolean is
     ((c >= '0' and c <= '9') or (c >= 'A' and c <= 'F') or (c >= 'a' and c <= 'f'))
       with Global => Null;
   pragma Pure_Function(isHex);
   function hexDigit(c : Character) return BBS.uint16;
   function toHex(value : BBS.uint8) return String;
   function toHex(value : BBS.uint16) return String;
   function toHex(s : String) return BBS.uint16;
   --
   --  Parse a line of an Intex Hex file
   --
   procedure IntelHex(s : String; valid : out Boolean);
   --
   --  This loads data from a file specified by "name" into the simulator memory.
   --  Currently, only Intel Hex format is supported.
   --
   procedure load(name : String);
end cpm_util;
