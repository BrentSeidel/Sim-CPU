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
package BBS.Sim_CPU.io.disk is
   --
   type disk_ctrl is new io_device with record
      null;
   end record;
   type disk_access is access all disk_ctrl'Class;
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
   --  I/O device actions.  Since there doesn't appear to be a way to derive an
   --  abstract class from an abstract class, the abstract routines in io_device
   --  have to be implemented.  They are all null as are the routines specifically
   --  defined for disk_ctrl.
   --
   procedure write(self : in out disk_ctrl; addr : addr_bus; data : data_bus) is null;
   function read(self : in out disk_ctrl; addr : addr_bus) return data_bus is (0);
   procedure setException(self : in out disk_ctrl; except : long) is null;
   --
   --  Open the attached file
   --
   procedure open(self : in out disk_ctrl; drive : Natural;
     geom : geometry; name : String) is null;
   --
   --  Get/Set geometry for drive
   --
   function getGeometry(self : in out disk_ctrl; drive : Natural) return geometry is (null_geom);
   procedure setGeometry(self : in out disk_ctrl; drive : Natural; geom : geometry) is null;
   --
   --  Get the name of the attached file, if any.
   --
   function fname(self : in out disk_ctrl; drive : Natural) return String is ("");
   --
   --  Is a file attached to the specified drive?
   --
   function present(self : in out disk_ctrl; drive : Natural) return Boolean is (False);
   --
   --  Is the specified drive read-only?
   --
   function readonly(self : in out disk_ctrl; drive : Natural) return Boolean is (True);
   --
   --  Set the specified drive's read-disk_ctrl state?
   --
   procedure readonly(self : in out disk_ctrl; drive : Natural; state : Boolean) is null;
   --
   --  Close the attached file
   --
   procedure close(self : in out disk_ctrl; drive : Natural) is null;
   --
   --  Return maximum drive number
   --
   function max_drive(self : in out disk_ctrl) return Natural is (0);
   -- =========================================================================
end;
