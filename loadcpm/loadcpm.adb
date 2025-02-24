--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
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
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with cpm_util;

procedure LoadCPM is
   str    : Ada.Strings.Unbounded.Unbounded_String;
   start  : BBS.uint16 := 16#E400#;
   finish : BBS.uint16;
   sects  : Positive := 2*Integer(cpm_util.floppy8_geom.sectors);
begin
   Ada.Text_IO.Put_Line("Make bootable CP/M disk image and bootstrap");
   Ada.Text_IO.Put_Line("The starting address is the value of the CBASE symbol in the .map file");
   Ada.Text_IO.Put("Enter starting address: [" & cpm_util.toHex(start) & "]: ");
   start := cpm_util.defaultHex(start);
   Ada.Text_IO.Put_Line("Using starting address of " & cpm_util.toHex(start));
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("The ending address is the value of the CPMEND symbol in the .map file");
   Ada.Text_IO.Put("Enter ending address: ");
   Ada.Text_IO.Unbounded_IO.Get_Line(str);
   cpm_util.nextValue(finish, str);
--
   Ada.Text_IO.Put("Do you wish to write to the disk image (Y/N)? ");
   Ada.Text_IO.Unbounded_IO.Get_Line(str);
   if str = "y" or str = "Y" then
      sects := cpm_util.write_os(start, finish);
   end if;
--
   Ada.Text_IO.Put("Do you wish to create a level 0 bootstrap program (Y/N)? ");
   Ada.Text_IO.Unbounded_IO.Get_Line(str);
   if str = "y" or str = "Y" then
      cpm_util.write_boot(start, sects);
   end if;
--
   Ada.Text_IO.Put_Line("All done.");
end LoadCPM;
