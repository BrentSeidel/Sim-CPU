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
with BBS;
use type BBS.uint8;
use type BBS.uint32;
with BBS.Sim_CPU;
with BBS.Sim_CPU.disk;
with BBS.Sim_CPU.i8080;
with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;

procedure LoadCPM is
   sector_size : constant BBS.uint8 := 128;
   type sector is array (0 .. sector_size - 1) of BBS.uint8;
   package image_file is new Ada.Direct_IO(sector);
   buff   : sector;
   image  : image_file.File_Type;
   fname  : Ada.Strings.Unbounded.Unbounded_String;
   hname  : Ada.Strings.Unbounded.Unbounded_String;
   str    : Ada.Strings.Unbounded.Unbounded_String;
   start  : BBS.uint32;
   finish : BBS.uint32;
   size   : BBS.uint32;
   sect   : Integer := 1;  --  Start at first sector
   i8080  : aliased BBS.Sim_CPU.i8080.i8080;
   --
   --  Pull the next hexidecimal value off of a string
   --
   procedure nextValue(v : out BBS.uint32;
                       s : in out Ada.Strings.Unbounded.Unbounded_String) is
      first : Ada.Strings.Unbounded.Unbounded_String;
      rest  : Ada.Strings.Unbounded.Unbounded_String;
      index : Natural;
   begin
      index := ada.Strings.Unbounded.Index(s, " ");
      if index = 0 then
         first := s;
         rest := Ada.Strings.Unbounded.Null_Unbounded_String;
      else
         first := Ada.Strings.Unbounded.Unbounded_Slice(s, 1, index - 1);
         rest := Ada.Strings.Unbounded.Unbounded_Slice(s, index + 1,
                                                       Ada.Strings.Unbounded.Length(s));
      end if;
      v := BBS.Sim_CPU.toHex(Ada.Strings.Unbounded.To_String(first));
      s := rest;
   end;

begin
   Ada.Text_IO.Put_Line("Make bootable CP/M disk image");
--
   Ada.Text_IO.Put("Enter disk image name: ");
   Ada.Text_IO.Unbounded_IO.Get_Line(fname);
   image_file.Open(image, image_file.Inout_File, Ada.Strings.Unbounded.To_String(fname));
--
   Ada.Text_IO.Put("Enter CP/M Hex file name: ");
   Ada.Text_IO.Unbounded_IO.Get_Line(hname);
   i8080.init;
   i8080.load(Ada.Strings.Unbounded.To_String(hname));
--
   Ada.Text_IO.Put("Enter starting address: ");
   Ada.Text_IO.Unbounded_IO.Get_Line(str);
   nextValue(start, str);
   Ada.Text_IO.Put("Enter ending address: ");
   Ada.Text_IO.Unbounded_IO.Get_Line(str);
   nextValue(finish, str);
   size := finish - start;
   Ada.Text_IO.Put_Line("Processing " & BBS.uint32'Image(size) &
     " bytes of data, or approximately " &
     BBS.uint32'Image((size / BBS.uint32(sector_size)) + 1) & " sectors");
--
    while start < finish loop
       for x in sector'Range loop
          buff(x) := BBS.uint8(i8080.Read_Mem(BBS.Sim_CPU.addr_bus(start) +
             BBS.Sim_CPU.addr_bus(x)));
       end loop;
       image_file.Set_Index(image, image_file.Count(sect));
       image_file.Write(image, buff);
       start := start + BBS.uint32(sector_size);
       sect := sect + 1;
       Ada.Text_IO.Put_Line("Sector " & Integer'Image(sect) & " written.");
    end loop;
    Ada.Text_IO.Put_Line("Finished writing.");
--   image_file.Close(image);
end LoadCPM;
