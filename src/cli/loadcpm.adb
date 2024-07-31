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
with BBS.Sim_CPU;
with BBS.Sim_CPU.disk;
with test_util;
with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;

procedure LoadCPM is
   sector_size : constant uint8 := 128;
   type sector is array (0 .. sector_size - 1) of uint8;
   package image_file is new Ada.Direct_IO(sector);
   buff   : sector;
   image  : image_file.File_Type;
   fname  : Ada.Strings.Unbounded.Unbounded_String;
   hname  : Ada.Strings.Unbounded.Unbounded_String;
   str    : Ada.Strings.Unbounded.Unbounded_String;
   start  : uint32;
   finish : uint32;
   size   : uint32;
   sect   : Integer := 1;  --  Start at first sector

begin
   Ada.Text_IO.Put_Line("Make bootable CP/M disk image");
--
   Ada.Text_IO.Put("Enter disk image name: ");
   Ada.Text_IO.Unbounded_IO.Get_Line(fname);
   image_file.Open(image, image_file.Inout_File, Ada.Strings.Unbounded.To_String(fname));
--
   Ada.Text_IO.Put("Enter CP/M Hex file name: ");
   Ada.Text_IO.Unbounded_IO.Get_Line(hname);
   test_util.cpu.init;
   test_util.CPU.load(Ada.Strings.Unbounded.To_String(hname));
--
   Ada.Text_IO.Put("Enter starting address: ");
   Ada.Text_IO.Unbounded_IO.Get_Line(str);
   test_util.nextValue(start, str);
   Ada.Text_IO.Put("Enter ending address: ");
   Ada.Text_IO.Unbounded_IO.Get_Line(str);
   test_util.nextValue(finish, str);
   size := finish - start;
   Ada.Text_IO.Put_Line("Processing " & uint32'Image(size) &
     " bytes of data, or approximately " &
     uint32'Image((size / uint32(sector_size)) + 1) & " sectors");
--
    while start < finish loop
       for x in sector'Range loop
          buff(x) := uint8(test_util.CPU.Read_Mem(BBS.Sim_CPU.addr_bus(start) +
             BBS.Sim_CPU.addr_bus(x)));
       end loop;
       image_file.Set_Index(image, image_file.Count(sect));
       image_file.Write(image, buff);
       start := start + uint32(sector_size);
       sect := sect + 1;
       Ada.Text_IO.Put_Line("Sector " & Integer'Image(sect) & " written.");
    end loop;
    Ada.Text_IO.Put_Line("Finished writing.");
--   image_file.Close(image);
end LoadCPM;
