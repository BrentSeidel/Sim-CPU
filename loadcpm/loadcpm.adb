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

procedure LoadCPM is
   sector_size : constant BBS.uint8 := 128;
   type sector is array (0 .. sector_size - 1) of BBS.uint8;
   package image_file is new Ada.Direct_IO(sector);
   buff   : sector;
   image  : image_file.File_Type;
   boot   : Ada.Text_IO.File_Type;
   bname  : Ada.Strings.Unbounded.Unbounded_String;
   fname  : Ada.Strings.Unbounded.Unbounded_String;
   hname  : Ada.Strings.Unbounded.Unbounded_String;
   str    : Ada.Strings.Unbounded.Unbounded_String;
   start  : BBS.uint32 := 16#E400#;
   finish : BBS.uint32;
   size   : BBS.uint32;
   sect   : Positive := 1;  --  Start at first sector
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
   Ada.Text_IO.Put_Line("Make bootable CP/M disk image and bootstrap");
--
   Ada.Text_IO.Put_Line("The starting address is the value of the CBASE symbol in the .map file");
   Ada.Text_IO.Put("Enter starting address: [" & BBS.Sim_CPU.toHex(start) & "] ");
   Ada.Text_IO.Unbounded_IO.Get_Line(str);
   if Ada.Strings.Unbounded.Length(str) > 0 then
      nextValue(start, str);
   else
      Ada.Text_IO.Put_Line("Using default starting address of E400.");
   end if;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("The ending address is the value of the CPMEND symbol in the .map file");
   Ada.Text_IO.Put("Enter ending address: ");
   Ada.Text_IO.Unbounded_IO.Get_Line(str);
   nextValue(finish, str);
   Ada.Text_IO.Put("Do you wish to write to the disk image (Y/N)? ");
   Ada.Text_IO.Unbounded_IO.Get_Line(str);
   if str = "y" or str = "Y" then
--
      size := finish - start;
      Ada.Text_IO.Put_Line("Processing " & BBS.uint32'Image(size) &
         " bytes of data, or approximately " &
         BBS.uint32'Image((size / BBS.uint32(sector_size)) + 1) & " sectors");
      if ((size / BBS.uint32(sector_size)) + 1) > 52 then
         Ada.Text_IO.Put_Line("Warning:  Size exceeds two disk tracks.");
         Ada.Text_IO.Put_Line("Press <Return> to continue, <ctrl>C to abort:");
         Ada.Text_IO.Unbounded_IO.Get_Line(str);
      end if;
--
      Ada.Text_IO.Put("Enter disk image name: ");
      Ada.Text_IO.Unbounded_IO.Get_Line(fname);
      Ada.Text_IO.Put("Enter CP/M Hex file name: ");
      Ada.Text_IO.Unbounded_IO.Get_Line(hname);
--
      image_file.Open(image, image_file.Inout_File, Ada.Strings.Unbounded.To_String(fname));
      i8080.init;
      i8080.load(Ada.Strings.Unbounded.To_String(hname));
      while start < finish loop
         for x in sector'Range loop
            buff(x) := BBS.uint8(i8080.Read_Mem(BBS.Sim_CPU.addr_bus(start) +
               BBS.Sim_CPU.addr_bus(x)));
         end loop;
         image_file.Set_Index(image, image_file.Count(sect));
         image_file.Write(image, buff);
         start := start + BBS.uint32(sector_size);
         Ada.Text_IO.Put_Line("Sector " & Integer'Image(sect) & " written.");
         sect := sect + 1;
      end loop;
      Ada.Text_IO.Put_Line("Finished writing to boot disk.");
      image_file.Close(image);
   end if;
--
   Ada.Text_IO.Put("Do you wish to create a level 0 bootstrap program (Y/N)? ");
   Ada.Text_IO.Unbounded_IO.Get_Line(str);
   if str = "y" or str = "Y" then
      Ada.Text_IO.Put_Line("Finished writing bootstrap program.");
      Ada.Text_IO.Put("Enter file name for boot loader: ");
      Ada.Text_IO.Unbounded_IO.Get_Line(bname);
      Ada.Text_IO.Create(boot, Ada.Text_IO.Out_File, Ada.Strings.Unbounded.To_String(bname));
      Ada.Text_IO.Put_Line(boot, ";");
      Ada.Text_IO.Put_Line(boot, "; This is an autogenerated boot loader.");
      Ada.Text_IO.Put_Line(boot, ";");
      Ada.Text_IO.Put_Line(boot, "FDCTL   .EQU 3          ; Floppy control port");
      Ada.Text_IO.Put_Line(boot, "FDSEC   .EQU FDCTL+1    ; Select sector number");
      Ada.Text_IO.Put_Line(boot, "FDTRK   .EQU FDCTL+2    ; Select track number");
      Ada.Text_IO.Put_Line(boot, "FDLSB   .EQU FDCTL+3    ; LSB of DMA address");
      Ada.Text_IO.Put_Line(boot, "FDMSB   .EQU FDCTL+4    ; MSB of DMA address");
      Ada.Text_IO.Put_Line(boot, "FDCNT   .EQU FDCTL+5    ; Number of sectors to transfer");
      Ada.Text_IO.Put_Line(boot, "RD      .EQU 0H40       ; Read command");
      Ada.Text_IO.Put_Line(boot, "SEL0    .EQU 0HC0       ; Select drive 0 command");
      Ada.Text_IO.Put_Line(boot, "LOAD    .EQU 0H" & BBS.Sim_CPU.toHex(BBS.Sim_CPU.word(start and 16#FFFF#)) &
         "     ; Load start address");
      Ada.Text_IO.Put_Line(boot, "ENTRY   .EQU 0HF9FD     ; CP/M entry point on boot");
      Ada.Text_IO.Put_Line(boot, ";");
      Ada.Text_IO.Put_Line(boot, "    .AREA BOOT (ABS)");
      Ada.Text_IO.Put_Line(boot, "    .ORG 0H0");
      Ada.Text_IO.Put_Line(boot, "START: MVI A,SEL0");
      Ada.Text_IO.Put_Line(boot, "    OUT FDCTL       ; Select drive 0");
      Ada.Text_IO.Put_Line(boot, "    XRA A");
      Ada.Text_IO.Put_Line(boot, "    OUT FDTRK       ; Select track 0");
      Ada.Text_IO.Put_Line(boot, "    MVI A,1");
      Ada.Text_IO.Put_Line(boot, "    OUT FDSEC       ; Select sector 1 (sector numbers start at 1)");
      Ada.Text_IO.Put_Line(boot, "    MVI A,(LOAD >> 8)");
      Ada.Text_IO.Put_Line(boot, "    OUT FDMSB       ; DMA MSB");
      Ada.Text_IO.Put_Line(boot, "    MVI A,(LOAD & 0HFF)");
      Ada.Text_IO.Put_Line(boot, "    OUT FDLSB       ; DMA LSB");
      Ada.Text_IO.Put_Line(boot, "    MVI A,52");
      Ada.Text_IO.Put_Line(boot, "    OUT FDCNT       ; Load 52 sectors (17 sectors to load CCP)");
      Ada.Text_IO.Put_Line(boot, "    MVI A,RD");
      Ada.Text_IO.Put_Line(boot, "    OUT FDCTL       ; Read sector");
      Ada.Text_IO.Put_Line(boot, "    JMP ENTRY       ; Transfer control to loaded code");
      Ada.Text_IO.Put_Line(boot, "    .END START");
      Ada.Text_IO.Close(boot);
   end if;
   Ada.Text_IO.Put_Line("All done.");
end LoadCPM;
