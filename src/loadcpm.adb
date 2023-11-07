with BBS.embed;
use type BBS.embed.uint8;
use type BBS.embed.uint32;
with BBS.Sim_CPU;
with BBS.Sim_CPU.disk;
with test_util;
with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;

procedure LoadCPM is
   sector_size : constant BBS.embed.uint8 := 128;
   type sector is array (0 .. sector_size - 1) of BBS.embed.uint8;
   package image_file is new Ada.Direct_IO(sector);
   buff   : sector;
   image  : image_file.File_Type;
   fname  : Ada.Strings.Unbounded.Unbounded_String;
   hname  : Ada.Strings.Unbounded.Unbounded_String;
   str    : Ada.Strings.Unbounded.Unbounded_String;
   start  : BBS.embed.uint32;
   finish : BBS.embed.uint32;
   size   : BBS.embed.uint32;
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
   Ada.Text_IO.Put_Line("Processing " & BBS.embed.uint32'Image(size) &
     " bytes of data, or approximately " &
     BBS.embed.uint32'Image((size / BBS.embed.uint32(sector_size)) + 1) & " sectors");
--
    while start < finish loop
       for x in sector'Range loop
          buff(x) := BBS.embed.uint8(test_util.CPU.Read_Mem(BBS.Sim_CPU.addr_bus(start) +
             BBS.Sim_CPU.addr_bus(x)));
       end loop;
       image_file.Set_Index(image, image_file.Count(sect));
       image_file.Write(image, buff);
       start := start + BBS.embed.uint32(sector_size);
       sect := sect + 1;
       Ada.Text_IO.Put_Line("Sector " & Integer'Image(sect) & " written.");
    end loop;
    Ada.Text_IO.Put_Line("Finished writing.");
--   image_file.Close(image);
end LoadCPM;
