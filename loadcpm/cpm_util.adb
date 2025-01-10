with Ada.Exceptions;
package body cpm_util is
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
   --
   --  Open or create a disk image file.
   --
   function open_image(img : in out image_file.File_Type; name : String)
         return Boolean is
      str : Ada.Strings.Unbounded.Unbounded_String;
   begin
      image_file.Open(img, image_file.Inout_File, name);
      return True;
   exception
      when E : image_file.Name_Error =>
         Ada.Text_IO.Put_Line("File could not be opened: " & Ada.Exceptions.Exception_Message(E));
         Ada.Text_IO.Put("Do you wish to try creating the file? (Y/N)? ");
         Ada.Text_IO.Unbounded_IO.Get_Line(str);
         if str = "y" or str = "Y" then
            return create_image(img, name);
         else
            return False;
         end if;
      when E : others =>
         Ada.Text_IO.Put_Line("Error occured while opening file: " & Ada.Exceptions.Exception_Message(E));
         return False;
   end;
   --
   --  Create a disk image file.
   --
   function create_image(img : in out image_file.File_Type; name : String)
         return Boolean is
      str : Ada.Strings.Unbounded.Unbounded_String;
      buff   : sector;
   begin
      image_file.Create(img, image_file.Inout_File, name);
         Ada.Text_IO.Put("Do you wish to initialize the disk image (Y/N)? ");
         Ada.Text_IO.Unbounded_IO.Get_Line(str);
         if str = "y" or str = "Y" then
            for i in sector'Range loop
               buff(i) := 16#E5#;   --  CP/M code for deleted directory entry
            end loop;
            for i in 1 .. disk_geom.sectors*disk_geom.tracks loop
               image_file.Set_Index(img, image_file.Count(i));
               image_file.Write(img, buff);
            end loop;
         end if;
         return True;
   exception
      when E : image_file.Name_Error =>
         Ada.Text_IO.Put_Line("File could not be created: " & Ada.Exceptions.Exception_Message(E));
         return False;
      when E : others =>
         Ada.Text_IO.Put_Line("Error occured while creating file: " & Ada.Exceptions.Exception_Message(E));
         return False;
   end;
   --
   --  Write the CP/M operating system to the first two tracks of a disk
   --  image.  Currently only 26 sector per track disks are supported.
   --  Perhaps in the future additional geometries will be added.  Note
   --  that BIOS changes will be needed to support this as well.
   --
   procedure write_os(start, finish : BBS.uint32) is
      fname  : Ada.Strings.Unbounded.Unbounded_String;
      hname  : Ada.Strings.Unbounded.Unbounded_String;
      str    : Ada.Strings.Unbounded.Unbounded_String;
      size   : constant BBS.uint32 := finish - start;
      ptr    : BBS.uint32 := start;
      sect   : Positive := 1;  --  Start at first sector
      i8080  : aliased BBS.Sim_CPU.i8080.i8080;
      buff   : sector;
      image  : image_file.File_Type;
   begin
      Ada.Text_IO.Put_Line("Processing " & BBS.uint32'Image(size) &
         " bytes of data, or approximately " &
         BBS.uint32'Image((size / BBS.uint32(sector_size)) + 1) & " sectors");
      if ((size / BBS.uint32(sector_size)) + 1) > BBS.uint32(disk_geom.sectors)*2 then
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
      if open_image(image, Ada.Strings.Unbounded.To_String(fname)) then
      --
      --  The uses the 8080 simulator to load the Intel hex file and then
      --  reads the contents of the simulator memory.
      --
         i8080.init;
         i8080.load(Ada.Strings.Unbounded.To_String(hname));
         while ptr < finish loop
            for x in sector'Range loop
               buff(x) := BBS.uint8(i8080.Read_Mem(BBS.Sim_CPU.addr_bus(ptr) +
                  BBS.Sim_CPU.addr_bus(x)));
            end loop;
            image_file.Set_Index(image, image_file.Count(sect));
            image_file.Write(image, buff);
            ptr := ptr + BBS.uint32(sector_size);
            Ada.Text_IO.Put_Line("Sector " & Integer'Image(sect) & " written.");
            sect := sect + 1;
         end loop;
         Ada.Text_IO.Put_Line("Finished writing to boot disk.");
         image_file.Close(image);
      end if;
   end;
   --
   --  Write a level 0 boot program.
   --
   procedure write_boot(start : BBS.uint32) is
      boot   : Ada.Text_IO.File_Type;
      bname  : Ada.Strings.Unbounded.Unbounded_String;
      str    : Ada.Strings.Unbounded.Unbounded_String;
      aEntry : BBS.uint32 := 16#F9FD# - 16#E400# + start;
      ctrl   : Natural := 3;  --  Base port address for floppy disk controller
   begin
      Ada.Text_IO.Put_Line("The CP/M entry point is the value of the BOOT symbol in the .map file");
      Ada.Text_IO.Put("Enter starting address: [" & BBS.Sim_CPU.toHex(aEntry) & "] ");
      Ada.Text_IO.Unbounded_IO.Get_Line(str);
      if Ada.Strings.Unbounded.Length(str) > 0 then
         nextValue(aEntry, str);
      else
         Ada.Text_IO.Put_Line("Using default starting address of E400.");
      end if;
      Ada.Text_IO.Put("Enter file name for boot loader: ");
      Ada.Text_IO.Unbounded_IO.Get_Line(bname);
      Ada.Text_IO.Create(boot, Ada.Text_IO.Out_File, Ada.Strings.Unbounded.To_String(bname));
      Ada.Text_IO.Put_Line(boot, ";");
      Ada.Text_IO.Put_Line(boot, "; This is an autogenerated boot loader.");
      Ada.Text_IO.Put_Line(boot, ";");
      Ada.Text_IO.Put_Line(boot, "FDCTL   .EQU " & Natural'Image(ctrl) &
            "          ; Floppy control port");
      Ada.Text_IO.Put_Line(boot, "FDSEC   .EQU FDCTL+1    ; Select sector number");
      Ada.Text_IO.Put_Line(boot, "FDTRK   .EQU FDCTL+2    ; Select track number");
      Ada.Text_IO.Put_Line(boot, "FDLSB   .EQU FDCTL+3    ; LSB of DMA address");
      Ada.Text_IO.Put_Line(boot, "FDMSB   .EQU FDCTL+4    ; MSB of DMA address");
      Ada.Text_IO.Put_Line(boot, "FDCNT   .EQU FDCTL+5    ; Number of sectors to transfer");
      Ada.Text_IO.Put_Line(boot, "RD      .EQU 0H40       ; Read command");
      Ada.Text_IO.Put_Line(boot, "SEL0    .EQU 0HC0       ; Select drive 0 command");
      Ada.Text_IO.Put_Line(boot, "LOAD    .EQU 0H" & BBS.Sim_CPU.toHex(BBS.Sim_CPU.word(start and 16#FFFF#)) &
            "     ; Load start address");
      Ada.Text_IO.Put_Line(boot, "ENTRY   .EQU 0H" & BBS.Sim_CPU.toHex(BBS.Sim_CPU.word(aEntry and 16#FFFF#)) &
            "     ; CP/M entry point on boot");
      Ada.Text_IO.Put_Line(boot, ";");
      Ada.Text_IO.Put_Line(boot, "    .AREA BOOT (ABS)");
      Ada.Text_IO.Put_Line(boot, "    .ORG 0H0");
      Ada.Text_IO.Put_Line(boot, "START: MVI A,SEL0");
      --
      --  The bootstrap could potentially boot off of something besides
      --  drive 0, but CP/M seems to want to be on drive 0.  So for now,
      --  just use drive 0.
      --
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
      Ada.Text_IO.Put_Line("Finished writing bootstrap program.");
   end;
end cpm_util;
