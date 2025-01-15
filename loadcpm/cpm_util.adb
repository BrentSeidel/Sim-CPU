with Ada.Exceptions;
package body cpm_util is
   --
   --  Pull the next hexidecimal value off of a string
   --
   procedure nextValue(v : out BBS.uint16;
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
      v := toHex(Ada.Strings.Unbounded.To_String(first));
      s := rest;
   end;
   --
   -- Enter a hexidecimal value with a given default
   --
   function defaultHex(v : BBS.uint16) return BBS.uint16 is
      str : Ada.Strings.Unbounded.Unbounded_String;
      value : BBS.uint16 := v;
   begin
      Ada.Text_IO.Unbounded_IO.Get_Line(str);
      if Ada.Strings.Unbounded.Length(str) > 0 then
         nextValue(value, str);
      end if;
      return BBS.uint16(value and 16#FFFF#);
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
            for i in 1 .. Integer(disk_geom.sectors)*Integer(disk_geom.tracks) loop
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
   procedure write_os(start, finish : BBS.uint16) is
      fname  : Ada.Strings.Unbounded.Unbounded_String;
      hname  : Ada.Strings.Unbounded.Unbounded_String;
      str    : Ada.Strings.Unbounded.Unbounded_String;
      size   : constant BBS.uint16 := finish - start;
      ptr    : BBS.uint16 := start;
      sect   : Positive := 1;  --  Start at first sector
      buff   : sector;
      image  : image_file.File_Type;
   begin
      Ada.Text_IO.Put_Line("Processing " & BBS.uint16'Image(size) &
         " bytes of data, or approximately " &
         BBS.uint16'Image((size / BBS.uint16(sector_size)) + 1) & " sectors");
      if ((size / BBS.uint16(sector_size)) + 1) > BBS.uint16(sectors) then
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
         load(Ada.Strings.Unbounded.To_String(hname));
         while ptr < finish loop
            for x in sector'Range loop
               buff(x) := memory(ptr + BBS.uint16(x));
            end loop;
            image_file.Set_Index(image, image_file.Count(sect));
            image_file.Write(image, buff);
            ptr := ptr + BBS.uint16(sector_size);
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
   procedure write_boot(start : BBS.uint16) is
      boot   : Ada.Text_IO.File_Type;
      bname  : Ada.Strings.Unbounded.Unbounded_String;
      str    : Ada.Strings.Unbounded.Unbounded_String;
      aEntry : BBS.uint16 := 16#F9FD# - 16#E400# + start;
      ctrl   : Natural := 3;   --  Base port address for floppy disk controller
   begin
      Ada.Text_IO.Put_Line("The CP/M entry point is the value of the BOOT symbol in the .map file");
      Ada.Text_IO.Put("Enter entry point address: [" & toHex(aEntry) & "] ");
      aEntry := cpm_util.defaultHex(aEntry);
      Ada.Text_IO.Put_Line("Using entry point address of " & cpm_util.toHex(aEntry));
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
      Ada.Text_IO.Put_Line(boot, "LOAD    .EQU 0H" & toHex(start) &
            "     ; Load start address");
      Ada.Text_IO.Put_Line(boot, "ENTRY   .EQU 0H" & toHex(aEntry) &
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
      Ada.Text_IO.Put_Line(boot, "    MVI A," & Natural'Image(sectors));
      Ada.Text_IO.Put_Line(boot, "    OUT FDCNT       ; Load 52 sectors (17 sectors to load CCP)");
      Ada.Text_IO.Put_Line(boot, "    MVI A,RD");
      Ada.Text_IO.Put_Line(boot, "    OUT FDCTL       ; Read sector");
      Ada.Text_IO.Put_Line(boot, "    JMP ENTRY       ; Transfer control to loaded code");
      Ada.Text_IO.Put_Line(boot, "    .END START");
      Ada.Text_IO.Close(boot);
      Ada.Text_IO.Put_Line("Finished writing bootstrap program.");
   end;
   --
   --  Utility functions
   --
   hex_digit : String := "0123456789ABCDEF";
   --
   function toHex(value : BBS.uint8) return String is
   begin
      return hex_digit(Integer(value/16#10#) + 1) & hex_digit(Integer(value and 16#0F#) + 1);
   end;
   --
   function toHex(value : BBS.uint16) return String is
   begin
      return hex_digit(Integer(value/16#1000#) + 1) &
        hex_digit(Integer((value/16#100#) and 16#0F#) + 1) &
        hex_digit(Integer((value/16#10#) and 16#0F#) + 1) &
        hex_digit(Integer(value and 16#0F#) + 1);
   end;
   --
   --  Return a value from a hexidecimal string
   --
   function toHex(s : String) return BBS.uint16 is
      v : BBS.uint16 := 0;
   begin
      for i in s'Range loop
         exit when not isHex(s(i));
         v := v*16#10# + hexDigit(s(i));
      end loop;
      return v;
   end;
   --
   --  Return the hexidecimal digit
   --
   function hexDigit(c : Character) return BBS.uint16 is
   begin
      case c is
         when '0' =>
            return 0;
         when '1' =>
            return 1;
         when '2' =>
            return 2;
         when '3' =>
            return 3;
         when '4' =>
            return 4;
         when '5' =>
            return 5;
         when '6' =>
            return 6;
         when '7' =>
            return 7;
         when '8' =>
            return 8;
         when '9' =>
            return 9;
         when 'A' | 'a' =>
            return 10;
         when 'B' | 'b' =>
            return 11;
         when 'C' | 'c' =>
            return 12;
         when 'D' | 'd' =>
            return 13;
         when 'E' | 'e' =>
            return 14;
         when 'F' | 'f' =>
            return 15;
         when others =>
            return 0;
      end case;
   end;
   --
   --  Parse a line of an Intex Hex file.
   --
   --  s     - The input string to parse
   --  count - The number of data bytes in the record
   --  addr  - The memory address for the data bytes
   --  rec   - The record type
   --  data  - An array containing the data bytes (0 .. count) are valid
   --  valid - True for a valid record parsed.
   --
   procedure IntelHex(s : String; valid : out Boolean) is
      start : Natural := Ada.Strings.Fixed.Index(s, ":");
      ptr   : Natural := start + 1;
      t1    : BBS.uint16;
      t2    : BBS.uint16;
      check : BBS.uint8;
      count : BBS.uint16 := 0;
      addr  : BBS.uint16 := 0;
      rec   : BBS.uint8  := 0;
   begin
      valid := False;
      --
      --  Get byte count
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := BBS.uint8(t1);
      count := BBS.uint16(t1);
      --
      -- Get address
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + BBS.uint8(t1);
      if isHex(s(ptr)) then
         t2 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t2 := t2*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + BBS.uint8(t2);
      addr := BBS.uint16(t1*16#100# + t2);
      --
      --  Get record type
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + BBS.uint8(t1);
      rec := BBS.uint8(t1);
      if rec /= 0 then
         Ada.Text_IO.Put_Line("Ignoring record type " & toHex(rec));
         return;
      end if;
      --
      --  Get data
      --
      if count > 0 then
         for i in 0 .. (count - 1) loop
            if isHex(s(ptr)) then
               t1 := hexDigit(s(ptr));
            else
               return;
            end if;
            ptr := ptr + 1;
            if isHex(s(ptr)) then
               t1 := t1*16 + hexDigit(s(ptr));
            else
               return;
            end if;
            ptr := ptr + 1;
            check := check + BBS.uint8(t1);
            memory(BBS.uint16(i) + addr) := BBS.uint8(t1);
         end loop;
         addr := addr + count;
      end if;
      --
      --  Check checksum
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + BBS.uint8(t1);
      if check = 0 then
         valid := True;
      else
         Ada.Text_IO.Put_Line("Checksum value for " & s & " is " & toHex(check));
      end if;
   end;
   --
   --  This loads data from a file specified by "name" into the simulator memory.
   --  Currently, only Intel Hex format is supported.
   --
   procedure load(name : String) is
      inp   : Ada.Text_IO.File_Type;
      line  : Ada.Strings.Unbounded.Unbounded_String;
      valid : Boolean;
   begin
      Ada.Text_IO.Open(inp, Ada.Text_IO.In_File, name);
      while not Ada.Text_IO.End_Of_File(inp) loop
         Ada.Text_IO.Unbounded_IO.Get_Line(inp, line);
         IntelHex(Ada.Strings.Unbounded.To_String(line), valid);
      end loop;
      Ada.Text_IO.Close(inp);
   exception
      when Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line("Error in file name: " & name);
      when others =>
         Ada.Text_IO.Put_Line("Error occured processing " & name);
         Ada.Text_IO.Close(inp);
   end;
end cpm_util;
