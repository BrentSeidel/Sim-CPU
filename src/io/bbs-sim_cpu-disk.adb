--
--  Contains I/O devices for various kinds of disk
--
with Ada.Text_IO;
with BBS.embed;
use type BBS.embed.uint32;
package body BBS.Sim_CPU.disk is
   --    0 - Control port
   --    1 - Sector number
   --    2 - Track number
   --    3 - DMA address LSB
   --    4 - DMA address MSB
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out floppy8; addr : addr_bus; data : data_bus) is
      offset : byte := byte((addr - self.base) and 16#FF#);
      value  : byte := byte(data and 16#FF#);
      drive  : byte;
      action : byte;
   begin
      case offset is
         when 0 =>  --  Control port
            drive := value and 16#0F#;
            action := (value and 16#C0#)/16#40#;
            case action is
               when 0 =>  --  None
                  null;
               when 1 =>  --  Read
                  self.read;
               when 2 =>  --  Write
                  self.write;
               when 3 =>  -- Select Disk
                  if Natural(drive) < drives then
                     self.selected_drive := Natural(drive);
                  end if;
               when others =>  --  Should never happen
                  null;
            end case;
         when 1 =>  --  Sector number
            if value < floppy8_geom.sectors then
               self.sector := value;
            end if;
         when 2 =>  --  Track number
            if value < floppy8_geom.tracks then
               self.track := value;
            end if;
         when 3 =>  --  DMA address LSB
            self.dma := (self.dma and 16#FF00#) or (addr_bus(data) and 16#FF#);
         when 4 =>  --  DMA address MSB
            self.dma := (self.dma and 16#00FF#) or (addr_bus(data) and 16#FF#) * 16#100#;
         when others =>
            null;
      end case;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out floppy8; addr : addr_bus) return data_bus is
      offset : byte := byte((addr - self.base) and 16#FF#);
   begin
      case offset is
         when 0 =>  --  Control port
            if self.drive_info(self.selected_drive).present then
               return data_bus(self.selected_drive) + 16#10#;
            else
               return data_bus(self.selected_drive);
            end if;
         when 1 =>  --  Sector number
            return data_bus(self.sector);
         when 2 =>  --  Track number
            return data_bus(self.track);
         when 3 =>  --  DMA address LSB
            return data_bus(self.dma and 16#FF#);
         when 4 =>  --  DMA address MSB
            self.dma := data_bus((self.dma and 16#FF00#)/16#100#);
         when others =>
            null;
      end case;
      return 0;
   end;
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out floppy8) return addr_bus is
   begin
      return self.base;
   end;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out floppy8; base : addr_bus) is
   begin
      self.base := base;
   end;
   --
   --  Set the owner (used mainly for DMA)
   --
   overriding
   procedure setOwner(self : in out floppy8; owner : sim_access) is
   begin
      self.host := owner;
   end;
   --
   --  Open the attached file.  If file does not exist, then create it.
   --
   procedure open(self : in out floppy8; drive : Natural; name : String) is
      buff : floppy_sector := (others => 0);
   begin
      if drive < drives then
         if self.drive_info(drive).present then
            floppy_io.Close(self.drive_info(drive).Image);
         end if;
         begin
            floppy_io.Open(self.drive_info(drive).image, floppy_io.Inout_File,
                           name);
         exception
            when floppy_io.Name_Error =>
               floppy_io.Create(self.drive_info(drive).image, floppy_io.Inout_File,
                                name);
               Ada.Text_IO.Put_Line("FD: Extending image for drive " & Natural'Image(drive) &
                             " as file " & name);
               for sect in 0 .. floppy8_geom.sectors - 1 loop
                  for track in 0 .. floppy8_geom.tracks - 1 loop
                     floppy_io.Write(self.drive_info(drive).image, buff);
                  end loop;
               end loop;
         end;
         self.drive_info(drive).present := True;
      end if;
   end;
   --
   --  Close the attached file
   --
   procedure close(self : in out floppy8; drive : Natural) is
   begin
      if drive < drives then
         if self.drive_info(drive).present then
            floppy_io.Close(self.drive_info(drive).Image);
         end if;
         self.drive_info(drive).present := False;
      end if;
   end;
   --
   --  Read a sector from the selected drive to owner's memeory
   --
   procedure read(self : in out floppy8) is
      buff : floppy_sector;
      sect : constant Natural := Natural(self.track)*Natural(floppy8_geom.sectors)
        + Natural(self.sector);
   begin
      if self.drive_info(self.selected_drive).present then
         floppy_io.Set_Index(self.drive_info(self.selected_drive).image,
                             floppy_io.Count(sect + 1));
         floppy_io.Read(self.drive_info(self.selected_drive).image, buff);
         for addr in 0 .. floppy8_geom.size - 1 loop
            self.host.set_mem(addr_bus(addr) + self.dma, data_bus(buff(addr)));
         end loop;
      end if;
   end;
   --
   --  write to the selected drive
   --
   procedure write(self : in out floppy8) is
      buff : floppy_sector;
      sect : constant Natural := Natural(self.track)*Natural(floppy8_geom.sectors)
        + Natural(self.sector);
   begin
      for addr in 0 .. floppy8_geom.size - 1 loop
         buff(addr) := byte(self.host.read_mem(addr_bus(addr) + self.dma) and 16#FF#);
      end loop;
      if self.drive_info(self.selected_drive).present then
         floppy_io.Set_Index(self.drive_info(self.selected_drive).image,
                             floppy_io.Count(sect + 1));
         floppy_io.Write(self.drive_info(self.selected_drive).image, buff);
      end if;
   end;
end;
