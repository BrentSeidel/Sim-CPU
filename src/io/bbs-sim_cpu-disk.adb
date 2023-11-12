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
   --    5 - Count (number of sectors to read)
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out disk_ctrl; addr : addr_bus; data : data_bus) is
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
                  if (word(self.host.trace) and 8) = 8 then
                     Ada.Text_IO.Put_Line("DSK: Read drive " & Natural'Image(self.selected_drive) &
                                            "  Sector " & byte'Image(self.sector) & ", Track " &
                                         byte'Image(self.track));
                  end if;
                  self.read;
               when 2 =>  --  Write
                  if (word(self.host.trace) and 8) = 8 then
                     Ada.Text_IO.Put_Line("DSK: Write drive " & Natural'Image(self.selected_drive) &
                                            "  Sector " & byte'Image(self.sector) & ", Track " &
                                            byte'Image(self.track));
                  end if;
                  self.write;
               when 3 =>  -- Select Disk
                  if (word(self.host.trace) and 8) = 8 then
                     Ada.Text_IO.Put_Line("DSK: Select drive " & Natural'Image(Natural(drive)));
                  end if;
                  self.selected_drive := Natural(drive);
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
         when 5 =>  --  Sector count
            self.count := value;
         when others =>
            null;
      end case;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out disk_ctrl; addr : addr_bus) return data_bus is
      offset : byte := byte((addr - self.base) and 16#FF#);
      disk_geom   : geometry := self.drive_info(self.selected_drive).geom;
      ret_val : data_bus := data_bus(self.selected_drive);
      range_err : Boolean := False;
   begin
      case offset is
         when 0 =>  --  Control port
            if self.sector > disk_geom.sectors then
               range_err := True;
            end if;
            if self.track > disk_geom.tracks then
               range_err := True;
            end if;
            if range_err then
               ret_val := ret_val + 16#40#;
            end if;
            if not self.drive_info(self.selected_drive).present then
               ret_val := ret_val + 16#80#;
            end if;
            return ret_val;
         when 1 =>  --  Sector number
            return data_bus(disk_geom.sectors);
         when 2 =>  --  Track number
            return data_bus(disk_geom.tracks);
         when 3 =>  --  DMA address LSB
            return data_bus(self.dma and 16#FF#);
         when 4 =>  --  DMA address MSB
            return data_bus((self.dma and 16#FF00#)/16#100#);
         when 5 =>  --  Sector count
            return data_bus(self.count);
         when others =>
            null;
      end case;
      return 0;
   end;
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out disk_ctrl) return addr_bus is
   begin
      return self.base;
   end;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out disk_ctrl; base : addr_bus) is
   begin
      self.base := base;
   end;
   --
   --  Set the owner (used mainly for DMA)
   --
   overriding
   procedure setOwner(self : in out disk_ctrl; owner : sim_access) is
   begin
      self.host := owner;
   end;
   --
   --  Open the attached file.  If file does not exist, then create it.
   --
   procedure open(self : in out disk_ctrl; drive : drive_num; name : String) is
      buff : disk_sector := (others => 0);
   begin
      if self.drive_info(drive).present then
         disk_io.Close(self.drive_info(drive).Image);
      end if;
      begin
         disk_io.Open(self.drive_info(drive).image, disk_io.Inout_File,
                        name);
      exception
      when disk_io.Name_Error =>
            disk_io.Create(self.drive_info(drive).image, disk_io.Inout_File,
                             name);
            Ada.Text_IO.Put_Line("FD: Extending image for drive " & Natural'Image(drive) &
                          " as file " & name);
            for sect in 0 .. floppy8_geom.sectors - 1 loop
               for track in 0 .. floppy8_geom.tracks - 1 loop
                  disk_io.Write(self.drive_info(drive).image, buff);
               end loop;
            end loop;
      end;
      self.drive_info(drive).present := True;
   end;
   --
   --  Close the attached file
   --
   procedure close(self : in out disk_ctrl; drive : drive_num) is
   begin
      if self.drive_info(drive).present then
         disk_io.Close(self.drive_info(drive).Image);
      end if;
      self.drive_info(drive).present := False;
   end;
   --
   --  Read a sector from the selected drive to owner's memeory
   --
   procedure read(self : in out disk_ctrl) is
      buff : disk_sector;
      sect : Natural := Natural(self.track)*Natural(floppy8_geom.sectors)
        + Natural(self.sector - 1);
      count : byte := self.count;
      base  : addr_bus := self.dma;
   begin
      if self.drive_info(self.selected_drive).present then
         for i in 1 .. count loop
            disk_io.Set_Index(self.drive_info(self.selected_drive).image,
                                disk_io.Count(sect + 1));
            disk_io.Read(self.drive_info(self.selected_drive).image, buff);
            for addr in 0 .. sector_size - 1 loop
               self.host.set_mem(addr_bus(addr) + base, data_bus(buff(addr)));
            end loop;
            sect := sect + 1;
            base := base + addr_bus(sector_size);
         end loop;
      end if;
   end;
   --
   --  write to the selected drive
   --
   procedure write(self : in out disk_ctrl) is
      buff : disk_sector;
      sect : Natural := Natural(self.track)*Natural(floppy8_geom.sectors)
        + Natural(self.sector - 1);
      count : byte := self.count;
      base  : addr_bus := self.dma;
   begin
      if self.drive_info(self.selected_drive).present then
         for i in 1 .. count loop
            for addr in 0 .. sector_size - 1 loop
               buff(addr) := byte(self.host.read_mem(addr_bus(addr) + base) and 16#FF#);
            end loop;
            disk_io.Set_Index(self.drive_info(self.selected_drive).image,
                                disk_io.Count(sect + 1));
            disk_io.Write(self.drive_info(self.selected_drive).image, buff);
            sect := sect + 1;
            base := base + addr_bus(sector_size);
         end loop;
      end if;
   end;
end;
