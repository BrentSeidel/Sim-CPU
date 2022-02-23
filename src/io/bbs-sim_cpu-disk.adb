--
--  Contains I/O devices for various kinds of disk
--
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
                  null;
               when 2 =>  --  Write
                  null;
               when 3 =>  -- Select Disk
                  null;
               when others =>  --  Should never happen
                  null;
            end case;
         when 1 =>  --  Sector number
            if value < floppy_sectors then
               self.sector := value;
            end if;
         when 2 =>  --  Track number
            if value < floppy_tracks then
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
            return data_bus(self.selected_drive);
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
   --  Open the attached file
   --
   procedure open(self : in out floppy8; drive : Natural; name : String) is
   begin
      if drive < drives then
         if self.drive_info(drive).present then
            floppy_io.Close(self.drive_info(drive).Image);
         end if;
         floppy_io.Open(self.drive_info(drive).image, floppy_io.Inout_File,
                        name);
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
end;
