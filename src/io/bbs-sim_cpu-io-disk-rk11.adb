--
--  Author: Brent Seidel
--  Date: 22-Jan-2026
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
--  Contains I/O device for simulated RK11 disk controller
--
with Ada.Text_IO;
package body BBS.Sim_CPU.io.disk.rk11 is
   --    0 - Control port
   --    1 - Sector number LSB
   --    2 - Sector number MSB
   --    3 - Track number LSB
   --    4 - Track number MSB
   --    5 - DMA address LSB
   --    6 - DMA address MSB
   --    7 - Count (number of sectors to read)
   --    8 - Head number (not yet implemented)
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out rk11; addr : addr_bus; data : data_bus) is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
      value  : constant byte := byte(data and 16#FF#);
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
                                            "  Sector " & word'Image(self.sector) & ", Track " &
                                         word'Image(self.track));
                  end if;
                  self.read;
               when 2 =>  --  Write
                  if (word(self.host.trace) and 8) = 8 then
                     Ada.Text_IO.Put_Line("DSK: Write drive " & Natural'Image(self.selected_drive) &
                                            "  Sector " & word'Image(self.sector) & ", Track " &
                                            word'Image(self.track));
                  end if;
                  self.write;
               when 3 =>  -- Select Disk
                  if (word(self.host.trace) and 8) = 8 then
                     Ada.Text_IO.Put_Line("DSK: Select drive " & byte'Image(drive));
                  end if;
                  self.selected_drive := Natural(drive);
               when others =>  --  Should never happen
                  null;
            end case;
         when 1 =>  --  Sector number LSB
            self.sector := (self.sector and 16#FF00#) or word(value);
            if (word(self.host.trace) and 8) = 8 then
               Ada.Text_IO.Put_Line("DSK: Set sector LSB" & byte'Image(value) &
                                    ", actual " & word'Image(self.sector));
            end if;
         when 2 =>  --  Sector number MSB
            self.sector := (self.sector and 16#FF#) or (word(value)*16#100#);
            if (word(self.host.trace) and 8) = 8 then
               Ada.Text_IO.Put_Line("DSK: Set sector MSB" & byte'Image(value) &
                                    ", actual " & word'Image(self.sector));
            end if;
         when 3 =>  --  Track number LSB
            self.track := (self.track and 16#FF00#) or word(value);
            if (word(self.host.trace) and 8) = 8 then
               Ada.Text_IO.Put_Line("DSK: Set track LSB " & byte'Image(value) &
                                    ", actual " & word'Image(self.track));
            end if;
         when 4 =>  --  Track number MSB
            self.track := (self.track and 16#FF#) or (word(value)*16#100#);
            if (word(self.host.trace) and 8) = 8 then
               Ada.Text_IO.Put_Line("DSK: Set track MSB " & byte'Image(value) &
                                    ", actual " & word'Image(self.track));
            end if;
         when 5 =>  --  DMA address LSB
            self.dma := (self.dma and 16#FF00#) or (addr_bus(data) and 16#FF#);
         when 6 =>  --  DMA address MSB
            self.dma := (self.dma and 16#00FF#) or (addr_bus(data) and 16#FF#) * 16#100#;
         when 7 =>  --  Sector count
            self.count := value;
         when others =>
            null;
      end case;
   end;
   --
   --  Read from a port address
   --
   --  Control port bits:
   --  0 - Selected drive
   --  1 - Selected drive
   --  2 - Selected drive
   --  3 - Selected drive
   --  4 - Read-only
   --  5 - Changed (cleared when port read)
   --  6 - Sector or track out of range
   --  7 - Not present
   overriding
   function read(self : in out rk11; addr : addr_bus) return data_bus is
      offset    : constant byte := byte((addr - self.base) and 16#FF#);
      disk_geom : constant geometry := self.drive_info(self.selected_drive).geom;
      ret_val   : data_bus := data_bus(self.selected_drive) and 16#0F#;
      range_err : Boolean := False;
   begin
      case offset is
         when 0 =>  --  Control port
            if not self.drive_info(self.selected_drive).writeable then
               ret_val := ret_val + 16#10#;
            end if;
            if self.drive_info(self.selected_drive).changed then
               ret_val := ret_val + 16#20#;
               self.drive_info(self.selected_drive).changed := False;
            end if;
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
         when 1 =>  --  Sector number LSB
            return data_bus(disk_geom.sectors and 16#FF#);
         when 2 =>  --  Sector number MSB
            return data_bus(disk_geom.sectors/16#100# and 16#FF#);
         when 3 =>  --  Track number LSB
            return data_bus(disk_geom.tracks and 16#FF#);
         when 4 =>  --  Track number MSB
            return data_bus(disk_geom.tracks/16#100# and 16#FF#);
         when 5 =>  --  DMA address LSB
            return data_bus(self.dma and 16#FF#);
         when 6 =>  --  DMA address MSB
            return data_bus(self.dma/16#100# and 16#FF#);
         when 7 =>  --  Sector count
            return data_bus(self.count);
         when others =>
            null;
      end case;
      return 0;
   end;
   --
   --  Open the attached file.  If file does not exist, then create it.
   --
   procedure open(self : in out rk11; drive : Natural;
         geom : geometry; name : String) is
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
            self.extend(drive, geom, name);
            return;
      end;
      self.drive_info(drive).geom      := geom;
      self.drive_info(drive).present   := True;
      self.drive_info(drive).changed   := True;
      self.drive_info(drive).writeable := True;
   end;
   --
   procedure extend(self : in out rk11; drive : Natural;
                  geom : geometry; name : String) is
      buff : disk_sector := (others => 0);
   begin
      begin
         disk_io.Create(self.drive_info(drive).image, disk_io.Inout_File,
                        name);
      exception
         when disk_io.Name_Error =>
            Ada.Text_IO.Put_Line("FD: Unable to attach to file <" & name & ">");
            self.drive_info(drive).present := False;
            return;
      end;
      Ada.Text_IO.Put_Line("FD: Extending image for drive " & Natural'Image(drive) &
                                   " as file " & name);
      for sect in 0 .. geom.sectors - 1 loop
         for track in 0 .. geom.tracks - 1 loop
            disk_io.Write(self.drive_info(drive).image, buff);
         end loop;
      end loop;
      self.drive_info(drive).geom      := geom;
      self.drive_info(drive).present   := True;
      self.drive_info(drive).changed   := True;
      self.drive_info(drive).writeable := True;
   end;
   --
   --  Get the name of the attached file, if any.
   --
   function fname(self : in out rk11; drive : Natural) return String is
   begin
      if self.drive_info(drive).present then
         return disk_io.Name(self.drive_info(drive).image);
      else
         return ">closed<";
      end if;
   end;
   --
   --  Is a file attached to the specified drive?
   --
   function present(self : in out rk11; drive : Natural) return Boolean is
   begin
      return self.drive_info(drive).present;
   end;
   --
   --  Is the specified drive read-only?
   --
   function readonly(self : in out rk11; drive : Natural) return Boolean is
   begin
      return not self.drive_info(drive).writeable;
   end;
   --
   --  Set the specified drive's read-only state?
   --
   procedure readonly(self : in out rk11; drive : Natural; state : Boolean) is
   begin
      self.drive_info(drive).writeable := not state;
   end;
   --
   --  Close the attached file
   --
   procedure close(self : in out rk11; drive : Natural) is
   begin
      if self.drive_info(drive).present then
         disk_io.Close(self.drive_info(drive).Image);
      end if;
      self.drive_info(drive).present := False;
      self.drive_info(drive).writeable := False;
   end;
   --
   --  Read a sector from the selected drive to owner's memeory
   --
   procedure read(self : in out rk11) is
      buff : disk_sector;
      sect : Natural := Natural(self.track)*Natural(self.drive_info(self.selected_drive).geom.sectors)
        + Natural(self.sector - 1);
      count : byte := self.count;
      base  : addr_bus := self.dma;
   begin
      if halt_on_io_error then
         if (self.sector > self.drive_info(self.selected_drive).geom.sectors) or (self.sector = 0) then
            Ada.Text_IO.Put_Line("FD Read sector out of range: " & word'Image(self.sector));
            self.host.halt;
            return;
         end if;
         if self.track > self.drive_info(self.selected_drive).geom.tracks then
            Ada.Text_IO.Put_Line("FD Read track out of range: " & word'Image(self.track));
            self.host.halt;
            return;
         end if;
      end if;
      if self.drive_info(self.selected_drive).present then
         for i in 1 .. count loop
--            if self.selected_drive = 7 then
--               Ada.Text_IO.Put_Line("FD Read block " & Natural'Image(sect) & ", track " &
--                                      Natural'Image(Natural(self.track)) & ", sector " &
--                                      Natural'Image(Natural(self.sector)));
--            end if;
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
   procedure write(self : in out rk11) is
      buff : disk_sector;
      sect : Natural := Natural(self.track)*Natural(self.drive_info(self.selected_drive).geom.sectors)
        + Natural(self.sector - 1);
      count : byte := self.count;
      base  : addr_bus := self.dma;
   begin
      if halt_on_io_error then
         if (self.sector > self.drive_info(self.selected_drive).geom.sectors) or (self.sector = 0) then
            Ada.Text_IO.Put_Line("FD Write sector out of range: " & word'Image(self.sector));
            self.host.halt;
            return;
         end if;
         if self.track > self.drive_info(self.selected_drive).geom.tracks then
            Ada.Text_IO.Put_Line("FD Write track out of range: " & word'Image(self.track));
            self.host.halt;
            return;
         end if;
      end if;
      if self.drive_info(self.selected_drive).present and
         self.drive_info(self.selected_drive).writeable then
         for i in 1 .. count loop
--            if self.selected_drive = 7 then
--               Ada.Text_IO.Put_Line("FD Write block " & Natural'Image(sect) & ", track " &
--                                      Natural'Image(Natural(self.track)) & ", sector " &
--                                      Natural'Image(Natural(self.sector)));
--            end if;
            for addr in 0 .. sector_size - 1 loop
               buff(addr) := byte(self.host.read_mem(addr_bus(addr) + base) and 16#FF#);
            end loop;
--            if self.selected_drive = 7 then
--               dump_sect(buff);
--            end if;
            disk_io.Set_Index(self.drive_info(self.selected_drive).image,
                                disk_io.Count(sect + 1));
            disk_io.Write(self.drive_info(self.selected_drive).image, buff);
            sect := sect + 1;
            base := base + addr_bus(sector_size);
         end loop;
      end if;
   end;
   -- -------------------------------------------------------------------------
   --
   --  Dump disk buffer
   --
   procedure dump_sect(buff : disk_sector) is
      temp : byte;
   begin
      Ada.Text_IO.Put("    ");
      for i in 0 ..  15 loop
         Ada.Text_IO.Put(" " & toHex(byte(i)));
      end loop;
      Ada.Text_IO.New_Line;
      for i in 0 .. ((sector_size + 1)/16) - 1 loop
         Ada.Text_IO.Put(toHex(byte(i)) & " :");
         for j in 0 .. 15 loop
            Ada.Text_IO.Put(" " & toHex(buff(uint16(j) + i*16)));
         end loop;
         Ada.Text_IO.Put(" ");
         for j in 0 .. 15 loop
            temp := buff(uint16(j) + i*16);
            if (temp < 32) or (temp > 126) then  --  Check for printable character
               Ada.Text_IO.Put(".");
            else
               Ada.Text_IO.Put(Character'Val(temp));
            end if;
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end;
end;
