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
--  Contains I/O devices for various kinds of disk
--
with Ada.Text_IO;
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
   procedure write(self : in out fd_ctrl; addr : addr_bus; data : data_bus) is
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
   function read(self : in out fd_ctrl; addr : addr_bus) return data_bus is
      offset    : constant byte := byte((addr - self.base) and 16#FF#);
      disk_geom : constant geometry := self.drive_info(self.selected_drive).geom;
      ret_val   : data_bus := data_bus(self.selected_drive);
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
   --  Open the attached file.  If file does not exist, then create it.
   --
   procedure open(self : in out fd_ctrl; drive : drive_num;
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
      self.drive_info(drive).geom := geom;
      self.drive_info(drive).present := True;
      self.drive_info(drive).writeable := True;
   end;
   --
   --  Get the name of the attached file, if any.
   --
   function fname(self : in out fd_ctrl; drive : drive_num) return String is
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
   function present(self : in out fd_ctrl; drive : drive_num) return Boolean is
   begin
      return self.drive_info(drive).present;
   end;
   --
   --  Is the specified drive read-only?
   --
   function readonly(self : in out fd_ctrl; drive : drive_num) return Boolean is
   begin
      return not self.drive_info(drive).writeable;
   end;
   --
   --  Close the attached file
   --
   procedure close(self : in out fd_ctrl; drive : drive_num) is
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
   procedure read(self : in out fd_ctrl) is
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
   procedure write(self : in out fd_ctrl) is
      buff : disk_sector;
      sect : Natural := Natural(self.track)*Natural(floppy8_geom.sectors)
        + Natural(self.sector - 1);
      count : byte := self.count;
      base  : addr_bus := self.dma;
   begin
      if self.drive_info(self.selected_drive).present and
         self.drive_info(self.selected_drive).writeable then
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
   -- -------------------------------------------------------------------------
   --
   --  Definitions for a hard disk controller with 32 bit addressing.
   --  The geomentry of this device is simplified into a simple linear
   --  sequence of blocks.
   --
   --  Port usage (base +)
   --    0 - Command
   --    1 - Status
   --    2 - Value MSB
   --    3 - Value
   --    4 - Value
   --    5 - Value LSB
   --
   --  Commands are:
   --    0 - Do nothing
   --    1 - Set drive
   --    2 - Set starting block
   --    3 - Set block count
   --    4 - Set DMA address
   --    5 - Read data
   --    6 - Write data
   --    7 - Read max drive number
   --    8 - Read max block number
   --  Status bits are
   --    7 - Unused
   --    6 - Unused
   --    5 - Unused
   --    4 - Bad command
   --    3 - Drive out of range
   --    2 - Count out of range
   --    1 - Starting block out of range
   --    0 - Drive out of range
   --
   --  In operation, write the value first and then issue the set command to
   --  set a value.  To read a value, issue a read command, then read the value.
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out hd_ctrl; addr : addr_bus; data : data_bus) is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
      value  : constant byte := byte(data and 16#FF#);
      temp   : data_bus;
   begin
      case offset is
         when 0 =>  --  Command port
            temp := data_bus(self.t0)*16#0100_0000# +
                    data_bus(self.t1)*16#0001_0000# +
                    data_bus(self.t2)*16#0000_0100# +
                    data_bus(self.t3);
            self.status := self.status and 16#EF#;
            case value is
               when 0 =>  --  None
                  null;
               when 1 =>  --  Set drive
                  if temp > data_bus(drive_num'Last) then
                     self.status := self.status or 16#08#;
                  else
                     self.drive := byte(temp and 16#FF#);
                     self.status := self.status and 16#F7#;
                  end if;
               when 2 =>  --  Set starting block
                  if self.drive_info(Integer(self.drive)).present and then
                     self.drive_info(Integer(self.drive)).size > Natural(temp) then
                     self.block := addr_bus(temp);
                     self.status := self.status and 16#FD#;
                  else
                     self.status := self.status or 16#02#;
                  end if;
               when 3 =>  --  Set block count
                  if self.drive_info(Integer(self.drive)).present and then
                     self.drive_info(Integer(self.drive)).size > Natural(temp + self.block) then
                     self.count := addr_bus(temp);
                     self.status := self.status and 16#FB#;
                  else
                     self.status := self.status or 16#04#;
                  end if;
               when 4 =>  --  Set DMA address
                  self.dma := addr_bus(temp);
               when 5 =>  --  Read data
                  null;
               when 6 =>  --  Write data
                  null;
               when 7 =>  --  Read max drive number
                  self.t0 := byte((data_bus(drive_num'Last)/16#0100_0000#) and 16#FF#);
                  self.t1 := byte((data_bus(drive_num'Last)/16#0001_0000#) and 16#FF#);
                  self.t2 := byte((data_bus(drive_num'Last)/16#0000_0100#) and 16#FF#);
                  self.t3 := byte(data_bus(drive_num'Last) and 16#FF#);
               when 8 =>  --  Read max block number for drive
                  null;
               when others =>  --  Should never happen
                  self.status := self.status or 16#10#;
            end case;
         when 1 =>  --  Status port (RO so writes are ignored)
            null;
         when 2 =>  --  Value MSB
            self.t0 := value;
         when 3 =>  --  Value
            self.t1 := value;
         when 4 =>  --  Value
            self.t2 := value;
         when 5 =>  --  Value LSB
            self.t3 := value;
         when others =>
            null;
      end case;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out hd_ctrl; addr : addr_bus) return data_bus is
      offset    : constant byte := byte((addr - self.base) and 16#FF#);
--      disk_geom : constant geometry := self.drive_info(self.selected_drive).geom;
--      ret_val   : data_bus := data_bus(self.selected_drive);
--      range_err : Boolean := False;
   begin
      case offset is
         when 0 =>  --  Command port (WO so reads are ignored)
            return 0;
         when 1 =>  --  Status port
            return data_bus(self.status);
         when 2 =>  --  Value MSB
            return data_bus(self.t0);
         when 3 =>  --  Value
            return data_bus(self.t1);
         when 4 =>  --  Value
            return data_bus(self.t2);
         when 5 =>  --  Value LSB
            return data_bus(self.t3);
         when others =>
            null;
      end case;
      return 0;
   end;
   --
   --  Set which exception to use
   --
   procedure setException(self : in out hd_ctrl; except : long) is
   begin
      self.int_code := except;
   end;
   --
   --  Open the attached file.  If file does not exist, then create it.
   --
   procedure open(self : in out hd_ctrl; drive : drive_num;
         size : Natural; name : String) is
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
            Ada.Text_IO.Put_Line("HD: Extending image for drive " & Natural'Image(drive) &
                          " as file " & name);
            for block in 0 .. size - 1 loop
               disk_io.Write(self.drive_info(drive).image, buff);
            end loop;
      end;
      self.drive_info(drive).size := size;
      self.drive_info(drive).present := True;
   end;
   --
   --  Close the attached file
   --
   procedure close(self : in out hd_ctrl; drive : drive_num) is
   begin
      if self.drive_info(drive).present then
         disk_io.Close(self.drive_info(drive).Image);
      end if;
      self.drive_info(drive).present := False;
   end;
end;
