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
with Ada.Unchecked_Conversion;
package body BBS.Sim_CPU.io.disk.rk11 is
   function RKDS_to_word is new Ada.Unchecked_Conversion(source => tRKDS,
                                                         target => word);
   function RKER_to_word is new Ada.Unchecked_Conversion(source => tRKER,
                                                         target => word);
   function RKCS_to_word is new Ada.Unchecked_Conversion(source => tRKCS,
                                                         target => word);
   function RKDA_to_word is new Ada.Unchecked_Conversion(source => tRKDA,
                                                         target => word);
   --
   function word_to_RKDS is new Ada.Unchecked_Conversion(source => word,
                                                         target => tRKDS);
   function word_to_RKER is new Ada.Unchecked_Conversion(source => word,
                                                         target => tRKER);
   function word_to_RKCS is new Ada.Unchecked_Conversion(source => word,
                                                         target => tRKCS);
   function word_to_RKDA is new Ada.Unchecked_Conversion(source => word,
                                                         target => tRKDA);
   --
   --  Port useage (base +)
   --     0/ 1 - RKDS - Drive status register
   --     2/ 3 - RKER - Error register
   --     4/ 5 - RKCS - Control status register
   --     6/ 7 - RKWC - Word count register
   --     8/ 9 - RKBA - Bus address register (current memory address)
   --    10/11 - RKDA - Disk address register
   --    12/13 - Unused
   --    14/15 - RKDB - Data buffer register
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out rk11; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
      value  : constant byte := byte(data and 16#FF#);
      drive  : byte;
      action : byte;
   begin
      case size is
         when bits8 =>
            case offset is
               when RKDSlsb =>  --  Drive status register LSB (read only)
                  null;
               when RKDSmsb =>  --  Drive status register MSB (read only)
                  null;
               when RKERlsb =>  --  Error register LSB (read only)
                  null;
               when RKERmsb =>  --  Error register MSB (read only)
                  null;
               when RKCSlsb =>  --  Control status register LSB
                  self.RKCS := word_to_RKCS((RKCS_to_word(self.RKCS) and 16#FF00#) or word(data and 16#FF#));
               when RKCSmsb =>  --  Control status register MSB
                  self.RKCS := word_to_RKCS((RKCS_to_word(self.RKCS) and 16#FF#) or word(data and 16#FF#)*16#100#);
               when RKWClsb =>  --  Transfer word count LSB
                  self.RKWC := (self.RKWC and 16#FF00#) or word(data and 16#FF#);
               when RKWCmsb =>  --  Transfer word count MSB
                  self.RKWC := (self.RKWC and 16#FF#) or word(data and 16#FF#)*16#100#;
               when RKBAlsb =>  --  DMA address LSB
                  self.RKBA := (self.RKBA and 16#FFF00#) or (addr_bus(data) and 16#FF#);
               when RKBAmsb =>  --  DMA address MSB
                  self.RKBA := (self.RKBA and 16#00FF#) or (addr_bus(data) and 16#FF#) * 16#100#;
               when RKDAlsb =>  --  Drive sector and track/cylinder
                  self.RKDA := word_to_RKDA((RKDA_to_word(self.RKDA) and 16#FF00#) or word(data and 16#FF#));
               when RKDAmsb =>  --  Drive sector and track/cylinder
                  self.RKDA := word_to_RKDA((RKDA_to_word(self.RKDA) and 16#FF#) or word(data and 16#FF#)*16#100#);
               when RKun1 | RKun2 =>  --  unused offset
                  status := BUS_NONE;
               when RKDBlsb | RKDBmsb =>  --  Data buffer register
                  null;
               when others =>
                  status := BUS_NONE;
            end case;
         when bits16 =>
            case offset is
               when RKDSlsb =>  --  Drive status register LSB (read only)
                  null;
               when RKERlsb =>  --  Error register LSB (read only)
                  null;
               when RKCSlsb =>  --  Control status register LSB
                  self.RKCS := word_to_RKCS(word(data and 16#FFFF#));
                  self.RKBA := (self.RKBA and 16#FFFF#) or addr_bus(self.RKCS.ext_addr)*16#10000#;
               when RKWClsb =>  --  Transfer word count LSB
                  self.RKWC := word(data and 16#FFFF#);
               when RKBAlsb =>  --  DMA address LSB
                  self.RKBA := (self.RKBA and 16#F0000#) or (addr_bus(data) and 16#FFFF#);
               when RKDAlsb =>  --  Drive sector and track/cylinder
                  self.RKDA := word_to_RKDA(word(data and 16#FFFF#));
               when RKun1 =>  --  unused offset
                  status := BUS_NONE;
               when RKDBlsb =>  --  Data buffer register
                  null;
               when others =>
                  status := BUS_NONE;
            end case;
         when others =>
            status := BUS_NONE;
      end case;
      if self.RKCS.go then
         self.process_command;
      end if;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out rk11; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus is
      drive     : disk_info renames self.drive_info(self.selected_drive);
      offset    : constant byte := byte((addr - self.base) and 16#FF#);
      disk_geom : constant geometry := self.drive_info(self.selected_drive).geom;
      ret_val   : data_bus := 0;
      range_err : Boolean := False;
   begin
      case size is
         when bits8 =>
            case offset is
               when RKDSlsb =>  --  Drive status register LSB (read only)
                  ret_val := data_bus(RKDS_to_word(self.RKDS) and 16#FF#);
               when RKDSmsb =>  --  Drive status register MSB (read only)
                  ret_val := data_bus(RKDS_to_word(self.RKDS) and 16#FF00#)/16#100#;
               when RKERlsb =>  --  Error register LSB (read only)
                  ret_val := data_bus(RKER_to_word(self.RKER) and 16#FF#);
               when RKERmsb =>  --  Error register MSB (read only)
                  ret_val := data_bus(RKER_to_word(self.RKER) and 16#FF00#)/16#100#;
               when RKCSlsb =>  --  Control status register LSB
                  ret_val := data_bus(RKCS_to_word(self.RKCS) and 16#FF#);
               when RKCSmsb =>  --  Control status register MSB
                  ret_val := data_bus(RKCS_to_word(self.RKCS) and 16#FF00#)/16#100#;
               when RKWClsb =>  --  Transfer word count LSB
                  ret_val := data_bus(self.RKWC and 16#FF#);
               when RKWCmsb =>  --  Transfer word count MSB
                  ret_val := data_bus(self.RKWC and 16#FF00#)/16#100#;
               when RKBAlsb =>  --  DMA address LSB
                  ret_val := data_bus(self.RKBA and 16#FF#);
               when RKBAmsb =>  --  DMA address MSB
                  ret_val := data_bus(self.RKBA and 16#FF00#)/16#100#;
               when RKDAlsb =>  --  Drive sector and track/cylinder
                  ret_val := data_bus(RKDA_to_word(self.RKDA) and 16#FF#);
               when RKDAmsb =>  --  Drive sector and track/cylinder
                  ret_val := data_bus(RKDA_to_word(self.RKDA) and 16#FF00#)/16#100#;
               when RKun1 | RKun2 =>  --  unused offset
                  ret_val := 0;
                  status := BUS_NONE;
               when RKDBlsb | RKDBmsb =>  --  Data buffer register
                  ret_val := 0;
               when others =>
                  ret_val := 0;
                  status := BUS_NONE;
            end case;
         when bits16 =>
            case offset is
               when RKDSlsb =>  --  Drive status register LSB (read only)
                  ret_val := data_bus(RKDS_to_word(self.RKDS));
               when RKERlsb =>  --  Error register LSB (read only)
                  ret_val := data_bus(RKER_to_word(self.RKER));
               when RKCSlsb =>  --  Control status register LSB
                  ret_val := data_bus(RKCS_to_word(self.RKCS));
               when RKWClsb =>  --  Transfer word count LSB
                  ret_val := data_bus(self.RKWC);
               when RKBAlsb =>  --  DMA address LSB
                  ret_val := data_bus(self.RKBA and 16#FFFF#);
               when RKDAlsb =>  --  Drive sector and track/cylinder
                  ret_val := data_bus(RKDA_to_word(self.RKDA));
               when RKun1 =>  --  unused offset
                  ret_val := 0;
                  status := BUS_NONE;
               when RKDBlsb =>  --  Data buffer register
                  ret_val := 0;
               when others =>
                  ret_val := 0;
                  status := BUS_NONE;
            end case;
         when others =>
            ret_val := 0;
            status := BUS_NONE;
      end case;
      return 0;
   end;
   --
   --  Process the command specified in RKCS
   --  Drive function codes are:
   --    0 - Control Reset
   --    1 - Write
   --    2 - Read
   --    3 - Write Check
   --    4 - Seek
   --    5 - Read Check
   --    6 - Drive Reset
   --    7 - Write lock
   --
   procedure process_command(self : in out rk11) is
   begin
      case self.RKCS.drv_func is
         when 0 =>  --  control reset
            self.RKDS.pwr_low := False;
            self.RKDS.drv_id := 0;
            self.RKWC := 0;
            self.RKBA := 0;
            self.RKDA := word_to_RKDA(0);
            self.RKCS := word_to_RKCS(0);
            self.RKCS.ctrl_rdy := True;
         when 1 =>  --  Write
            null;
         when 2 =>  --  Read
            null;
         when 3 =>  --  Write check
            null;
         when 4 =>  --  Seek
            null;
         when 5 =>  --  Read check
            null;
         when 6 =>  --  Drive reset
            null;
         when 7 =>  --  Write lock
            self.drive_info(byte(self.RKDA.drive)).writeable := False;
            self.RKCS.go := False;
      end case;
   end;
   --
   --  Open the attached file.  If file does not exist, then create it.
   --
   procedure open(self : in out rk11; drive : byte;
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
   procedure extend(self : in out rk11; drive : byte;
                  geom : geometry; name : String) is
      buff : disk_sector := (others => 0);
   begin
      begin
         disk_io.Create(self.drive_info(drive).image, disk_io.Inout_File,
                        name);
      exception
         when disk_io.Name_Error =>
            Ada.Text_IO.Put_Line("RK11: Unable to attach to file <" & name & ">");
            self.drive_info(drive).present := False;
            return;
      end;
      Ada.Text_IO.Put_Line("RK11: Extending image for drive " & byte'Image(drive) &
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
   function fname(self : in out rk11; drive : byte) return String is
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
   function present(self : in out rk11; drive : byte) return Boolean is
   begin
      return self.drive_info(drive).present;
   end;
   --
   --  Is the specified drive read-only?
   --
   function readonly(self : in out rk11; drive : byte) return Boolean is
   begin
      return not self.drive_info(drive).writeable;
   end;
   --
   --  Set the specified drive's read-only state?
   --
   procedure readonly(self : in out rk11; drive : byte; state : Boolean) is
   begin
      self.drive_info(drive).writeable := not state;
   end;
   --
   --  Close the attached file
   --
   procedure close(self : in out rk11; drive : byte) is
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
      drive : disk_info renames self.drive_info(self.selected_drive);
      buff : disk_sector;
      sect : Natural := Natural(drive.track)*Natural(drive.geom.sectors)
        + Natural(drive.sector - 1);
      count : word := self.RKWC;
      base  : addr_bus := self.RKBA;
   begin
      if halt_on_io_error then
         if (drive.sector > drive.geom.sectors) or (drive.sector = 0) then
            Ada.Text_IO.Put_Line("RK11 Read sector out of range: " & word'Image(drive.sector));
            self.host.halt;
            return;
         end if;
         if drive.track > drive.geom.tracks then
            Ada.Text_IO.Put_Line("RK11 Read track out of range: " & word'Image(drive.track));
            self.host.halt;
            return;
         end if;
      end if;
      if drive.present then
         for i in 1 .. count loop
            disk_io.Set_Index(drive.image,
                                disk_io.Count(sect + 1));
            disk_io.Read(drive.image, buff);
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
      drive : disk_info renames self.drive_info(self.selected_drive);
      buff : disk_sector;
      sect : Natural := Natural(drive.track)*Natural(drive.geom.sectors)
        + Natural(drive.sector - 1);
      count : word := self.RKWC;
      base  : addr_bus := self.RKBA;
   begin
      if halt_on_io_error then
         if (drive.sector > drive.geom.sectors) or (drive.sector = 0) then
            Ada.Text_IO.Put_Line("RK11 Write sector out of range: " & word'Image(drive.sector));
            self.host.halt;
            return;
         end if;
         if drive.track > drive.geom.tracks then
            Ada.Text_IO.Put_Line("RK11 Write track out of range: " & word'Image(drive.track));
            self.host.halt;
            return;
         end if;
      end if;
      if drive.present and drive.writeable then
         for i in 1 .. count loop
            for addr in 0 .. sector_size - 1 loop
               buff(addr) := byte(self.host.read_mem(addr_bus(addr) + base) and 16#FF#);
            end loop;
            disk_io.Set_Index(drive.image,
                                disk_io.Count(sect + 1));
            disk_io.Write(drive.image, buff);
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
