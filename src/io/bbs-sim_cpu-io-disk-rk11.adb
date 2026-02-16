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
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out rk11; except : long) is
   begin
      self.vector := byte(except and 16#FF#);
   end;
   --
   --  Reset/Initialize device
   --
   overriding
   procedure reset(self : in out rk11) is
   begin
      self.RKDS.pwr_low := False;
      self.RKDS.drv_id := 0;
      self.RKWC := 0;
      self.RKBA := 0;
      self.RKDA := word_to_RKDA(0);
      self.RKCS := word_to_RKCS(0);
      self.RKER := word_to_RKER(0);
      self.RKCS.ctrl_rdy := True;
      for i in self.drive_info'Range loop
--         self.drive_info(i).sector  := 0;
         self.drive_info(i).track   := 0;
--         self.drive_info(i).surface := False;
      end loop;
   end;
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
            Ada.Text_IO.Put("RK11: Writing byte " & toOct(byte(data and 16#FF#)) & " to address " & toOct(addr));
            case offset is
               when RKDSlsb =>  --  Drive status register LSB (read only)
                  Ada.Text_IO.Put_Line(" RKDS lsb (read only)");
                  null;
               when RKDSmsb =>  --  Drive status register MSB (read only)
                  Ada.Text_IO.Put_Line(" RKDS Msb (read only)");
                  null;
               when RKERlsb =>  --  Error register LSB (read only)
                  Ada.Text_IO.Put_Line(" RKER lsb (read only)");
                  null;
               when RKERmsb =>  --  Error register MSB (read only)
                  Ada.Text_IO.Put_Line(" RKER msb (read only)");
                  null;
               when RKCSlsb =>  --  Control status register LSB
                  Ada.Text_IO.Put_Line(" RKCS lsb");
                  self.RKCS := word_to_RKCS((RKCS_to_word(self.RKCS) and 16#FF00#) or word(data and 16#FF#));
               when RKCSmsb =>  --  Control status register MSB
                  Ada.Text_IO.Put_Line(" RKCS msb");
                  self.RKCS := word_to_RKCS((RKCS_to_word(self.RKCS) and 16#FF#) or word(data and 16#FF#)*16#100#);
               when RKWClsb =>  --  Transfer word count LSB
                  Ada.Text_IO.Put_Line(" RKWC lsb");
                  self.RKWC := (self.RKWC and 16#FF00#) or word(data and 16#FF#);
               when RKWCmsb =>  --  Transfer word count MSB
                  Ada.Text_IO.Put_Line(" RKWC msb");
                  self.RKWC := (self.RKWC and 16#FF#) or word(data and 16#FF#)*16#100#;
               when RKBAlsb =>  --  DMA address LSB
                  Ada.Text_IO.Put_Line(" RKBA lsb");
                  self.RKBA := (self.RKBA and 16#FFF00#) or (addr_bus(data) and 16#FF#);
               when RKBAmsb =>  --  DMA address MSB
                  Ada.Text_IO.Put_Line(" RKBA msb");
                  self.RKBA := (self.RKBA and 16#00FF#) or (addr_bus(data) and 16#FF#) * 16#100#;
               when RKDAlsb =>  --  Drive sector and track/cylinder
                  Ada.Text_IO.Put_Line(" RKDA lsb");
                  self.RKDA := word_to_RKDA((RKDA_to_word(self.RKDA) and 16#FF00#) or word(data and 16#FF#));
               when RKDAmsb =>  --  Drive sector and track/cylinder
                  Ada.Text_IO.Put_Line(" RKDA lsb");
                  self.RKDA := word_to_RKDA((RKDA_to_word(self.RKDA) and 16#FF#) or word(data and 16#FF#)*16#100#);
               when RKun1 | RKun2 =>  --  unused offset
                  Ada.Text_IO.Put_Line(" RKDS lsb or msb (read only)");
                  null;
               when RKDBlsb | RKDBmsb =>  --  Data buffer register
                  Ada.Text_IO.Put_Line(" RKDS lsb or msb (read only)");
                  null;
               when others =>
                  status := BUS_NONE;
            end case;
         when bits16 =>
            Ada.Text_IO.Put("RK11: Writing word " & toOct(word(data and 16#FFFF#)) & " to address " & toOct(addr));
            case offset is
               when RKDSlsb =>  --  Drive status register LSB (read only)
                  Ada.Text_IO.Put_Line(" RKDS (read only)");
                  null;
               when RKERlsb =>  --  Error register LSB (read only)
                  Ada.Text_IO.Put_Line(" RKER (read only)");
                  null;
               when RKCSlsb =>  --  Control status register LSB
                  Ada.Text_IO.Put_Line(" RKCS");
                  self.RKCS := word_to_RKCS(word(data and 16#FFFF#));
                  self.RKBA := (self.RKBA and 16#FFFF#) or addr_bus(self.RKCS.ext_addr)*16#10000#;
               when RKWClsb =>  --  Transfer word count LSB
                  Ada.Text_IO.Put_Line(" RKWC");
                  self.RKWC := word(data and 16#FFFF#);
               when RKBAlsb =>  --  DMA address LSB
                  Ada.Text_IO.Put_Line(" RKBA");
                  self.RKBA := (self.RKBA and 16#F0000#) or (addr_bus(data) and 16#FFFF#);
               when RKDAlsb =>  --  Drive sector and track/cylinder
                  Ada.Text_IO.Put_Line(" RKDA");
                  self.RKDA := word_to_RKDA(word(data and 16#FFFF#));
               when RKun1 =>  --  unused offset
                  Ada.Text_IO.Put_Line(" unused (read only)");
                  null;
               when RKDBlsb =>  --  Data buffer register
                  Ada.Text_IO.Put_Line(" RKDB (read only)");
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
      ret_val   : data_bus := 0;
      range_err : Boolean := False;
   begin
      case size is
         when bits8 =>
            Ada.Text_IO.Put("RK11: Reading byte from address " & toOct(addr));
            case offset is
               when RKDSlsb =>  --  Drive status register LSB (read only)
                  self.RKDS.protect := True;
                  ret_val := data_bus(RKDS_to_word(self.RKDS) and 16#FF#);
               when RKDSmsb =>  --  Drive status register MSB (read only)
                  self.RKDS.protect := True;
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
               when RKDBlsb | RKDBmsb =>  --  Data buffer register
                  ret_val := 0;
               when others =>
                  ret_val := 0;
                  status := BUS_NONE;
            end case;
         when bits16 =>
            Ada.Text_IO.Put("RK11: Reading word from address " & toOct(addr));
            case offset is
               when RKDSlsb =>  --  Drive status register (read only)
                  self.RKDS.protect := True;
                  Ada.Text_IO.Put(", RKDS");
                  ret_val := data_bus(RKDS_to_word(self.RKDS));
               when RKERlsb =>  --  Error register (read only)
                  Ada.Text_IO.Put(", RKER");
                  ret_val := data_bus(RKER_to_word(self.RKER));
               when RKCSlsb =>  --  Control status register
                  Ada.Text_IO.Put(", RKCS");
                  ret_val := data_bus(RKCS_to_word(self.RKCS));
               when RKWClsb =>  --  Transfer word count
                  Ada.Text_IO.Put(", RKWC");
                  ret_val := data_bus(self.RKWC);
               when RKBAlsb =>  --  DMA address
                  Ada.Text_IO.Put(", RKBA");
                  ret_val := data_bus(self.RKBA and 16#FFFF#);
               when RKDAlsb =>  --  Drive sector and track/cylinder
                  Ada.Text_IO.Put(", RKDA");
                  ret_val := data_bus(RKDA_to_word(self.RKDA));
               when RKun1 =>  --  unused offset
                  Ada.Text_IO.Put(", unused");
                  ret_val := 0;
               when RKDBlsb =>  --  Data buffer register
                  Ada.Text_IO.Put(", RKDB");
                  ret_val := 0;
               when others =>
                  ret_val := 0;
                  status := BUS_NONE;
            end case;
         when others =>
            ret_val := 0;
            status := BUS_NONE;
      end case;
      Ada.Text_IO.Put_Line(", value " & toOct(ret_val));
      return ret_val;
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
            Ada.Text_IO.Put_Line("RK11: Implemented function Control Reset");
         when 1 =>  --  Write
            Ada.Text_IO.Put_Line("RK11: *Unimplemented* function Write");
         when 2 =>  --  Read
            Ada.Text_IO.Put_Line("RK11: Implemented function Read");
            self.read;
         when 3 =>  --  Write check
            Ada.Text_IO.Put_Line("RK11: *Unimplemented* function Write Check");
         when 4 =>  --  Seek
            Ada.Text_IO.Put_Line("RK11: Implemented function Seek");
            self.seek;
         when 5 =>  --  Read check
            Ada.Text_IO.Put_Line("RK11: *Unimplemented* function Read Check");
         when 6 =>  --  Drive reset
            Ada.Text_IO.Put_Line("RK11: *Unimplemented function* Drive Reset");
         when 7 =>  --  Write lock
            Ada.Text_IO.Put_Line("RK11: Implemented function Write Lock");
            self.drive_info(byte(self.RKDA.drive)).writeable := False;
            self.RKCS.go := False;
      end case;
   end;
   --
   --  Seek a specific track, cylinder, and surface
   --
   procedure seek(self : in out rk11) is
      drive : byte := byte(self.RKDA.drive);
      error : Boolean := False;
   begin
      --
      --  Check for drive preset
      --
      if not self.drive_info(drive).present then
         self.RKER.bad_disk := True;
         self.RKDS.drv_id := self.RKDA.drive;
         self.RKCS.go := False;
         self.RKCS.error := True;
         error := True;
      else
         self.RKER.bad_disk := False;
      end if;
      --
      --  Check for cylinder out of range
      --
      if word(self.RKDA.cylinder) > rk05_geom.tracks then
         self.RKER.bad_cyl := True;
         self.RKDS.drv_id := self.RKDA.drive;
         self.RKCS.go := False;
         self.RKCS.error := True;
         error := True;
      else
         self.RKER.bad_cyl := False;
      end if;
      --
      --  Check for error and interrupt
      --
      if error then
         if self.RKCS.inte then
            self.host.interrupt(long(self.vector));
         end if;
         return;
      else
         self.RKCS.error := False;
      end if;
      --
      --  Do the actual seek
      --
      self.selected_drive := drive;
      self.drive_info(drive).track   := word(self.RKDA.cylinder);
      self.RKDS.drv_id := self.RKDA.drive;
      self.RKDS.sector := self.RKDA.sector;
      self.RKDS.equal  := True;
      self.RKDS.protect := not self.drive_info(drive).writeable;
      self.RKDS.rws_ready := True;
      self.RKDS.drv_ready := True;
      self.RKDS.sect_ok   := True;
      self.RKDS.seek_inc  := False;
      self.RKDS.unsafe    := False;
      self.RKDS.rk05      := True;
      self.RKDS.pwr_low   := False;
      self.RKCS.go       := False;
      self.RKCS.ctrl_rdy := True;
      self.RKCS.search   := True;
      self.RKCS.hard_err := False;
      self.RKCS.error    := False;
      if self.RKCS.inte then
         self.host.interrupt(long(self.vector));
      end if;
   end;
   --
   --  Compute block number
   --
   --  Update so that surfce is lower order than track.
   --
   function compute_block(sect : word; surf : Boolean; track : word) return Natural is
      s : constant Natural := (if surf then Natural(rk05_geom.sectors) else 0);
   begin
      return Natural(sect) + s + Natural(track)*Natural(rk05_geom.sectors)*2;
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
      sect : Natural := compute_block(word(self.RKDA.sector), self.RKDA.surface, drive.track);
      count : Natural := 0;
   begin
      Ada.Text_IO.Put_Line("RK11: Reading cylinder " & word'Image(drive.track) &
                             ", sector " & word'Image(word(self.RKDA.sector)) & ", surface " &
                             Boolean'Image(self.RKDA.surface));
      if drive.present then
         while self.RKWC /= 0 loop
            Ada.Text_IO.Put_Line("RK11: Reading block " & Natural'Image(sect) &
                                " destination memory address " & toOct(self.RKBA));
            disk_io.Set_Index(drive.image,
                                disk_io.Count(sect + 1));
            disk_io.Read(drive.image, buff);
--            dump_sect(buff);
            for addr in 0 .. (sector_size - 1)/2 loop
               self.host.set_mem(self.RKBA, data_bus(buff(addr*2)));
               self.host.set_mem(self.RKBA + 1, data_bus(buff(addr*2 + 1)));
               self.RKWC := self.RKWC + 1;
               count := count + 2;
               if not self.RKCS.not_incr then
                  self.RKBA := self.RKBA + 2;
               end if;
               exit when self.RKWC = 0;
            end loop;
            sect := sect + 1;
         end loop;
      else
         self.RKER.bad_disk := True;
         self.RKCS.error := True;
      end if;
      Ada.Text_IO.Put_Line("RK11: Finishing read, " & Natural'Image(count) & " words");
      self.RKCS.go := False;
      self.RKCS.ctrl_rdy := True;
      if self.RKCS.inte then
         self.host.interrupt(long(self.vector));
      end if;
   end;
   --
   --  write to the selected drive
   --
   procedure write(self : in out rk11) is
      drive : disk_info renames self.drive_info(self.selected_drive);
      buff : disk_sector;
--      sect : Natural := Natural(drive.track)*Natural(rk05_geom.sectors)
--        + Natural(drive.sector - 1);
      count : word := self.RKWC;
      base  : addr_bus := self.RKBA;
   begin
--      if halt_on_io_error then
--         if (drive.sector > rk05_geom.sectors) or (drive.sector = 0) then
--            Ada.Text_IO.Put_Line("RK11 Write sector out of range: " & word'Image(drive.sector));
--            self.host.halt;
--            return;
--         end if;
--         if drive.track > rk05_geom.tracks then
--            Ada.Text_IO.Put_Line("RK11 Write track out of range: " & word'Image(drive.track));
--            self.host.halt;
--            return;
--         end if;
--      end if;
--      if drive.present and drive.writeable then
--         for i in 1 .. count loop
--            for addr in 0 .. sector_size - 1 loop
--               buff(addr) := byte(self.host.read_mem(addr_bus(addr) + base) and 16#FF#);
--            end loop;
--            disk_io.Set_Index(drive.image,
--                                disk_io.Count(sect + 1));
--            disk_io.Write(drive.image, buff);
--            sect := sect + 1;
--            base := base + addr_bus(sector_size);
--         end loop;
--      end if;
      null;
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
