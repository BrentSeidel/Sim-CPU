--
--  Author: Brent Seidel
--  Date: 19-May-2026
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
--  Contains I/O device for simulated RK611 disk controller
--
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
package body BBS.Sim_CPU.io.disk.rk611 is
   function RKCS1_to_word is new Ada.Unchecked_Conversion(source => tRKCS1,
                                                         target => word);
   function RKCS2_to_word is new Ada.Unchecked_Conversion(source => tRKCS2,
                                                         target => word);
   function RKDS_to_word is new Ada.Unchecked_Conversion(source => tRKDS,
                                                         target => word);
   function RKER_to_word is new Ada.Unchecked_Conversion(source => tRKER,
                                                         target => word);
   function RKDA_to_word is new Ada.Unchecked_Conversion(source => tRKDA,
                                                         target => word);
   --
   function word_to_RKCS1 is new Ada.Unchecked_Conversion(source => word,
                                                         target => tRKCS1);
   function word_to_RKCS2 is new Ada.Unchecked_Conversion(source => word,
                                                         target => tRKCS2);
   function word_to_RKDS is new Ada.Unchecked_Conversion(source => word,
                                                         target => tRKDS);
   function word_to_RKER is new Ada.Unchecked_Conversion(source => word,
                                                         target => tRKER);
   function word_to_RKDA is new Ada.Unchecked_Conversion(source => word,
                                                         target => tRKDA);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out rk611; except : long) is
   begin
      self.vector := except;
   end;
   --
   --  Reset/Initialize device
   --
   overriding
   procedure reset(self : in out rk611) is
   begin
      self.RKCS1 := word_to_RKCS1(0);
      self.RKCS1.ctrl_rdy := True;
      self.RKWC := 0;
      self.RKBA := 0;
      self.RKDA := word_to_RKDA(0);
      self.RKER := word_to_RKER(0);
      for i in self.drive_info'Range loop
         self.drive_info(i).track   := 0;
      end loop;
--      if self.host.trace.io then
         Ada.Text_IO.Put_Line("RK611: Reset commanded by bus");
--      end if;
   end;
   --
   --  Port useage (base +)
   --     0/ 1 - RKCS1 - Control/status register #1
   --     2/ 3 - RKWC  - Word count register
   --     4/ 5 - RKBA  - Bus address register (current memory address)
   --     6/ 7 - RKDA  - Disk address register
   --     8/ 9 - RKCS2 - Control/status register #2
   --    10/11 - RKDS  - Drive status register
   --    12/13 - RKER  - Error register
   --    14/15 - RKAS/OF - Attention Summary/Offset Register
   --    16/17 - RKDC  - Desired Cylinder Register
   --    18/19 - Unused
   --    20/21 - RKDB  - Data buffer register
   --    22/23 - RKMR1 - Maintenance Register 1
   --    24/25 - RKECPS - ECC Position Register
   --    26/27 - RKECPT - ECC Pattern Register
   --    28/29 - RKMR2 - Maintenance Register 2
   --    30/31 - RKMR3 - Maintenance Register 3
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out rk611; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
      bvalue : constant byte := byte(data and 16#FF#);
      wvalue : constant word := word(data and 16#FFFF#);
   begin
      case size is
         when bits8 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("RK611: Writing byte " & toOct(bvalue) & " to address " & toOct(addr));
            end if;
            case offset is
               when RKCS1lsb =>  --  Control status register #1 LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKCS1 lsb");
                  end if;
               when RKCS1msb =>  --  Control status register #1 MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKCS1 Msb");
                  end if;
               when RKWClsb =>  --  Transfer word count LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKWC lsb");
                  end if;
               when RKWCmsb =>  --  Transfer word count MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKWC msb");
                  end if;
               when RKBAlsb =>  --  DMA address LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKBA lsb");
                  end if;
               when RKBAmsb =>  --  DMA address MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKBA msb");
                  end if;
               when RKDAlsb =>  --  Drive Address
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKDA lsb");
                  end if;
               when RKDAmsb =>  --  Drive Address
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKDA lsb");
                  end if;
               when RKCS2lsb =>  --  Control status register #2 LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKCS2 lsb");
                  end if;
               when RKCS2msb =>  --  Control status register #2 MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKCS2 Msb");
                  end if;
               when RKDSlsb =>  --  Drive Status (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKDS lsb (read only)");
                  end if;
               when RKDSmsb =>  --  Drive Status (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKDS msb (read only)");
                  end if;
               when RKERlsb =>  --  Error register LSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKER lsb (read only)");
                  end if;
               when RKERmsb =>  --  Error register MSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKER msb (read only)");
                  end if;
               when RKASlsb =>  --  Attention Summary/Offset Register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKAS lsb");
                  end if;
               when RKASmsb =>  --  Attention Summary/Offset Register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKAS msb");
                  end if;
               when RKDClsb =>  --  Desired Cylinder LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKDC lsb");
                  end if;
               when RKDCmsb =>  --  Desired Cylinder MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKDC lsb");
                  end if;
               when RKunus1 | RKunus2 =>  --  unused offset
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" Unused lsb or msb");
                  end if;
               when RKDBlsb | RKDBmsb =>  --  Data buffer register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKDS lsb or msb");
                  end if;
               when RKMR1lsb =>  --  Maintenance Register #1 LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKMR1 lsb");
                  end if;
               when RKMR1msb =>  --  Maintenance Register #1 LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKMR1 msb");
                  end if;
               when RKECPSlsb =>  --  ECC Position Register LSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKECPS lsb (read only)");
                  end if;
               when RKECPSmsb =>  --  ECC Position Register LSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKECPS msb (read only)");
                  end if;
               when RKECPTlsb =>  --  ECC Pattern Register LSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKECPT lsb (read only)");
                  end if;
               when RKECPTmsb =>  --  ECC Pattern Register LSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKECPT msb (read only)");
                  end if;
               when RKMR2lsb =>  --  Maintenance Register #2 LSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKMR2 lsb (read only)");
                  end if;
               when RKMR2msb =>  --  Maintenance Register #2 LSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKMR2 msb (read only)");
                  end if;
               when RKMR3lsb =>  --  Maintenance Register #3 LSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKMR3 lsb (read only)");
                  end if;
               when RKMR3msb =>  --  Maintenance Register #3 LSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKMR3 msb (read only)");
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when bits16 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("RK611: Writing word " & toOct(wvalue) & " to address " & toOct(addr));
            end if;
            case offset is
               when RKCS1lsb =>  --  Control status register #1
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" RKCS1");
                  end if;
                  self.RKCS1 := word_to_RKCS1(wvalue);
               when RKWClsb =>  --  Transfer word count
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" RKWC");
                  end if;
                  self.RKWC := wvalue;
               when RKBAlsb =>  --  DMA address
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" RKBA");
                  end if;
                  self.RKBA := addr_bus(wvalue);
               when RKDAlsb =>  --  Drive Address
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" RKDA");
                  end if;
                  self.RKDA := word_to_RKDA(wvalue);
               when RKCS2lsb =>  --  Control status register #2
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" RKCS2");
                  end if;
                  self.RKCS2 := word_to_RKCS2(wvalue);
               when RKDSlsb =>  --  Drive Status (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKDS (read only)");
                  end if;
               when RKERlsb =>  --  Error register (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKER (read only)");
                  end if;
               when RKASlsb =>  --  Attention Summary/Offset Register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKAS");
                  end if;
               when RKDClsb =>  --  Desired Cylinder
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" RKDC");
                  end if;
                  self.RKDC := wvalue;
               when RKunus1 =>  --  unused offset
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" unused (read only)");
                  end if;
               when RKDBlsb =>  --  Data buffer register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKDB (read only)");
                  end if;
               when RKMR1lsb =>  --  Maintenance Register #1
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKMR1");
                  end if;
               when RKECPSlsb =>  --  ECC Position Register (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKECPS (read only)");
                  end if;
               when RKECPTlsb =>  --  ECC Pattern Register (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKECPT (read only)");
                  end if;
               when RKMR2lsb =>  --  Maintenance Register #2 (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKMR2 (read only)");
                  end if;
               when RKMR3lsb =>  --  Maintenance Register #3 (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *RKMR3 (read only)");
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when others =>
            status := BUS_NONE;
      end case;
      if self.RKCS1.go then
         self.process_command;
      end if;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out rk611; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus is
      drive     : disk_info renames self.drive_info(self.selected_drive);
      offset    : constant byte := byte((addr - self.base) and 16#FF#);
      ret_val   : word := 0;
      range_err : Boolean := False;
   begin
      case size is
         when bits8 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("RK611: Reading byte from address " & toOct(addr));
            end if;
            case offset is
               when RKCS1lsb =>  --  Control status register #1 LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKCS1 lsb");
                  end if;
                  ret_val := RKCS1_to_word(self.RKCS1) and 16#FF#;
               when RKCS1msb =>  --  Control status register #1 MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKCS1 msb");
                  end if;
                  ret_val := (RKCS1_to_word(self.RKCS1)/16#100#) and 16#FF#;
               when RKWClsb =>  --  Transfer word count LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKWC lsb");
                  end if;
                  ret_val := self.RKWC and 16#FF#;
               when RKWCmsb =>  --  Transfer word count MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKWC msb");
                  end if;
                  ret_val := (self.RKWC/16#100#) and 16#FF#;
               when RKBAlsb =>  --  DMA address LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKBA lsb");
                  end if;
                  ret_val := word(self.RKBA and 16#FF#);
               when RKBAmsb =>  --  DMA address MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKBA msb");
                  end if;
                  ret_val := word((self.RKBA/16#100#) and 16#FF#);
               when RKDAlsb =>  --  Drive Address
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKDA lsb");
                  end if;
               when RKDAmsb =>  --  Drive Address
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKDA msb");
                  end if;
               when RKCS2lsb =>  --  Control status register #2 LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKCS2 lsb");
                  end if;
                  ret_val := RKCS2_to_word(self.RKCS2) and 16#FF#;
               when RKCS2msb =>  --  Control status register #2 MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKCS2 msb");
                  end if;
                  ret_val := (RKCS2_to_word(self.RKCS2)/16#100#) and 16#FF#;
               when RKDSlsb =>  --  Drive Status
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKDS lsb");
                  end if;
                  ret_val := RKDS_to_word(self.RKDS) and 16#FF#;
               when RKDSmsb =>  --  Drive Status
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKDS msb");
                  end if;
                  self.RKDS.stat_val := True;
                  ret_val := (RKDS_to_word(self.RKDS)/16#100#) and 16#FF#;
               when RKERlsb =>  --  Error register LSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKER lsb");
                  end if;
                  ret_val := RKER_to_word(self.RKER) and 16#FF#;
               when RKERmsb =>  --  Error register MSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKER msb");
                  end if;
                  ret_val := (RKER_to_word(self.RKER)/16#100#) and 16#FF#;
               when RKASlsb =>  --  Attention Summary/Offset Register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKAS lsb");
                  end if;
               when RKASmsb =>  --  Attention Summary/Offset Register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKAS msb");
                  end if;
               when RKDClsb =>  --  Desired Cylinder
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKDC lsb");
                 end if;
                  ret_val := self.RKDC and 16#FF#;
               when RKDCmsb =>  --  Desired Cylinder
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKDC msb");
                  end if;
                  ret_val := (self.RKDC/16#100#) and 16#FF#;
               when RKunus1 | RKunus2 =>  --  unused offset
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", Unused lsb or msb");
                  end if;
               when RKDBlsb | RKDBmsb =>  --  Data buffer register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKDB lsb or msb");
                  end if;
               when RKMR1lsb =>  --  Maintenance Register #1 LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKMR1 lsb");
                  end if;
               when RKMR1msb =>  --  Maintenance Register #1 MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKMR1 msb");
                  end if;
               when RKECPSlsb =>  --  ECC Position Register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKECPS lsb");
                  end if;
               when RKECPSmsb =>  --  ECC Position Register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKECPS msb");
                  end if;
               when RKECPTlsb =>  --  ECC Pattern Register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKECPT lsb");
                  end if;
               when RKECPTmsb =>  --  ECC Pattern Register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKECPT msb");
                  end if;
               when RKMR2lsb =>  --  Maintenance Register #2 LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKMR2 lsb");
                  end if;
               when RKMR2msb =>  --  Maintenance Register #2 MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKMR2 msb");
                  end if;
               when RKMR3lsb =>  --  Maintenance Register #3 LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKMR3 lsb");
                  end if;
               when RKMR3msb =>  --  Maintenance Register #3 MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKMR3 msb");
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when bits16 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("RK611: Reading word from address " & toOct(addr));
            end if;
            case offset is
               when RKCS1lsb =>  --  Control status register #1
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKCS1");
                  end if;
                  ret_val := RKCS1_to_word(self.RKCS1);
               when RKWClsb =>  --  Transfer word count
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKWC");
                  end if;
                  ret_val := self.RKWC;
               when RKBAlsb =>  --  DMA address
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKBA");
                  end if;
                  ret_val := word(self.RKBA and 16#FFFF#);
               when RKDAlsb =>  --  Drive Address
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKDA");
                  end if;
                  ret_val := RKDA_to_word(self.RKDA);
               when RKCS2lsb =>  --  Control status register #2
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKCS2");
                  end if;
                  ret_val := RKCS2_to_word(self.RKCS2);
               when RKDSlsb =>  --  Drive Status
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKDS");
                  end if;
                  self.RKDS.stat_val := True;
                  ret_val := RKDS_to_word(self.RKDS);
                  self.RKDS.drv_attn  := False;
               when RKERlsb =>  --  Error Register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKER");
                  end if;
                  ret_val := RKER_to_word(self.RKER);
               when RKASlsb =>  --  Attention Summary/Offset Register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKAS");
                 end if;
               when RKDClsb =>  --  Desired Cylinder
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RKDC");
                  end if;
                  ret_val := self.RKDC;
               when RKunus1 =>  --  unused offset
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", unused");
                  end if;
               when RKDBlsb =>  --  Data buffer register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKDB");
                  end if;
               when RKMR1lsb =>  --  Maintenance Register #1
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKMR1");
                  end if;
               when RKECPSlsb =>  --  ECC Position Register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKECPS lsb");
                  end if;
               when RKECPTlsb =>  --  ECC Pattern Register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKECPT lsb");
                  end if;
               when RKMR2lsb =>  --  Maintenance Register #2
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKMR2");
                  end if;
               when RKMR3lsb =>  --  Maintenance Register #3
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *RKMR3");
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when others =>
            status := BUS_NONE;
      end case;
      if self.host.trace.io or debug then
         Ada.Text_IO.Put_Line(", value " & toOct(ret_val));
      end if;
      return data_bus(ret_val);
   end;
   --
   --  Process the command specified in RKCS1

   --  Drive function codes are:
   --    0 - Select Drive
   --    1 - Pack Acknowledge
   --    2 - Drive Clear
   --    3 - Unload
   --    4 - Start Spindle
   --    5 - Recalibrate
   --    6 - Offset
   --    7 - Seek
   --    8 - Read Data
   --    9 - Write Data
   --   10 - Read Header
   --   11 - Write Header
   --   12 - Write Check
   --   13-15 Unused
   --
   procedure process_command(self : in out rk611) is
   begin
      self.RKER.bad_fun := False;
      case self.RKCS1.drv_func is
         when 0 =>   --  Select Drive
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: Implemented function: Select Drive");
            end if;
            self.selected_drive := byte(self.RKCS2.drive);
            self.drive_select;
         when 1 =>   --  Pack Acknowledge
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: Implemented function: Pack Acknowledge");
            end if;
            self.pack_acknowledge;
         when 2 =>   --  Drive Clear
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: Implemented function: Drive Clear");
            end if;
            self.drive_clear;
         when 3 =>   --  Unload
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: *Unimplemented function: Unload");
            end if;
         when 4 =>   --  Start Spindle
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: *Unimplemented function: Start Spindle");
            end if;
         when 5 =>   --  Recalibrate
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: *Unimplemented function: Recalibrate");
            end if;
         when 6 =>   --  Offset
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: *Unimplemented function: Offset");
            end if;
         when 7 =>   --  Seek
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: Implemented function: Seek");
            end if;
            self.seek;
         when 8 =>   --  Read Data
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: Implemented function: Read Data");
            end if;
            self.read;
         when 9 =>   --  Write Data
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: Implemented function: Write Data");
            end if;
            self.write;
         when 10 =>  --  Read Header
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: *Unimplemented function: Read Header");
            end if;
         when 11 =>  --  Write Header
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: *Unimplemented function: Write Header");
            end if;
         when 12 =>  --  Write Check
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: *Unimplemented function: Write Check");
            end if;
         when others =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: *Unimplemented drive command unknown: " &
                                      uint4'Image(self.RKCS1.drv_func));
            end if;
            self.RKER.bad_fun := True;
      end case;
      self.RKCS1.ctrl_rdy := True;
      self.RKCS1.go := False;
      self.RKCS2.inp_rdy := True;
      self.RKCS2.out_rdy := True;
   end;
   --
   --  Other functions for command processing
   --
   procedure drive_select(self : in out rk611) is
      drive : disk_info renames self.drive_info(byte(self.selected_drive));
   begin
      self.RKDS.DRA       := drive.present;
      self.RKDS.offset    := False;
      self.RKDS.ac_low    := False;
      self.RKDS.speed_los := False;
      self.RKDS.DROT      := False;
      self.RKDS.valid     := drive.present;
      self.RKDS.drv_rdy   := drive.present;
      self.RKDS.drv_type  := True;  --  For now, assume everything is RK07
      self.RKDS.wrt_prot  := (not drive.writeable) or drive.sw_prot;
      self.RKDS.pos_prog  := False;
      self.RKDS.drv_attn  := False;
      self.RKDS.stat_val  := True;
   end;
   --
   procedure pack_acknowledge(self : in out rk611) is
      drive : disk_info renames self.drive_info(byte(self.selected_drive));
   begin
      self.RKDS.DRA       := drive.present;
      self.RKDS.offset    := False;
      self.RKDS.ac_low    := False;
      self.RKDS.speed_los := False;
      self.RKDS.DROT      := False;
      self.RKDS.valid     := drive.present;
      self.RKDS.drv_rdy   := drive.present;
      self.RKDS.drv_type  := True;  --  For now, assume everything is RK07
      self.RKDS.wrt_prot  := (not drive.writeable) or drive.sw_prot;
      self.RKDS.pos_prog  := False;
      self.RKDS.drv_attn  := False;
      self.RKDS.stat_val  := True;
   end;
   --
   procedure drive_clear(self : in out rk611) is
      drive : disk_info renames self.drive_info(byte(self.selected_drive));
   begin
      self.RKDS.DRA       := drive.present;
      self.RKDS.offset    := False;
      self.RKDS.ac_low    := False;
      self.RKDS.speed_los := False;
      self.RKDS.DROT      := False;
      self.RKDS.valid     := drive.present;
      self.RKDS.drv_rdy   := drive.present;
      self.RKDS.drv_type  := True;  --  For now, assume everything is RK07
      self.RKDS.wrt_prot  := (not drive.writeable) or drive.sw_prot;
      self.RKDS.pos_prog  := False;
      self.RKDS.drv_attn  := False;
      self.RKDS.stat_val  := True;
      self.RKER.bad_seek  := False;
      self.RKER.non_fun   := False;
      self.RKER.bad_type  := False;
      self.RKER.cyl_over  := False;
      self.RKER.bad_daddr := False;
      self.RKER.write_loc := False;
   end;
   --
   --  Internal seek to combine common code from read/write/seek
   --  Returns True if successful, False if error.
   --
   function internal_seek(self : in out rk611) return Boolean is
      drive : disk_info renames self.drive_info(byte(self.selected_drive));
   begin
      --
      --  Check for drive preset
      --
      if not drive.present then
         self.RKCS2.nxdrive := True;
         self.RKCS1.error := True;
         return False;
      end if;
      --
      --  Check for cylinder out of range
      --
      if word(self.RKDC) > rk07_geom.tracks then
         self.RKER.bad_daddr := True;
         self.RKCS1.error := True;
         return False;
         --
      --  It's not entirely clear from the documentation of RKDA.surface (Track
      --  Address in the documentation) if this field is a number or if it's
      --  three bits where each bit selects one of the heads.  Interpreting it as a
      --  number seems to work.
         --
      elsif byte(self.RKDA.surface) > (rk07_geom.heads - 1) then
         self.RKER.bad_daddr := True;
         self.RKCS1.error := True;
         return False;
      else
         self.RKER.bad_daddr := False;
      end if;
      --
      --  Do the actual seek
      --
      drive.track   := word(self.RKDC);
--      self.RKDS.drv_id := self.RKDA.drive;
--      self.RKDS.sector := self.RKDA.sector;
--      self.RKDS.equal  := True;
--      self.RKDS.protect := not drive.writeable;
--      self.RKDS.rws_ready := True;
--      self.RKDS.drv_ready := True;
--      self.RKDS.sect_ok   := True;
--      self.RKDS.seek_inc  := False;
--      self.RKDS.unsafe    := False;
--      self.RKDS.rk05      := True;
--      self.RKDS.pwr_low   := False;
--      self.RKCS.go       := False;
--      self.RKCS.ctrl_rdy := True;
--      self.RKCS.search   := True;
--      self.RKCS.hard_err := False;
--      self.RKCS.error    := False;
      self.RKDS.drv_attn  := True;
      return True;
   end;
   --
   --  Seek a specific track, cylinder, and surface
   --
   procedure seek(self : in out rk611) is
   begin
      if internal_seek(self) then
         self.RKCS1.error := False;
         if self.RKCS1.inte then
            self.host.interrupt(self.vector + 16#10_00_0000#);
         end if;
      else
         if self.RKCS1.inte then
            self.host.interrupt(self.vector + 16#10_00_0000#);
         end if;
      end if;
   end;
   --
   --  Compute block number
   --
   function compute_block(sect : word; surf : uint3; track : word) return Natural is
      --
      --  It's not entirely clear from the documentation of RKDA.surface (Track
      --  Address in the documentation) if this field is a number or if it's
      --  three bits where each bit selects one of the heads.  Interpreting it as a
      --  number seems to work.
      --
      s : constant Natural := Natural(surf)*Natural(rk07_geom.sectors);
      t : constant Natural := Natural(track)*Natural(rk07_geom.sectors)*Natural(rk07_geom.heads);
   begin
      return Natural(sect) + s + t;
   end;
   --
   --  Open the attached file.  If file does not exist, then create it.
   --
   procedure open(self : in out rk611; drive : byte;
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
      self.drive_info(drive).writeable := True;
   end;
   --
   procedure extend(self : in out rk611; drive : byte;
                  geom : geometry; name : String) is
      buff : disk_sector := (others => 0);
   begin
      begin
         disk_io.Create(self.drive_info(drive).image, disk_io.Inout_File,
                        name);
      exception
         when disk_io.Name_Error =>
            Ada.Text_IO.Put_Line("RK611: Unable to attach to file <" & name & ">");
            self.drive_info(drive).present := False;
            return;
      end;
      Ada.Text_IO.Put_Line("RK611: Extending image for drive " & byte'Image(drive) &
                                   " as file " & name);
      for sect in 0 .. geom.sectors - 1 loop
         for track in 0 .. geom.tracks - 1 loop
            for head in 0 .. geom.heads - 1 loop
               disk_io.Write(self.drive_info(drive).image, buff);
            end loop;
         end loop;
      end loop;
      self.drive_info(drive).present   := True;
      self.drive_info(drive).writeable := True;
   end;
   --
   --  Get the name of the attached file, if any.
   --
   function fname(self : in out rk611; drive : byte) return String is
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
   function present(self : in out rk611; drive : byte) return Boolean is
   begin
      return self.drive_info(drive).present;
   end;
   --
   --  Is the specified drive read-only?
   --
   function readonly(self : in out rk611; drive : byte) return Boolean is
   begin
      return not self.drive_info(drive).writeable;
   end;
   --
   --  Set the specified drive's read-only state.
   --  Note that setting readonly to False will clear the software write protect.
   --
   procedure readonly(self : in out rk611; drive : byte; state : Boolean) is
   begin
      if state then
         self.drive_info(drive).writeable := False;
      else
         self.drive_info(drive).writeable := True;
         self.drive_info(drive).sw_prot   := False;
      end if;
   end;
   --
   --  Close the attached file
   --
   procedure close(self : in out rk611; drive : byte) is
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
   procedure read(self : in out rk611) is
      drive : disk_info renames self.drive_info(byte(self.RKCS2.drive));
      buff  : disk_sector;
      sect  : Natural;
      count : Natural := 0;
   begin
      --
      --  Disk read does an implied seek.
      --
      if internal_seek(self) then
         self.RKCS1.error := False;
      else
         if self.RKCS1.inte then
            self.host.interrupt(self.vector);
         end if;
         return;
      end if;
      sect := compute_block(word(self.RKDA.sector), self.RKDA.surface, drive.track);
      if self.host.trace.io or debug then
         Ada.Text_IO.Put_Line("RK611: Reading drive " & byte'Image(byte(self.RKCS2.drive)) &
                                " cylinder " & word'Image(drive.track) &
                                ", sector " & word'Image(word(self.RKDA.sector)) & ", surface " &
                                uint3'Image(self.RKDA.surface));
      end if;
      if drive.present then
         while self.RKWC /= 0 loop
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: Reading block " & Natural'Image(sect) &
                                      " destination memory address " & toOct(self.RKBA));
            end if;
            disk_io.Set_Index(drive.image, disk_io.Count(sect + 1));
            if disk_io.End_Of_File(drive.image) then
               Ada.Text_IO.Put_Line("RK611: Extending disk image " & disk_io.Name(drive.image));
               buff := (others => 0);
               disk_io.write(drive.image, buff);
               disk_io.Set_Index(drive.image, disk_io.Count(sect + 1));
            end if;
            disk_io.Read(drive.image, buff);
            for addr in 0 .. (sector_size - 1)/2 loop
               self.host.set_mem(self.RKBA, data_bus(buff(addr*2)));
               self.host.set_mem(self.RKBA + 1, data_bus(buff(addr*2 + 1)));
               self.RKWC := self.RKWC + 1;
               count := count + 2;
               if not self.RKCS2.inc_inhib then
                  self.RKBA := self.RKBA + 2;
               end if;
               exit when self.RKWC = 0;
            end loop;
            sect := sect + 1;
         end loop;
      else
         self.RKCS2.nxdrive := True;
         self.RKCS1.error := True;
      end if;
      if self.host.trace.io or debug then
         Ada.Text_IO.Put_Line("RK611: Finishing read, " & Natural'Image(count) & " words");
      end if;
      self.RKCS1.go := False;
      self.RKCS1.ctrl_rdy := True;
      if self.RKCS1.inte then
--         self.host.interrupt(self.vector);
         self.host.interrupt(self.vector + 16#10_00_0000#);
      end if;
   end;
   --
   --  write to the selected drive
   --
   procedure write(self : in out rk611) is
      drive : disk_info renames self.drive_info(byte(self.RKCS2.drive));
      buff  : disk_sector;
      sect  : Natural;
      count : Natural := 0;
   begin
      --
      --  Check if drive is writeable
      --
      if (not drive.writeable) or drive.sw_prot then
         Ada.Text_IO.Put_Line("RK611: Attempt to write to write protected drive " & uint3'Image(self.RKCS2.drive));
         self.RKCS1.error := True;
         self.RKER.write_loc := True;
         if self.RKCS1.inte then
            self.host.interrupt(self.vector);
         end if;
         return;
      end if;
      --
      --  Disk write does an implied seek.
      --
      if internal_seek(self) then
         self.RKCS1.error := False;
      else
         if self.RKCS1.inte then
            self.host.interrupt(self.vector);
         end if;
         return;
      end if;
      sect := compute_block(word(self.RKDA.sector), self.RKDA.surface, drive.track);
      if self.host.trace.io or debug then
         Ada.Text_IO.Put_Line("RK611: Writing  drive " & byte'Image(byte(self.RKCS2.drive)) &
                                " cylinder " & word'Image(drive.track) &
                                ", sector " & word'Image(word(self.RKDA.sector)) & ", surface " &
                                uint3'Image(self.RKDA.surface));
      end if;
      if drive.present then
         while self.RKWC /= 0 loop
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("RK611: Writing block " & Natural'Image(sect) &
                                      " source memory address " & toOct(self.RKBA));
            end if;
            disk_io.Set_Index(drive.image,
                                disk_io.Count(sect + 1));
            for addr in 0 .. (sector_size - 1)/2 loop
               buff(addr*2) := byte(self.host.read_mem(self.RKBA) and 16#FF#);
               buff(addr*2 + 1) := byte(self.host.read_mem(self.RKBA + 1) and 16#FF#);
               self.RKWC := self.RKWC + 1;
               count := count + 2;
               if not self.RKCS2.inc_inhib then
                  self.RKBA := self.RKBA + 2;
               end if;
               exit when self.RKWC = 0;
            end loop;
            disk_io.Write(drive.image, buff);
            sect := sect + 1;
         end loop;
      else
         self.RKCS2.nxdrive := True;
         self.RKCS1.error := True;
      end if;
      if self.host.trace.io or debug then
         Ada.Text_IO.Put_Line("RK611: Finishing write, " & Natural'Image(count) & " words");
      end if;
      self.RKCS1.go := False;
      self.RKCS1.ctrl_rdy := True;
      if self.RKCS1.inte then
         self.host.interrupt(self.vector + 16#10_00_0000#);
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
