--
--  Author: Brent Seidel
--  Date: 8-Jun-2026
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
--  This package contains a TM11 magnetic tape controller.
--
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
package body BBS.Sim_CPU.io.disk.tm11 is
   --  ----------------------------------------------------------------------
   --  This is an I/O device for a TM11 magnetic tape controller.  It is
   --  designed to work with the PDP-11 simulations.
   --
   function MTS_to_word is new Ada.Unchecked_Conversion(source => tMTS,
                                                        target => word);
   function MTC_to_word is new Ada.Unchecked_Conversion(source => tMTC,
                                                        target => word);
   --
   function word_to_MTS is new Ada.Unchecked_Conversion(source => word,
                                                        target => tMTS);
   function word_to_MTC is new Ada.Unchecked_Conversion(source => word,
                                                        target => tMTC);
   --
   --  Set which exception to use.  The RX vector is the LSW of except.  The TX
   --  vector is the next MSW of except.  16#04_0000# is added to represent the
   --  interrupt level of BR4.  The 16#1000_0000# is to delay the actual execution
   --  of the interrupt to allow some time for the CPU to complete the service
   --  routine.  This may need to be adjusted.
   --
   procedure setException(self : in out tm11; except : long) is
      prio    : constant long := 16#0004_0000#;   --  BR4 priority level
      timeout : constant long := 16#10_00_0000#;  --  16#10# instructions before interrupt triggers
   begin
      self.vector := (except and 16#FFFF#) + prio + timeout;
   end;
   --
   --  Reset/Initialize device
   --
   overriding
   procedure reset(self : in out tm11) is
   begin
      self.MTC.int_enb := False;
      if self.host.trace.io then
         Ada.Text_IO.Put_Line("TM11: Reset commanded by bus");
      end if;
   end;
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out tm11; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
      bValue : constant byte := byte(data and 16#FF#);
      wValue : constant word := word(data and 16#FFFF#);
   begin
      case size is
         when bits8 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("TM11: Writing byte " & toOct(byte(data and 16#FF#)) & " to address " & toOct(addr));
            end if;
            case offset is
               when MTSlsb =>  --  Status register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTS lsb");
                  end if;
               when MTSmsb =>  --  Status register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTS msb");
                  end if;
               when MTClsb =>  --  Command register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MTC lsb");
                  end if;
                  self.MTC := word_to_MTC((MTC_to_word(self.MTC) and 16#FF00#) or word(bValue));
                  if self.MTC.go then
                     self.process_command;
                  end if;
               when MTCmsb =>  --  Command register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MTC msb");
                  end if;
                  self.MTC := word_to_MTC((MTC_to_word(self.MTC) and 16#FF#) or (word(bValue)*16#100#));
                  --
                  --  This won't set the MCT.go bit, so no need to check and
                  --  process the command.
                  --
               when MTBCRlsb =>  --  Byte Record Counter register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTBCR lsb");
                  end if;
               when MTBCRmsb =>  --  Byte Record Counter register MSB (read only)
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTBCR msb");
                  end if;
               when MTCMAlsb =>  --  Current Memory Address register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTCMA lsb");
                  end if;
               when MTCMAmsb =>  --  Current Memory Address register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTCMA msb");
                  end if;
               when MTDlsb =>  --  Data Buffer Register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTD lsb");
                  end if;
               when MTDmsb =>  --  Data Buffer Register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTD msb");
                  end if;
               when MTRDlsb =>  --  TU10 Read Lines Register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTRD lsb");
                  end if;
               when MTRDmsb =>  --  TU10 Read Lines Register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTRD msb");
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when bits16 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("TM11: Writing word " & toOct(word(data and 16#FFFF#)) & " to address " & toOct(addr));
            end if;
            case offset is
               when MTSlsb =>  --  Status register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTS");
                  end if;
               when MTClsb =>  --  Command register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MTC");
                  end if;
                  self.MTC := word_to_MTC(wValue);
                  if self.MTC.go then
                     self.process_command;
                  end if;
               when MTBCRlsb =>  --  Byte Record Counter register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MTBCR");
                  end if;
                  self.MTBCR := wValue;
               when MTCMAlsb =>  --  Current Memory Address register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MTCMA");
                  end if;
                  self.MTCMA := wValue;
               when MTDlsb =>  --  Data Buffer Register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTD");
                  end if;
               when MTRDlsb =>  --  TU10 Read Lines Register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MTRD");
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when others =>
            status := BUS_NONE;
      end case;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out tm11; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
      temp   : word := 0;
   begin
      case size is
         when bits8 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("TM11: Reading byte from ");
            end if;
            case offset is
               when MTSlsb =>  --  Status register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTS lsb");
                  end if;
                  temp := MTS_to_word(self.MTS) and 16#FF#;
               when MTSmsb =>  --  Status register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTS msb");
                  end if;
                  temp := MTS_to_word(self.MTS)/16#100# and 16#FF#;
               when MTClsb =>  --  Command register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTC lsb");
                  end if;
                  temp := MTC_to_word(self.MTC) and 16#FF#;
               when MTCmsb =>  --  Command register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTC msb");
                  end if;
                  temp := MTC_to_word(self.MTC)/16#100# and 16#FF#;
               when MTBCRlsb =>   --  Byte Record Counter register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTBCR lsb");
                  end if;
                  temp := self.MTBCR and 16#FF#;
               when MTBCRmsb =>  --  Byte Record Counter register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTBCR msb");
                  end if;
                  temp := self.MTBCR/16#100# and 16#FF#;
               when MTCMAlsb =>  --  Current Memory Address register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTCMA lsb");
                  end if;
                  temp := self.MTCMA and 16#FF#;
               when MTCMAmsb =>  --  Current Memory Address register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTCMA msb");
                  end if;
                  temp := self.MTCMA/16#100# and 16#FF#;
               when MTDlsb =>  --  Data Buffer Register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("*MTD lsb");
                  end if;
               when MTDmsb =>  --  Data Buffer Register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("*MTD msb");
                  end if;
               when MTRDlsb =>  --  TU10 Read Lines Register LSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("*MTRD lsb");
                  end if;
               when MTRDmsb =>  --  TU10 Read Lines Register MSB
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("*MTRD msb");
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line(" value " & toOct(byte(temp)));
            end if;
         when bits16 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("TM11: Reading word from ");
            end if;
            case offset is
               when MTSlsb =>  --  Status register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTS");
                  end if;
                  temp := MTS_to_word(self.MTS);
               when MTClsb =>  --  Command register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTC");
                  end if;
                  temp := MTC_to_word(self.MTC);
               when MTBCRlsb =>   --  Byte Record Counter register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTBCR");
                  end if;
                  temp := self.MTBCR;
               when MTCMAlsb =>  --  Current Memory Address register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("MTCMA");
                  end if;
                  temp := self.MTCMA;
               when MTDlsb =>  --  Data Buffer Register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("*MTD");
                  end if;
               when MTRDlsb =>  --  TU10 Read Lines Register
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put("*MTRD");
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line(" value " & toOct(word(temp)));
            end if;
         when others =>
            status := BUS_NONE;
      end case;
      return data_bus(temp);
   end;
   --
   --  Process the command specified in TMC
   --
   --  Functions are (self.TMC.funct):
   --  0 - Off-Line
   --  1 - Read
   --  2 - Write
   --  3 - Write End of File
   --  4 - Space Forward
   --  5 - Space Reverse
   --  6 - Write With Extended IRG
   --  7 - Rewind
   --
   procedure process_command(self : in out tm11) is
   begin
      case self.MTC.funct is
         when 0 =>  --  Off-Line
            Ada.Text_IO.Put_Line("TM11: Unimplemented command off-line");
         when 1 =>  --  Read
            Ada.Text_IO.Put_Line("TM11: Unimplemented command read");
            self.read;
         when 2 =>  --  Write
            Ada.Text_IO.Put_Line("TM11: Unimplemented command write");
            self.write;
         when 3 =>  --  Write End Of File
            Ada.Text_IO.Put_Line("TM11: Unimplemented command write end-of-file");
         when 4 =>  --  Space Forward
            Ada.Text_IO.Put_Line("TM11: Unimplemented command space forward");
         when 5 =>  --  Space Reverse
            Ada.Text_IO.Put_Line("TM11: Unimplemented command space reverse");
         when 6 =>  --  Write with Extended IRG
            Ada.Text_IO.Put_Line("TM11: Unimplemented command write with extended IRG");
         when 7 =>  --  Rewind
            Ada.Text_IO.Put_Line("TM11: Testing command rewind");
            self.rewind;
      end case;
      self.MTC.go := False;
   end;
   --
   --  Open the attached file.  If file does not exist, then create it.
   --
   procedure open(self : in out tm11; drive : byte;
         geom : geometry; name : String) is
   begin
      if self.drive_info(drive).present then
         tape_io.Close(self.drive_info(drive).Image);
      end if;
      begin
         tape_io.Open(self.drive_info(drive).image, tape_io.Inout_File,
                        name);
      exception
         when tape_io.Name_Error =>
            self.extend(drive, name);
            return;
      end;
      self.drive_info(drive).present   := True;
      self.drive_info(drive).writeable := True;
   end;
   --
   procedure extend(self : in out tm11; drive : byte;
                    name : String) is
   begin
      begin
         tape_io.Create(self.drive_info(drive).image, tape_io.Inout_File,
                        name);
      exception
         when tape_io.Name_Error =>
            Ada.Text_IO.Put_Line("TM11: Unable to attach to file <" & name & ">");
            self.drive_info(drive).present := False;
            return;
      end;
      Ada.Text_IO.Put_Line("TM11: Extending image for drive " & byte'Image(drive) &
                             " as file " & name);
      for i in 0 .. 255 loop
         tape_io.Write(self.drive_info(drive).image, 0);
      end loop;
      self.drive_info(drive).position  := 4;
      self.drive_info(drive).rec_size  := 0;
      self.drive_info(drive).rec_pos   := 0;
      self.drive_info(drive).present   := True;
      self.drive_info(drive).writeable := True;
   end;
   --
   --  Get the name of the attached file, if any.
   --
   function fname(self : in out tm11; drive : byte) return String is
   begin
      if self.drive_info(drive).present then
         return tape_io.Name(self.drive_info(drive).image);
      else
         return ">closed<";
      end if;
   end;
   --
   --  Is a file attached to the specified drive?
   --
   function present(self : in out tm11; drive : byte) return Boolean is
   begin
      return self.drive_info(drive).present;
   end;
   --
   --  Is the specified drive read-only?
   --
   function readonly(self : in out tm11; drive : byte) return Boolean is
   begin
      return not self.drive_info(drive).writeable;
   end;
   --
   --  Set the specified drive's read-only state.
   --  Note that setting readonly to False will clear the software write protect.
   --
   procedure readonly(self : in out tm11; drive : byte; state : Boolean) is
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
   procedure close(self : in out tm11; drive : byte) is
   begin
      if self.drive_info(drive).present then
         tape_io.Close(self.drive_info(drive).Image);
      end if;
      self.drive_info(drive).present := False;
      self.drive_info(drive).writeable := False;
   end;
   --
   --  Read from the selected drive
   --
   procedure read(self : in out tm11) is
      selected : constant byte := byte(self.MTC.SEL);
      drive    : tape_info renames self.drive_info(selected);
   begin
      null;
   end;
   --
   --  write to the selected drive
   --
   procedure write(self : in out tm11) is
      selected : constant byte := byte(self.MTC.SEL);
      drive    : tape_info renames self.drive_info(selected);
   begin
      null;
   end;
   --
   --  Rewind the selected drive
   --
   procedure rewind(self : in out tm11) is
      selected : constant byte := byte(self.MTC.SEL);
      drive    : tape_info renames self.drive_info(selected);
      b1, b2, b3, b4 : byte;
   begin
      self.MTS.ready := True;
      self.MTS.BOT   := True;
      if drive.present then
         tape_io.Set_Index(drive.image, 1);
         tape_io.Read(drive.image, b1);
         tape_io.Read(drive.image, b2);
         tape_io.Read(drive.image, b3);
         tape_io.Read(drive.image, b4);
         drive.position := 5;
         drive.rec_size := uint32(b1) + uint32(b2)*16#100# + uint32(b3)*16#1_0000# + uint32(b4)*16#100_0000#;
         Ada.Text_IO.Put_Line("TM11: Drive " & byte'Image(byte(selected)) & " first record size is " & toOct(drive.rec_size));
         drive.rec_pos  := 0;
      else
         drive.position := 0;
         drive.rec_size := 0;
         drive.rec_pos  := 0;
      end if;
      self.MTS.sel_rem := drive.present;
      self.MTS.cmd_err := not drive.present;
      self.MTC.err := (MTS_to_word(self.MTS) and 16#FF80#) /= 0;
      if self.MTC.int_enb then
         self.host.interrupt(self.vector);
      end if;
   end;
   --
end;
