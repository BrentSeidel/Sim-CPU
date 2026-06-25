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
with BBS.Sim_CPU.cpu.pdp11;
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
   --  Set which exception to use.  16#05_0000# is added to represent the
   --  interrupt level of BR5.  The 16#1000_0000# is to delay the actual execution
   --  of the interrupt to allow some time for the CPU to complete the service
   --  routine.  This may need to be adjusted.
   --
   procedure setException(self : in out tm11; except : long) is
      prio    : constant long := 16#00_04_0000#;  --  BR5 priority level
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
      self.MTS := word_to_MTS(0);
      self.MTC := word_to_MTC(0);
      self.MTC.DEN5 := True;
      self.MTC.DEN8 := True;
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
                  self.MTC.CU_RDY := True;
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
                  self.MTC.CU_RDY := True;
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
   --  Process the command specified in MTC
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
   --  This also clears several bits in MTS at the beginning of operation and
   --  does a check for the tape drive being present.  This way, these actions
   --  do not need to be performed in each procedure.
   --
   procedure process_command(self : in out tm11) is
      selected : constant byte := byte(self.MTC.SEL);
      drive    : tape_info renames self.drive_info(selected);
   begin
      self.MTS.EOF := False;
      self.MTS.EOT := False;
      self.MTS.BOT := False;
      self.MTS.RLE := False;
      self.MTS.BTE := False;
      self.MTS.NXM := False;
      self.MTS.cmd_err := False;
      self.MTS.wrt_lock := (drive.sw_prot or not drive.writeable);
      if not drive.present then
         self.MTS.cmd_err := True;
         self.MTS.sel_rem := False;
      else
         case self.MTC.funct is
         when 0 =>  --  Off-Line
--            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("TM11: Implemented command off-line drive " & byte'Image(selected));
--            end if;
            self.rewind;
         when 1 =>  --  Read
--            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("TM11: Implemented command read drive " & byte'Image(selected));
--            end if;
            self.read;
         when 2 =>  --  Write
            Ada.Text_IO.Put_Line("TM11: *Testing* command write drive " & byte'Image(selected));
            self.write;
         when 3 =>  --  Write End Of File
            Ada.Text_IO.Put_Line("TM11: *Testing* command write end-of-file drive " & byte'Image(selected));
            self.write_EOF;
         when 4 =>  --  Space Forward
--            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("TM11: Implemented command space forward drive " & byte'Image(selected));
--            end if;
            self.space_fore;
         when 5 =>  --  Space Reverse
            Ada.Text_IO.Put_Line("TM11: Implemented command space reverse drive " & byte'Image(selected));
            self.space_back;
         when 6 =>  --  Write with Extended IRG
            Ada.Text_IO.Put_Line("TM11: *Unimplemented* command write with extended IRG drive " & byte'Image(selected));
            self.write;
         when 7 =>  --  Rewind
--            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("TM11: Implemented command rewind drive " & byte'Image(selected));
--            end if;
            self.rewind;
         end case;
      end if;
      self.MTS.BOT := drive.position = 1;
      self.MTC.err := (MTS_to_word(self.MTS) and 16#FF80#) /= 0;
      self.MTC.go := False;
      if self.MTC.int_enb then
         self.host.interrupt(self.vector);
      end if;
   end;
   --
   --  Open the attached file.  If file does not exist, then create it.
   --
   procedure open(self : in out tm11; drive : byte; geom : geometry; name : String) is
   begin
      if self.drive_info(drive).present then
         tape_io.Close(self.drive_info(drive).Image);
      end if;
      begin
         tape_io.Open(self.drive_info(drive).image, tape_io.Inout_File, name);
      exception
         when tape_io.Name_Error =>
            self.extend(drive, name);
            return;
      end;
      self.drive_info(drive).present   := True;
      self.drive_info(drive).writeable := True;
      self.drive_info(drive).sw_prot   := False;
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
      self.drive_info(drive).position  := 1;
      self.drive_info(drive).rec_size  := 0;
      self.drive_info(drive).rec_pos   := 0;
      self.drive_info(drive).present   := True;
      self.drive_info(drive).writeable := True;
      self.drive_info(drive).sw_prot   := False;
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
   --  Read should begin with the file positioned at a record size entry.  If the
   --  entry is zero, an End-Of-File is reported.  Otherwise, the requested number
   --  of bytes are read into memory.  If the request is equal to or greated than
   --  the record size, the full record is read and reading is terminated.  If the
   --  request is less than the record size, an incomplete record is read and
   --  self.MTS.RLE is set.  Checks are also done for drive not present and drive
   --  position at the end of the attached file.
   --
   procedure read(self : in out tm11) is
      selected : constant byte := byte(self.MTC.SEL);
      drive    : tape_info renames self.drive_info(selected);
      data     : byte;
      size     : uint32;
      addr     : addr_bus := addr_bus(self.MTCMA) + (if self.MTC.addr16 then 16#1_0000# else 0) +
        (if self.MTC.addr17 then 16#2_0000# else 0);
   begin
      if tape_io.Size(drive.image) <= drive.position then
         Ada.Text_IO.Put_Line("TM11: Attempt to read at end of tape.");
         self.MTS.EOF := True;
         self.MTS.EOT := True;
         return;
      end if;
      --
      --  Check if read starting at the beginning of a record
      --
      if (drive.rec_size = 0) or (drive.rec_pos = 0) then
         drive.rec_size := self.record_size(drive);
         drive.rec_pos  := 0;
      end if;
      if self.host.trace.io or debug then
         Ada.Text_IO.Put_Line("TM11: Reading " & toOct((not self.MTBCR) + 1) & " bytes of data to address " &
                                toOct(addr) & ", starting from tape position " &
                                tape_io.Positive_Count'Image(drive.position) &
                                " (" & toHex(uint32(drive.position)) & ")" & " on drive " &
                                byte'Image(selected));
         Ada.Text_IO.Put_Line("TM11: Starting position " & uint32'Image(drive.rec_pos) & " of record size " &
                                uint32'Image(drive.rec_size));
      end if;
      if drive.rec_size > 0 then
         for i in 1 .. drive.rec_size loop
            tape_io.Read(drive.image, data);
            drive.position := drive.position + 1;
            drive.rec_pos := drive.rec_pos + 1;
            self.host.set_mem(addr, data_bus(data));
            addr := addr + 1;
            self.MTBCR := self.MTBCR + 1;
            exit when (self.MTBCR = 0);
         end loop;
         if self.host.trace.io or debug then
            Ada.Text_IO.Put_Line("TM11: Read attempt finished, " & toOct((not self.MTBCR) + 1) &
                                   ", bytes remaining, final address " & toOct(addr));
            Ada.Text_IO.Put_Line("TM11: Record position is " & uint32'Image(drive.rec_pos));
         end if;
         if drive.rec_pos = drive.rec_size then  --  End of record reached
            self.MTS.RLE := False;
         else
            self.MTS.RLE := True;
            drive.position := drive.position + tape_io.Positive_Count(drive.rec_size - drive.rec_pos);
            tape_io.Set_Index(drive.image, drive.position);
         end if;
         size := self.record_size(drive);     --  Read ending record size (should be the same as beginning record size)
         if size /= drive.rec_size then
            Ada.Text_IO.Put_Line("TM11: Record size mismatch is " & uint32'Image(drive.rec_size) &
                                      " at beginning and " & uint32'Image(size) & " at end.");
            Ada.Text_IO.Put_Line("TM11: Drive position is  " & tape_io.Positive_Count'Image(drive.position) &
                                      " (" & toHex(uint32(drive.position)) & ")");
            Ada.Text_IO.Put_Line("TM11: Record position is " & uint32'Image(drive.rec_pos));
            Ada.Text_IO.Put_Line("TM11: Record size is     " & uint32'Image(drive.rec_size));
         end if;
         drive.rec_pos := 0;  --  Moving to a new record
      else
         self.MTS.EOF  := True;
         if tape_io.End_Of_File(drive.image) then
            self.MTS.EOT := True;
         end if;
         if self.MTS.EOT then
            Ada.Text_IO.Put_Line("TM11: Read End of Tape found");
         end if;
      end if;
      self.MTCMA := word(addr and 16#FFFF#);
      self.MTC.addr16 := (addr and 16#1_0000#) /= 0;
      self.MTC.addr17 := (addr and 16#2_0000#) /= 0;
   exception
      when e : tape_io.End_Error =>  --  End of file
         self.MTS.EOF := True;
         self.MTS.EOT := True;
         Ada.Text_IO.Put_Line("TM11: End of file *exception* during read");
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(e));
   end;
   --
   --  write to the selected drive.
   --
   --  While the real TM11 can start writing in the middle of a record, doing
   --  that in this simulation is forbidden.  In both the real and simulated
   --  cases, it can lead to corrupted data.  There are three possible solutions:
   --  1. Move to the beginning of the current record and overwrite it.
   --  2. Move to the end of the current record and write a new record after.
   --  3. Report an error.
   --
   --  If at the proper position, the size of the record to be written is compared
   --  with the existing record size.  If they are the same, then the new record
   --  simply replaces the existing one.  If they are different, an end of tape
   --  marker will be written after the record as the rest of the tape will be
   --  corrupted.
   --
   procedure write(self : in out tm11) is
      selected : constant byte := byte(self.MTC.SEL);
      drive    : tape_info renames self.drive_info(selected);
      addr     : long := long(self.MTCMA) + (if self.MTC.addr16 then 16#1_0000# else 0) +
        (if self.MTC.addr17 then 16#2_0000# else 0);
      count    : word := (not self.MTBCR) + 1;
      data     : byte;
      size     : uint32;
   begin
      Ada.Text_IO.Put_Line("TM11: Writing " & toOct((not self.MTBCR) + 1) & " bytes of data from address " &
                                toOct(addr) & ", starting at tape position " &
                                tape_io.Positive_Count'Image(drive.position) &
                                " (" & toHex(uint32(drive.position)) & ")" & " on drive " &
                                byte'Image(selected));
      Ada.Text_IO.Put_Line("TM11: Starting position " & uint32'Image(drive.rec_pos) & " of record size " &
                             uint32'Image(drive.rec_size));
      --
      --  Check for drive writeable
      --
      if self.MTS.wrt_lock then
         Ada.Text_IO.Put_Line("TM11: *Write to write locked drive " & byte'Image(selected));
         self.MTS.cmd_err := True;
         return;
      end if;
      --
      --  Check for write in the middle of a record
      --
      if (drive.rec_size /= drive.rec_pos) and (drive.rec_pos /= 0) then  --  Somewhere in a record
         Ada.Text_IO.Put_Line("TM11: *Write attempt in the middle of a record");
         self.MTS.cmd_err := True;  --  This is an error
         return;
      end if;
      --
      -- Check for different record sizes (this includes end of file mark)
      --
      if drive.position = tape_io.size(drive.image) then
         drive.position := drive.position - mark_size;
         tape_io.Set_Index(drive.image, drive.position);
      end if;
      size := self.record_size(drive);
      if size /= uint32(count) then
         Ada.Text_IO.Put_Line("TM11: Write attempt for different size record");
         drive.position := drive.position - mark_size;
         drive.rec_pos  := 0;
         drive.rec_size := size;
         tape_io.Set_Index(drive.image, drive.position);
         record_size(drive, uint32(count));
         for i in 1 .. count loop
            data := byte(self.host.read_mem(addr) and 16#FF#);
            tape_io.Write(drive.image, data);
            drive.position := drive.position + 1;
            drive.rec_pos := drive.rec_pos + 1;
            addr := addr + 1;
            self.MTBCR := self.MTBCR + 1;
            exit when (self.MTBCR = 0);
         end loop;
         self.MTCMA := word(addr and 16#FFFF#);
         self.MTC.addr16 := (addr and 16#1_0000#) /= 0;
         self.MTC.addr17 := (addr and 16#2_0000#) /= 0;
         record_size(drive, uint32(count));
         Ada.Text_IO.Put_Line("TM11: Drive position is  " & tape_io.Positive_Count'Image(drive.position) &
                                " (" & toHex(uint32(drive.position)) & ")");
         Ada.Text_IO.Put_Line("TM11: Record position is " & uint32'Image(drive.rec_pos));
         Ada.Text_IO.Put_Line("TM11: Record size is     " & uint32'Image(drive.rec_size));
         --
         --  Since the end of the record is in the middle of another, write two
         --  end of file marks to indicate that this is the end of valid data.
         --
         record_size(drive, 0);
--         record_size(drive, 0);
         drive.position := drive.position - mark_size;
         tape_io.Set_Index(drive.image, drive.position);
         drive.rec_pos := 0;  --  Moving to a new record
         return;
      end if;
      --
      --  Same size, so just replace existing record.
      --
      Ada.Text_IO.Put_Line("TM11: Write attempt for same size record");
      for i in 1 .. count loop
         data := byte(self.host.read_mem(addr) and 16#FF#);
         tape_io.Write(drive.image, data);
         drive.position := drive.position + 1;
         drive.rec_pos := drive.rec_pos + 1;
         addr := addr + 1;
         self.MTBCR := self.MTBCR + 1;
         exit when (self.MTBCR = 0);
      end loop;
      self.MTCMA := word(addr and 16#FFFF#);
      self.MTC.addr16 := (addr and 16#1_0000#) /= 0;
      self.MTC.addr17 := (addr and 16#2_0000#) /= 0;
      record_size(drive, size);
      drive.rec_pos := 0;  --  Moving to a new record
      Ada.Text_IO.Put_Line("TM11: Drive position is  " & tape_io.Positive_Count'Image(drive.position) &
                                " (" & toHex(uint32(drive.position)) & ")");
      Ada.Text_IO.Put_Line("TM11: Record position is " & uint32'Image(drive.rec_pos));
      Ada.Text_IO.Put_Line("TM11: Record size is     " & uint32'Image(drive.rec_size));
      --
      --  If at the end of the physical file, write two end of file marks and back up.
      --
      if tape_io.Size(drive.image) <= drive.position then
         record_size(drive, 0);
         record_size(drive, 0);
         drive.position := drive.position - 2*mark_size;
         tape_io.Set_Index(drive.image, drive.position);
      end if;
   end;
   --
   --  Write end of file to the selected drive
   --
   --  While the real TM11 can start writing in the middle of a record, doing
   --  that in this simulation is forbidden.  In both the real and simulated
   --  cases, it can lead to corrupted data.  There are three possible solutions:
   --  1. Move to the beginning of the current record and overwrite it.
   --  2. Move to the end of the current record and write a new record after.
   --  3. Report an error.
   --
   procedure write_EOF(self : in out tm11) is
      selected : constant byte := byte(self.MTC.SEL);
      drive    : tape_info renames self.drive_info(selected);
   begin
      Ada.Text_IO.Put_Line("TM11: Writing EOF starting at tape position " &
                                tape_io.Positive_Count'Image(drive.position) &
                                " (" & toHex(uint32(drive.position)) & ")" & " on drive " &
                                byte'Image(selected));
      --
      --  Check for drive writeable
      --
      if self.MTS.wrt_lock then
         Ada.Text_IO.Put_Line("TM11: *Write EOF to write locked drive " & byte'Image(selected));
         self.MTS.cmd_err := True;
         return;
      end if;
      --
      --  Check for write in the middle of a record
      --
      if (drive.rec_size /= drive.rec_pos) and (drive.rec_pos /= 0) then  --  Somewhere in a record
         Ada.Text_IO.Put_Line("TM11: *Write EOF attempt in the middle of a record");
         Ada.Text_IO.Put_Line("TM11: Drive position is  " & tape_io.Positive_Count'Image(drive.position) &
                                " (" & toHex(uint32(drive.position)) & ")");
         Ada.Text_IO.Put_Line("TM11: Record position is " & uint32'Image(drive.rec_pos));
         Ada.Text_IO.Put_Line("TM11: Record size is     " & uint32'Image(drive.rec_size));
         self.MTS.cmd_err := True;  --  This is an error
         return;
      end if;
      --
      --  Between records
      --
      record_size(drive, 0);
      record_size(drive, 0);
      drive.position := drive.position - mark_size;
      tape_io.Set_Index(drive.image, drive.position);
      drive.rec_size := 0;
      drive.rec_pos  := 0;
   end;
   --
   --  Space forward by records
   --
   --  Space forward may begin with the current pointer in the middle of a record.
   --  If so, the remainder of the record is skipped and end of record size entry
   --  is read.  This should match the current record size.  If at the end of a
   --  record, the record size entry for the next record is read. If the entry is
   --  zero, an End-Of-File is reported and the operation terminated.  Otherwise,
   --  the file pointer is advanced to the record size entry at the end of the
   --  record and that read.  This is repeated for every record to space over.
   --  Checks are also done for drive not present and drive position at the end
   --  of the attached file.
   --
   procedure space_fore(self : in out tm11) is
      selected : constant byte := byte(self.MTC.SEL);
      drive    : tape_info renames self.drive_info(selected);
      count    : word := (not self.MTBCR) + 1;
      size     : uint32;
   begin
      if tape_io.Size(drive.image) <= drive.position then
         Ada.Text_IO.Put_Line("TM11: Attempt to space forward at end of tape.");
         self.MTS.EOF := True;
         self.MTS.EOT := True;
         return;
      end if;
      if self.host.trace.io or debug then
         Ada.Text_IO.Put_Line("TM11: Spacing forward " & word'Image(count) & " records on drive " & byte'Image(selected));
      end if;
      for i in 1 .. count loop
         exit when tape_io.End_Of_File(drive.image);
         --
         --  Check for a partial record read.  If so, finish the record, otherwise skip record.
         --
         if (drive.rec_size /= drive.rec_pos) and (drive.rec_pos /= 0) then  --  If they are equal, then the difference is zero.
--            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("TM11: Spacing forward partial record");
               Ada.Text_IO.Put_Line("TM11: Drive position is  " & tape_io.Positive_Count'Image(drive.position) &
                                " (" & toHex(uint32(drive.position)) & ")");
               Ada.Text_IO.Put_Line("TM11: Record position is " & uint32'Image(drive.rec_pos));
               Ada.Text_IO.Put_Line("TM11: Record size is     " & uint32'Image(drive.rec_size));
--            end if;
            drive.position := drive.position + tape_io.Positive_Count(drive.rec_size - drive.rec_pos);
            drive.rec_pos  := 0;
            tape_io.Set_Index(drive.image, drive.position);
            size := self.record_size(drive);
            if size /= drive.rec_size then
               Ada.Text_IO.Put_Line("TM11: Partial record size mismatch is " & uint32'Image(drive.rec_size) &
                                      " at beginning and " & uint32'Image(size) & " (" &
                                      toHex(size) & ") at end of space forward.");
               self.MTS.BTE := True;
               return;
            end if;
         else
            --
            --  Get size for next record
            --
            if self.host.trace.io or debug then
               Ada.Text_IO.Put_Line("TM11: Spacing forward over complete record at " &
                                      tape_io.Positive_Count'Image(drive.position) &
                                      " (" & toHex(uint32(drive.position)) & ")");
            end if;
            drive.rec_size := self.record_size(drive);
            drive.rec_pos  := 0;
            self.MTBCR     := self.MTBCR + 1;
            if drive.rec_size = 0 then  --  End of file
               self.MTS.EOF := True;
               if self.host.trace.io or debug then
                  Ada.Text_IO.Put_Line("TM11: Space forward terminating due to EOF.");
               end if;
               --
               --  Check for end of tape
               --
               drive.rec_size := self.record_size(drive);
               if drive.rec_size = 0 then  --  End of tape
                  self.MTS.EOT := True;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line("TM11: Space forward exiting due to EOT");
                  end if;
                  exit;
               else
                  drive.position := Drive.position - mark_size;
                  tape_io.Set_Index(drive.image, drive.position);
               end if;
               exit;
            else  --  Skip an ordinary record
               drive.position := drive.position + tape_io.Positive_Count(drive.rec_size);
               tape_io.Set_Index(drive.image, drive.position);
               size := self.record_size(drive);
               self.MTS.BTE := size /= drive.rec_size;
               if self.MTS.BTE then
                  Ada.Text_IO.Put_Line("TM11: Complete record size mismatch is " & uint32'Image(drive.rec_size) &
                                         " at beginning and " & uint32'Image(size) & " at end for space forward.");
                  Ada.Text_IO.Put_Line("TM11: Drive position is  " &
                                         tape_io.Positive_Count'Image(drive.position) &
                                         " (" & toHex(uint32(drive.position)) & ")");
                  Ada.Text_IO.Put_Line("TM11: Record position is " & uint32'Image(drive.rec_pos));
                  Ada.Text_IO.Put_Line("TM11: Record size is     " & uint32'Image(drive.rec_size));
                  return;
               end if;
            end if;
         end if;
      end loop;
      if tape_io.End_Of_File(drive.image) then
         self.MTS.EOF := True;
         self.MTS.EOT := True;
      end if;
      if self.MTS.EOT then
         drive.position := drive.position - 2*mark_size;
         tape_io.Set_Index(drive.image, drive.position);
         drive.rec_size := 0;
         drive.rec_pos  := 0;
      end if;
   exception
      when e : tape_io.End_Error =>  --  End of file
         self.MTS.EOF := True;
         self.MTS.EOT := True;
         Ada.Text_IO.Put_Line("TM11: End of file *exception* during Space forward");
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(e));
   end;
   --
   --  Space backward by records
   --
   --  First, check if the tape is at the beginning.  If so, exit since it can't
   --  go back any further.  Next check if at EOF/EOT marks and move before them.
   --  Finally, start spacing backwards by blocks until an EOF (or BOT) is
   --  encountered.
   --
   procedure space_back(self : in out tm11) is
      selected : constant byte := byte(self.MTC.SEL);
      drive    : tape_info renames self.drive_info(selected);
      count    : word := (not self.MTBCR) + 1;
      size     : uint32;
   begin
      --
      --  Check for already at beginning of tape
      --
      if drive.position = 1 then
         Ada.Text_IO.Put_Line("TM11: Attempt to space backward at beginning of tape.");
         self.MTS.BOT := True;
         return;
      end if;
      --
      --  Check for EOF/EOT marks and move past them to a full record.
      --
      while drive.rec_size = 0 loop
         Ada.Text_IO.Put_Line("TM11: Space backwards past EOF marker at " &
                                tape_io.Positive_Count'Image(drive.position) &
                                " (" & toHex(uint32(drive.position)) & ")");
         drive.position := drive.position - mark_size;
         tape_io.Set_Index(drive.image, drive.position);
         size := self.record_size(drive);
         drive.rec_size := size;
         drive.rec_pos  := 0;
         if size = 0 then
            drive.position := drive.position - mark_size;
            tape_io.Set_Index(drive.image, drive.position);
         end if;
         --
         --  Check if moved to beginning of tape.
         --
         if drive.position = 1 then
            return;
         end if;
      end loop;
      Ada.Text_IO.Put_Line("TM11: Spacing backwards " & word'Image(count) & " records on drive " & byte'Image(selected));
      for i in 1 .. count loop
         --
         --  Check for a partial record read.  If so, finish the record, otherwise skip record.
         --
         if (drive.rec_size /= drive.rec_pos) and (drive.rec_pos /= 0) then  --  If they are equal, then the difference is zero.
            Ada.Text_IO.Put_Line("TM11: Spacing backward partial record");
            Ada.Text_IO.Put_Line("TM11: Drive position is  " & tape_io.Positive_Count'Image(drive.position));
            Ada.Text_IO.Put_Line("TM11: Record position is " & uint32'Image(drive.rec_pos));
            Ada.Text_IO.Put_Line("TM11: Record size is     " & uint32'Image(drive.rec_size));
            drive.position := drive.position - tape_io.Positive_Count(drive.rec_pos);  --  Get to beginning position of record
            drive.position := drive.position - mark_size;                              --  Now at beginning of mark for record
            tape_io.Set_Index(drive.image, drive.position);
            drive.rec_pos  := 0;
            drive.rec_size := self.record_size(drive);
            drive.position := drive.position - mark_size;                              --  Now at beginning of mark for record
            tape_io.Set_Index(drive.image, drive.position);
         else  --  Skip an ordinary record
            --
            --  Index is expected to be between ending record size for previous
            --  record and beginning size for next record.
            --
            Ada.Text_IO.Put_Line("TM11: Spacing backward over complete record at " &
                                   tape_io.Positive_Count'Image(drive.position) &
                                   " (" & toHex(uint32(drive.position)) & ")");
            drive.position := drive.position - mark_size;  --  End of record size mark
            tape_io.Set_Index(drive.image, drive.position);
            size := self.record_size(drive);
            drive.rec_size := size;
            drive.rec_pos  := 0;
            drive.position := drive.position - mark_size;  --  Back to end of record size mark
            if size = 0 then  --  Check for EOF
               self.MTS.EOF := True;
               exit;
            else
               Ada.Text_IO.Put_Line("TM11: Drive position is  " & tape_io.Positive_Count'Image(drive.position) &
                                      " (" & toHex(uint32(drive.position)) & ")");
               Ada.Text_IO.Put_Line("TM11: size is            " & uint32'Image(size) &
                                     " (" & toHex(size) & ")");
               drive.position := drive.position - tape_io.Positive_Count(size);  --  Back to beginning of record
               drive.position := drive.position - mark_size;  --  Back to beginning of record size mark
            end if;
            tape_io.Set_Index(drive.image, drive.position);
         end if;
      end loop;
   end;
   --
   --  Rewind the selected drive
   --
   procedure rewind(self : in out tm11) is
      selected : constant byte := byte(self.MTC.SEL);
      drive    : tape_info renames self.drive_info(selected);
   begin
      self.MTS.ready := True;
      if drive.present then
         tape_io.Set_Index(drive.image, 1);
         drive.position := 1;
         drive.rec_size := 0;
         drive.rec_pos  := 0;
         if self.host.trace.io or debug then
            Ada.Text_IO.Put_Line("TM11: Drive " & byte'Image(byte(selected)) & " rewound ");
         end if;
      end if;
      self.MTS.sel_rem := True;
   end;
   --
   --  Read record size
   --
   function record_size(self : in out tm11; drive : in out tape_info) return uint32 is
      b1, b2, b3, b4 : byte;
   begin
      tape_io.Read(drive.image, b1);
      tape_io.Read(drive.image, b2);
      tape_io.Read(drive.image, b3);
      tape_io.Read(drive.image, b4);
      drive.position := drive.position + mark_size;
      return uint32(b1) + uint32(b2)*16#100# + uint32(b3)*16#1_0000# + uint32(b4)*16#100_0000#;
   exception
      when e : tape_io.End_Error =>  --  End of file
         self.MTS.EOF := True;
         self.MTS.EOT := True;
         Ada.Text_IO.Put_Line("TM11: End of file *exception* during record_size");
         return 0;  --  A value of zero indicates an end-of-file marker.
   end;
   --
   --  Write record size
   --
   procedure record_size(drive : in out tape_info; size : uint32) is
      b1 : constant byte := byte(size and 16#FF#);
      b2 : constant byte := byte((size/16#100#) and 16#FF#);
      b3 : constant byte := byte((size/16#1_0000#) and 16#FF#);
      b4 : constant byte := byte((size/16#100_0000#) and 16#FF#);
   begin
      tape_io.Write(drive.image, b1);
      tape_io.Write(drive.image, b2);
      tape_io.Write(drive.image, b3);
      tape_io.Write(drive.image, b4);
      drive.position := drive.position + mark_size;
   end;
   --
end;
