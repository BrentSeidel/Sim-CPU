--
--  Author: Brent Seidel
--  Date: 13-Apr-2026
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
--  This package contains a PC11 paper tape punch/reader.
--
with Ada.Exceptions;
with Ada.Text_IO;
package body BBS.Sim_CPU.io.tape.pc11 is
   --  ----------------------------------------------------------------------
   --  This is an I/O device for a PC11 paper tape reader/punch.  It is
   --  designed to work with the PDP-11 simulations.
   --
   --  Set which exception to use.  The RX vector is the LSW of except.  The TX
   --  vector is the next MSW of except.  16#04_0000# is added to represent the
   --  interrupt level of BR4.  The 16#1000_0000# is to delay the actual execution
   --  of the interrupt to allow some time for the CPU to complete the service
   --  routine.  This may need to be adjusted.
   --
   procedure setException(self : in out pc11; except : long) is
      prio    : constant long := 16#0004_0000#;  --  BR4 priority level
      timeout : constant long := 16#1000_0000#;  --  16#10# instruction delete before interrupt triggers
   begin
      self.rx_vect := (except and 16#FFFF#) + prio + timeout;
      self.tx_vect := (except/16#10000# and 16#FFFF#) + prio + timeout;
   end;
   --
   --  Reset/Initialize device
   --
   overriding
   procedure reset(self : in out pc11) is
   begin
      self.rx_en := False;
      self.tx_en := False;
      self.rx_eof := False;
      if self.host.trace.io then
         Ada.Text_IO.Put_Line("PC11: Reset commanded by bus");
      end if;
   end;
   --
   --  Open attached file(s)
   --
   procedure openIn(self : in out pc11; name : String) is
   begin
      if self.inPresent then
         tape_io.Close(self.inFile);
      end if;
      begin
         tape_io.Open(self.inFile, tape_io.In_File, name);
         self.inPresent := True;
         self.rx_eof := False;
      exception
         when tape_io.Name_Error =>
            Ada.Text_IO.Put_Line("TAPE RDR: Proposed input file does not exist");
            self.inPresent := False;
            self.rx_eof := True;
      end;
   end;
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out pc11; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
      value  : constant byte := byte(data and 16#FF#);
      drive  : byte;
      action : byte;
   begin
      case size is
         when bits8 =>
--            if self.host.trace.io then
               Ada.Text_IO.Put("PC11: Writing byte " & toOct(byte(data and 16#FF#)) & " to address " & toOct(addr));
--            end if;
            case offset is
               when PRSlsb =>  --  Reader status register LSB (read/write only)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PRS lsb");
--                  end if;
                  self.rx_en := (data and 16#40#) /= 0;
                  if self.inPresent and ((data and 1) /= 0) then
                     self.rx_data := self.read_tape;
                  end if;
                  --
                  --  If interrupts are enabled and data is requested or no input file,
                  --  then either data is ready or an error has occured.  Either way, send an interrupt.
                  --
                  if self.rx_en and (((data and 1) /= 0) or not self.inPresent) then
                     self.host.interrupt(self.rx_vect);
                  end if;
               when PRSmsb =>  --  Reader status register MSB (read only)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PRS msb");
--                  end if;
                  null;
               when PBRlsb =>  --  Reader buffer register LSB (read only)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PBR lsb");
--                  end if;
                  null;
               when PRBmsb =>  --  Reader buffer register MSB (unused)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PRB msb");
--                  end if;
                  null;
               when PPSlsb =>  --  Punch status register LSB
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPS lsb");
--                  end if;
                  self.tx_en := (data and 16#40#) /= 0;
                  --
                  --  If interrupts are enabled, then either there is an error
                  --  condition or the punch is ready to accept data.
                  --
                  if self.tx_en then
                     self.host.interrupt(self.tx_vect);
                  end if;
               when PPSmsb =>  --  Punch status register MSB (read only)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPS msb");
--                  end if;
               when PPBlsb =>  --  Punch buffer register LSB (write obly)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPB lsb");
--                  end if;
                  tape_io.Write(self.outFile, byte((data and 16#FF#)));
                  if self.tx_en then
                     self.host.interrupt(self.tx_vect);
                  end if;
               when PPBmsb =>  --  Punch buffer register MSB (unused)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPB msb");
--                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when bits16 =>
--            if self.host.trace.io then
               Ada.Text_IO.Put("PC11: Writing word " & toOct(word(data and 16#FFFF#)) & " to address " & toOct(addr));
--            end if;
            case offset is
               when PRSlsb =>  --  Reader status register (read/write only)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PRS");
--                  end if;
                  self.rx_en := (data and 16#40#) /= 0;
                  if self.inPresent and ((data and 1) /= 0) then
                     self.rx_data := self.read_tape;
                  end if;
                  --
                  --  If interrupts are enabled and data is requested or no input file,
                  --  then either data is ready or an error has occured.  Either way, send an interrupt.
                  --
                  if self.rx_en and (((data and 1) /= 0) or not self.inPresent) then
                     self.host.interrupt(self.rx_vect);
                  end if;
               when PBRlsb =>  --  Reader buffer register (read only)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PBR");
--                  end if;
                  null;
               when PPSlsb =>  --  Punch status register
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPS");
--                  end if;
                  self.tx_en := (data and 16#40#) /= 0;
                  --
                  --  If interrupts are enabled, then either there is an error
                  --  condition or the punch is ready to accept data.
                  --
                  if self.tx_en then
                     self.host.interrupt(self.tx_vect);
                  end if;
               when PPBlsb =>  --  Punch buffer register (write only)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPB");
--                  end if;
                  tape_io.Write(self.outFile, byte((data and 16#FF#)));
                  if self.tx_en then
                     self.host.interrupt(self.tx_vect);
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when others =>
            status := BUS_NONE;
      end case;
   end;
   --
   --  Read a character from tape.  If no character can be read, return
   --  a ^Z as an end of file marker.
   --
   function read_tape(self : in out pc11) return data_bus is
      t : byte;
   begin
      if not tape_io.end_of_file(self.inFile) then
         tape_io.read(self.inFile, t);
         return data_bus(t);
      else
         self.rx_eof := True;
         return ctrl_z;
      end if;
   exception
      when e : others =>
         Ada.Text_IO.Put_Line("PC11: Error reading from file: " &
               Ada.Exceptions.Exception_Message(e));
         return ctrl_z;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out pc11; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
      temp   : data_bus := 0;
      retval : data_bus;
   begin
      case size is
         when bits8 =>
--            if self.host.trace.io then
               Ada.Text_IO.Put("PC11: Reading byte from ");
--            end if;
            case offset is
               when PRSlsb =>  --  Reader status register LSB
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PRS lsb");
--                  end if;
                  temp := (if self.inPresent then 128 else 0) +
                    (if self.rx_en then 64 else 0);
               when PRSmsb =>  --  Reader status register MSB
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PRS msb");
--                  end if;
                  temp := (if (self.inPresent and not self.rx_eof) then 0 else 128);
               when PBRlsb =>  --  Reader reciver buffer LSB (character received)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PBR lsb");
--                  end if;
                  if self.inPresent then
                     temp := self.rx_data;
                  else
                     temp := ctrl_z;
                  end if;
               when PRBmsb =>  --  Reader buffer register MSB (unused)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PRB msb");
--                  end if;
                  temp := 0;
               when PPSlsb =>   --  Punch status register LSB
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPS lsb");
--                  end if;
                  temp := (if self.outPresent then 128 else 0) +
                    (if self.tx_en then 64 else 0);
               when PPSmsb =>  --  Punch status register MSB
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPS msb");
--                  end if;
                  temp := (if self.outPresent then 0 else 128);
               when PPBlsb =>  --  Punch buffer register LSB (write only)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPB lsb");
--                  end if;
                  temp := 0;
               when PPBmsb =>  --  Punch buffer register MSB (unused)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPB msb");
--                  end if;
                  temp := 0;
               when others =>
                  status := BUS_NONE;
            end case;
--            if self.host.trace.io then
               Ada.Text_IO.Put_Line(" value " & toOct(byte(temp)));
--            end if;
         when bits16 =>
--            if self.host.trace.io then
               Ada.Text_IO.Put("PC11: Reading word from ");
--            end if;
            case offset is
               when PRSlsb =>  --  Reader status register
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PRS");
--                  end if;
                  temp := (if (self.inPresent and not self.rx_eof) then 0 else 32768) +
                    (if (self.inPresent and not self.rx_eof) then 128 else 0) +
                    (if self.rx_en then 64 else 0);
               when PBRlsb =>  --  Reader buffer register (character received)
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PRB");
--                  end if;
                  if self.inPresent then
                     temp := self.rx_data;
                  else
                     temp := ctrl_z;
                  end if;
               when PPSlsb =>   --  Punch status register
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPS");
--                  end if;
                  temp := (if self.outPresent then 0 else 32768) +
                    (if self.outPresent then 128 else 0) +
                    (if self.tx_en then 64 else 0);
               when PPBlsb =>  --  Punch buffer register
--                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPB");
--                  end if;
                  temp := 0;
               when others =>
                  status := BUS_NONE;
            end case;
--            if self.host.trace.io then
               Ada.Text_IO.Put_Line(" value " & toOct(word(temp)));
--            end if;
         when others =>
            status := BUS_NONE;
      end case;
      return temp;
   end;
   --
end;
