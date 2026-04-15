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
   --  interrupt level of BR4.
   --
   procedure setException(self : in out pc11; except : long) is
   begin
      self.rx_vect := (except and 16#FF#) + 16#04_0000#;
      self.tx_vect := (except/16#100# and 16#FF#) + 16#04_0000#;
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
            if self.host.trace.io then
               Ada.Text_IO.Put("PC11: Writing byte " & toOct(byte(data and 16#FF#)) & " to address " & toOct(addr));
            end if;
            case offset is
               when PRSlsb =>  --  Reader status register LSB (read/write only)
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PRS lsb");
                  end if;
                  self.rx_en := (data and 16#40#) /= 0;
               when PRSmsb =>  --  Reader status register MSB (read only)
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PRS msb");
                  end if;
                  null;
               when PBRlsb =>  --  Reader buffer register LSB (read only)
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PBR lsb");
                  end if;
                  null;
               when PRBmsb =>  --  Reader buffer register MSB (unused)
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PRB msb");
                     end if;
                  null;
               when PPSlsb =>  --  Punch status register LSB
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPS lsb");
                  end if;
                  self.tx_en := (data and 16#40#) /= 0;
               when PPSmsb =>  --  Punch status register MSB (read only)
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPS msb");
                  end if;
               when PPBlsb =>  --  Punch buffer register LSB (write obly)
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPB lsb");
                  end if;
                  tape_io.Write(self.outFile, byte((data and 16#FF#)));
               when PPBmsb =>  --  Punch buffer register MSB (unused)
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPB msb");
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when bits16 =>
            if self.host.trace.io then
               Ada.Text_IO.Put("PC11: Writing word " & toOct(word(data and 16#FFFF#)) & " to address " & toOct(addr));
            end if;
            case offset is
               when PRSlsb =>  --  Reader status register (read/write only)
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PRS)");
                  end if;
                  self.rx_en := (data and 16#40#) /= 0;
               when PBRlsb =>  --  Reader buffer register (read only)
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PBR");
                  end if;
                  null;
               when PPSlsb =>  --  Punch status register
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPS");
                  end if;
                  self.tx_en := (data and 16#40#) /= 0;
               when PPBlsb =>  --  Punch buffer register (write only)
                  if self.host.trace.io then
                     Ada.Text_IO.Put_Line(" PPB");
                  end if;
                  tape_io.Write(self.outFile, byte((data and 16#FF#)));
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
      tape_io.read(self.inFile, t);
      return data_bus(t);
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
            if self.host.trace.io then
               Ada.Text_IO.Put("PC11: Reading byte from ");
            end if;
            case offset is
               when PRSlsb =>  --  Reader status register LSB
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PRS lsb");
                  end if;
                  temp := 128 + (if self.rx_en then 64 else 0);
               when PRSmsb =>  --  Reader status register MSB
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PRS msb");
                  end if;
                  temp := (if self.inPresent then 0 else 128);
               when PBRlsb =>  --  Reader reciver buffer LSB (character received)
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PBR lsb");
                  end if;
                  if self.inPresent then
                     temp := self.read_tape;
                  else
                     temp := ctrl_z;
                  end if;
               when PRBmsb =>  --  Reader buffer register MSB (unused)
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PRB msb");
                  end if;
                  temp := 0;
               when PPSlsb =>   --  Punch status register LSB
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPS lsb");
                  end if;
                  temp := 128 + (if self.tx_en then 64 else 0);
               when PPSmsb =>  --  Punch status register MSB
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPS msb");
                  end if;
                  temp := (if self.outPresent then 0 else 128);
               when PPBlsb =>  --  Punch buffer register LSB (write only)
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPB lsb");
                  end if;
                  temp := 0;
               when PPBmsb =>  --  Punch buffer register MSB (unused)
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPB msb");
                  end if;
                  temp := 0;
               when others =>
                  status := BUS_NONE;
            end case;
            if self.host.trace.io then
               Ada.Text_IO.Put_Line(" value " & toOct(byte(temp)));
            end if;
         when bits16 =>
            if self.host.trace.io then
               Ada.Text_IO.Put("PC11: Reading byte from ");
            end if;
            case offset is
               when PRSlsb =>  --  Reader status register
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PRS)");
                  end if;
                  temp := (if self.inPresent then 0 else 32768) +
                    128 + (if self.rx_en then 64 else 0);
               when PBRlsb =>  --  Reader buffer register (character received)
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PBR");
                  end if;
                  if self.inPresent then
                     temp := self.read_tape;
                  else
                     temp := ctrl_z;
                  end if;
               when PPSlsb =>   --  Punch status register
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPS");
                  end if;
                  temp := (if self.outPresent then 0 else 32768) +
                    128 + (if self.tx_en then 64 else 0);
               when PPBlsb =>  --  Punch buffer register
                  if self.host.trace.io then
                     Ada.Text_IO.Put("PPB");
                  end if;
                  temp := 0;
               when others =>
                  status := BUS_NONE;
            end case;
            if self.host.trace.io then
               Ada.Text_IO.Put_Line(" value " & toOct(word(temp)));
            end if;
         when others =>
            status := BUS_NONE;
      end case;
      return temp;
   end;
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out pc11) return addr_bus is
   begin
      return self.base;
   end;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out pc11; base : addr_bus) is
   begin
      self.base := base;
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
      exception
         when tape_io.Name_Error =>
            Ada.Text_IO.Put_Line("PC11: Proposed input file does not exist");
            self.inPresent := False;
      end;
   end;
   --
   procedure openOut(self : in out pc11; name : String) is
   begin
      if self.outPresent then
         tape_io.Close(self.outFile);
      end if;
      begin
         tape_io.Open(self.outFile, tape_io.Append_File, name);
      exception
         when tape_io.Name_Error =>
            tape_io.Create(self.outFile, tape_io.Out_File, name);
      end;
      self.outPresent := True;
   end;
   --
   --  Close the attached file
   --
   procedure closeIn(self : in out pc11) is
   begin
      if self.inPresent then
         tape_io.Close(self.inFile);
         self.inPresent := False;
      end if;
   end;
   --
   procedure closeOut(self : in out pc11) is
   begin
      if self.outPresent then
         tape_io.Close(self.outFile);
         self.outPresent := False;
      end if;
   end;
   --
   --  Get the name of the attached file, if any.
   --
   function fnameIn(self : in out pc11) return String is
   begin
      if self.inPresent then
         return tape_io.Name(self.inFile);
      else
         return ">closed<";
      end if;
   end;
   --
   function fnameOut(self : in out pc11) return String is
   begin
      if self.outPresent then
         return tape_io.Name(self.outFile);
      else
         return ">closed<";
      end if;
   end;
   --
   --  Get the presence of the attached file, if any.
   --
   function presentIn(self : in out pc11) return Boolean is
   begin
      return self.inPresent;
   end;
   --
   function presentOut(self : in out pc11) return Boolean is
   begin
      return self.outPresent;
   end;
   --
end;
