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
with Ada.Exceptions;
with Ada.Text_IO;
package body BBS.Sim_CPU.serial is
--  ----------------------------------------------------------------------
--
--  Printer device actions
--
--  Write to a port address
--
   overriding
   procedure write(self : in out print8; addr : addr_bus; data : data_bus) is
   begin
      if self.ready then
         Ada.Text_IO.Put(Character'Val(Integer(data and 16#FF#)));
      end if;
   end;
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out print8) return addr_bus is
   begin
      return self.base;
   end;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out print8; base : addr_bus) is
   begin
      self.base := base;
   end;
   --
   --  Open the attached file
   --  If the file exists, then append to it.  If it does not exist, create it
   --  for output.
   --
   procedure open(self : in out print8; name : String) is
   begin
      if self.ready then
         Ada.Text_IO.Close(self.file);
      end if;
      begin
         Ada.Text_IO.Open(self.file, Ada.Text_IO.Append_File, name);
      exception
         when Ada.Text_IO.Name_Error =>
            Ada.Text_IO.Create(self.file, Ada.Text_IO.Out_File, name);
      end;
      self.ready := True;
   end;
   --
   --  Close the attached file
   --
   procedure close(self : in out print8) is
   begin
      if self.ready then
         Ada.Text_IO.Close(self.file);
         self.ready := False;
      end if;
   end;
--
--  ----------------------------------------------------------------------
--  This is an I/O device for a simple 8-bit paper tape interface.  It
--  may get expanded to be usable as a magnetic tape simulation.
--
--  Two addresses are used.
--  base + 0 - Data (R/W)
--  base + 1 - Status (RO)
--
--  Data read and write to the data port complete immediately as far as
--  the simulator is concerned
--
--  The status port is read only (writes are ignored) with the following
--  bits defined:
--  0 - Read file attached
--  1 - Write file attached
--  2 - Read file EOF
--  3-7 - unused
--
--  Write to a port address
--
   overriding
   procedure write(self : in out tape8; addr : addr_bus; data : data_bus) is
   begin
      if addr = self.base and self.outPresent then  --  Data register
         tape_io.Write(self.outFile, byte((data and 16#FF#)));
      end if;
   end;
   --
   --  Read a character from tape.  If no character can be read, return
   --  a ^Z as an end of file marker.
   --
   function read_tape(self : in out tape8) return data_bus is
      t : byte;
   begin
      tape_io.read(self.inFile, t);
      return data_bus(t);
   exception
      when e : others =>
         Ada.Text_IO.Put_Line("TAPE RDR: Error reading from file: " &
               Ada.Exceptions.Exception_Message(e));
         return ctrl_z;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out tape8; addr : addr_bus) return data_bus is
      retval : data_bus;
   begin
      if addr = self.base then  --  Data register
         if self.inPresent then
            return self.read_tape;
         else
            return ctrl_z;
         end if;
      elsif addr = (self.base + 1) then  --  Status register
         if self.inPresent then
            retval := 1;
            if tape_io.End_Of_File(self.inFile) then
               retval := retval + 4;
            end if;
         else
            retval := 0;
         end if;
         if self.outPresent then
            retval := retval + 2;
         end if;
         return retval;
      end if;
      return 0;
   end;
   --
   --  Get the base address
   --
   overriding
   function getBase(self : in out tape8) return addr_bus is
   begin
      return self.base;
   end;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out tape8; base : addr_bus) is
   begin
      self.base := base;
   end;
   --
   --  Open attached file(s)
   --
   procedure openIn(self : in out tape8; name : String) is
   begin
      if self.inPresent then
         tape_io.Close(self.inFile);
      end if;
      begin
         tape_io.Open(self.inFile, tape_io.In_File, name);
         self.inPresent := True;
      exception
         when tape_io.Name_Error =>
            Ada.Text_IO.Put_Line("TAPE RDR: Proposed input file does not exist");
            self.inPresent := False;
      end;
   end;
   --
   procedure openOut(self : in out tape8; name : String) is
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
   procedure closeIn(self : in out tape8) is
   begin
      if self.inPresent then
         tape_io.Close(self.inFile);
         self.inPresent := False;
      end if;
   end;
   --
   procedure closeOut(self : in out tape8) is
   begin
      if self.outPresent then
         tape_io.Close(self.outFile);
         self.outPresent := False;
      end if;
   end;
   --
   --  Get the name of the attached file, if any.
   --
   function fnameIn(self : in out tape8) return String is
   begin
      if self.inPresent then
         return tape_io.Name(self.inFile);
      else
         return ">closed<";
      end if;
   end;
   --
   function fnameOut(self : in out tape8) return String is
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
   function presentIn(self : in out tape8) return Boolean is
   begin
      return self.inPresent;
   end;
   --
   function presentOut(self : in out tape8) return Boolean is
   begin
      return self.outPresent;
   end;
   --
end;
