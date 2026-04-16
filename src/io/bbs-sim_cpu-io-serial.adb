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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
with Ada.Exceptions;
with Ada.Text_IO;
package body BBS.Sim_CPU.io.serial is
--  ----------------------------------------------------------------------
--
--  Printer device actions
--
--  Write to a port address
--
   overriding
   procedure write(self : in out print8; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is
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
   --  Get the name of the attached file, if any.
   --
   function fname(self : in out print8) return String is
   begin
      if self.ready then
         return Ada.Text_IO.Name(self.File);
      else
         return ">closed<";
      end if;
   end;
   --
   --  Get the presence of the attached file, if any.
   --
   function present(self : in out print8) return Boolean is
   begin
      return self.ready;
   end;
   --
end;
