--
--  Author: Brent Seidel
--  Date: 14-May-2026
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
package body BBS.Sim_CPU.io.BM792 is
   --
   --  Write to a port address.  This is a ROM, so writes do nothing.
   --
   overriding
   procedure write(self : in out BM792; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is
   begin
      null;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out BM792; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus is
      offset    : constant byte := byte((addr - self.base) and 16#FF#);
      ret_val   : data_bus := 0;
   begin
      Ada.Text_IO.Put_Line("BM792: Reading address " & toOct(addr) & ", offset " & toOct(offset));
      case size is
         when bits8 =>
            if self.host.trace.io then
               Ada.Text_IO.Put("BM792: Reading byte from address " & toOct(addr));
            end if;
            if (offset and 1) = 0 then
               ret_val := data_bus(self.data(offset/2) and 16#FF#);
            else
               ret_val := data_bus((self.data(offset/2)/16#100#) and 16#FF#);
            end if;
         when bits16 =>
            if self.host.trace.io then
               Ada.Text_IO.Put("BM792: Reading word from address " & toOct(addr));
            end if;
            ret_val := data_bus(self.data(offset/2));
         when others =>
            ret_val := 0;
            status := BUS_NONE;
      end case;
      if self.host.trace.io then
         Ada.Text_IO.Put_Line(", value " & toOct(ret_val));
      end if;
      return ret_val;
   end;
   -- -------------------------------------------------------------------------
end;
