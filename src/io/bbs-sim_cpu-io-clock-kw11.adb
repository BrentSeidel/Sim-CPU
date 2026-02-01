--
--  Author: Brent Seidel
--  Date: 20-Jan-2026
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
--
with Ada.Text_IO;
package body BBS.Sim_CPU.io.clock.KW11 is
   --  ----------------------------------------------------------------------
   --  This is an I/O device that simulates a KW11 line time clock.  It can provide
   --  interrupts at either 60Hz or 50Hz.
   --  Two byte addresses are used:
   --  base + 0
   --    7 - Monitor
   --    6 - Interrupt enable
   --  5-0 - Unused
   --  base + 1 (unused)
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out kw11; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
   begin
      case size is
         when bits8 =>
            case offset is
            when 0 =>
               self.monitor := (data and 128) /= 0;
               self.enable  := (data and 64) /= 0;
            when 1 =>
               null;
            when others =>  --  Should never happen due to other checks
               status := BUS_NONE;
            end case;
         when bits16 =>
--            Ada.Text_IO.Put_Line("KW11: 16bit write " & toHex(data) & " to offset " & toHex(offset));
            if offset = 0 then
               self.monitor := (data and 128) /= 0;
               self.enable  := (data and 64) /= 0;
            else
               status := BUS_NONE;
            end if;
         when others =>
            status := BUS_NONE;
      end case;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out kw11; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
   begin
      case size is
         when bits8 =>
            case offset is
            when 0 =>
               return (if self.monitor then 128 else 0) +
                 (if self.enable then 64 else 0);
            when 1 =>
               null;
            when others =>  --  Should never happen due to other checks
               status := BUS_NONE;
            end case;
         when bits16 =>
--            Ada.Text_IO.Put_Line("KW11: 16bit read from offset " & toHex(offset));
            if offset = 0 then
               return (if self.monitor then 128 else 0) +
                 (if self.enable then 64 else 0);
            else
               status := BUS_NONE;
            end if;
         when others =>
            status := BUS_NONE;
      end case;
      return 0;
   end;
   --
   --  This must be done before using the device.
   --
   procedure init(self : in out kw11; ptr : kw11_access; r : rate) is
   begin
      self.T.start(ptr, self.host);
      if r = Hz60 then
         self.interval := 1.0/60.0;
      else
         self.interval := 1.0/50.0;
      end if;
   end;
   --
   --  Set the number of ticks per second as the base interval rate.
   --  (default value is 10 ticks per second).  This will apply to all
   --  clock objects.
   --
   procedure setBaseRate(self : in out kw11; r : rate) is
   begin
      if r = Hz60 then
         self.interval := 1.0/60.0;
      else
         self.interval := 1.0/50.0;
      end if;
   end;
   --
   --  Halt the tasks.
   --
   overriding
   procedure shutdown(self : in out kw11) is
   begin
      abort self.T;
   end;
   --
   --  Set which exception to use
   --
   procedure setException(self : in out kw11; except : long) is
   begin
      self.int_code := except;
   end;
   --
   task body kw11_server is
      data : kw11_access;
      host : BBS.Sim_CPU.CPU.sim_access;
      exit_flag : Boolean := False;
   begin
      accept start(self : kw11_access; owner : BBS.Sim_CPU.CPU.sim_access) do
      begin
         data := self;
         host := owner;
      end;
      end start;
      loop
         select
            accept end_task do
               exit_flag := True;
            end end_task;
         or
            delay 0.0;
         end select;
         exit when exit_flag;
         delay data.interval;
         data.monitor := True;
         if data.enable then
            host.interrupt(data.int_code);
         end if;
      end loop;
   end kw11_server;
end;
