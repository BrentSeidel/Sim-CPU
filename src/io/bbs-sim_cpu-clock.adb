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
with Ada.Text_IO;
package body BBS.Sim_CPU.clock is
   --
   --  I/O device actions
   --
   --  Port offset + 0 is the control port.
   --  Port offset + 1 is the delay port.
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out clock_device; addr : addr_bus; data : data_bus) is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
   begin
      case offset is
         when 0 =>
            self.enable := (data and 1) = 1;
         when 1 =>
            self.interval := Duration(data)/base_ticks;
         when others =>  --  Should never happen due to other checks
            null;
      end case;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out clock_device; addr : addr_bus) return data_bus is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
   begin
      case offset is
         when 0 =>
            if self.enable then
               return 1;
            else
               return 0;
            end if;
         when 1 =>
            return (data_bus(self.interval*base_ticks) and 16#FF#);
         when others =>  --  Should never happen due to other checks
            null;
      end case;
      return 0;
   end;
   --
   --  This must be done before using the device.
   --
   procedure init(self : in out clock_device; ptr : clock_access) is
   begin
      self.T.start(ptr, self.host);
   end;
   --
   --  Set the number of ticks per second as the base interval rate.
   --  (default value is 10 ticks per second).  This will apply to all
   --  clock objects.
   --
   procedure setBaseRate(b : Duration) is
   begin
      base_ticks := b;
   end;
   --
   --  Halt the tasks.
   --
   procedure shutdown(self : in out clock_device) is
   begin
      abort self.T;
   end;
   --
   --  Set which exception to use
   --
   procedure setException(self : in out clock_device; except : long) is
   begin
      self.int_code := except;
   end;
   --
   task body clock_server is
      data : clock_access;
      host : BBS.Sim_CPU.sim_access;
      exit_flag : Boolean := False;
   begin
      accept start(self : clock_access; owner : BBS.Sim_CPU.sim_access) do
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
         if data.enable then
            host.interrupt(data.int_code);
         end if;
      end loop;
   end clock_server;
end;
