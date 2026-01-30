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
with Ada.Calendar;
with BBS.Sim_CPU.CPU;
with BBS.Sim_CPU.io;
package BBS.Sim_CPU.io.clock.KW11 is
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
   type kw11 is new BBS.Sim_CPU.io.io_device with private;
   type kw11_access is access all kw11;
   type rate is (Hz60, Hz50);
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out kw11; addr : addr_bus; data : data_bus; size : bus_size; status : out bus_stat);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out kw11; addr : addr_bus; size : bus_size; status : out bus_stat) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out kw11) return addr_bus is (2);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out kw11) return string is ("KW11");
   overriding
   function description(self : in out kw11) return string is ("KW11 Line Time Clock");
   overriding
   function dev_class(self : in out kw11) return dev_type is (CL);
   --
   --  This must be done before using the device.
   --
   procedure init(self : in out kw11; ptr : kw11_access; r : rate);
   --
   --  Halt the tasks.
   --
   overriding
   procedure shutdown(self : in out kw11);
   --
   --  Set which exception to use
   --
   procedure setException(self : in out kw11; except : long);
   --
   --  Set the interrupt rate to either 60Hz or 50Hz.
   --
   procedure setBaseRate(self : in out kw11; r : rate);
   --
private
   --
   --  Once started, the clock server task periodically sends an interrupt
   --  to the simulator.
   --
   task type kw11_server is
      entry start(self : kw11_access; owner : BBS.Sim_CPU.CPU.sim_access);
      entry end_task;
   end kw11_server;
   --
   type kw11 is new BBS.Sim_CPU.io.io_device with record
      int_code : long;
      enable   : Boolean := False;
      monitor  : Boolean := False;
      interval : Duration;
      T        : kw11_server;
   end record;

end;
