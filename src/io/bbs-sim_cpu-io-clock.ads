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
with Ada.Calendar;
with BBS.Sim_CPU.CPU;
package BBS.Sim_CPU.io.clock is
   --
   --  This contains a clock device that sends a periodic interrupt.  There
   --  are two ports for the device.  The first is control/status and the
   --  second is for the interval in 1/10 of a second.
   --
   type clock_device is new io_device with private;
   type clock_access is access all clock_device;
   --
   --  Once started, the clock server task periodically sends an interrupt
   --  to the simulator.
   --
   task type clock_server is
      entry start(self : clock_access; owner : BBS.Sim_CPU.CPU.sim_access);
      entry end_task;
   end clock_server;
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out clock_device; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out clock_device; addr : addr_bus) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out clock_device) return addr_bus is (2);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out clock_device) return string is ("CLK");
   overriding
   function description(self : in out clock_device) return string is ("Periodic Interrupt Generator");
   overriding
   function dev_class(self : in out clock_device) return dev_type is (CL);
   --
   --  This must be done before using the device.
   --
   procedure init(self : in out clock_device; ptr : clock_access);
   --
   --  Halt the tasks.
   --
   overriding
   procedure shutdown(self : in out clock_device);
   --
   --  Set which exception to use
   --
   procedure setException(self : in out clock_device; except : long);
   --
   --  Set the number of ticks per second as the base interval rate.
   --  (default value is 10 ticks per second).  This will apply to all
   --  clock objects.  This should be called, if needed, before the
   --  simulated software tries to initialize the clock.
   --
   procedure setBaseRate(b : Duration);
   --
private
   --
   --  The base number of ticks per second.
   --
   base_ticks : Duration := 10.0;

   type clock_device is new io_device with record
      int_code : long;
      enable   : Boolean := False;
      interval : Duration;
      T        : clock_server;
   end record;

end;
