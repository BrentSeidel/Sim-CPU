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
with GNAT.Sockets;
with Ada.Characters.Latin_1;
--  ----------------------------------------------------------------------
--  This is an I/O device for a simple 8-bit console interface via network.
--  The user can telnet to the specified port to access the device.
--
--  Two addresses are used.
--  base + 0 - Status (R/W)
--  base + 1 - Data (R/W)
--    Status bits:
--      0 - Ready (RO)
--      1 - Connected (RO)
--      2 - Enable interrupt (R/W)
--      3 - Reset (WO)
--
--  Writes to the data port complete immediately as far as the simulator is concerned
--  Reads from the data port return the buffered read character and clear the ready
--  flag.
--
with BBS.Sim_CPU.CPU;
with BBS.Sim_CPU.io;
package BBS.Sim_CPU.io.serial.telnet is
   --
   --  The device object for a network based TTY.
   --
   type tel_tty is new io_device with private;
   type telnet_access is access all tel_tty;
   --
   --  Task type for telnet type server.
   --
   --  Note that the "Start" entry should only be called once.  Other
   --  calls are ignored.
   --
   task type telnet_server is
     entry start(self : telnet_access; port : GNAT.Sockets.Port_Type; owner : BBS.Sim_CPU.CPU.sim_access);
     entry write(char : Character);
     entry end_task;
   end telnet_server;
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out tel_tty; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out tel_tty; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out tel_tty) return addr_bus is (2);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out tel_tty) return string is ("TEL");
   overriding
   function description(self : in out tel_tty) return string is ("8 Bit Telnet Port");
   overriding
   function dev_class(self : in out tel_tty) return dev_type is (TT);
   --
   --  Set device port and do the network initialiation.  This must be
   --  done before using the device.
   --
   procedure init(self : in out tel_tty; ptr : telnet_access; port : GNAT.Sockets.Port_Type);
   --
   --  Close the network connection and halt the tasks.
   --
   overriding
   procedure shutdown(self : in out tel_tty);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out tel_tty; except : long);
   --
private
   CRLF : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
   --
   --  The definition of the 8 bit console object via telnet
   --
   type tel_tty is new io_device with record
      ready     : Boolean := False;  --  Data ready to read
      connected : Boolean := False;
      disconnecting : Boolean := False;
      int_e     : Boolean := False;  --  Interrupt enable
      int_code  : long;
      char      : Character := Character'Val(0);
      T         : BBS.sim_cpu.io.serial.telnet.telnet_server;
   end record;
   --
   --  Task for telnet receiver
   --
   task type telnet_rx is
      entry start(self : telnet_access; sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.CPU.sim_access);
      entry end_task;
   end telnet_rx;

end;
