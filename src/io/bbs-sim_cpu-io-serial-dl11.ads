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
with BBS.Sim_CPU.CPU;
with BBS.Sim_CPU.io;
--  ----------------------------------------------------------------------
--  This is an I/O device for a simple 8-bit console interface via network.
--  The user can telnet to the specified port to access the device.  On the
--  host side, it simulates a DL11 single line serial interface.
--
--  Two addresses are used.
--  base + 0/1 - Reveiver status register
--    15 - Dataset Int (DL11-E only)
--    14 - Ring (DL11-E only)
--    13 - Clr to Send (DL11-E only)
--    12 - Car Det (DL11-E only(
--    11 - Rcvr Act (all)
--    10 - Sec Rec (DL11-E only)
--     9 - Unused (all)
--     8 - Unused (all)
--     7 - Rcvr Done (all)
--     6 - Rcvr Int Enb (all)
--     5 - Dataset Int Enb (DL11-E only)
--     4 - Unused (all)
--     3 - Sec Xmit (DL11-E only)
--     2 - Req to Send (DL11-E only)
--     1 - DTR (DL11-E only)
--     0 - Rdr Enb (DL11-A,C)
--  base + 2/3 - Receiver buffer register
--    15 - Error (DL11-C,D,E)
--    14 - OR Err (DL11-C,D,E)
--    13 - FR Err (DL11-C,D,E)
--    12 - P Err (DL11-C,D,E)
--  11-8 - Unused (all)
--   7-0 - Received Data Bits (all)
--  base + 4/5 - Transmitter status register
--  15-8 - Unused (all)
--     7 - Xmit Rdy (all)
--     6 - Xmit Int Enb (all)
--   5-3 - Unused (all)
--     2 - Maint (all)
--     1 - Unused (all)
--     0 - Break (DL11-C,D,E)
--  base + 6/7 - Transmitter buffer register
--  15-8 - Unused (all)
--   7-0 - Transmitter Data Buffer (all)
--
--  Writes to the data port complete immediately as far as the simulator is concerned
--  Reads from the data port return the buffered read character and clear the ready
--  flag.
--
package BBS.Sim_CPU.io.serial.dl11 is
   --
   --  The device object for a network based TTY.
   --
   type dl11x is new io_device with private;
   type dl11_access is access all dl11x;
   --
   --  Task type for telnet type server.
   --
   --  Note that the "Start" entry should only be called once.  Other
   --  calls are ignored.
   --
   task type dl11_server is
     entry start(self : dl11_access; port : GNAT.Sockets.Port_Type; owner : BBS.Sim_CPU.CPU.sim_access);
     entry write(char : Character);
     entry end_task;
   end dl11_server;
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out dl11x; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out dl11x; addr : addr_bus) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out dl11x) return addr_bus is (8);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out dl11x) return string is ("DL11");
   overriding
   function description(self : in out dl11x) return string is ("DL11 Telnet Port");
   overriding
   function dev_class(self : in out dl11x) return dev_type is (TT);
   --
   --  Set device port and do the network initialiation.  This must be
   --  done before using the device.
   --
   procedure init(self : in out dl11x; ptr : dl11_access; port : GNAT.Sockets.Port_Type);
   --
   --  Close the network connection and halt the tasks.
   --
   overriding
   procedure shutdown(self : in out dl11x);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out dl11x; except : long);
   --
private
   CRLF : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
   --
   --  The definition of the 8 bit console object via telnet
   --
   type dl11x is new io_device with record
      ready     : Boolean := False;  --  Data ready to read
      connected : Boolean := False;
      disconnecting : Boolean := False;
      int_e     : Boolean := False;  --  Interrupt enable
      int_code  : long;
      char      : Character := Character'Val(0);
      T         : BBS.sim_cpu.io.serial.dl11.dl11_server;
   end record;
   --
   --  Task for telnet receiver
   --
   task type dl11_rx is
      entry start(self : dl11_access; sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.CPU.sim_access);
      entry end_task;
   end dl11_rx;

end;
