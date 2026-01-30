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
with GNAT.Sockets;
with Ada.Characters.Latin_1;
with BBS.Sim_CPU.CPU;
with BBS.Sim_CPU.io;
--  ----------------------------------------------------------------------
--  This is an I/O device for a simple 8-bit console interface via network.
--  The user can telnet to the specified port to access the device.  On the
--  host side, it simulates a DL11 single line serial interface.  Initially,
--  only DL11-A/B features are implemented.
--
--  Eight byte addresses are used.
--  base + 0/1 - Receiver status register
--    15 - Dataset Int (DL11-E only)
--    14 - Ring (DL11-E only)
--    13 - Clr to Send (DL11-E only)
--    12 - Car Det (DL11-E only)
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
--  Most of DL11-B, except for the Maint bit are implemented.  More functionality
--  may be added later, if needed.
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
   procedure write(self : in out dl11x; addr : addr_bus; data : data_bus; size : bus_size; status : out bus_stat);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out dl11x; addr : addr_bus; size : bus_size; status : out bus_stat) return data_bus;
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
   --  Set which exception to use.  The RX vector is the LSB of except.  The TX
   --  vector is the next MSB of except.
   --
   overriding
   procedure setException(self : in out dl11x; except : long);
   --
private
   CRLF : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
   --
   --  Constants for port offsets
   --
   off_rx_statl : constant addr_bus := 0;  --  LSB of receive status register
   off_rx_statm : constant addr_bus := 1;  --  MSB of receive status register
   off_rx_datal : constant addr_bus := 2;  --  LSB of reciver buffer register (character received)
   off_rx_datam : constant addr_bus := 3;  --  MSB of reciver buffer register
   off_tx_statl : constant addr_bus := 4;  --  LSB of transmitter status register
   off_tx_statm : constant addr_bus := 5;  --  MSB of transmitter status register (unused)
   off_tx_datal : constant addr_bus := 6;  --  LSB of transmitter buffer register
   off_tx_datam : constant addr_bus := 7;  --  MSB of transmitter buffer register (unused)
   --
   character_delay : constant Duration := 0.01;  --  Time between processing characters (about 1200 baud)
   --
   --  The definition of the 8 bit console object via telnet
   --
   type dl11x is new io_device with record
      ready     : Boolean := False;  --  Data ready to read
      connected : Boolean := False;
      disconnecting : Boolean := False;
      rx_en     : Boolean := False;  --  RX Interrupt enable
      tx_en     : Boolean := False;  --  TX Interrupt enable
      rx_vect   : byte;              --  Receiver interrupt vector
      tx_vect   : byte;              --  Transmitter interrupt vector
      rx_act    : Boolean := False;  --  Receiver active
      rx_done   : Boolean := False;  --  Receiver done (character ready)
      tx_rdy    : Boolean := True;   --  Transmitter ready
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
