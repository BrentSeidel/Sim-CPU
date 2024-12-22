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
with GNAT.Sockets;
with Ada.Characters.Latin_1;
--  ----------------------------------------------------------------------
--  This is an I/O device for an 8 line telnet multiplexer.  The simulation
--  sees one device that controls 8 serial lines.
--
--  Two addresses are used.
--  base + 0 - Ready (RO)
--  base + 1 - Status
--  base + 2 - Channel 0 Data (R/W)
--  base + 3 - Channel 1 Data (R/W)
--  base + 4 - Channel 2 Data (R/W)
--  base + 5 - Channel 3 Data (R/W)
--  base + 6 - Channel 4 Data (R/W)
--  base + 7 - Channel 5 Data (R/W)
--  base + 8 - Channel 6 Data (R/W)
--  base + 9 - Channel 7 Data (R/W)
--    Status bits:
--      0 - Enable interrupt (R/W)
--      1 - Reset (WO)
--  There is one bit in the Ready register for each channel.  Bit 0 is
--  for channel 0, and so on.
--
--  Writes to the data port complete immediately as far as the simulator is concerned
--  Reads from the data port return the buffered read character and clear the ready
--  flag.
--
package BBS.Sim_CPU.serial.mux is
   --
   --  The device object for a network based TTY.
   --
   type mux_tty is new io_device with private;
   type mux_access is access all mux_tty;
   --
   --  The channel object
   --
   type mux_channel is limited private;
   type channel_access is access all mux_channel;
   --
   --  Task type for telnet type server.
   --
   --  Note that the "Start" entry should only be called once.  Other
   --  calls are ignored.
   --
   task type mux_server is
     entry start(self : mux_access; index : integer; port : GNAT.Sockets.Port_Type; owner : BBS.Sim_CPU.sim_access);
     entry write(char : Character);
     entry end_task;
   end mux_server;
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out mux_tty; addr : addr_bus; data : data_bus);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out mux_tty; addr : addr_bus) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out mux_tty) return addr_bus is (10);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out mux_tty) return string is ("MUX");
   overriding
   function description(self : in out mux_tty) return string is ("8 Channel Telnet Port");
   overriding
   function dev_class(self : in out mux_tty) return dev_type is (TT);
   --
   --  Set device port and do the network initialiation.  This must be
   --  done before using the device.
   --
   procedure init(self : in out mux_tty; ptr : mux_access; port : GNAT.Sockets.Port_Type);
   --
   --  Close the network connection and halt the tasks.
   --
   procedure shutdown(self : in out mux_tty);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out mux_tty; except : long);
   --
private
   CRLF : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
   --
   --  The definition of a single channel of the interface
   --
   type mux_channel is limited record
      ready     : Boolean := False;
      connected : Boolean := False;
      disconnecting : Boolean := False;
      char      : Character := Character'Val(0);
      T         : BBS.sim_cpu.serial.mux.mux_server;
   end record;
   type channels is array (0 .. 7) of mux_channel;
   --
   --  The definition of the 8 channel interface
   --
   type mux_tty is new io_device with record
      int_e     : Boolean := False;  --  Interrupt enable
      int_code  : long;
      chan      : channels;
   end record;
   --
   --  Task for telnet receiver
   --
   task type mux_rx is
      entry start(self : mux_access; index : Integer; sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.sim_access);
      entry end_task;
   end mux_rx;

end;
