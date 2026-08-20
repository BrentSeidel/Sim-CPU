--
--  Author: Brent Seidel
--  Date: 20-Aug-2026
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
with BBS.Sim_CPU.CPU;
with BBS.Sim_CPU.io;
package BBS.Sim_CPU.io.serial.DZ11 is
   --
   --  The device object for a network based TTY.
   --
   type DZ11 is new io_device with private;
   type DZ11_access is access all DZ11;
   --
   --  The channel object
   --
   type DZ11_channel is limited private;
   type channel_access is access all DZ11_channel;
   --
   --  Task type for telnet type server.
   --
   --  Note that the "Start" entry should only be called once.  Other
   --  calls are ignored.
   --
   task type DZ11_server is
     entry start(self : DZ11_access; index : integer; port : GNAT.Sockets.Port_Type; owner : BBS.Sim_CPU.CPU.sim_access);
     entry write(char : Character);
     entry end_task;
   end DZ11_server;
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out DZ11; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out DZ11; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out DZ11) return addr_bus is (8);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out DZ11) return string is ("DZ11");
   overriding
   function description(self : in out DZ11) return string is ("8 Channel Terminal Multiplexer");
   overriding
   function dev_class(self : in out DZ11) return dev_type is (TT);
   --
   --  Set device port and do the network initialiation.  This must be
   --  done before using the device.
   --
   procedure init(self : in out DZ11; ptr : DZ11_access; port : GNAT.Sockets.Port_Type);
   --
   --  Close the network connection and halt the tasks.
   --
   overriding
   procedure shutdown(self : in out DZ11);
   --
   --  Set which exception to use
   --
   overriding
   procedure setException(self : in out DZ11; except : long);
   --
private
   CRLF : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
   --
   --  Constants for debugging
   --
   debug : constant Boolean := False;             --  Enable/Disable debugging message for this device specifically
   --
   --  Device register offsets
   --
   CSRlsb  : constant byte := 0;  --  Control and status register
   CSRmsb  : constant byte := 1;  --  Control and status register
   RBUFlsb : constant byte := 2;  --  Receive buffer (read only)
   RBUFmsb : constant byte := 3;  --  Receive buffer (read only)
   LPRlsb  : constant byte := 2;  --  Line parameter (write only)
   LPRmsb  : constant byte := 3;  --  Line parameter (write only)
   TCRlsb  : constant byte := 4;  --  Transmit control
   TCRmsb  : constant byte := 5;  --  Transmit control
   MSRlsb  : constant byte := 6;  --  Modem status (read only)
   MSRmsb  : constant byte := 7;  --  Modem status (read only)
   TDRlsb  : constant byte := 6;  --  Transmit data (write only)
   TDRmsb  : constant byte := 7;  --  Transmit data (write only)
   --
   --  The definition of a single channel of the interface
   --
   type DZ11_channel is limited record
      ready     : Boolean := False;
      connected : Boolean := False;
      disconnecting : Boolean := False;
      char      : Character := Character'Val(0);
      T         : BBS.sim_cpu.io.serial.DZ11.DZ11_server;
   end record;
   type channels is array (0 .. 7) of DZ11_channel;
   --
   --  The definition of the 8 channel interface
   --
   type DZ11 is new io_device with record
      int_e     : Boolean := False;  --  Interrupt enable
      int_code  : long;
      chan      : channels;
   end record;
   --
   --  Task for telnet receiver
   --
   task type DZ11_rx is
      entry start(self : DZ11_access; index : Integer; sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.CPU.sim_access);
      entry end_task;
   end DZ11_rx;

end;
