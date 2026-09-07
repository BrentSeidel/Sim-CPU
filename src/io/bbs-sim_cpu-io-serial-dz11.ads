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
with Ada.Characters.Latin_1;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;
use type Ada.Containers.Count_Type;
with BBS.Sim_CPU.CPU;
with BBS.Sim_CPU.io;
with GNAT.Sockets;
package BBS.Sim_CPU.io.serial.DZ11 is
   --
   --  The device object for a network based TTY simulating a DZ11 8 line multiplexer.
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
   function name(self : in out DZ11) return string is ("DZ");
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
   --  Reset the controller
   --
   overriding
   procedure reset(self : in out DZ11);
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
   debug : constant Boolean := False;  --  Enable/Disable debugging message for this device specifically
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
   --  Record definitions for device registers
   --
   type tCSR is record  --  Control and Status Register
      unused1 : uint3;
      maint   : Boolean;  --  Set maintenance mode
      clr     : Boolean;  --  Clear controller command
      mse     : Boolean;  --  Master Scan Enable
      rx_ie   : Boolean;  --  Receiver interrupt enable
      rdone   : Boolean;  --  Silo contains a character (read only)
      tline   : uint3;    --  Transmit line (read only)
      unused2 : Boolean;
      sae     : Boolean;  --  Silo alarm enable
      sa      : Boolean;  --  Silo alarm (read only)
      tie     : Boolean;  --  Transmitter interrupt enable
      trdy    : Boolean;  --  Transmitter ready (read only)
   end record;
   for tCSR use record
      unused1 at 0 range  0 ..  2;
      maint   at 0 range  3 ..  3;
      clr     at 0 range  4 ..  4;
      mse     at 0 range  5 ..  5;
      rx_ie   at 0 range  6 ..  6;
      rdone   at 0 range  7 ..  7;
      tline   at 0 range  8 .. 10;
      unused2 at 0 range 11 .. 11;
      sae     at 0 range 12 .. 12;
      sa      at 0 range 13 .. 13;
      tie     at 0 range 14 .. 14;
      trdy    at 0 range 15 .. 15;
   end record;
   for tCSR'Size use 16;
   --
   type tRBUF is record  --  Receive buffer (read only, same address as LPR)
      rbuf    : byte;     --  Receive buffer
      rxline  : uint3;    --  Receive line
      unused1 : Boolean;
      perror  : Boolean;  --  Parity error
      ferror  : Boolean;  --  Frame error
      overrun : Boolean;  --  Buffer Overrun error
      valid   : Boolean;  --  Data valid
   end record;
   for tRBUF use record
      rbuf    at 0 range  0 ..  7;
      rxline  at 0 range  8 .. 10;
      unused1 at 0 range 11 .. 11;
      perror  at 0 range 12 .. 12;
      ferror  at 0 range 13 .. 13;
      overrun at 0 range 14 .. 14;
      valid   at 0 range 15 .. 15;
   end record;
   for tRBUF'Size use 16;
   --
   type tLPR is record  --  Line parameters (write only, same address as RBUF)
      lnum   : uint3;    --  Line number
      clen   : uint2;    --  Character length
      stop   : Boolean;  --  Stop code
      parity : Boolean;  --  Enable/disable parity
      oddpar : Boolean;  --  Select odd/even parity
      baud   : uint4;    --  Baud rate for selected line
      rcvron : Boolean;  --  Receiver on (selected)
      unused : uint3;
   end record;
   for tLPR use record
      lnum   at 0 range  0 ..  2;
      clen   at 0 range  3 ..  4;
      stop   at 0 range  5 ..  5;
      parity at 0 range  6 ..  6;
      oddpar at 0 range  7 ..  7;
      baud   at 0 range  8 .. 11;
      rcvron at 0 range 12 .. 12;
      unused at 0 range 13 .. 15;
   end record;
   for tLPR'Size use 16;
   --
   type tTCR is record  --  Transmit Control Register (read/write)
      line0_en : Boolean;  --  Line 0 enable
      line1_en : Boolean;  --  Line 1 enable
      line2_en : Boolean;  --  Line 2 enable
      line3_en : Boolean;  --  Line 3 enable
      line4_en : Boolean;  --  Line 4 enable
      line5_en : Boolean;  --  Line 5 enable
      line6_en : Boolean;  --  Line 6 enable
      line7_en : Boolean;  --  Line 7 enable
      dtr0     : Boolean;  --  Data Terminal Ready 0
      dtr1     : Boolean;  --  Data Terminal Ready 1
      dtr2     : Boolean;  --  Data Terminal Ready 2
      dtr3     : Boolean;  --  Data Terminal Ready 3
      dtr4     : Boolean;  --  Data Terminal Ready 4
      dtr5     : Boolean;  --  Data Terminal Ready 5
      dtr6     : Boolean;  --  Data Terminal Ready 6
      dtr7     : Boolean;  --  Data Terminal Ready 7
   end record;
   for tTCR use record
      line0_en at 0 range  0 ..  0;
      line1_en at 0 range  1 ..  1;
      line2_en at 0 range  2 ..  2;
      line3_en at 0 range  3 ..  3;
      line4_en at 0 range  4 ..  4;
      line5_en at 0 range  5 ..  5;
      line6_en at 0 range  6 ..  6;
      line7_en at 0 range  7 ..  7;
      dtr0     at 0 range  8 ..  8;
      dtr1     at 0 range  9 ..  9;
      dtr2     at 0 range 10 .. 10;
      dtr3     at 0 range 11 .. 11;
      dtr4     at 0 range 12 .. 12;
      dtr5     at 0 range 13 .. 13;
      dtr6     at 0 range 14 .. 14;
      dtr7     at 0 range 15 .. 15;
   end record;
   for tTCR'Size use 16;
   --
   type tMSR is record  --  Modem Status Register (read only, same address as TDR)
      ri0 : Boolean;  --  Ring Indicator 0
      ri1 : Boolean;  --  Ring Indicator 1
      ri2 : Boolean;  --  Ring Indicator 2
      ri3 : Boolean;  --  Ring Indicator 3
      ri4 : Boolean;  --  Ring Indicator 4
      ri5 : Boolean;  --  Ring Indicator 5
      ri6 : Boolean;  --  Ring Indicator 6
      ri7 : Boolean;  --  Ring Indicator 7
      co0 : Boolean;  --  Carrier 0
      co1 : Boolean;  --  Carrier 1
      co2 : Boolean;  --  Carrier 2
      co3 : Boolean;  --  Carrier 3
      co4 : Boolean;  --  Carrier 4
      co5 : Boolean;  --  Carrier 5
      co6 : Boolean;  --  Carrier 6
      co7 : Boolean;  --  Carrier 7
   end record;
   for tMSR use record
      ri0 at 0 range  0 ..  0;
      ri1 at 0 range  1 ..  1;
      ri2 at 0 range  2 ..  2;
      ri3 at 0 range  3 ..  3;
      ri4 at 0 range  4 ..  4;
      ri5 at 0 range  5 ..  5;
      ri6 at 0 range  6 ..  6;
      ri7 at 0 range  7 ..  7;
      co0 at 0 range  8 ..  8;
      co1 at 0 range  9 ..  9;
      co2 at 0 range 10 .. 10;
      co3 at 0 range 11 .. 11;
      co4 at 0 range 12 .. 12;
      co5 at 0 range 13 .. 13;
      co6 at 0 range 14 .. 14;
      co7 at 0 range 15 .. 15;
   end record;
   for tMSR'Size use 16;
   --
   type tTDR is record  --  Transmit Data Register (write only, same address as MSR)
      tbuf : byte;     --  Transmit buffer
      brk0 : Boolean;  --  Line 0 break
      brk1 : Boolean;  --  Line 1 break
      brk2 : Boolean;  --  Line 2 break
      brk3 : Boolean;  --  Line 3 break
      brk4 : Boolean;  --  Line 4 break
      brk5 : Boolean;  --  Line 5 break
      brk6 : Boolean;  --  Line 6 break
      brk7 : Boolean;  --  Line 7 break
   end record;
   for tTDR use record
      tbuf at 0 range  0 ..  7;
      brk0 at 0 range  8 ..  8;
      brk1 at 0 range  9 ..  9;
      brk2 at 0 range 10 .. 10;
      brk3 at 0 range 11 .. 11;
      brk4 at 0 range 12 .. 12;
      brk5 at 0 range 13 .. 13;
      brk6 at 0 range 14 .. 14;
      brk7 at 0 range 15 .. 15;
   end record;
   for tTDR'Size use 16;
   --
   --  Queue for receiver silo.  The DZ11 hardware has a capacity of 64 entries.
   --
   package silo_interface is new Ada.Containers.Synchronized_Queue_Interfaces(Element_Type => tRBUF);
   package rx_silo is new Ada.Containers.Bounded_Synchronized_Queues(Queue_Interfaces => silo_interface,
                                                                        Default_Capacity => 64);
   --
   --  The definition of a single channel of the interface
   --
   type DZ11_channel is limited record
      connected : Boolean := False;
      disconnecting : Boolean := False;
      T         : BBS.sim_cpu.io.serial.DZ11.DZ11_server;
      LPR       : tLPR;  --  Line parameter register for this channel
   end record;
   type channels is array (0 .. 7) of DZ11_channel;
   --
   --  The definition of the 8 channel interface
   --
   type DZ11 is new io_device with record
      chan   : channels;
      vector : long;           --  Exception vector
      CSR    : tCSR;           --  Control and status register
      RBUF   : tRBUF;          --  Receive buffer (read only, same address as LPR)
      silo   : rx_silo.Queue;  --  Receive character silo
      TCR    : tTCR;           --  Transmit control register
      TDR    : tTDR;           --  Transmit data register
   end record;
   --
   --  Task for telnet receiver
   --
   task type DZ11_rx is
      entry start(self : DZ11_access; index : Integer; sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.CPU.sim_access);
      entry end_task;
   end DZ11_rx;
   --
   --  Check if a transmit interrupt should be sent.
   --
   procedure check_tx(self : in out DZ11);
   --
   --  Process clear command in CSR
   --
   procedure clear(self : in out DZ11);
   --
end;
