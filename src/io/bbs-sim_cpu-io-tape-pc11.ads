--
--  Author: Brent Seidel
--  Date: 13-Apr-2026
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
--  This package contains a PC11 paper tape reader/punch.
--
with Ada.Text_IO;
with BBS.Sim_CPU.CPU;
with BBS.Sim_CPU.io;
package BBS.Sim_CPU.io.tape.pc11 is
   --
   --  ----------------------------------------------------------------------
   --  This is an I/O device for a PC11 paper tape reader/punch.  It is
   --  designed to work with the PDP-11 simulations.
   --
   --  Four word addresses are used.
   --  base + 0 - Reader Status Register (LSB)
   --  base + 1 - Reader Status Register (MSB)
   --  base + 2 - Reader Buffer Register (LSB)
   --  base + 3 - Reader Buffer Register (MSB) (unused)
   --  base + 4 - Punch Status Register (LSB)
   --  base + 5 - Punch Status Register (MSB)
   --  base + 6 - Punch Buffer Register (LSB)
   --  base + 7 - Punch Buffer Register (MSB) (unused)
   --
   --  Data read and write to the data port complete immediately as far as
   --  the simulator is concerned
   --
   --  The device object for a PC11.
   --
   type pc11 is new ptape with private;
   type pc11_access is access all pc11'Class;
   --
   package pc11_io is new Ada.Sequential_IO(byte);
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out pc11; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out pc11; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out pc11) return addr_bus is (8);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out pc11) return string is ("PC");
   overriding
   function description(self : in out pc11) return string is ("PC11 Paper Tape Reader/Punch");
   overriding
   function dev_class(self : in out pc11) return dev_type is (PT);
   --
   --  Open input file
   --
   overriding
   procedure openIn(self : in out pc11; name : String);
   --
   --  Set which exception to use.  The RX vector is the LSB of except.  The TX
   --  vector is the next MSB of except.
   --
   overriding
   procedure setException(self : in out pc11; except : long);
   --
   --  Reset/Initialize device
   --
   overriding
   procedure reset(self : in out pc11);
   --
private
   --
   function read_tape(self : in out pc11) return data_bus;
   --
   --  Register addresses
   --
   PRSlsb : constant byte := 0;
   PRSmsb : constant byte := 1;
   PBRlsb : constant byte := 2;
   PRBmsb : constant byte := 3;  --  Unused
   PPSlsb : constant byte := 4;
   PPSmsb : constant byte := 5;
   PPBlsb : constant byte := 6;
   PPBmsb : constant byte := 7;  --  Unused
   --
   --  Ctrl-Z character
   --
   ctrl_z : constant data_bus := 26;
   --
   --  The definition of the 8 bit paper tape object
   --
   type pc11 is new ptape with record
      rx_en      : Boolean := False;  --  RX Interrupt enable
      tx_en      : Boolean := False;  --  TX Interrupt enable
      rx_vect    : long;              --  Receiver interrupt vector
      tx_vect    : long;              --  Transmitter interrupt vector
      rx_data    : data_bus;
      rx_eof     : Boolean := False;  --  End of File on reader
   end record;
end;
