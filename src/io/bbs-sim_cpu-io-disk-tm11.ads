--
--  Author: Brent Seidel
--  Date: 8-Jun-2026
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
--  This package contains a TM11 magnetic tape controller.
--
with Ada.Direct_IO;
with Ada.Text_IO;
with BBS.Sim_CPU.CPU;
with BBS.Sim_CPU.io;
package BBS.Sim_CPU.io.disk.tm11 is
   --
   --  ----------------------------------------------------------------------
   --  This is an I/O device for a TM11 magnetic tape controller.  It is
   --  designed to work with the PDP-11 simulations.
   --
   --  The device object for a TM11.
   --
   --  Note that while this is a tape drive, it has more in common with disks than
   --  it does with paper tapes.
   --
   --  For compatibility with simh produced tapes, the following from simh pdp11_tm.c
   --  is followed:
   --   Magnetic tapes are represented as a series of variable 8b records
   --   of the form:
   --
   --     32b record length in bytes - exact number
   --     byte 0
   --     byte 1
   --     :
   --     byte n-2
   --     byte n-1
   --     32b record length in bytes - exact number
   --
   --  If the byte count is odd, the record is padded with an extra byte
   --  of junk.  File marks are represented by a single record length of 0.
   --  End of tape is two consecutive end of file marks.
   --
   type tm11 is new disk_ctrl with private;
   type tm11_access is access all tm11'Class;
   --
   --
   --  I/O device actions
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out tm11; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out tm11; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out tm11) return addr_bus is (12);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out tm11) return string is ("MT");
   overriding
   function description(self : in out tm11) return string is ("TM11 Magtape Controller");
   overriding
   function dev_class(self : in out tm11) return dev_type is (MT);
   --
   --  Open the attached file
   --
   procedure open(self : in out tm11; drive : byte; geom : geometry; name : String);
   --
   --  Set which exception to use.
   --
   overriding
   procedure setException(self : in out tm11; except : long);
   --
   --  Reset/Initialize device
   --
   overriding
   procedure reset(self : in out tm11);
   --
   --
   --  Get the name of the attached file, if any.
   --
   function fname(self : in out tm11; drive : byte) return String;
   --
   --  Is a file attached to the specified drive?
   --
   function present(self : in out tm11; drive : byte) return Boolean;
   --
   --  Is the specified drive read-only?
   --
   function readonly(self : in out tm11; drive : byte) return Boolean;
   --
   --  Set the specified drive's read-only state?
   --
   procedure readonly(self : in out tm11; drive : byte; state : Boolean);
   --
   --  Close the attached file
   --
   procedure close(self : in out tm11; drive : byte);
   --
   --  Return maximum drive number
   --
   function max_drive(self : in out tm11) return byte is (7);
private
   --
   --  For debugging
   --
   debug : constant Boolean := False;
   --
   --  Register addresses
   --
   MTSlsb   : constant byte :=  0;  --  Status Register
   MTSmsb   : constant byte :=  1;
   MTClsb   : constant byte :=  2;  --  Command Register
   MTCmsb   : constant byte :=  3;
   MTBCRlsb : constant byte :=  4;  --  Byte Record Counter
   MTBCRmsb : constant byte :=  5;
   MTCMAlsb : constant byte :=  6;  --  Current Memory Address Register
   MTCMAmsb : constant byte :=  7;
   MTDlsb   : constant byte :=  8;  --  Data Buffer Register
   MTDmsb   : constant byte :=  9;
   MTRDlsb  : constant byte := 10;  --  TU10 Read Lines
   MTRDmsb  : constant byte := 11;
   -- -------------------------------------------------------------------------
   --
   --  Locally needed types
   --
   type uint3 is mod 2**3
     with size => 3;
   package tape_io is new Ada.Direct_IO(byte);
   use type tape_io.Positive_Count;
   -- -------------------------------------------------------------------------
   --
   --  TM11 register definitions
   --
   type tMTS is record      --  Magtape Status Register
      ready     : Boolean;  --  Tape Unit Ready
      rewind    : Boolean;  --  Rewind status
      wrt_lock  : Boolean;  --  Write lock
      settle    : Boolean;  --  Settle Down
      ch7       : Boolean;  --  7 Channel
      BOT       : Boolean;  --  Beginning of Tape
      sel_rem   : Boolean;  --  Select Remote
      NXM       : Boolean;  --  Nonexistant memory
      BTE       : Boolean;  --  Bad Tape Error
      RLE       : Boolean;  --  Record Length Error
      EOT       : Boolean;  --  End of Tape
      BGL       : Boolean;  --  Bus Grant Late
      parity    : Boolean;  --  Parity Error
      CRC_err   : Boolean;  --  CRC Error
      EOF       : Boolean;  --  End of file
      cmd_err   : Boolean;  --  Illegal command
   end record;
   for tMTS use record
      ready     at 0 range  0 ..  0;
      rewind    at 0 range  1 ..  1;
      wrt_lock  at 0 range  2 ..  2;
      settle    at 0 range  3 ..  3;
      ch7       at 0 range  4 ..  4;
      BOT       at 0 range  5 ..  5;
      sel_rem   at 0 range  6 ..  6;
      NXM       at 0 range  7 ..  7;
      BTE       at 0 range  8 ..  8;
      RLE       at 0 range  9 ..  9;
      EOT       at 0 range 10 .. 10;
      BGL       at 0 range 11 .. 11;
      parity    at 0 range 12 .. 12;
      CRC_err   at 0 range 13 .. 13;
      EOF       at 0 range 14 .. 14;
      cmd_err   at 0 range 15 .. 15;
   end record;
   for tMTS'size use 16;
   --
   --  Functions are:
   --  0 - Off-Line
   --  1 - Read
   --  2 - Write
   --  3 - Write End of File
   --  4 - Space Forward
   --  5 - Space Reverse
   --  6 - Write With Extended IRG
   --  7 - Rewind
   --
   type tMTC is record      --  Magtape Status Register
      go        : Boolean;  --  Execute Function
      funct     : uint3;    --  Function
      Addr16    : Boolean;  --  Address bit 16
      Addr17    : Boolean;  --  Address bit 17
      int_enb   : Boolean;  --  Interrupt Enable
      CU_RDY    : Boolean;  --  Controller ready
      SEL       : uint3;    --  Select drive
      PEVN      : Boolean;  --  Parity Even
      pwr_clr   : Boolean;  --  Clear all units
      DEN5      : Boolean;  --  With DEN8 tape density (200, 556, 800 7 channel)
      DEN8      : Boolean;  --  or 800 9 channel
      err       : Boolean;  --  Error has occured
   end record;
   for tMTC use record
      go        at 0 range  0 ..  0;
      funct     at 0 range  1 ..  3;
      Addr16    at 0 range  4 ..  4;
      Addr17    at 0 range  5 ..  5;
      int_enb   at 0 range  6 ..  6;
      CU_RDY    at 0 range  7 ..  7;
      SEL       at 0 range  8 .. 10;
      PEVN      at 0 range 11 .. 11;
      pwr_clr   at 0 range 12 .. 12;
      DEN5      at 0 range 13 .. 13;
      DEN8      at 0 range 14 .. 14;
      err       at 0 range 15 .. 15;
   end record;
   for tMTC'size use 16;
   --
   --  The definition of the TU10 tape drive object.
   --
   type tape_info is record
      present   : Boolean := False;  --  File has been attached to drive
      writeable : Boolean := False;  --  Hardware write protect
      sw_prot   : Boolean := False;  --  Software write protect
      position  : tape_io.Positive_Count;  --  Position in image file
      rec_size  : uint32;            --  Current record size
      rec_pos   : uint32;            --  Position in record
      image     : tape_io.File_Type;
   end record;
   type info_array is array (byte range <>) of tape_info;
   --
   --  The definition of the TM11 tape controller object
   --
   type tm11 is new disk_ctrl with record
      drive_info : info_array(0 .. 7);
      vector     : long;              --  Interrupt vector
      MTS        : tMTS;              --  Magtape Status Register
      MTC        : tMTC;              --  Magtape Command Register
      MTBCR      : word;              --  Byte Count Register
      MTCMA      : word;              --  Current Memory Address Register
   end record;
   --
   --  Process the command specified in RKCS
   --
   procedure process_command(self : in out tm11);
   --
   --  Read from the selected drive
   --
   procedure read(self : in out tm11);
   --
   --  Write to the selected drive
   --
   procedure write(self : in out tm11);
   --
   --  Write end of file to the selected drive
   --
   procedure write_EOF(self : in out tm11);
   --
   --  Rewind the selected drive
   --
   procedure rewind(self : in out tm11);
   --
   --  Space forward or reverse by records
   --
   procedure space_fore(self : in out tm11);
   procedure space_back(self : in out tm11);
   --
   --  Create a file, if it doesn't alread exist.
   --
   procedure extend(self : in out tm11; drive : byte; name : String);
   --
   --  Read record size
   --
   mark_size : constant := 4;  --  The record size entry is four bytes.
   function record_size(self : in out tm11; drive : in out tape_info) return uint32;
   --
   --  Write record size
   --
   procedure record_size(drive : in out tape_info; size : uint32);
end;
