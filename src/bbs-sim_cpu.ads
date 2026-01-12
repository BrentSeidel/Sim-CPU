--
--  Author: Brent Seidel
--  Date: 8-Jun-2025
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
--  This package mainly contains types and definitions are are common to all parts
--  of a system simulator.
--
limited with BBS.Sim_CPU.io;
package BBS.Sim_CPU is
   --
   --  Basic types for address and data bus.  Currently these support up to 32
   --  bits of address and data.  These will need to change if larger processors
   --  are to be supported.
   --
   subtype data_bus is uint32;  --  Data bus
   subtype addr_bus is uint32;  --  Address bus
   subtype ad_bus   is uint32;  --  Greater of address and data bus
   subtype byte     is uint8;
   subtype word     is uint16;
   subtype long     is uint32;
   subtype long_long is uint64;
   --
   --  Memory page for reading various hex formats
   --
   type page is array (0 .. 255) of byte;
   --
   --  Processor modes
   --
   type proc_mode is (PROC_NONE, PROC_KERN, PROC_EXEC, PROC_SUP, PROC_USER);
   --
   for proc_mode use (PROC_NONE => 0,
                      PROC_KERN => 16#01#,
                      PROC_EXEC => 16#02#,
                      PROC_SUP  => 16#04#,
                      PROC_USER => 16#08#);
   for proc_mode'Size use 4;
   --
   --  Address types
   --
   type addr_type is (ADDR_NONE, ADDR_INTR, ADDR_DATA, ADDR_INST, ADDR_IO);
   --
   for addr_type use (ADDR_NONE => 0,
                      ADDR_INTR => 16#01#,
                      ADDR_DATA => 16#02#,
                      ADDR_INST => 16#04#,
                      ADDR_IO   => 16#08#);
   for addr_type'Size use 4;
   --
   --  For processors with multiple bus types.  If only a single bus type is used,
   --  it should be BUS_MEMORY.
   --
   type bus_type is (BUS_MEMORY, BUS_IO);
   --
   --  Possible bus results
   --
   type bus_stat is (BUS_SUCC,    --  Successful result
                     BUS_NONE,    --  Nothing at this address
                     BUS_ALIGN,   --  Illegal allignment, such as odd address
                     BUS_PROT,    --  Protection violation
                     BUS_PARITY,  --  Parity error
                     BUS_ECC1,    --  Correctable ECC error
                     BUS_ECC2);   --  Uncorrectable ECC error
   --
   --  Control and mode switches and LEDs
   --
   type ctrl_mode is record
      unused0 : Boolean;    --  LED/Switch 0 is hardwired to power
      ready   : Boolean;    --  LED only
      exam    : Boolean;    --  Examine
      dep     : Boolean;    --  Deposit
      addr    : Boolean;    --  Address/Data
      auto    : Boolean;    --  Auto/Man, enable remote control via web server
      start   : Boolean;    --  Start
      run     : Boolean;    --  Run
      atype   : addr_type;  --  LED only, address type
      mode    : proc_mode;  --  LED only, processor mode
   end record;
   --
   for ctrl_mode use record
      unused0 at 0 range  0 ..  0;
      ready   at 0 range  1 ..  1;
      exam    at 0 range  2 ..  2;
      dep     at 0 range  3 ..  3;
      addr    at 0 range  4 ..  4;
      auto    at 0 range  5 ..  5;
      start   at 0 range  6 ..  6;
      run     at 0 range  7 ..  7;
      atype   at 0 range  8 .. 11;
      mode    at 0 range 12 .. 15;
   end record;
   --
   for ctrl_mode'Size use 16;
   --
   --  ----------------------------------------------------------------------
   --  Utility functions
   --
   function isHex(c : Character) return Boolean is
     ((c >= '0' and c <= '9') or (c >= 'A' and c <= 'F') or (c >= 'a' and c <= 'f'))
       with Global => Null;
   pragma Pure_Function(isHex);
   function hexDigit(c : Character) return uint32;
   function toHex(value : byte) return String;
   function toHex(value : word) return String;
   function toHex(value : uint32) return String;
   function toHex(s : String) return uint32;
   --
   function isOct(c : Character) return Boolean is
     (c >= '0' and c <= '7')
       with Global => Null;
   pragma Pure_Function(isoct);
   function octDigit(c : Character) return uint32;
   function toOct(value : byte) return String;
   function toOct(value : word) return String;
   function toOct(value : uint32) return String;
   function toOct(s : String) return uint32;
   --
   --  Parse a line of an Intex Hex file
   --
   procedure IntelHex(s : String; count : out byte; addr : out word; rec : out byte;
                      data : out page; valid : out Boolean);
   --
   --  Parse a line of an S-Record file
   --
   procedure S_Record(s : String; count : out byte; addr : out ad_bus; rec : out byte;
                      data : out page; valid : out Boolean);
   --
end BBS.Sim_CPU;
