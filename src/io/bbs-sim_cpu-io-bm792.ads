--
--  Author: Brent Seidel
--  Date: 14-May-2026
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
--  Contains a simple boot ROM for PDP-11 RK11 based on a BM792
--
with BBS.Sim_CPU.io;
package BBS.Sim_CPU.io.BM792 is
   --
   type BM792 is new io_device with private;
   type BM792_access is access all BM792'Class;
   --
   --  I/O device actions
   --
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out BM792; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat);
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out BM792; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus;
   --
   --  This is a ROM.  No exceptions, so this function does nothing.
   --
   procedure setException(self : in out BM792; except : long) is null;
   --
   --  How many addresses are used by the port
   --
   overriding
   function getSize(self : in out BM792) return addr_bus is (32);
   --
   --  Get device name/description
   --
   overriding
   function name(self : in out BM792) return string is ("BT");
   overriding
   function description(self : in out BM792) return string is ("BM792 Boot ROM");
   overriding
   function dev_class(self : in out BM792) return dev_type is (NL);
   -- =========================================================================
private
   type ROM is array (byte range 0 .. 31) of word;
   -- -------------------------------------------------------------------------
   --
   --  Definition of the boot ROM
   --
   type BM792 is new io_device with record
      data : ROM := (8#012700#,  --01  MOV #177406,R0    ; Move the address of the Word Count register into R0
                     8#177406#,  --02
                     8#012710#,  --03  MOV #177400,(R0)  ; Move block size (negative) into Word Count register
                     8#177400#,  --04
                     8#012740#,  --05  MOV #5,-(R0)      ; Move 'Read Go' command into CSR
                     8#000005#,  --06
                     8#105710#,  --07  TSTB (R0)         ; Test for 'Done' bit in CSR
                     8#100376#,  --08  BPL 1014          ; Jump backward if not set
                     8#005007#,  --09  CLR PC            ; Start loaded bootstrap with jump to 0
                     others => 0);
   end record;
   --
end;
