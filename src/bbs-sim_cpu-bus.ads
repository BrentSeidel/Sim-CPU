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
--
--  This package is the basis of the system bus.  This includes both memory,  I/O
--  devices, and anything else that can be addressed by the processor.  It is
--  possible to create memory systems with non-contiguous blocks of memory and
--  introduce various errors into the memory system.  Any address translation or
--  or memory management would be included in one of these objects.
--
package BBS.Sim_CPU.bus is
   --
   --  The memory object.  This object will contain memory,  I/O, and an optional
   --  memory management unit.
   --
   type memory is abstract tagged private;
   type mem_access is access all memory'Class;
   --
   function read(self : memory; addr : addr_bus) return data_bus;
private
   --
   --  Memory object.
   --
   type memory is abstract tagged record
      null;
   end record;
end;
