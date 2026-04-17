--
--  Author: Brent Seidel
--  Date: 17-Apr-2026
--
--  This file is part of SimCPU CLI.
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
with BBS;
use type BBS.uint32;
package cli.common is
   --
   --  Set the CPU type.
   --  Name is a string representing the desired CPU type.
   --  Returns false if name is unrecognized, otherwise True.
   --
   function set_cpu(name : String) return Boolean
     with pre => (cpu_selected = False);
   --
   --  Install hardware device
   --  dev is the name of the device
   --  bus is the type of bus (memory or I/O)
   --  port is the address on the bus
   --  except is an exception code specific to the device and system
   --  extra is additional device dependant information
   --  present is True is extra is present
   --  returns False is unrecognized device or other error
   --
   function install_hw(dev : String; which_bus : BBS.Sim_CPU.bus_type; port : BBS.uint32;
                       except : BBS.uint32; except_present : Boolean; extra : BBS.uint32;
                       extra_present : Boolean) return Boolean;
end;
