--
--  Author: Brent Seidel
--  Date: 15-Dec-2025
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
--  Code for PDP-11 MOV and MOVB instructions
--
with Ada.Text_IO;
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package body BBS.Sim_CPU.CPU.PDP11.move is
   --
   --  Move word
   --
   procedure mov(self : in out PDP11) is
      ea_src  : constant operand := self.get_ea(instr_2op.reg_src, instr_2op.mode_src, data_word);
      val     : constant word := self.get_ea(ea_src);
   begin
      self.post_ea(ea_src);
      declare
         ea_dest : constant operand := self.get_ea(instr_2op.reg_dest, instr_2op.mode_dest, data_word);
      begin
         self.set_ea(ea_dest, val);
         self.post_ea(ea_dest);
      end;
   end;
   --
   --  Move byte
   --
   procedure movb(self : in out PDP11) is
   begin
      null;
   end;
end;
