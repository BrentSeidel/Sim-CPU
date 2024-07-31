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
--  Package for decoding Group 5 - ADDQ/SUBQ/Scc/DBcc/TRAPcc
--
package BBS.Sim_CPU.m68000.line_5 is
   procedure decode_5(self : in out m68000);
   --
private
   type step_addq is record
     reg_y  : reg_num;
     mode_y : mode_code;
     size   : data_size;
     code   : Boolean;  --  False for ADDQ instruction, True for SUBQ
     data   : uint3;
     pre    : prefix;
   end record;
   for step_addq use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      size   at 0 range 6 .. 7;
      code   at 0 range 8 .. 8;
      data   at 0 range 9 .. 11;
      pre    at 0 range 12 ..15;
   end record;
   type step_dbcc is record
     reg_y : reg_num;
     code  : uint5;  --  16#19# for DBcc instructions
     cond  : uint4;
     pre   : prefix;
   end record;
   for step_dbcc use record
      reg_y at 0 range 0 .. 2;
      code  at 0 range 3 .. 7;
      cond  at 0 range 8 .. 11;
      pre   at 0 range 12 ..15;
   end record;
   type step_scc is record
     reg_y  : reg_num;
     mode_y : mode_code;
     code   : uint2;  --  3 for Scc instructions
     cond   : uint4;
     pre    : prefix;
   end record;
   for step_scc use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      code   at 0 range 6 .. 7;
      cond   at 0 range 8 .. 11;
      pre    at 0 range 12 ..15;
   end record;
   --
   instr_addq : step_addq  --  Decode ADDQ instructions
      with address => instr'Address;
   instr_dbcc : step_dbcc  --  Decode DBcc instructions
      with address => instr'Address;
   instr_scc : step_Scc
      with address => instr'Address;
   --
   procedure decode_ADDQ(self : in out m68000)
      with pre => ((not instr_addq.code) and (instr_addq.size /= data_long_long));
   procedure decode_DBcc(self : in out m68000)
      with pre => (instr_dbcc.code = 16#19#);
   procedure decode_Scc(self: in out m68000)
      with pre => ((instr_scc.code = 3) and (instr_scc.mode_y /= 1));
   procedure decode_SUBQ(self : in out m68000)
      with pre => (instr_addq.code and (instr_addq.size /= data_long_long));
end;
