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
--  Package for decoding Line 0 instructions -  - Bit manipulation/MOVEP/Immediate
--
package BBS.Sim_CPU.m68000.line_0 is
   procedure decode_0(self : in out m68000);
private
   bit_pos : array (long range 0 .. 31) of long := (
               16#0000_0001#,
               16#0000_0002#,
               16#0000_0004#,
               16#0000_0008#,
               16#0000_0010#,
               16#0000_0020#,
               16#0000_0040#,
               16#0000_0080#,
               16#0000_0100#,
               16#0000_0200#,
               16#0000_0400#,
               16#0000_0800#,
               16#0000_1000#,
               16#0000_2000#,
               16#0000_4000#,
               16#0000_8000#,
               16#0001_0000#,
               16#0002_0000#,
               16#0004_0000#,
               16#0008_0000#,
               16#0010_0000#,
               16#0020_0000#,
               16#0040_0000#,
               16#0080_0000#,
               16#0100_0000#,
               16#0200_0000#,
               16#0400_0000#,
               16#0800_0000#,
               16#1000_0000#,
               16#2000_0000#,
               16#4000_0000#,
               16#8000_0000#);
   --
   type step_imm is record  --  Immediate instructions
     reg_y  : reg_num;
     mode_y : mode_code;
     size   : data_size;
     code   : uint4;  --  0 for ORI, 2 for ANDI, 4 for SUBI
                      --  6 for ADDI, A for EORI, C for CMPI
     pre    : prefix;
   end record;
   for step_imm use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      size   at 0 range 6 .. 7;
      code   at 0 range 8 .. 11;
      pre    at 0 range 12 ..15;
   end record;
   type step_bit is record
      reg_y   : reg_num;
      mode_y  : mode_code;
      code    : uint3;  --  Specifies which bit instruction
      reg_x   : reg_num;
      pre     : prefix;
   end record;
   for step_bit use record
      reg_y   at 0 range 0 .. 2;
      mode_y  at 0 range 3 .. 5;
      code    at 0 range 6 .. 8;
      reg_x   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   type step_movep is record
      reg_y : reg_num;
      code  : uint3;
      mode  : uint3;
      reg_x : reg_num;
      pre   : prefix;
   end record;
   for step_movep use record
      reg_y at 0 range 0 .. 2;
      code  at 0 range 3 .. 5;
      mode  at 0 range 6 .. 8;
      reg_x at 0 range 9 ..11;
      pre   at 0 range 12 .. 15;
   end record;
   --
   instr_imm : step_imm  --  Decode immediate instructions
      with address => instr'Address;
   instr_bit : step_bit  --  Decode test the various bit instructions
      with address => instr'Address;
   instr_movep : step_movep
      with address => instr'Address;
   --
   procedure decode_ADDI(self : in out m68000)
      with pre => ((instr_imm.code = 6) and (instr_imm.size /= data_long_long));
   procedure decode_ANDI(self : in out m68000)
      with pre => ((instr_imm.code = 2) and (instr_imm.size /= data_long_long));
   procedure decode_BCHG(self : in out m68000)
      with pre => ((instr_bit.code = 5) or ((instr_bit.code = 1) and (instr_bit.reg_x = 4)));
   procedure decode_BCLR(self : in out m68000)
      with pre => ((instr_bit.code = 6) or ((instr_bit.code = 2) and (instr_bit.reg_x = 4)));
   procedure decode_BSET(self : in out m68000)
      with pre => ((instr_bit.code = 7) or ((instr_bit.code = 3) and (instr_bit.reg_x = 4)));
   procedure decode_BTST(self : in out m68000)
      with pre => ((instr_bit.code = 4) or ((instr_bit.code = 0) and (instr_bit.reg_x = 4)));
   procedure decode_CMPI(self : in out m68000)
      with pre => ((instr_imm.code = 16#C#) and (instr_imm.size /= data_long_long));
   procedure decode_EORI(self : in out m68000)
      with pre => ((instr_imm.code = 16#A#) and (instr_imm.size /= data_long_long));
   procedure decode_ORI(self : in out m68000)
      with pre => ((instr_imm.code = 0) and (instr_imm.size /= data_long_long));
   procedure decode_MOVEP(self : in out m68000)
      with pre => ((instr_movep.code = 1) and ((instr_movep.mode = 4) or
                  (instr_movep.mode = 5) or (instr_movep.mode = 6) or
                  (instr_movep.mode = 7)));
   procedure decode_SUBI(self : in out m68000)
      with pre => ((instr_imm.code = 4) and (instr_imm.size /= data_long_long));
   --
end;
