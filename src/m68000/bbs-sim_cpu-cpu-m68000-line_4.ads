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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
--  Package for decoding Line 4 instructions - Miscellaneous
--
package BBS.Sim_CPU.CPU.m68000.line_4 is
   procedure decode_4(self : in out m68000);

private
   --
   procedure decode_CHK(self : in out m68000)
      with pre => (not self.instr.chk.code and ((self.instr.chk.size = 3) or (self.instr.chk.size = 2)));
   procedure decode_CLR(self : in out m68000)
      with pre => ((self.instr.op1_size.code = 2) and (self.instr.op1_size.size /= data_long_long));
   procedure decode_EXT(self : in out m68000)
      with pre => ((self.instr.ext.code0 = 0) and (self.instr.ext.code1 = 4) and
                  ((self.instr.ext.mode = 2) or (self.instr.ext.mode = 3)));
   procedure decode_ILLEGAL(self : in out m68000)
      with pre => (self.instr.b = 16#4AFC#);
   procedure decode_JMP(self : in out m68000)
      with pre => (self.instr.ea1.code = 16#3b#) and ((self.instr.ea1.mode_y = 2) or
            (self.instr.ea1.mode_y = 5) or (self.instr.ea1.mode_y = 6) or
            (self.instr.ea1.mode_y = 7));
   procedure decode_JSR(self : in out m68000)
      with pre => (self.instr.ea1.code = 16#3a#) and ((self.instr.ea1.mode_y = 2) or
            (self.instr.ea1.mode_y = 5) or (self.instr.ea1.mode_y = 6) or
            (self.instr.ea1.mode_y = 7));
   procedure decode_LEA(self : in out m68000)
      with pre => (self.instr.op2.code = 7) and ((self.instr.op2.mode_y = 2) or
            (self.instr.op2.mode_y = 5) or (self.instr.op2.mode_y = 6) or
            (self.instr.op2.mode_y = 7));
   procedure decode_LINK(self : in out m68000)
      with pre => (self.instr.regy.code = 16#1ca#);
   procedure decode_MOVEtCCR(self : in out m68000)
      with pre => ((self.instr.ea1.code = 16#13#) and (self.instr.ea1.mode_y /= 1));
   procedure decode_MOVEtSR(self : in out m68000)
      with pre => ((self.instr.ea1.code = 16#1b#) and (self.instr.ea1.mode_y /= 1));
   procedure decode_MOVEfSR(self : in out m68000)
      with pre => ((self.instr.ea1.code = 16#03#) and (self.instr.ea1.mode_y /= 1) and
            not ((self.instr.ea1.mode_y = 7) and ((self.instr.ea1.reg_y = 2) or
               (self.instr.ea1.reg_y = 3) or (self.instr.ea1.reg_y = 4))));
   procedure decode_MtfUSP(self : in out m68000)
      with pre => (self.instr.musp.code = 16#e6#);
   procedure decode_MOVEM(self : in out m68000)
      with pre => ((self.instr.movem.code0 = 1) and self.instr.movem.code1 and
                  (self.instr.movem.mode_y /= 0) and (self.instr.movem.mode_y /= 1));
   procedure decode_NBCD(self : in out m68000)
      with pre => ((self.instr.ea1.code = 16#20#) and (self.instr.ea1.mode_y /= 1) and
            not ((self.instr.ea1.mode_y = 7) and ((self.instr.ea1.reg_y = 2) or
               (self.instr.ea1.reg_y = 3) or (self.instr.ea1.reg_y = 4))));
   procedure decode_NEG(self : in out m68000)
      with pre => (((self.instr.op1_size.code = 4) or (self.instr.op1_size.code = 0))
            and (self.instr.op1_size.mode_y /= 1) and
            not ((self.instr.op1_size.mode_y = 7) and ((self.instr.op1_size.reg_y = 2) or
               (self.instr.op1_size.reg_y = 3) or (self.instr.op1_size.reg_y = 4))));
   procedure decode_NOT(self : in out m68000)
      with pre => ((self.instr.op1_size.code = 6) and (self.instr.op1_size.mode_y /= 1) and
            not ((self.instr.op1_size.mode_y = 7) and ((self.instr.op1_size.reg_y = 2) or
               (self.instr.op1_size.reg_y = 3) or (self.instr.op1_size.reg_y = 4))));
   procedure decode_PEA(self : in out m68000)
      with pre => (self.instr.ea1.code = 16#21#) and ((self.instr.ea1.mode_y = 2) or
            (self.instr.ea1.mode_y = 5) or (self.instr.ea1.mode_y = 6) or
            (self.instr.ea1.mode_y = 7));
   procedure decode_RESET(self : in out m68000)
      with pre => (self.instr.b = 16#4e70#);
   procedure decode_RTD(self : in out m68000)
      with pre => ((self.instr.b = 16#4e74#) and (self.cpu_model /= var_68008)
                  and (self.cpu_model /= var_68000));
   procedure decode_RTE(self : in out m68000)
      with pre => (self.instr.b = 16#4e73#);
   procedure decode_RTR(self : in out m68000)
      with pre => (self.instr.b = 16#4e77#);
   procedure decode_RTS(self : in out m68000)
      with pre => (self.instr.b = 16#4e75#);
   procedure decode_STOP(self : in out m68000)
      with pre => (self.instr.b = 16#4e72#);
   procedure decode_SWAP(self : in out m68000)
      with pre => (self.instr.regy.code = 16#108#);
   procedure decode_TAS(self : in out m68000)
      with pre => ((self.instr.ea1.code = 16#2b#) and (self.instr.ea1.mode_y /= 1));
   procedure decode_TRAP(self : in out m68000)
      with pre => (self.instr.trap.code = 16#e4#);
   procedure decode_TRAPV(self : in out m68000)
      with pre => (self.instr.b = 16#4e76#);
   procedure decode_TST(self : in out m68000)
      with pre => ((self.instr.op1_size.code = 10) and (self.instr.op1_size.mode_y /= 1) and
                  (self.instr.op1_size.size /= data_long_long));
   procedure decode_UNLK(self : in out m68000)
      with pre => (self.instr.regy.code = 16#1cb#);
end;
