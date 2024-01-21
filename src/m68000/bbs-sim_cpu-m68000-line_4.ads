--
--  Package for decoding Line 4 instructions - Miscellaneous
--
package BBS.Sim_CPU.m68000.line_4 is
   procedure decode_4(self : in out m68000);

private
   type step_chk is record
      reg_y   : reg_num;
      mode_y  : mode_code;
      code    : Boolean;
      size    : uint2;  --  Uses different coding from other size fields
      reg_x   : reg_num;
      pre     : prefix;
   end record;
   for step_chk use record
      reg_y   at 0 range 0 .. 2;
      mode_y  at 0 range 3 .. 5;
      code    at 0 range 6 .. 6;
      size    at 0 range 7 .. 8;
      reg_x   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   type step_clr is record
      reg_y   : reg_num;
      mode_y  : mode_code;
      size    : data_size;
      code    : uint4;  --  2 for CLR, 4 for NEG, 0 for NEGX, 6 for NOT,
                        --  10 for TST
      pre     : prefix;
   end record;
   for step_clr use record
      reg_y   at 0 range 0 .. 2;
      mode_y  at 0 range 3 .. 5;
      size    at 0 range 6 .. 7;
      code    at 0 range 8 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   type step_ext is record
      reg_y : reg_num;
      code0 : uint3;  --  0 for EXT
      mode  : uint3;  --  2 & 3 for EXT (later processors allow 7)
      code1 : uint3;  --  4 for EXT
      pre   : prefix;
   end record;
   for step_ext use record
      reg_y at 0 range 0 .. 2;
      code0 at 0 range 3 .. 5;
      mode  at 0 range 6 .. 8;
      code1 at 0 range 9 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   type step_1ea is record  --  One effective address
      reg_y  : reg_num;
      mode_y : mode_code;
      code   : uint6;  --  16#3B# for jmp, 16#3A# for jsr,
                       --  16#13# for MOVE to CCR, 16#1b# for MOVE to SR,
                       --  16#03# for MOVE from SR, 16#0B for move from CCR,
                       --  16#20# for NBCD, 16#21# for PEA, 16#2b# for TAS
      pre    : prefix;
   end record;
   for step_1ea use record
      reg_y   at 0 range 0 .. 2;
      mode_y  at 0 range 3 .. 5;
      code    at 0 range 6 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   type step_lea is record
      reg_y  : reg_num;
      mode_y : mode_code;
      code   : uint3;  --  7 for LEA
      reg_x  : reg_num;
      pre    : prefix;
   end record;
   for step_lea use record
      reg_y   at 0 range 0 .. 2;
      mode_y  at 0 range 3 .. 5;
      code    at 0 range 6 .. 8;
      reg_x   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   type step_link is record
      reg_y : reg_num;
      code  : uint9;  --  16#1ca# for LINK, 16#1cb# for UNLK
      pre   : prefix;
   end record;
   for step_link use record
      reg_y at 0 range 0 .. 2;
      code  at 0 range 3 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   type step_musp is record
      reg_y : reg_num;
      dir   : Boolean;
      code  : uint8;  --  16#e6# for MOVE to/from USP
      pre   : prefix;
   end record;
   for step_musp use record
      reg_y at 0 range 0 .. 2;
      dir   at 0 range 3 .. 3;
      code  at 0 range 4 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   type step_movem is record
      reg_y  : reg_num;
      mode_y : mode_code;
      size   : Boolean;  --  True = long, False = word
      code0  : uint3;    --  1 for MOVEM
      dir    : Boolean;  --  True = mem to reg, False = reg to mem
      code1  : Boolean;  --  True for MOVEM
      pre    : prefix;
   end record;
   for step_movem use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      size   at 0 range 6 .. 6;
      code0  at 0 range 7 .. 9;
      dir    at 0 range 10 .. 10;
      code1  at 0 range 11 .. 11;
      pre    at 0 range 12 .. 15;
   end record;
   type step_swap is record
      reg_y : reg_num;
      code  : uint9;  --  16#108# for SWAP, 16#1cb# for UNLK
      pre   : prefix;
   end record;
   for step_swap use record
      reg_y at 0 range 0 .. 2;
      code  at 0 range 3 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   type step_trap is record
      vect : uint4;
      code : uint8;  --  16#e4# for TRAP
      pre  : prefix;
   end record;
   for step_trap use record
      vect at 0 range 0 .. 3;
      code at 0 range 4 .. 11;
      pre  at 0 range 12 .. 15;
   end record;
   --
   instr_chk : step_chk     with address => instr'Address;
   instr_clr : step_clr     with address => instr'Address;
   instr_ext : step_ext     with address => instr'Address;
   instr_1ea : step_1ea     with address => instr'Address;
   instr_lea : step_lea     with address => instr'Address;
   instr_link : step_link   with address => instr'Address;
   instr_musp : step_musp   with address => instr'Address;
   instr_movem : step_movem with address => instr'Address;
   instr_swap : step_swap   with address => instr'Address;
   instr_trap : step_trap   with address => instr'Address;

   procedure decode_CHK(self : in out m68000)
      with pre => (not instr_chk.code and ((instr_chk.size = 3) or (instr_chk.size = 2)));
   procedure decode_CLR(self : in out m68000)
      with pre => ((instr_clr.code = 2) and (instr_clr.size /= data_long_long));
   procedure decode_EXT(self : in out m68000)
      with pre => ((instr_ext.code0 = 0) and (instr_ext.code1 = 4) and
                  ((instr_ext.mode = 2) or (instr_ext.mode = 3)));
   procedure decode_ILLEGAL(self : in out m68000)
      with pre => (instr = 16#4AFC#);
   procedure decode_JMP(self : in out m68000)
      with pre => (instr_1ea.code = 16#3b#) and ((instr_1ea.mode_y = 2) or
            (instr_1ea.mode_y = 5) or (instr_1ea.mode_y = 6) or
            (instr_1ea.mode_y = 7));
   procedure decode_JSR(self : in out m68000)
      with pre => (instr_1ea.code = 16#3a#) and ((instr_1ea.mode_y = 2) or
            (instr_1ea.mode_y = 5) or (instr_1ea.mode_y = 6) or
            (instr_1ea.mode_y = 7));
   procedure decode_LEA(self : in out m68000)
      with pre => (instr_lea.code = 7) and ((instr_lea.mode_y = 2) or
            (instr_lea.mode_y = 5) or (instr_lea.mode_y = 6) or
            (instr_lea.mode_y = 7));
   procedure decode_LINK(self : in out m68000)
      with pre => (instr_link.code = 16#1ca#);
   procedure decode_MOVEtCCR(self : in out m68000)
      with pre => ((instr_1ea.code = 16#13#) and (instr_1ea.mode_y /= 1));
   procedure decode_MOVEtSR(self : in out m68000)
      with pre => ((instr_1ea.code = 16#1b#) and (instr_1ea.mode_y /= 1));
   procedure decode_MOVEfSR(self : in out m68000)
      with pre => ((instr_1ea.code = 16#03#) and (instr_1ea.mode_y /= 1) and
            not ((instr_1ea.mode_y = 7) and ((instr_1ea.reg_y = 2) or
               (instr_1ea.reg_y = 3) or (instr_1ea.reg_y = 4))));
   procedure decode_MtfUSP(self : in out m68000)
      with pre => (instr_musp.code = 16#e6#);
   procedure decode_MOVEM(self : in out m68000)
      with pre => ((instr_movem.code0 = 1) and instr_movem.code1 and
                  (instr_movem.mode_y /= 0) and (instr_movem.mode_y /= 1));
   procedure decode_NBCD(self : in out m68000)
      with pre => ((instr_1ea.code = 16#20#) and (instr_1ea.mode_y /= 1) and
            not ((instr_1ea.mode_y = 7) and ((instr_1ea.reg_y = 2) or
               (instr_1ea.reg_y = 3) or (instr_1ea.reg_y = 4))));
   procedure decode_NEG(self : in out m68000)
      with pre => (((instr_clr.code = 4) or (instr_clr.code = 0))
            and (instr_clr.mode_y /= 1) and
            not ((instr_clr.mode_y = 7) and ((instr_clr.reg_y = 2) or
               (instr_clr.reg_y = 3) or (instr_clr.reg_y = 4))));
   procedure decode_NOT(self : in out m68000)
      with pre => ((instr_clr.code = 6) and (instr_clr.mode_y /= 1) and
            not ((instr_clr.mode_y = 7) and ((instr_clr.reg_y = 2) or
               (instr_clr.reg_y = 3) or (instr_clr.reg_y = 4))));
   procedure decode_PEA(self : in out m68000)
      with pre => (instr_1ea.code = 16#21#) and ((instr_1ea.mode_y = 2) or
            (instr_1ea.mode_y = 5) or (instr_1ea.mode_y = 6) or
            (instr_1ea.mode_y = 7));
   procedure decode_RESET(self : in out m68000)
      with pre => (instr = 16#4e70#);
   procedure decode_RTD(self : in out m68000)
      with pre => ((instr = 16#4e74#) and (self.cpu_model /= var_68008)
                  and (self.cpu_model /= var_68000));
   procedure decode_RTE(self : in out m68000)
      with pre => (instr = 16#4e73#);
   procedure decode_RTR(self : in out m68000)
      with pre => (instr = 16#4e77#);
   procedure decode_RTS(self : in out m68000)
      with pre => (instr = 16#4e75#);
   procedure decode_STOP(self : in out m68000)
      with pre => (instr = 16#4e72#);
   procedure decode_SWAP(self : in out m68000)
      with pre => (instr_swap.code = 16#108#);
   procedure decode_TAS(self : in out m68000)
      with pre => ((instr_1ea.code = 16#2b#) and (instr_1ea.mode_y /= 1));
   procedure decode_TRAP(self : in out m68000)
      with pre => (instr_trap.code = 16#e4#);
   procedure decode_TRAPV(self : in out m68000)
      with pre => (instr = 16#4e76#);
   procedure decode_TST(self : in out m68000)
      with pre => ((instr_clr.code = 10) and (instr_clr.mode_y /= 1) and
                  (instr_clr.size /= data_long_long));
   procedure decode_UNLK(self : in out m68000)
      with pre => (instr_swap.code = 16#1cb#);
end;
