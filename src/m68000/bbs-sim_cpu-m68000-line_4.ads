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
      code    : uint4;
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
   type step_jmp is record
      reg_y  : reg_num;
      mode_y : mode_code;
      code   : uint6;  --  16#3B# for jmp, 16#3A# for jsr, 16#13# for MOVE to CCR
      pre    : prefix;
   end record;
   for step_jmp use record
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
      code  : uint9;
      pre   : prefix;
   end record;
   for step_link use record
      reg_y at 0 range 0 .. 2;
      code  at 0 range 3 .. 11;
      pre   at 0 range 12 .. 15;
   end record;
   --
   instr_chk : step_chk
      with address => instr'Address;
   instr_clr : step_clr
      with address => instr'Address;
   instr_ext : step_ext
      with address => instr'Address;
   instr_jmp : step_jmp
      with address => instr'Address;
   instr_lea : step_lea
      with address => instr'Address;
   instr_link : step_link
      with address => instr'Address;

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
      with pre => (instr_jmp.code = 16#3b#) and ((instr_jmp.mode_y = 2) or
            (instr_jmp.mode_y = 5) or (instr_jmp.mode_y = 6) or
            (instr_jmp.mode_y = 7));
   procedure decode_JSR(self : in out m68000)
      with pre => (instr_jmp.code = 16#3a#) and ((instr_jmp.mode_y = 2) or
            (instr_jmp.mode_y = 5) or (instr_jmp.mode_y = 6) or
            (instr_jmp.mode_y = 7));
   procedure decode_LEA(self : in out m68000)
      with pre => (instr_lea.code = 7) and ((instr_lea.mode_y = 2) or
            (instr_lea.mode_y = 5) or (instr_lea.mode_y = 6) or
            (instr_lea.mode_y = 7));
   procedure decode_LINK(self : in out m68000)
      with pre => (instr_link.code = 16#1ca#);
   procedure decode_MOVECCR(self : in out m68000)
      with pre => ((instr_jmp.code = 16#13#) and (instr_jmp.mode_y /= 1));

end;
