--
--  Package for decoding Line 4 instructions - Miscellaneous
--
package BBS.Sim_CPU.m68000.line_4 is
   procedure decode_4(self : in out m68000);

private
   type step_chk is record
      reg_y   : uint3;
      mode_y  : uint3;
      code    : Boolean;
      size    : uint2;  --  Uses different coding from other size fields
      reg_x   : uint3;
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
      reg_y   : uint3;
      mode_y  : uint3;
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
      reg_y : uint3;
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
   --
   instr_chk : step_chk  --  Decode CHK (bounds check) instruction
      with address => instr'Address;
   instr_clr : step_clr  --  Decode CLR instruction
      with address => instr'Address;
   instr_ext : step_ext
      with address => instr'Address;

   procedure decode_CHK(self : in out m68000)
      with pre => (not instr_chk.code and ((instr_chk.size = 3) or (instr_chk.size = 2)));
   procedure decode_CLR(self : in out m68000)
      with pre => ((instr_clr.code = 2) and (instr_clr.size /= data_long_long));
   procedure decode_EXT(self : in out m68000)
      with pre => ((instr_ext.code0 = 0) and (instr_ext.code1 = 4) and
                  ((instr_ext.mode = 2) or (instr_ext.mode = 3)));

end;
