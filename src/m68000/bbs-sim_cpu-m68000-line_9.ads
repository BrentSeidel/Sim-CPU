--
--  Package for decoding Line 9 instructions - SUB/SUBX
--
package BBS.Sim_CPU.m68000.line_9 is
   procedure decode_9(self : in out m68000);
private
   type step_sub is record
      reg_y  : reg_num;
      mode_y : mode_code;
      opmode : uint3;
      reg_x  : reg_num;
      pre    : prefix;
   end record;
   for step_sub use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      opmode at 0 range 6 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 .. 15;
   end record;
   type step_subx is record
      reg_y   : reg_num;
      reg_mem : reg_type;
      code1   : uint2;    --  0 For SUBX instruction
      size    : data_size;
      code2   : Boolean;  --  True for SUBX instruction
      reg_x   : reg_num;
      pre     : prefix;
   end record;
   for step_subx use record
      reg_y   at 0 range 0 .. 2;
      reg_mem at 0 range 3 .. 3;
      code1   at 0 range 4 .. 5;
      size    at 0 range 6 .. 7;
      code2   at 0 range 8 .. 8;
      reg_x   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   --
   instr_sub : step_sub    --  Decode SUB instructions
      with address => instr'Address;
   instr_subx : step_subx  --  Decode ADDX instructions
      with address => instr'Address;
   --
   --  Note that some addressing modes for the ADD instruction are
   --  unusable and have been repurposed for ADDX instructions.  Need
   --  to check for that.
   --
   procedure decode_SUB(self : in out m68000);
   procedure decode_SUBX(self : in out m68000);
end;