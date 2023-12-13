--
--  Package for decoding Line D (13) instructions - ADD/ADDX
--
package BBS.Sim_CPU.m68000.line_d is
   procedure decode_d(self : in out m68000);
   --
   --  Note that some addressing modes for the ADD instruction are
   --  unusable and have been repurposed for ADDX instructions.  Need
   --  to check for that.
   --
   procedure add_instr(self : in out m68000);
   procedure addx_instr(self : in out m68000);
private
   type step_add is record  --  Also used for AND instruction
      reg_y  : uint3;
      mode_y : uint3;
      opmode : uint3;
      reg_x  : uint3;
      pre    : prefix;
   end record;
   for step_add use record
      reg_y  at 0 range 0 .. 2;
      mode_y at 0 range 3 .. 5;
      opmode at 0 range 6 .. 8;
      reg_x  at 0 range 9 .. 11;
      pre    at 0 range 12 .. 15;
   end record;
   type step_addx is record
      reg_y   : uint3;
      reg_mem : reg_type;
      code1   : uint2;    --  0 For ADDX instruction
      size    : data_size;
      code2   : Boolean;  --  True for ADDX instruction
      reg_x   : uint3;
      pre     : prefix;
   end record;
   for step_addx use record
      reg_y   at 0 range 0 .. 2;
      reg_mem at 0 range 3 .. 3;
      code1   at 0 range 4 .. 5;
      size    at 0 range 6 .. 7;
      code2   at 0 range 8 .. 8;
      reg_x   at 0 range 9 .. 11;
      pre     at 0 range 12 .. 15;
   end record;
   --
   instr_add : step_add    --  Decode ADD/ADDA/AND instructions
      with address => instr'Address;
   instr_addx : step_addx  --  Decode ADDX instructions
      with address => instr'Address;
end;
