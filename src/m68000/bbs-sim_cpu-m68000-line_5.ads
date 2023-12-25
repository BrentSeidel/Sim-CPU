--
--  Package for decoding Group 5 - ADDQ/SUBQ/Scc/DBcc/TRAPcc
--
package BBS.Sim_CPU.m68000.line_5 is
   procedure decode_5(self : in out m68000);
   procedure decode_ADDQ(self : in out m68000);
   procedure decode_DBcc(self : in out m68000);
private
   type step_addq is record
     reg_y  : uint3;
     mode_y : uint3;
     size   : data_size;
     code   : Boolean;  --  False for ADDQ instruction
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
     reg_y : uint3;
     code  : uint5;
     cond  : uint4;
     pre   : prefix;
   end record;
   for step_dbcc use record
      reg_y at 0 range 0 .. 2;
      code  at 0 range 3 .. 7;
      cond  at 0 range 8 .. 11;
      pre   at 0 range 12 ..15;
   end record;
   --
   instr_addq : step_addq  --  Decode ADDQ instructions
      with address => instr'Address;
   instr_dbcc : step_dbcc  --  Decode DBcc instructions
      with address => instr'Address;
end;
