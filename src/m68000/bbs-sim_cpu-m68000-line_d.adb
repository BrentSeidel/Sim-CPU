package body BBS.Sim_CPU.m68000.line_d is
   --
   --  Package for decoding Line D (13) instructions - ADD/ADDX
   --
   procedure decode_d(self : in out m68000) is
     reg_x  : reg_num := instr_add.reg_x;
     reg_y  : reg_num := instr_add.reg_y;
     mode_y : reg_num := instr_add.mode_y;
     opmode : reg_num := instr_add.opmode;
   begin
        null;
   end;
   --
end;
