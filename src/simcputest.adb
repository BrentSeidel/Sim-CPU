with BBS.embed;
use type BBS.embed.uint32;
with BBS.Sim_CPU;
with BBS.Sim_CPU.i8080;
with Ada.Text_IO;
with test_util;

procedure Simcputest is
begin
   test_util.cpu.init;
   test_util.cpu.attach_io(test_util.con'Access, 0, 0);
   Ada.Text_IO.Put_Line("CPU Simulator Test Program");
   Ada.Text_IO.Put_Line("Simulator name: " & test_util.cpu.name);
   test_util.cmds;
end Simcputest;
