with BBS.embed;
use type BBS.embed.uint32;
with BBS.Sim_CPU;
with BBS.Sim_CPU.i8080;
with Ada.Text_IO;
with test_util;

procedure Simcputest is
begin
   Ada.Text_IO.Put_Line("CPU Simulator Test Program");
   test_util.cpu.init;
   Ada.Text_IO.Put_Line("Simulator name: " & test_util.cpu.name);
   test_util.cpu.attach_io(test_util.con'Access, 0, 0);
   test_util.cpu.attach_io(test_util.print'Access, 2, 0);
   test_util.print.open("list-8080.txt");
   test_util.cmds;
end Simcputest;
