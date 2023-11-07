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
   test_util.cpu.attach_io(test_util.tel'Access, 0, BBS.Sim_CPU.BUS_IO);
   test_util.tel.init(test_util.tel'Access, 2171);
   test_util.cpu.attach_io(test_util.print'Access, 2, BBS.Sim_CPU.BUS_IO);
   test_util.print.open("printer.txt");
   test_util.cpu.attach_io(test_util.fd'Access, 3, BBS.Sim_CPU.BUS_IO);
   test_util.fd.setOwner(test_util.cpu'Access);
   test_util.fd.open(0, "drv0.img");
   test_util.fd.open(1, "drv1.img");
   test_util.fd.open(2, "drv2.img");
   test_util.fd.open(3, "drv3.img");
   test_util.cmds;
end Simcputest;
