with BBS.embed;
use type BBS.embed.uint32;
with BBS.Sim_CPU;
with BBS.Sim_CPU.i8080;
with BBS.Sim_CPU.m68000;
with BBS.Sim_CPU.disk;
with Ada.Text_IO;
with test_util;

procedure Simcputest is
   use_8080 : Boolean := False;
begin
   Ada.Text_IO.Put_Line("CPU Simulator Test Program");
   test_util.i8080.variant(BBS.Sim_CPU.i8080.variants_i8080'Pos(BBS.Sim_CPU.i8080.var_8085));
   test_util.m68000.variant(BBS.Sim_CPU.m68000.variants_m68000'Pos(BBS.Sim_CPU.m68000.var_68000));
   if use_8080 then
      test_util.cpu := test_util.i8080'Access;
      test_util.cpu.init;
      Ada.Text_IO.Put_Line("Simulator name: " & test_util.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & test_util.cpu.variant(test_util.cpu.variant));
      test_util.cpu.attach_io(test_util.tel'Access, 0, BBS.Sim_CPU.BUS_IO);
      test_util.cpu.attach_io(test_util.print'Access, 2, BBS.Sim_CPU.BUS_IO);
      test_util.cpu.attach_io(test_util.fd'Access, 3, BBS.Sim_CPU.BUS_IO);
   else
      test_util.cpu := test_util.m68000'Access;
      test_util.cpu.init;
      Ada.Text_IO.Put_Line("Simulator name: " & test_util.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & test_util.cpu.variant(test_util.cpu.variant));
      test_util.cpu.attach_io(test_util.tel'Access, 16#00FF_FF00#, BBS.Sim_CPU.BUS_MEMORY);
      test_util.cpu.attach_io(test_util.print'Access, 16#00FF_FF02#, BBS.Sim_CPU.BUS_MEMORY);
      test_util.cpu.attach_io(test_util.fd'Access, 16#00FF_FF04#, BBS.Sim_CPU.BUS_MEMORY);
   end if;
   test_util.tel.init(test_util.tel'Access, 2171);
   test_util.print.open("printer.txt");
   test_util.fd.setOwner(test_util.cpu);
   test_util.fd.open(0, test_util.floppy_ctrl.floppy8_geom, "drv0.img");
   test_util.fd.open(1, test_util.floppy_ctrl.floppy8_geom, "drv1.img");
   test_util.fd.open(2, test_util.floppy_ctrl.floppy8_geom, "drv2.img");
   test_util.fd.open(3, test_util.floppy_ctrl.floppy8_geom, "drv3.img");
   test_util.cmds;
end Simcputest;
