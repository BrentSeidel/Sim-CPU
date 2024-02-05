with BBS.Sim_CPU;
with BBS.Sim_CPU.i8080;
with BBS.Sim_CPU.m68000;
with BBS.Sim_CPU.disk;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with test_util;

procedure Simcputest is
   selection : Integer := 0;
begin
   Ada.Text_IO.Put_Line("CPU Simulator Test Program");
   loop
      Ada.Text_IO.Put_Line("Available simulators are:");
      Ada.Text_IO.Put_Line("1. Intel 8080/8085");
      Ada.Text_IO.Put_Line("2. Motorola 68000");
      Ada.Text_IO.Put("Selection: ");
      Ada.Integer_Text_IO.Get(selection, 0);
      --
      --  This is just to clear out any text on the rest of the line.
      --
      declare
         dummy : String := Ada.Text_IO.Get_line;
      begin
         null;  --  Nothing to do here.
      end;
      exit when (selection > 0) and (selection < 3);
   end loop;
   if selection = 1 then
      test_util.i8080.variant(BBS.Sim_CPU.i8080.variants_i8080'Pos(BBS.Sim_CPU.i8080.var_8085));
      test_util.cpu := test_util.i8080'Access;
      test_util.cpu.init;
      Ada.Text_IO.Put_Line("Simulator name: " & test_util.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & test_util.cpu.variant(test_util.cpu.variant));
      test_util.cpu.attach_io(test_util.tel'Access, 0, BBS.Sim_CPU.BUS_IO);
      test_util.tel.init(test_util.tel'Access, 2171);
      test_util.cpu.attach_io(test_util.print'Access, 2, BBS.Sim_CPU.BUS_IO);
      test_util.print.open("printer.txt");
      test_util.cpu.attach_io(test_util.fd'Access, 3, BBS.Sim_CPU.BUS_IO);
      test_util.fd.setOwner(test_util.cpu);
      test_util.fd.open(0, test_util.floppy_ctrl.floppy8_geom, "cpmboot.img");
      test_util.fd.open(1, test_util.floppy_ctrl.floppy8_geom, "cpma.cpm");
      test_util.fd.open(2, test_util.floppy_ctrl.floppy8_geom, "zork1.cpm");
      test_util.fd.open(3, test_util.floppy_ctrl.floppy8_geom, "drv3.img");
   else
      test_util.m68000.variant(BBS.Sim_CPU.m68000.variants_m68000'Pos(BBS.Sim_CPU.m68000.var_68000));
      test_util.cpu := test_util.m68000'Access;
      test_util.cpu.init;
      Ada.Text_IO.Put_Line("Simulator name: " & test_util.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & test_util.cpu.variant(test_util.cpu.variant));
      test_util.cpu.attach_io(test_util.tel'Access, 16#400#, BBS.Sim_CPU.BUS_MEMORY);
      test_util.tel.init(test_util.tel'Access, 2171);
      test_util.tel.setException(65);
      test_util.cpu.attach_io(test_util.clock'Access, 16#402#,BBS.Sim_CPU.BUS_MEMORY);
      test_util.clock.setOwner(test_util.cpu);
      test_util.clock.init(test_util.clock'Access);
      test_util.clock.setException(64);
--      test_util.cpu.attach_io(test_util.print'Access, 16#00FF_FF02#, BBS.Sim_CPU.BUS_MEMORY);
--      test_util.cpu.attach_io(test_util.fd'Access, 16#00FF_FF04#, BBS.Sim_CPU.BUS_MEMORY);
   end if;
   test_util.cmds;
   test_util.tel.shutdown;
   test_util.clock.shutdown;
end Simcputest;
