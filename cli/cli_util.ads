--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of SimCPU CLI.
--  SimCPU is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  SimCPU is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.--
--
with BBS;
with BBS.Sim_CPU;
use type BBS.Sim_CPU.io_access;
with BBS.Sim_CPU.Example;
with BBS.Sim_CPU.i8080;
with BBS.Sim_CPU.serial;
with BBS.Sim_CPU.serial.telnet;
with BBS.Sim_CPU.serial.mux;
with BBS.Sim_CPU.m68000;
with BBS.Sim_CPU.disk;
with BBS.Sim_CPU.Clock;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
--
--  This is a collection of utility functions to support testing CPU simulators.
--
package cli_util is
   --
   --  Vector to hold devices
   --
   package dev_vect is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => BBS.Sim_CPU.io_access);
   --
   --  Instantiate disk controller
   --
  package floppy_ctrl is new BBS.Sim_CPU.disk(sector_size => 128, max_drives => 16);
   --
   --  The CPU simulator object and I/O devices
   --
   i8080  : aliased BBS.Sim_CPU.i8080.i8080;
   m68000 : aliased BBS.Sim_CPU.m68000.m68000;
   cpu    : BBS.Sim_CPU.sim_access;
   devs   : dev_vect.Vector;
   con    : aliased BBS.Sim_CPU.serial.con8;
   tel0   : aliased BBS.Sim_CPU.serial.telnet.tel_tty;
   tel1   : aliased BBS.Sim_CPU.serial.telnet.tel_tty;
   tel2   : aliased BBS.Sim_CPU.serial.telnet.tel_tty;
   mux    : aliased BBS.Sim_CPU.serial.mux.mux_tty;
   print  : aliased BBS.Sim_CPU.serial.print8;
   fd     : aliased floppy_ctrl.disk_ctrl;
   clock  : aliased BBS.Sim_CPU.Clock.clock_device;
   --
   --  Set variant
   --
   procedure set_var(c : in out BBS.Sim_CPU.simulator'Class);
   --
   --  Register dump
   --
   procedure dump_reg(c : BBS.Sim_CPU.simulator'Class);
   --
   --  Do initialization
   --
   procedure init;
   --
   --  Command loop
   --
   procedure cmds;
   --
   --  Memory
   --
   procedure dump_mem(start : BBS.Sim_CPU.addr_bus);
   --
   --  For hexidecimal numbers
   --
   procedure nextValue(v : out BBS.uint32;
                       s : in out Ada.Strings.Unbounded.Unbounded_String);
   --
private
   --
   --  This needs to be set to True when on a Windows machine when using
   --  the git bash shell because it doesn't seem to handle get_immediate properly.
   --
   gitbash : constant Boolean := False;
   --
end cli_util;
