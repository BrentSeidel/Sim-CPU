--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of SimCPU.
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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
with Ada.Containers.Indefinite_Ordered_Maps;
with BBS.Sim_CPU.bus;
with BBS.Sim_CPU.io;
use type BBS.Sim_CPU.io.io_access;
package BBS.Sim_CPU.CPU.msc6502 is
   --
   --  The simple Intel 8080 simulator inheriting from Sim.simulator.
   --
   type msc6502 is new simulator with private;
   --
   memory_size : constant word := 2**16;
   --
   --  The trace level is interpreted as follows for this simulator:
   --  Bit  Use
   --   0   List instructions being traced
   --   1   List I/O operations
   --   2   Unused
   --   3   Unused
   --   4   Unused
   --   5   Unused
   --   6   Unused
   --   7   Unused
   --
   --  Variants of processor
   --
   type variants_msc6502 is (var_6502,
                             var_other);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out msc6502);
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out msc6502);
   --
   --  Called to start simulator execution at a specific address.
   --
   overriding
   procedure start(self : in out msc6502; addr : addr_bus);
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out msc6502);
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out msc6502);
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out msc6502);
   --
   --  This loads data from a file specified by "name" into the simulator memory.
   --
   overriding
   procedure load(self : in out msc6502; name : String);
   --
   --  Called to attach an I/O device to a simulator at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --  For bus:
   --    0 - I/O space (currently unimplemented)
   --    1 - Memory space
   --
   overriding
   procedure attach_io(self : in out msc6502; io_dev : BBS.Sim_CPU.io.io_access;
                       base_addr : addr_bus; bus : bus_type);
   --
   --  Attach CPU to a bus.  Index is provided for use in mult-cpu systems to
   --  identify the CPU on the bus.
   --
   overriding
   procedure attach_bus(self : in out msc6502; bus : BBS.Sim_CPU.bus.bus_access;
                       index : Natural);
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   overriding
   function name(self : in out msc6502) return String is ("msc6502");
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out msc6502) return uint32;
   --
   --  Called to get number of variants
   --
   overriding
   function variants(self : in out msc6502) return Natural is (2);
   --
   --  Called to get variant name
   --
   overriding
   function variant(self : in out msc6502; v : natural) return String;
   --
   --  Called to get current variant index
   --
   overriding
   function variant(self : in out msc6502) return Natural;
   --
   --  Called to set variant
   --
   overriding
   procedure variant(self : in out msc6502; v : natural);
   --
   --  Interrupt status.  Returns simulator dependent status of interrupts
   --
   overriding
   function intStatus(self : in out msc6502) return int32;
   --
   --  Input/Output debugging
   --
   overriding
   function lastOutAddr(self : in out msc6502) return addr_bus;
   overriding
   function lastOutData(self : in out msc6502) return data_bus;
   overriding
   procedure overrideIn(self : in out msc6502; addr : in addr_bus; data : in data_bus);
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  For memory mapped I/O devices
   --
   package io_map_type is new Ada.Containers.Indefinite_Ordered_maps
         (key_type => word, element_type => BBS.Sim_CPU.io.io_access);
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out msc6502; mem_addr : addr_bus;
                     data : data_bus);
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out msc6502; mem_addr : addr_bus)
      return data_bus;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out msc6502; num : uint32)
      return String;
   --
   --  Called to get register value as a number
   --
   overriding
   function read_reg(self : in out msc6502; num : uint32)
      return data_bus;
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   overriding
   function read_reg(self : in out msc6502; num : uint32)
      return String;
   --
   --  Called to set register value
   --
   overriding
   procedure set_reg(self : in out msc6502; num : uint32;
                     data : data_bus) is null;
   --
   --  Called to check if the CPU is halted
   --
   overriding
   function halted(self : in out msc6502) return Boolean;
   --
   --  This clears the halted flag allowing processing to continue.
   --
   overriding
   procedure continue_proc(self : in out msc6502);
   --
   --  Post an interrupt exception
   --
   INT_NIL : constant long := 0;  --  No interrupt
   INT_INT : constant long := 1;  --  Normal interrupt
   INT_NMI : constant long := 2;  --  Non-maskable interrupt
   INT_RST : constant long := 3;  --  Reset
   overriding
   procedure interrupt(self : in out msc6502; data : long);
   --
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out msc6502; addr : addr_bus);
   procedure clearBreak(self : in out msc6502; addr : addr_bus);
   --
   --  Unimplemented instruction response
   --
   procedure unimplemented(self : in out msc6502; addr : word; data : byte);
   --
   type status_word is record
      carry   : Boolean := False;
      zero    : Boolean := False;
      intdis  : Boolean := False;
      decmode : Boolean := False;
      break   : Boolean := True;
      unused  : Boolean := False;
      over    : Boolean := True;
      sign    : Boolean := False;
   end record;
   --
   for status_word use record
      carry   at 0 range 0 .. 0;
      zero    at 0 range 1 .. 1;
      intdis  at 0 range 2 .. 2;
      decmode at 0 range 3 .. 3;
      break   at 0 range 4 .. 4;
      unused  at 0 range 5 .. 5;
      over    at 0 range 6 .. 6;
      sign    at 0 range 7 .. 7;
   end record;
   --
   for status_word'Size use 8;
   --
private
   --
   type reg_id is (reg_a,     --  Accumulator (8 bits)
                   reg_psw,   --  Status word
                   reg_ix,    --  B register (8 bits)
                   reg_iy,    --  C register (8 bits)
                   reg_sp,    --  Stack pointer (8 bits)
                   reg_pc     --  Program counter (16 bits)
                   );
   --
   type mem_array is array (0 .. memory_size - 1) of byte;
   --
   type msc6502 is new simulator with record
      addr : word := 0;
      temp_addr : word := 0;
      a   : byte := 0;
      f   : status_word;  --  Flags (processor status word)
      ix  : byte := 0;
      iy  : byte := 0;
      sp  : byte := 0;
      pc  : word := 0;
      mem : mem_array := (others => 0);
      bus : BBS.Sim_CPU.bus.bus_access;
      intr         : Boolean := False;
      int_code     : long := INT_NIL;
      cpu_halt     : Boolean := False;
      break_enable : Boolean := False;
      break_point  : word;
      last_out_addr : addr_bus := 0;
      last_out_data : data_bus := 0;
      in_override  : Boolean := False;
      in_over_addr : addr_bus := 0;
      in_over_data : data_bus := 0;
      cpu_model    : variants_msc6502 := var_6502;
      io_ports     : io_map_type.Map;
   end record;
   --
   --  Interrupt vectors
   --
   vect_NMI   : constant word := 16#FFFA#;
   vect_RESET : constant word := 16#FFFC#;
   vect_IRQ   : constant word := 16#FFFE#;
   --
   --  Code for the instruction processing.
   --
   procedure decode(self : in out msc6502);
   function get_next(self : in out msc6502) return byte;
   procedure check_intr(self : in out msc6502) is null;
   --
   procedure addf(self : in out msc6502; v1 : byte);
   procedure subf(self : in out msc6502; v1 : byte);
   --
   --  All memory accesses should be routed through these functions so that they
   --  can do checks for memory-mapped I/O or shared memory.
   --
   procedure memory(self : in out msc6502; addr : word; value : byte; mode : addr_type);
   function memory(self : in out msc6502; addr : word; mode : addr_type) return byte;
   --
   --  Handle I/O port accesses
   --
   procedure port(self : in out msc6502; addr : byte; value : byte);
   function port(self : in out msc6502; addr : byte) return byte;
   --
   --  Common code for Jump, Call, and Return
   --
   procedure jump(self : in out msc6502; go : Boolean);
   procedure call(self : in out msc6502; go : Boolean);
   procedure ret(self : in out msc6502; go : Boolean);
   --
   --  Stack instructions
   --  Note that the stack is in page 1 and SP is only 8 bits, so a 1 in bit 9
   --  is implied.  Running all stack operations through here makes sure that
   --  this is uniformly applied.
   --
   stack_page : constant word := 16#100#;
   procedure push(self : in out msc6502; value : byte);
   function pull(self : in out msc6502) return byte;
   --
   --  Other utility functions
   --
   function sign_extend(t8 : byte) return word;
   --
end BBS.Sim_CPU.CPU.msc6502;
