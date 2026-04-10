--
--  Author: Brent Seidel
--  Date: 19-Jun-2025
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
with Ada.Unchecked_Conversion;
limited with BBS.Sim_CPU.bus;
package BBS.Sim_CPU.CPU is
   --
   --  The simulator object
   --
   type simulator is tagged limited private;
   type sim_access is access all simulator'Class;
   --
   --  The actual interface.  These are routines that are called under specific
   --  circumstances.  They can examine the switch register to further decide
   --  their actions and set the LED registers as desired.
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   procedure init(self : in out simulator) is Null;
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   procedure start(self : in out simulator) is Null;
   --
   --  Called to start simulator execution at a specific address.  This is made
   --  null rather than abstract so that simulators that don't use it don't need
   --  to override it.
   --
   procedure start(self : in out simulator; addr : addr_bus) is null;
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   procedure run(self : in out simulator) is Null;
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   procedure deposit(self : in out simulator) is Null;
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   procedure examine(self : in out simulator) is Null;
   --
   --  Called to load data into the simulator.
   --
   procedure load(self : in out simulator; name : String) is null;
   --
   --  Attach CPU to a bus.  Index is provided for use in mult-cpu systems to
   --  identify the CPU on the bus.  This procedure is made Null as some CPUs
   --  may not have a bus and this keeps them from having to implement this.
   --
   procedure attach_bus(self : in out simulator; bus : BBS.Sim_CPU.bus.bus_access;
                       index : Natural);
   --
   --  Request a processor reset.
   --
   procedure reset(self : in out simulator) is null;
   --
   --  Signal an interrupt from a device.
   --
   procedure interrupt(self : in out simulator; data : long) is null;
   --
   --  Allow an external user to enable or disable processing of interrupts
   --
   procedure interrupts(self : in out simulator; state : Boolean) is null;
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   function name(self : in out simulator) return String is ("No simulator");
   --
   --  Called to get number of registers
   --
   function registers(self : in out simulator) return uint32 is (0);
   --
   --  Called to get number of variants
   --
   function variants(self : in out simulator) return Natural is (1);
   --
   --  Called to get variant name
   --
   function variant(self : in out simulator; v : Natural) return String is ("No variant");
   --
   --  Called to get current variant index
   --
   function variant(self : in out simulator) return Natural is (0);
   --
   --  Called to set variant
   --
   procedure variant(self : in out simulator; v : Natural) is Null;
   --
   --  Check if simulator is halted
   --
   function halted(self : in out simulator) return Boolean is (False);
   --
   --  Force a simulator to enter a halt state (if implemented).  Can be used for
   --  some error conditions.
   --
   procedure halt(self : in out simulator) is null;
   --
   --  This clears the halted flag allowing processing to continue.
   --
   procedure continue_proc(self : in out simulator) is null;
   --
   --  Trace flags definition
   --
   --  Bit  Use
   --   0   List instructions being traced
   --   1   List I/O operations
   --   2   List data operations
   --   3   List bus specific items.
   --   4   Unused
   --   5   Unused
   --   6   Unused
   --   7   Unused
   type trace_flags is record
      instr    : Boolean;  --  Trace instructions
      io       : Boolean;  --  Trace I/O operations and devices
      data     : Boolean;  --  Trace data read/write
      bus      : Boolean;  --  Trace bus specific operations
      control  : Boolean;  --  Trace control transfers
      except   : Boolean;  --  Trace exceptions
      unused6  : Boolean;
      unused7  : Boolean;
      unused8  : Boolean;
      unused9  : Boolean;
      unused10 : Boolean;
      unused11 : Boolean;
      unused12 : Boolean;
      unused13 : Boolean;
      unused14 : Boolean;
      unused15 : Boolean;
      unused16 : Boolean;
      unused17 : Boolean;
      unused18 : Boolean;
      unused19 : Boolean;
      unused20 : Boolean;
      unused21 : Boolean;
      unused22 : Boolean;
      unused23 : Boolean;
      unused24 : Boolean;
      unused25 : Boolean;
      unused26 : Boolean;
      unused27 : Boolean;
      unused28 : Boolean;
      unused29 : Boolean;
      unused30 : Boolean;
      unused31 : Boolean;
   end record with size => 32;
   for trace_flags use record
      instr    at 0 range  0 ..  0;  --    1
      io       at 0 range  1 ..  1;  --    2
      data     at 0 range  2 ..  2;  --    4
      bus      at 0 range  3 ..  3;  --    8
      control  at 0 range  4 ..  4;  --   16
      except   at 0 range  5 ..  5;  --   32
      unused6  at 0 range  6 ..  6;  --   64
      unused7  at 0 range  7 ..  7;  --  128
      unused8  at 0 range  8 ..  8;  --  256
      unused9  at 0 range  9 ..  9;  --  512
      unused10 at 0 range 10 .. 10;
      unused11 at 0 range 11 .. 11;
      unused12 at 0 range 12 .. 12;
      unused13 at 0 range 13 .. 13;
      unused14 at 0 range 14 .. 14;
      unused15 at 0 range 15 .. 15;
      unused16 at 0 range 16 .. 16;
      unused17 at 0 range 17 .. 17;
      unused18 at 0 range 18 .. 18;
      unused19 at 0 range 19 .. 19;
      unused20 at 0 range 20 .. 20;
      unused21 at 0 range 21 .. 21;
      unused22 at 0 range 22 .. 22;
      unused23 at 0 range 23 .. 23;
      unused24 at 0 range 24 .. 24;
      unused25 at 0 range 25 .. 25;
      unused26 at 0 range 26 .. 26;
      unused27 at 0 range 27 .. 27;
      unused28 at 0 range 28 .. 28;
      unused29 at 0 range 29 .. 29;
      unused30 at 0 range 30 .. 30;
      unused31 at 0 range 31 .. 31;
   end record;
   --
   no_trace : trace_flags := (others => False);
   --
   function uint32_to_trace is new Ada.Unchecked_Conversion(source => uint32,
                                                           target => trace_flags);
   function trace_to_uint32 is new Ada.Unchecked_Conversion(source => trace_flags,
                                                           target => uint32);
   --
   --
   --  Set/Get trace level
   --
   procedure trace(self : in out simulator; l : trace_flags);
   function trace(self : in out simulator) return trace_flags;
   --
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out simulator; addr : addr_bus) is null;
   procedure clearBreak(self : in out simulator; addr : addr_bus) is null;
   --
   --  Interrupt status.  Returns simulator dependent status of interrupts
   --
   function intStatus(self : in out simulator) return int32 is (0);
   --
   --  Input/Output debugging
   --
   function lastOutAddr(self : in out simulator) return addr_bus is (0);
   function lastOutData(self : in out simulator) return data_bus is (0);
   procedure overrideIn(self : in out simulator; addr : in addr_bus; data : in data_bus) is null;
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   procedure set_mem(self : in out simulator; mem_addr : addr_bus;
                     data : data_bus) is Null;
   --
   --  Called to read a memory value
   --
   function read_mem(self : in out simulator; mem_addr : addr_bus) return
     data_bus is (0);
   --
   --  Called to get register name
   --
   function reg_name(self : in out simulator; num : uint32)
                     return String is ("No Reg");
   --
   --  Called to get register value as a number
   --
   function read_reg(self : in out simulator; num : uint32)
                     return data_bus is (0);
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   function read_reg(self : in out simulator; num : uint32)
                     return String is ("No Reg");
   --
   --  Called to set register value
   --
   procedure set_reg(self : in out simulator; num : uint32;
                     data : data_bus) is Null;
private
   --
   --  Base simulator object with stuff that all simulators need.
   --
   type simulator is tagged limited record
      trace   : trace_flags := no_trace;    --  Trace level
      bus     : access BBS.Sim_CPU.bus.bus'Class;
   end record;
   --
end;
