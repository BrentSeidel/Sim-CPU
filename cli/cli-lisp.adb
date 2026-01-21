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
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Tags;
use type Ada.Tags.Tag;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS;
use type BBS.uint16;
use type BBS.uint32;
with BBS.Lisp;
use type BBS.Lisp.int32;
use type BBS.Lisp.value_type;
with BBS.Lisp.evaluate;
with BBS.Lisp.strings;
with BBS.Sim_CPU;
with BBS.Sim_CPU.bus.mem8;
with BBS.Sim_CPU.io;
with GNAT.Sockets;
package body cli.Lisp is
   function int32_to_uint32 is new Ada.Unchecked_Conversion(source => BBS.lisp.int32,
                                                           target => BBS.Sim_CPU.long);
   function uint32_to_int32 is new Ada.Unchecked_Conversion(source => BBS.Sim_CPU.long,
                                                           target => BBS.lisp.int32);
   --
   --  Install the new lisp words into the lisp interpreter
   --
   procedure init is
   begin
      BBS.lisp.add_builtin("attach",          sim_attach'Access);
      BBS.lisp.add_builtin("disk-close",      sim_disk_close'Access);
      BBS.lisp.add_builtin("disk-geom",       sim_disk_geom'Access);
      BBS.lisp.add_builtin("disk-open",       sim_disk_open'Access);
      BBS.lisp.add_builtin("go",              sim_go'Access);
      BBS.lisp.add_builtin("halted",          sim_halted'Access);
      BBS.lisp.add_builtin("int-state",       sim_int_state'Access);
      BBS.lisp.add_builtin("last-out-addr",   sim_last_out_addr'Access);
      BBS.lisp.add_builtin("last-out-data",   sim_last_out_data'Access);
      BBS.lisp.add_builtin("memb",            sim_memb'Access);
      BBS.lisp.add_builtin("meml",            sim_meml'Access);
      BBS.lisp.add_builtin("memw",            sim_memw'Access);
      BBS.lisp.add_builtin("memll",           sim_memll'Access);
      BBS.lisp.add_builtin("memlw",           sim_memlw'Access);
      BBS.lisp.add_builtin("num-reg",         sim_num_reg'Access);
      BBS.lisp.add_builtin("override-in",     sim_override_in'Access);
      BBS.lisp.add_builtin("print-close",     sim_print_close'Access);
      BBS.lisp.add_builtin("print-open",      sim_print_open'Access);
      BBS.lisp.add_builtin("reg-val",         sim_reg_val'Access);
      BBS.lisp.add_builtin("send-int",        sim_send_int'Access);
      BBS.lisp.add_builtin("sim-cpu",         sim_cpu'Access);
      BBS.lisp.add_builtin("sim-init",        sim_init'Access);
      BBS.lisp.add_builtin("sim-load",        sim_load'Access);
      BBS.lisp.add_builtin("sim-step",        sim_step'Access);
      BBS.lisp.add_builtin("tape-close",      sim_tape_close'Access);
      BBS.lisp.add_builtin("tape-open",       sim_tape_open'Access);
      BBS.lisp.add_builtin("tape-open",       sim_tape_open'Access);
      BBS.lisp.add_builtin("mem-max",         sim_mem_max'Access);
      BBS.lisp.add_builtin("mem-limit",       sim_mem_limit'Access);
      BBS.lisp.add_builtin("set-pause-count", sim_pause_count'Access);
      BBS.lisp.add_builtin("set-pause-char",  sim_pause_char'Access);
   end;
   --
   --  Execute one instruction
   --  (sim-step)
   procedure sim_step(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
   begin
      if not cpu_selected then
         BBS.Lisp.error("memb", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      cpu.run;
      e := BBS.Lisp.NIL_ELEM;
   end;
   --
   --  Get/set memory (byte)
   --  (memb addr value)
   --  (memb addr)
   procedure sim_memb(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      value_elem : BBS.lisp.element_type;
      addr       : BBS.Sim_CPU.addr_bus;
      value      : BBS.Sim_CPU.byte;
      rest       : BBS.lisp.cons_index := s;
   begin
      if not cpu_selected then
         BBS.Lisp.error("memb", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      --
      --  Get the first and second values
      --
      addr_elem := BBS.lisp.evaluate.first_value(rest);
      value_elem := BBS.lisp.evaluate.first_value(rest);
      --
      --  Check if the address value is an integer element.
      --
      if addr_elem.kind = BBS.Lisp.V_INTEGER then
         addr := int32_to_uint32(addr_elem.i);
      else
         BBS.lisp.error("memb", "Address must be integer.");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      --
      --  Check if value exists.  If not, read memory.
      --
      if value_elem.kind = BBS.Lisp.V_NONE then
         value := BBS.Sim_CPU.byte(cpu.read_mem(addr) and 16#FF#);
         e := (kind => BBS.lisp.V_INTEGER, i => BBS.lisp.int32(value));
      else
         --
         --  Check if the value state is an integer element.
         --
         if value_elem.kind = BBS.Lisp.V_INTEGER then
            value := BBS.Sim_CPU.byte(int32_to_uint32(value_elem.i) and 16#FF#);
         else
            BBS.lisp.error("memb", "Value state must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
         end if;
         --
         --  If everything is OK, then write to memory
         --
         cpu.set_mem(addr, BBS.Sim_CPU.data_bus(value));
         e := BBS.Lisp.NIL_ELEM;
      end if;
   end;
   --
   --  Get/set memory (word/long) MSB first
   --  (memw addr value)
   --  (memw addr)
   --  (meml addr value)
   --  (meml addr)
   procedure sim_memw(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      value_elem : BBS.lisp.element_type;
      addr       : BBS.Sim_CPU.addr_bus;
      value      : BBS.Sim_CPU.word;
      rest       : BBS.lisp.cons_index := s;
   begin
      if not cpu_selected then
         BBS.Lisp.error("memw", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      --
      --  Get the first and second values
      --
      addr_elem := BBS.lisp.evaluate.first_value(rest);
      value_elem := BBS.lisp.evaluate.first_value(rest);
      --
      --  Check if the address value is an integer element.
      --
      if addr_elem.kind = BBS.Lisp.V_INTEGER then
         addr := int32_to_uint32(addr_elem.i);
      else
         BBS.lisp.error("memw", "Address must be integer.");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      --
      --  Check if value exists.  If not, read memory.
      --
      if value_elem.kind = BBS.Lisp.V_NONE then
         value := BBS.Sim_CPU.word(cpu.read_mem(addr) and 16#ff#)*16#100# +
                  BBS.Sim_CPU.word(cpu.read_mem(addr+1) and 16#ff#);
         e := (kind => BBS.lisp.V_INTEGER, i => BBS.lisp.int32(value));
      else
         --
         --  Check if the value state is an integer element.
         --
         if value_elem.kind = BBS.Lisp.V_INTEGER then
            value := BBS.Sim_CPU.word(int32_to_uint32(value_elem.i) and 16#ffff#);
         else
            BBS.lisp.error("memw", "Value state must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
         end if;
         --
         --  If everything is OK, then write to memory
         --
         cpu.set_mem(addr, BBS.Sim_CPU.data_bus((value/16#100#) and 16#ff#));
         cpu.set_mem(addr+1, BBS.Sim_CPU.data_bus(value and 16#ff#));
         e := BBS.Lisp.NIL_ELEM;
      end if;
   end;
   --
   procedure sim_meml(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      value_elem : BBS.lisp.element_type;
      addr       : BBS.Sim_CPU.addr_bus;
      value      : BBS.Sim_CPU.long;
      rest       : BBS.lisp.cons_index := s;
   begin
      if not cpu_selected then
         BBS.Lisp.error("meml", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      --
      --  Get the first and second values
      --
      addr_elem := BBS.lisp.evaluate.first_value(rest);
      value_elem := BBS.lisp.evaluate.first_value(rest);
      --
      --  Check if the address value is an integer element.
      --
      if addr_elem.kind = BBS.Lisp.V_INTEGER then
         addr := int32_to_uint32(addr_elem.i);
      else
         BBS.lisp.error("meml", "Address must be integer.");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      --
      --  Check if value exists.  If not, read memory.
      --
      if value_elem.kind = BBS.Lisp.V_NONE then
         value := BBS.Sim_CPU.long(cpu.read_mem(addr) and 16#ff#)*16#0100_0000# +
                  BBS.Sim_CPU.long(cpu.read_mem(addr+1) and 16#ff#)*16#0001_0000# +
                  BBS.Sim_CPU.long(cpu.read_mem(addr+2) and 16#ff#)*16#0000_0100# +
                  BBS.Sim_CPU.long(cpu.read_mem(addr+3) and 16#ff#);
         e := (kind => BBS.lisp.V_INTEGER, i => uint32_to_int32(value));
      else
         --
         --  Check if the value state is an integer element.
         --
         if value_elem.kind = BBS.Lisp.V_INTEGER then
            value := int32_to_uint32(value_elem.i);
         else
            BBS.lisp.error("meml", "Value state must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
         end if;
         --
         --  If everything is OK, then write to memory
         --
         cpu.set_mem(addr,   BBS.Sim_CPU.data_bus((value/16#0100_0000#) and 16#ff#));
         cpu.set_mem(addr+1, BBS.Sim_CPU.data_bus((value/16#0001_0000#) and 16#ff#));
         cpu.set_mem(addr+2, BBS.Sim_CPU.data_bus((value/16#0000_0100#) and 16#ff#));
         cpu.set_mem(addr+3, BBS.Sim_CPU.data_bus(value and 16#ff#));
         e := BBS.Lisp.NIL_ELEM;
      end if;
   end;
   --
   --  Get/set memory (word/long) LSB first
   --  (memlw addr value)
   --  (memlw addr)
   --  (memll addr value)
   --  (memll addr)
   procedure sim_memlw(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      value_elem : BBS.lisp.element_type;
      addr       : BBS.Sim_CPU.addr_bus;
      value      : BBS.Sim_CPU.word;
      rest       : BBS.lisp.cons_index := s;
   begin
      if not cpu_selected then
         BBS.Lisp.error("memlw", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      --
      --  Get the first and second values
      --
      addr_elem := BBS.lisp.evaluate.first_value(rest);
      value_elem := BBS.lisp.evaluate.first_value(rest);
      --
      --  Check if the address value is an integer element.
      --
      if addr_elem.kind = BBS.Lisp.V_INTEGER then
         addr := int32_to_uint32(addr_elem.i);
      else
         BBS.lisp.error("memlw", "Address must be integer.");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      --
      --  Check if value exists.  If not, read memory.
      --
      if value_elem.kind = BBS.Lisp.V_NONE then
         value := BBS.Sim_CPU.word(cpu.read_mem(addr+1) and 16#ff#)*16#100# +
                  BBS.Sim_CPU.word(cpu.read_mem(addr) and 16#ff#);
         e := (kind => BBS.lisp.V_INTEGER, i => BBS.lisp.int32(value));
      else
         --
         --  Check if the value state is an integer element.
         --
         if value_elem.kind = BBS.Lisp.V_INTEGER then
            value := BBS.Sim_CPU.word(int32_to_uint32(value_elem.i) and 16#ffff#);
         else
            BBS.lisp.error("memlw", "Value state must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
         end if;
         --
         --  If everything is OK, then write to memory
         --
         cpu.set_mem(addr+1, BBS.Sim_CPU.data_bus((value/16#100#) and 16#ff#));
         cpu.set_mem(addr, BBS.Sim_CPU.data_bus(value and 16#ff#));
         e := BBS.Lisp.NIL_ELEM;
      end if;
   end;
   --
   procedure sim_memll(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      value_elem : BBS.lisp.element_type;
      addr       : BBS.Sim_CPU.addr_bus;
      value      : BBS.Sim_CPU.long;
      rest       : BBS.lisp.cons_index := s;
   begin
      if not cpu_selected then
         BBS.Lisp.error("memll", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      --
      --  Get the first and second values
      --
      addr_elem := BBS.lisp.evaluate.first_value(rest);
      value_elem := BBS.lisp.evaluate.first_value(rest);
      --
      --  Check if the address value is an integer element.
      --
      if addr_elem.kind = BBS.Lisp.V_INTEGER then
         addr := int32_to_uint32(addr_elem.i);
      else
         BBS.lisp.error("memll", "Address must be integer.");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      --
      --  Check if value exists.  If not, read memory.
      --
      if value_elem.kind = BBS.Lisp.V_NONE then
         value := BBS.Sim_CPU.long(cpu.read_mem(addr+3) and 16#ff#)*16#0100_0000# +
                  BBS.Sim_CPU.long(cpu.read_mem(addr+2) and 16#ff#)*16#0001_0000# +
                  BBS.Sim_CPU.long(cpu.read_mem(addr+1) and 16#ff#)*16#0000_0100# +
                  BBS.Sim_CPU.long(cpu.read_mem(addr) and 16#ff#);
         e := (kind => BBS.lisp.V_INTEGER, i => uint32_to_int32(value));
      else
         --
         --  Check if the value state is an integer element.
         --
         if value_elem.kind = BBS.Lisp.V_INTEGER then
            value := int32_to_uint32(value_elem.i);
         else
            BBS.lisp.error("memll", "Value state must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
         end if;
         --
         --  If everything is OK, then write to memory
         --
         cpu.set_mem(addr+3,   BBS.Sim_CPU.data_bus((value/16#0100_0000#) and 16#ff#));
         cpu.set_mem(addr+2, BBS.Sim_CPU.data_bus((value/16#0001_0000#) and 16#ff#));
         cpu.set_mem(addr+1, BBS.Sim_CPU.data_bus((value/16#0000_0100#) and 16#ff#));
         cpu.set_mem(addr, BBS.Sim_CPU.data_bus(value and 16#ff#));
         e := BBS.Lisp.NIL_ELEM;
      end if;
   end;
   --
   --  Get/set memory limits
   --  (mem-max)
   --  (mem-limit value)
   --  (mem-limit)
   --
   procedure sim_mem_max(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
   begin
      if not cpu_selected then
         BBS.Lisp.error("mem-max", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      e := (kind => BBS.lisp.V_INTEGER, i => uint32_to_int32(bus.mem_size));
   end;
   --
   procedure sim_mem_limit(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      value_elem  : BBS.lisp.element_type;
      value       : BBS.Sim_CPU.addr_bus;
      rest        : BBS.lisp.cons_index := s;
   begin
      if not cpu_selected then
         BBS.Lisp.error("mem-limit", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      --
      --  Check if value exists.  If not, read memory.
      --
      value_elem := BBS.lisp.evaluate.first_value(rest);
      if value_elem.kind = BBS.Lisp.V_NONE then
         e := (kind => BBS.lisp.V_INTEGER, i => uint32_to_int32(bus.get_max_addr));
      else
         --
         --  Check if the value state is an integer element.
         --
         if value_elem.kind = BBS.Lisp.V_INTEGER then
            value := int32_to_uint32(value_elem.i);
         else
            BBS.lisp.error("mem-limit", "Value state must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
         end if;
         --
         --  If everything is OK, then write to memory
         --
         bus.set_max_addr(value);
         e := BBS.Lisp.NIL_ELEM;
      end if;
   end;
   --
   --  Set execution address
   --  (go address)
   procedure sim_go(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      addr       : BBS.Sim_CPU.addr_bus;
      rest       : BBS.lisp.cons_index := s;
   begin
      if not cpu_selected then
         BBS.Lisp.error("go", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      --
      --  Check if the address value is an integer element.
      --
      addr_elem := BBS.lisp.evaluate.first_value(rest);
      if addr_elem.kind = BBS.Lisp.V_INTEGER then
         addr := int32_to_uint32(addr_elem.i);
      else
         BBS.lisp.error("go", "Address must be integer.");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      CPU.start(addr);
      e := BBS.Lisp.NIL_ELEM;
   end;
   --
   --  Read register value (index is simulator dependent)
   --  (reg-val index)
   procedure sim_reg_val(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      index_elem : BBS.lisp.element_type;
      index      : BBS.Sim_CPU.addr_bus;
      value      : BBS.Sim_CPU.data_bus;
      rest       : BBS.lisp.cons_index := s;
      num_reg    : BBS.Sim_CPU.long;
   begin
      if not cpu_selected then
         BBS.Lisp.error("reg-val", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      num_reg := cpu.registers;
      --
      --  Check if the index value is an integer element.
      --
      index_elem := BBS.lisp.evaluate.first_value(rest);
      if index_elem.kind = BBS.Lisp.V_INTEGER then
         index := int32_to_uint32(index_elem.i);
      else
         BBS.lisp.error("reg-val", "Index must be integer.");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      --
      --  Check if the index value is in range.
      --
      if index > (num_reg - 1) then
         BBS.lisp.error("reg-val", "Index out of range.");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_RANGE);
         return;
      end if;
      value := cpu.read_reg(index);
      e := (kind => BBS.lisp.V_INTEGER, i => uint32_to_int32(value));
   end;
   --
   --  Return number of registers
   --  (num-reg)
   procedure sim_num_reg(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
   begin
      if not cpu_selected then
         BBS.Lisp.error("num-reg", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      e := (kind => BBS.lisp.V_INTEGER, i => uint32_to_int32(cpu.registers));
   end;
   --
   --  Return or clear simulator halted state.  Note that this can't be
   --  used to set a halted state.
   --  (halted state)
   --  (halted)
   procedure sim_halted(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      state_elem : BBS.lisp.element_type;
      state      : Boolean;
      rest       : BBS.lisp.cons_index := s;
   begin
      if not cpu_selected then
         BBS.Lisp.error("halted", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      --
      --  Check if the address value is an boolean element.
      --
      state_elem := BBS.lisp.evaluate.first_value(rest);
      if state_elem.kind = BBS.Lisp.V_NONE then
         e := (kind => BBS.Lisp.V_BOOLEAN, b => cpu.halted);
      elsif state_elem.kind = BBS.Lisp.V_BOOLEAN then
         state := state_elem.b;
         if not state then
            cpu.continue_proc;
         end if;
         e := BBS.Lisp.NIL_ELEM;
      else
         BBS.lisp.error("halted", "State must be boolean.");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
      end if;
   end;
   --
   --  Call the simulator's init routine
   --  (sim-init)
   procedure sim_init(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
   begin
      if not cpu_selected then
         BBS.Lisp.error("sim-init", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      cpu.init;
      e := BBS.Lisp.NIL_ELEM;
   end;
   --
   --  Get the interrupt status
   --  (int-state)
   procedure sim_int_state(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
   begin
      if not cpu_selected then
         BBS.Lisp.error("int-state", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      e := (kind => BBS.lisp.V_INTEGER, i => BBS.lisp.int32(cpu.intStatus));
   end;
   --
   --  Send an interrupt
   --  (send-int value)
   procedure sim_send_int(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      elem : BBS.Lisp.element_type;
      rest : BBS.lisp.cons_index := s;
   begin
      if not cpu_selected then
         BBS.Lisp.error("send-int", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      elem := BBS.Lisp.evaluate.first_value(rest);
      if elem.kind /= BBS.Lisp.V_INTEGER then
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      cpu.interrupt(int32_to_uint32(elem.i));
      e := BBS.Lisp.NIL_ELEM;
   end;
   --
   --  Get last output address and data
   --  (last-out-addr)
   --  (last-out-data)
   procedure sim_last_out_addr(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
   begin
      if not cpu_selected then
         BBS.Lisp.error("last-out-addr", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      e := (kind => BBS.lisp.V_INTEGER, i => BBS.lisp.int32(cpu.lastOutAddr));
   end;
   --
   procedure sim_last_out_data(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
   begin
      if not cpu_selected then
         BBS.Lisp.error("last-out-data", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      e := (kind => BBS.lisp.V_INTEGER, i => BBS.lisp.int32(cpu.lastOutData));
   end;
   --
   --  Load a file using simulator specific load command
   --  (load filename)
   procedure sim_load(e : out BBS.Lisp.element_type; s : BBS.Lisp.cons_index) is
      elem : BBS.Lisp.element_type;
      rest : BBS.lisp.cons_index := s;
   begin
      if not cpu_selected then
         BBS.Lisp.error("load", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      elem := BBS.Lisp.evaluate.first_value(rest);
      if elem.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("load", "Filename must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      declare
         str : String := BBS.Lisp.Strings.lisp_to_str(elem.s);
      begin
         Ada.Text_IO.Put_Line("Loading file: <" & str & ">");
         cpu.load(str);
      end;
      e := BBS.Lisp.NIL_ELEM;
   end;
   --
   --  Override input data for address (one time only)
   -- (override-in addr data)
   procedure sim_override_in(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      elem : BBS.lisp.element_type;
      rest : BBS.lisp.cons_index := s;
      addr : BBS.Sim_CPU.addr_bus;
      data : BBS.Sim_CPU.data_bus;
   begin
      if not cpu_selected then
         BBS.Lisp.error("override-in", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      elem := BBS.lisp.evaluate.first_value(rest);
      if elem.kind = BBS.Lisp.V_INTEGER then
         addr := int32_to_uint32(elem.i);
      else
            BBS.lisp.error("override-in", "Address must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
      end if;
      elem := BBS.lisp.evaluate.first_value(rest);
      if elem.kind = BBS.Lisp.V_INTEGER then
         data := int32_to_uint32(elem.i);
      else
            BBS.lisp.error("override-in", "Data must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
      end if;
      cpu.overrideIn(addr, data);
      e := BBS.Lisp.NIL_ELEM;
   end;
   --
   --  Select which simulator CPU to use.  This will need to be updated as more
   --  simulators are added.
   --  (sim-cpu cpu)
   procedure sim_cpu(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      elem : BBS.Lisp.element_type;
      rest : BBS.lisp.cons_index := s;
   begin
      if cpu_selected then
         BBS.Lisp.error("sim-cpu", "CPU already selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      elem := BBS.Lisp.evaluate.first_value(rest);
      if elem.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("sim-cpu", "CPU name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      declare
         name : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(elem.s));
      begin
         if name = "8080" then
            cpu := new BBS.Sim_CPU.CPU.i8080.i8080;
            bus := new BBS.Sim_CPU.bus.mem8.mem8io(2**16);
            cpu.attach_bus(bus, 1);
            cpu.variant(0);
         elsif name = "8085" then
            cpu := new BBS.Sim_CPU.CPU.i8080.i8080;
            bus := new BBS.Sim_CPU.bus.mem8.mem8io(2**16);
            cpu.attach_bus(bus, 1);
            cpu.variant(1);
         elsif name = "Z80" then
            cpu := new BBS.Sim_CPU.CPU.i8080.i8080;
            bus := new BBS.Sim_CPU.bus.mem8.mem8io(2**16);
            cpu.attach_bus(bus, 1);
            cpu.variant(2);
         elsif name = "68000" then
            cpu := new BBS.Sim_CPU.CPU.m68000.m68000;
            bus := new BBS.Sim_CPU.bus.mem8.mem8mem(2**24);
            cpu.attach_bus(bus, 1);
            cpu.variant(0);
         elsif name = "68008" then
            cpu := new BBS.Sim_CPU.CPU.m68000.m68000;
            bus := new BBS.Sim_CPU.bus.mem8.mem8mem(2**20);
            cpu.attach_bus(bus, 1);
            cpu.variant(1);
         elsif name = "6502" then
            cpu := new BBS.Sim_CPU.CPU.msc6502.msc6502;
            bus := new BBS.Sim_CPU.bus.mem8.mem8mem(2**16);
            cpu.attach_bus(bus, 1);
            cpu.variant(0);
         elsif name = "PDP-11/TEST" then
            cpu := new BBS.Sim_CPU.CPU.pdp11.pdp11;
            bus := new BBS.Sim_CPU.bus.pdp11.unibus(2**16);
            cpu.attach_bus(bus, 1);
            cpu.variant(0);
         else
            BBS.Lisp.error("sim-cpu", "Unrecognized CPU name");
            e := BBS.Lisp.make_error(BBS.Lisp.ERR_RANGE);
            return;
         end if;
      end;
      cli.cpu.init;
      cpu_selected := True;
      Ada.Text_IO.Put_Line("Simulator name: " & cli.cpu.name);
      Ada.Text_IO.Put_Line("Simulator variant: " & cli.cpu.variant(cli.cpu.variant));
      e := BBS.Lisp.NIL_ELEM;
   end;
   --
   --  Attach I/O devices at a specified address and bus
   --  (attach device address bus [device specific])
   --  ATTACH <device> <addr> <bus> [<dev-specific>]
   procedure sim_attach(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr : BBS.Lisp.element_type;
      bus_name  : BBS.Lisp.element_type;
      dev  : BBS.Lisp.element_type;
      elem : BBS.Lisp.element_type;
      rest : BBS.lisp.cons_index := s;
   begin
      if not cpu_selected then
         BBS.Lisp.error("attach", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      dev  := BBS.Lisp.evaluate.first_value(rest);
      if dev.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("attach", "Device name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      addr := BBS.Lisp.evaluate.first_value(rest);
      if addr.kind /= BBS.Lisp.V_INTEGER then
         BBS.Lisp.error("attach", "Address must be an integer");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      bus_name  := BBS.Lisp.evaluate.first_value(rest);
      if bus_name.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("attach", "Address bus must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      declare
         address  : constant BBS.Sim_CPU.addr_bus := int32_to_uint32(addr.i);
         addr_bus : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(bus_name.s));
         device   : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(dev.s));
         dev_bus  : BBS.Sim_CPU.bus_type;
         tel    : BBS.Sim_CPU.io.serial.telnet.telnet_access;
         dl11   : BBS.Sim_CPU.io.serial.DL11.dl11_access;
         kw11   : BBS.Sim_CPU.io.clock.KW11.kw11_access;
         fd     : floppy_ctrl.fd_access;
         ptp    : BBS.Sim_CPU.io.serial.tape8_access;
         mux    : BBS.Sim_CPU.io.serial.mux.mux_access;
         clk    : BBS.Sim_CPU.io.clock.clock_access;
         prn    : BBS.Sim_CPU.io.serial.print8_access;
         usern  : BBS.uint32;
      begin
         if addr_bus = "MEM" then
            dev_bus := BBS.Sim_CPU.BUS_MEMORY;
         elsif addr_bus = "IO" then
            dev_bus := BBS.Sim_CPU.BUS_IO;
         else
            BBS.Lisp.error("attach", "Unrecognized bus type.");
            e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
         end if;
         if device = "TEL" then
            elem := BBS.Lisp.evaluate.first_value(rest);
            if elem.kind /= BBS.Lisp.V_INTEGER then
               BBS.Lisp.error("attach", "TEL missing telnet port number.");
               e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
               return;
            end if;
            usern := int32_to_uint32(elem.i);
            tel := new BBS.Sim_CPU.io.serial.telnet.tel_tty;
            add_device(BBS.Sim_CPU.io.io_access(tel));
            bus.attach_io(BBS.Sim_CPU.io.io_access(tel), address, dev_bus);
            tel.setOwner(cpu);
            tel.init(tel, GNAT.Sockets.Port_Type(usern));
            elem := BBS.Lisp.evaluate.first_value(rest);
            if elem.kind = BBS.Lisp.V_INTEGER then
               tel.setException(int32_to_uint32(elem.i));
            end if;
         elsif device = "DL11" then
            elem := BBS.Lisp.evaluate.first_value(rest);
            if elem.kind /= BBS.Lisp.V_INTEGER then
               BBS.Lisp.error("attach", "DL11 missing telnet port number.");
               e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
               return;
            end if;
            usern := int32_to_uint32(elem.i);
            dl11 := new BBS.Sim_CPU.io.serial.DL11.DL11x;
            add_device(BBS.Sim_CPU.io.io_access(dl11));
            bus.attach_io(BBS.Sim_CPU.io.io_access(dl11), address, dev_bus);
            dl11.setOwner(cpu);
            dl11.init(dl11, GNAT.Sockets.Port_Type(usern));
            elem := BBS.Lisp.evaluate.first_value(rest);
            if elem.kind = BBS.Lisp.V_INTEGER then
               dl11.setException(int32_to_uint32(elem.i));
            end if;
         elsif device = "MUX" then
            elem := BBS.Lisp.evaluate.first_value(rest);
            if elem.kind /= BBS.Lisp.V_INTEGER then
               BBS.Lisp.error("attach", "MUX missing telnet port number.");
               e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
               return;
            end if;
            usern := int32_to_uint32(elem.i);
            mux := new BBS.Sim_CPU.io.serial.mux.mux_tty;
            add_device(BBS.Sim_CPU.io.io_access(mux));
            bus.attach_io(BBS.Sim_CPU.io.io_access(mux), address, dev_bus);
            mux.setOwner(cpu);
            mux.init(mux, GNAT.Sockets.Port_Type(usern));
            elem := BBS.Lisp.evaluate.first_value(rest);
            if elem.kind = BBS.Lisp.V_INTEGER then
               mux.setException(int32_to_uint32(elem.i));
            end if;
         elsif device = "FD" then
            elem := BBS.Lisp.evaluate.first_value(rest);
            if elem.kind /= BBS.Lisp.V_INTEGER then
               BBS.Lisp.error("attach", "FD missing number of drives.");
               e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
               return;
            end if;
            usern := int32_to_uint32(elem.i);
            if usern > 15 then
               BBS.Lisp.error("attach", "FD number of drives greater than 15.");
               e := BBS.Lisp.make_error(BBS.Lisp.ERR_RANGE);
               return;
            end if;
            fd := new floppy_ctrl.fd_ctrl(max_num => Integer(usern));
            add_device(BBS.Sim_CPU.io.io_access(fd));
            bus.attach_io(BBS.Sim_CPU.io.io_access(fd), address, dev_bus);
            fd.setOwner(cpu);
            elem := BBS.Lisp.evaluate.first_value(rest);
            if elem.kind = BBS.Lisp.V_INTEGER then
               fd.setException(int32_to_uint32(elem.i));
            end if;
         elsif device = "PTP" then
            ptp := new BBS.Sim_CPU.io.serial.tape8;
            add_device(BBS.Sim_CPU.io.io_access(ptp));
            bus.attach_io(BBS.Sim_CPU.io.io_access(ptp), address, dev_bus);
         elsif device = "CLK" then
            clk := new BBS.Sim_CPU.io.clock.clock_device;
            add_device(BBS.Sim_CPU.io.io_access(clk));
            bus.attach_io(BBS.Sim_CPU.io.io_access(clk), address, dev_bus);
            elem := BBS.Lisp.evaluate.first_value(rest);
            if elem.kind = BBS.Lisp.V_INTEGER then
               clk.setException(int32_to_uint32(elem.i));
            end if;
         elsif device = "KW11" then
            kw11 := new BBS.Sim_CPU.io.clock.kw11.kw11;
            add_device(BBS.Sim_CPU.io.io_access(kw11));
            bus.attach_io(BBS.Sim_CPU.io.io_access(kw11), 8#777546#, BBS.Sim_CPU.BUS_MEMORY);
            kw11.setException(8#100#);
         elsif device = "PRN" then
            prn := new BBS.Sim_CPU.io.serial.print8;
            add_device(BBS.Sim_CPU.io.io_access(prn));
            bus.attach_io(BBS.Sim_CPU.io.io_access(prn), address, dev_bus);
         else
            Ada.Text_IO.Put_Line("ATTACH unrecognized device");
         end if;
      end;
   end;
   --
   --  Attach a file to a disk drive
   --  (disk-open <device> <drive> <file>)
   procedure sim_disk_open(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      devname : BBS.Lisp.element_type;
      fname : BBS.Lisp.element_type;
      drive : BBS.Lisp.element_type;
      elem  : BBS.Lisp.element_type;
      rest  : BBS.lisp.cons_index := s;
      dev   : BBS.Sim_CPU.io.io_access;
      fd    : floppy_ctrl.fd_access;
   begin
      if not cpu_selected then
         BBS.Lisp.error("disk-open", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      devname := BBS.Lisp.evaluate.first_value(rest);
      if devname.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("disk-open", "Device name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      drive := BBS.Lisp.evaluate.first_value(rest);
      if drive.kind /= BBS.Lisp.V_INTEGER then
         BBS.Lisp.error("disk-open", "Unit number must be an integer");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      fname := BBS.Lisp.evaluate.first_value(rest);
      if fname.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("disk-open", "File name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      declare
         name : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(devname.s));
         pass : Boolean;
      begin
         dev := find_dev_by_name(Ada.Strings.Unbounded.To_Unbounded_String(name), pass);
         if not pass then
            BBS.Lisp.error("disk-open", "unable to find device.");
            e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
            return;
         end if;
      end;
      if dev'Tag /= floppy_ctrl.fd_ctrl'Tag then
         BBS.Lisp.error("disk-open", "device is not a disk controller.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      fd := floppy_ctrl.fd_access(dev);
      if (drive.i > BBS.Lisp.int32(floppy_ctrl.drive_num'Last)) or (drive.i < 0) then
         BBS.Lisp.error("disk-open", "FD number of drives greater than 15.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_RANGE);
         return;
      end if;
      --
      --  After all that error checking, finally close the file.
      --
      fd.open(floppy_ctrl.drive_num(drive.i), floppy_ctrl.floppy8_geom,
              BBS.Lisp.Strings.lisp_to_str(fname.s));
   end;
   --
   --  Close a file attached to a disk drive
   --  (disk-close <device> <drive>)
   procedure sim_disk_close(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      devname : BBS.Lisp.element_type;
      drive : BBS.Lisp.element_type;
      elem  : BBS.Lisp.element_type;
      rest  : BBS.lisp.cons_index := s;
      dev   : BBS.Sim_CPU.io.io_access;
      fd    : floppy_ctrl.fd_access;
   begin
      if not cpu_selected then
         BBS.Lisp.error("disk-close", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      devname := BBS.Lisp.evaluate.first_value(rest);
      if devname.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("disk-close", "Device name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      drive := BBS.Lisp.evaluate.first_value(rest);
      if drive.kind /= BBS.Lisp.V_INTEGER then
         BBS.Lisp.error("disk-close", "Unit number must be an integer");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      declare
         name : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(devname.s));
         pass : Boolean;
      begin
         dev := find_dev_by_name(Ada.Strings.Unbounded.To_Unbounded_String(name), pass);
         if not pass then
            BBS.Lisp.error("disk-close", "unable to find device.");
            e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
            return;
         end if;
      end;
      if dev'Tag /= floppy_ctrl.fd_ctrl'Tag then
         BBS.Lisp.error("disk-close", "device is not a disk controller.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      fd := floppy_ctrl.fd_access(dev);
      if (drive.i > BBS.Lisp.int32(floppy_ctrl.drive_num'Last)) or (drive.i < 0) then
         BBS.Lisp.error("disk-close", "FD number of drives greater than 15.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_RANGE);
         return;
      end if;
      --
      --  After all that error checking, finally close the file.
      --
      fd.close(floppy_ctrl.drive_num(drive.i));
   end;
   --
   --  Set the geometry of a disk drive
   --  (disk-geom <device> <drive> <geometry)
   procedure sim_disk_geom(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      devname : BBS.Lisp.element_type;
      drive : BBS.Lisp.element_type;
      elem  : BBS.Lisp.element_type;
      geom  : BBS.Lisp.element_type;
      rest  : BBS.lisp.cons_index := s;
      dev   : BBS.Sim_CPU.io.io_access;
      fd    : floppy_ctrl.fd_access;
   begin
      if not cpu_selected then
         BBS.Lisp.error("disk-geom", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      devname := BBS.Lisp.evaluate.first_value(rest);
      if devname.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("disk-geom", "Device name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      drive := BBS.Lisp.evaluate.first_value(rest);
      if drive.kind /= BBS.Lisp.V_INTEGER then
         BBS.Lisp.error("disk-geom", "Unit number must be an integer");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      geom := BBS.Lisp.evaluate.first_value(rest);
      if (geom.kind /= BBS.Lisp.V_LIST) and (geom.kind /= BBS.Lisp.V_STRING) then
         BBS.Lisp.error("disk-geom", "Geometry must be a list or string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      declare
         name : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(devname.s));
         pass : Boolean;
      begin
         dev := find_dev_by_name(Ada.Strings.Unbounded.To_Unbounded_String(name), pass);
         if not pass then
            BBS.Lisp.error("disk-geom", "unable to find device.");
            e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
            return;
         end if;
      end;
      if dev'Tag /= floppy_ctrl.fd_ctrl'Tag then
         BBS.Lisp.error("disk-geom", "device is not a disk controller.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      fd := floppy_ctrl.fd_access(dev);
      if (drive.i > BBS.Lisp.int32(floppy_ctrl.drive_num'Last)) or (drive.i < 0) then
         BBS.Lisp.error("disk-geom", "FD number of drives greater than 15.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_RANGE);
         return;
      end if;
      if geom.kind = BBS.Lisp.V_STRING then
         declare
            gname : String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(geom.s));
         begin
            if gname = "IBM" then
               fd.setGeometry(floppy_ctrl.drive_num(drive.i), floppy_ctrl.floppy8_geom);
            elsif gname = "HD" then
               fd.setGeometry(floppy_ctrl.drive_num(drive.i), floppy_ctrl.hd_geom);
            else
               BBS.Lisp.error("disk-geom", "Unrecognized disk geometry name.");
               e := BBS.Lisp.make_error(BBS.Lisp.ERR_RANGE);
               return;
            end if;
         end;
      else  --  It must be a V_LIST.  Checked for others above.
         declare
            track : BBS.Lisp.element_type;
            sect  : BBS.Lisp.element_type;
            head  : BBS.Lisp.element_type;
            geomt : floppy_ctrl.geometry;
         begin
            track := BBS.Lisp.evaluate.first_value(geom.l);
            if track.kind /= BBS.Lisp.V_INTEGER then
               BBS.Lisp.error("disk-geom", "Track count must be an integer");
               e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
               return;
            end if;
            if (track.i > 16#FFFF#) or (track.i <= 0) then
               BBS.Lisp.error("disk-geom", "Track count out of range");
               e := BBS.Lisp.make_error(BBS.Lisp.ERR_RANGE);
               return;
            end if;
            geomt.tracks := BBS.Sim_CPU.word(int32_to_uint32(track.i) and 16#FFFF#);
            sect := BBS.Lisp.evaluate.first_value(geom.l);
            if sect.kind /= BBS.Lisp.V_INTEGER then
               BBS.Lisp.error("disk-geom", "Sector count must be an integer");
               e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
               return;
            end if;
            if (sect.i > 16#FFFF#) or (sect.i <= 0) then
               BBS.Lisp.error("disk-geom", "Sector count out of range");
               e := BBS.Lisp.make_error(BBS.Lisp.ERR_RANGE);
               return;
            end if;
            geomt.sectors := BBS.Sim_CPU.word(int32_to_uint32(sect.i) and 16#FFFF#);
            head := BBS.Lisp.evaluate.first_value(geom.l);
            if head.kind /= BBS.Lisp.V_INTEGER then
               BBS.Lisp.error("disk-geom", "Sector count must be an integer");
               e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
               return;
            end if;
               --
               --  Currently the number of heads is ignored, so just set to zero
               --  and don't bother range checking.
               --
               geomt.heads := 0;
               fd.setGeometry(floppy_ctrl.drive_num(drive.i), geomt);
            end;
         --
         --  TODO:  Add code to decode a list of (track, sector, heads) into a
         --         geometry.
         null;
      end if;
   end;
   --
   --  Attach a file to a tape drive
   --  (tape-open <device> <drive> <file>)
   procedure sim_tape_open(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      devname : BBS.Lisp.element_type;
      fname : BBS.Lisp.element_type;
      drive : BBS.Lisp.element_type;
      elem  : BBS.Lisp.element_type;
      rest  : BBS.lisp.cons_index := s;
      dev   : BBS.Sim_CPU.io.io_access;
      tape  : BBS.Sim_CPU.io.serial.tape8_access;
   begin
      if not cpu_selected then
         BBS.Lisp.error("tape-open", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      devname := BBS.Lisp.evaluate.first_value(rest);
      if devname.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("tape-open", "Device name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      drive := BBS.Lisp.evaluate.first_value(rest);
      if drive.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("tape-open", "Unit name must be a string.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      fname := BBS.Lisp.evaluate.first_value(rest);
      if fname.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("tape-open", "File name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      declare
         name : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(devname.s));
         pass : Boolean;
      begin
         dev := find_dev_by_name(Ada.Strings.Unbounded.To_Unbounded_String(name), pass);
         if not pass then
            BBS.Lisp.error("disk-open", "unable to find device.");
            e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
            return;
         end if;
      end;
      if dev'Tag /= BBS.Sim_CPU.io.serial.tape8'Tag then
         BBS.Lisp.error("tape-open", "device is not a tape controller.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      tape := BBS.Sim_CPU.io.serial.tape8_access(dev);      declare
         name : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(drive.s));
      begin
         if name = "RDR" then
            tape.openIn(BBS.Lisp.Strings.lisp_to_str(fname.s));
         elsif name = "PUN" then
            tape.openOut(BBS.Lisp.Strings.lisp_to_str(fname.s));
         else
            BBS.Lisp.error("tape-open", "Unknown drive.");
            e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
            return;
         end if;
      end;
   end;
   --
   --  Close a file attached to a tape drive
   --  (tape-close <device> <drive>)
   procedure sim_tape_close(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      devname : BBS.Lisp.element_type;
      drive : BBS.Lisp.element_type;
      elem  : BBS.Lisp.element_type;
      rest  : BBS.lisp.cons_index := s;
      dev   : BBS.Sim_CPU.io.io_access;
      tape  : BBS.Sim_CPU.io.serial.tape8_access;
   begin
      if not cpu_selected then
         BBS.Lisp.error("tape-close", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      devname := BBS.Lisp.evaluate.first_value(rest);
      if devname.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("tape-close", "Device name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      drive := BBS.Lisp.evaluate.first_value(rest);
      if drive.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("tape-close", "Unit name must be a string.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      declare
         name : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(devname.s));
         pass : Boolean;
      begin
         dev := find_dev_by_name(Ada.Strings.Unbounded.To_Unbounded_String(name), pass);
         if not pass then
            BBS.Lisp.error("tape-close", "unable to find device.");
            e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
            return;
         end if;
      end;
      if dev'Tag /= BBS.Sim_CPU.io.serial.tape8'Tag then
         BBS.Lisp.error("tape-close", "device is not a tape controller.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      tape := BBS.Sim_CPU.io.serial.tape8_access(dev);
      declare
         name : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(drive.s));
      begin
         if name = "RDR" then
            tape.closeIn;
         elsif name = "PUN" then
            tape.closeOut;
         else
            BBS.Lisp.error("tape-close", "Unknown drive.");
            e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
            return;
         end if;
      end;
   end;
   --
   --  Attach a file to a printer
   --  (print-open <device> <file>)
   procedure sim_print_open(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      devname : BBS.Lisp.element_type;
      fname : BBS.Lisp.element_type;
      rest  : BBS.lisp.cons_index := s;
      dev   : BBS.Sim_CPU.io.io_access;
      prn   : BBS.Sim_CPU.io.serial.print8_access;
   begin
      if not cpu_selected then
         BBS.Lisp.error("print-open", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      devname := BBS.Lisp.evaluate.first_value(rest);
      if devname.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("print-open", "Device name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      fname := BBS.Lisp.evaluate.first_value(rest);
      if fname.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("print-open", "File name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      declare
         name : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(devname.s));
         pass : Boolean;
      begin
         dev := find_dev_by_name(Ada.Strings.Unbounded.To_Unbounded_String(name), pass);
         if not pass then
            BBS.Lisp.error("print-open", "unable to find device.");
            e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
            return;
         end if;
      end;
      if dev'Tag /= BBS.Sim_CPU.io.serial.print8'Tag then
         BBS.Lisp.error("print-open", "device is not a printer controller.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      prn := BBS.Sim_CPU.io.serial.print8_access(dev);
      prn.open(BBS.Lisp.Strings.lisp_to_str(fname.s));
   end;
   --
   --  Close a file attached to a printer
   --  (print-close <device>)
   procedure sim_print_close(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      devname : BBS.Lisp.element_type;
      drive : BBS.Lisp.element_type;
      rest  : BBS.lisp.cons_index := s;
      dev   : BBS.Sim_CPU.io.io_access;
      prn   : BBS.Sim_CPU.io.serial.print8_access;
   begin
      if not cpu_selected then
         BBS.Lisp.error("print-close", "No CPU Selected");
         e := BBS.lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      devname := BBS.Lisp.evaluate.first_value(rest);
      if devname.kind /= BBS.Lisp.V_STRING then
         BBS.Lisp.error("print-close", "Device name must be a string");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
         return;
      end if;
      declare
         name : constant String := Ada.Characters.Handling.To_Upper(BBS.Lisp.Strings.lisp_to_str(devname.s));
         pass : Boolean;
      begin
         dev := find_dev_by_name(Ada.Strings.Unbounded.To_Unbounded_String(name), pass);
         if not pass then
            BBS.Lisp.error("print-close", "unable to find device.");
            e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
            return;
         end if;
      end;
      if dev'Tag /= BBS.Sim_CPU.io.serial.print8'Tag then
         BBS.Lisp.error("print-close", "device is not a printer controller.");
         e := BBS.Lisp.make_error(BBS.Lisp.ERR_ADDON);
         return;
      end if;
      prn := BBS.Sim_CPU.io.serial.print8_access(dev);
      prn.close;
   end;
   --
   --  Set the CLI pause count
   --  (set-pause-count <integer>)
   procedure sim_pause_count(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      value_elem  : BBS.lisp.element_type;
      value       : BBS.uint32;
      rest        : BBS.lisp.cons_index := s;
   begin
      value_elem := BBS.lisp.evaluate.first_value(rest);
      if value_elem.kind /= BBS.Lisp.V_NONE then
         --
         --  Check if the value state is an integer element.
         --
         if value_elem.kind = BBS.Lisp.V_INTEGER then
            value := int32_to_uint32(value_elem.i);
         else
            BBS.lisp.error("set-pause-count", "Value state must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
         end if;
         --
         --  If everything is OK, then write to memory
         --
         pause_count := Integer(value);
      end if;
      e := (kind => BBS.lisp.V_INTEGER, i => uint32_to_int32(BBS.uint32(pause_count)));
   end;
   --
   --  Set the CLI pause character
   --  (set-pause-char <integer>)
   procedure sim_pause_char(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      value_elem  : BBS.lisp.element_type;
      value       : BBS.uint32;
      rest        : BBS.lisp.cons_index := s;
   begin
      value_elem := BBS.lisp.evaluate.first_value(rest);
      if value_elem.kind /= BBS.Lisp.V_NONE then
         --
         --  Check if the value state is an integer element.
         --
         if value_elem.kind = BBS.Lisp.V_INTEGER then
            value := int32_to_uint32(value_elem.i);
         else
            BBS.lisp.error("set-pause-char", "Value state must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
         end if;
         if value > 255 then
            BBS.lisp.error("set-pause-char", "Value out of range.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_RANGE);
            return;
         end if;
         --
         --  If everything is OK, then write to memory
         --
         interrupt := Character'Val(value);
      end if;
      e := (kind => BBS.lisp.V_CHARACTER, c => interrupt);
   end;
   --
end;
