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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.--
--
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS;
use type BBS.uint16;
use type BBS.uint32;
with BBS.Lisp;
use type BBS.Lisp.value_type;
with BBS.Lisp.evaluate;
with BBS.Lisp.strings;
with BBS.Sim_CPU;
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
      BBS.lisp.add_builtin("sim-init", sim_init'Access);
      BBS.lisp.add_builtin("sim-load", sim_load'Access);
      BBS.lisp.add_builtin("sim-step", sim_step'Access);
      BBS.lisp.add_builtin("memb", sim_memb'Access);
      BBS.lisp.add_builtin("memw", sim_memw'Access);
      BBS.lisp.add_builtin("meml", sim_meml'Access);
      BBS.lisp.add_builtin("go", sim_go'Access);
      BBS.lisp.add_builtin("reg-val", sim_reg_val'Access);
      BBS.lisp.add_builtin("num-reg", sim_num_reg'Access);
      BBS.lisp.add_builtin("halted", sim_halted'Access);
      BBS.lisp.add_builtin("int-state", sim_int_state'Access);
      BBS.lisp.add_builtin("last-out-addr", sim_last_out_addr'Access);
      BBS.lisp.add_builtin("last-out-data", sim_last_out_data'Access);
      BBS.lisp.add_builtin("override-in", sim_override_in'Access);
      BBS.lisp.add_builtin("send-int", sim_send_int'Access);
   end;
   --
   --  Execute one instruction
   --  (sim-step)
   procedure sim_step(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
   begin
      cpu.run;
      e := BBS.Lisp.NIL_ELEM;
   end;
   --
   --  Get/set memory (byte/word/long)
   --  (memb addr value)
   --  (memb addr)
   --  (memw addr value)
   --  (memw addr)
   --  (meml addr value)
   --  (meml addr)
   procedure sim_memb(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      value_elem : BBS.lisp.element_type;
      addr       : BBS.Sim_CPU.addr_bus;
      value      : BBS.Sim_CPU.byte;
      rest       : BBS.lisp.cons_index := s;
   begin
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
   procedure sim_memw(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      value_elem : BBS.lisp.element_type;
      addr       : BBS.Sim_CPU.addr_bus;
      value      : BBS.Sim_CPU.word;
      rest       : BBS.lisp.cons_index := s;
   begin
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
   --  Set execution address
   --  (go address)
   procedure sim_go(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      addr       : BBS.Sim_CPU.addr_bus;
      rest       : BBS.lisp.cons_index := s;
   begin
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
      num_reg    : BBS.Sim_CPU.long := cpu.registers;
   begin
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
      cpu.init;
      e := BBS.Lisp.NIL_ELEM;
   end;
   --
   --  Get the interrupt status
   --  (int-state)
   procedure sim_int_state(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
   begin
      e := (kind => BBS.lisp.V_INTEGER, i => BBS.lisp.int32(cpu.intStatus));
   end;
   --
   --  Send an interrupt
   --  (send-int value)
   procedure sim_send_int(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      elem : BBS.Lisp.element_type;
      rest : BBS.lisp.cons_index := s;
   begin
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
      e := (kind => BBS.lisp.V_INTEGER, i => BBS.lisp.int32(cpu.lastOutAddr));
   end;
   --
   procedure sim_last_out_data(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
   begin
      e := (kind => BBS.lisp.V_INTEGER, i => BBS.lisp.int32(cpu.lastOutData));
   end;
   --
   --  Load a file using simulator specific load command
   --  (load filename)
   procedure sim_load(e : out BBS.Lisp.element_type; s : BBS.Lisp.cons_index) is
      elem : BBS.Lisp.element_type;
      rest : BBS.lisp.cons_index := s;
   begin
      elem := BBS.Lisp.evaluate.first_value(rest);
      if elem.kind /= BBS.Lisp.V_STRING then
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
end;
