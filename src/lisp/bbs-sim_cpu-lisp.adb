with Ada.Unchecked_Conversion;
with BBS.lisp;
use type BBS.lisp.value_type;
with BBS.lisp.evaluate;
package body BBS.Sim_CPU.Lisp is
   function int32_to_uint32 is new Ada.Unchecked_Conversion(source => BBS.lisp.int32,
                                                           target => long);
   function uint32_to_int32 is new Ada.Unchecked_Conversion(source => long,
                                                           target => BBS.lisp.int32);
   --
   --  Install the new lisp words into the lisp interpreter
   --
   procedure init(sim : BBS.Sim_CPU.sim_access) is
   begin
      cpu := sim;
      BBS.lisp.add_builtin("sim-init", sim_init'Access);
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
      addr       : addr_bus;
      value      : byte;
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
         value := byte(cpu.read_mem(addr) and 16#FF#);
         e := (kind => BBS.lisp.V_INTEGER, i => BBS.lisp.int32(value));
      else
         --
         --  Check if the value state is an integer element.
         --
         if value_elem.kind = BBS.Lisp.V_INTEGER then
            value := byte(int32_to_uint32(value_elem.i) and 16#FF#);
         else
            BBS.lisp.error("memb", "Value state must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
         end if;
         --
         --  If everything is OK, then write to memory
         --
         cpu.set_mem(addr, data_bus(value));
         e := BBS.Lisp.NIL_ELEM;
      end if;
   end;
   --
   procedure sim_memw(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      value_elem : BBS.lisp.element_type;
      addr       : addr_bus;
      value      : word;
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
         value := word(cpu.read_mem(addr) and 16#ff#)*16#100# +
                  word(cpu.read_mem(addr+1) and 16#ff#);
         e := (kind => BBS.lisp.V_INTEGER, i => BBS.lisp.int32(value));
      else
         --
         --  Check if the value state is an integer element.
         --
         if value_elem.kind = BBS.Lisp.V_INTEGER then
            value := word(int32_to_uint32(value_elem.i) and 16#ffff#);
         else
            BBS.lisp.error("memw", "Value state must be integer.");
            e := BBS.lisp.make_error(BBS.Lisp.ERR_WRONGTYPE);
            return;
         end if;
         --
         --  If everything is OK, then write to memory
         --
         cpu.set_mem(addr, data_bus((value/16#100#) and 16#ff#));
         cpu.set_mem(addr+1, data_bus(value and 16#ff#));
         e := BBS.Lisp.NIL_ELEM;
      end if;
   end;
   --
   procedure sim_meml(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      value_elem : BBS.lisp.element_type;
      addr       : addr_bus;
      value      : long;
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
         value := long(cpu.read_mem(addr) and 16#ff#)*16#0100_0000# +
                  long(cpu.read_mem(addr+1) and 16#ff#)*16#0001_0000# +
                  long(cpu.read_mem(addr+2) and 16#ff#)*16#0000_0100# +
                  long(cpu.read_mem(addr+3) and 16#ff#);
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
         cpu.set_mem(addr, data_bus((value/16#0100_0000#) and 16#ff#));
         cpu.set_mem(addr+1, data_bus((value/16#0001_0000#) and 16#ff#));
         cpu.set_mem(addr+2, data_bus((value/16#0000_0100#) and 16#ff#));
         cpu.set_mem(addr+3, data_bus(value and 16#ff#));
         e := BBS.Lisp.NIL_ELEM;
      end if;
   end;
   --
   --  Set execution address
   --  (go address)
   procedure sim_go(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      addr_elem  : BBS.lisp.element_type;
      addr       : addr_bus;
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
      index      : addr_bus;
      value      : data_bus;
      rest       : BBS.lisp.cons_index := s;
      num_reg    : long := cpu.registers;
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
   --  Override input data for address (one time only)
   -- (override-in addr data)
   procedure sim_override_in(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index) is
      elem : BBS.lisp.element_type;
      rest : BBS.lisp.cons_index := s;
      addr : addr_bus;
      data : data_bus;
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
