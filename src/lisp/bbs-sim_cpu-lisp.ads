with BBS.lisp;
--
--  This package contains custom lisp words for the CPU simulator.
--
package BBS.Sim_CPU.Lisp is
   --
   --  Do any initialization and install the custom lisp words.
   --
   procedure init(sim : BBS.Sim_CPU.sim_access);
   --
   --  Execute one instruction
   --  (sim-step)
   procedure sim_step(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Get/set memory (byte/word/long)
   --  (memb addr value)
   --  (memb addr)
   --  (memw addr value)
   --  (memw addr)
   --  (meml addr value)
   --  (meml addr)
   procedure sim_memb(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   procedure sim_memw(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   procedure sim_meml(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Set execution address
   --  (go address)
   procedure sim_go(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Read register value (index is simulator dependent)
   --  (reg-val index)
   procedure sim_reg_val(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Return number of registers
   --  (num-reg)
   procedure sim_num_reg(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Return or set simulator halted state
   --  (halted state)
   --  (halted)
   procedure sim_halted(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Call the simulator's init function
   --  (sim-init)
   procedure sim_init(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Get the interrupt status
   --  (int-state)
   procedure sim_int_state(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Get last output address and data
   --  (last-out-addr)
   --  (last-out-data)
   procedure sim_last_out_addr(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   procedure sim_last_out_data(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Load a file using simulator specific load command
   --  (load filename)
   procedure sim_load(e : out BBS.Lisp.element_type; s : BBS.Lisp.cons_index);
   --
   --  Override input data for address (one time only)
   -- (override-in addr data)
   procedure sim_override_in(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
private
   --
   --  Local pointer to simulator
   --
   cpu    : BBS.Sim_CPU.sim_access;
end;
