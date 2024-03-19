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
private
   --
   --  Local pointer to simulator
   --
   cpu    : BBS.Sim_CPU.sim_access;
end;
