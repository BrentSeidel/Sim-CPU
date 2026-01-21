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
with BBS.lisp;
--
--  This package contains custom lisp words for the CPU simulator.
--
package cli.Lisp is
   --
   --  Do any initialization and install the custom lisp words.
   --
   procedure init;
   --
private
   --
   --  Execute one instruction
   --  (sim-step)
   procedure sim_step(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Get/set memory (byte)
   --  (memb addr value)
   --  (memb addr)
   procedure sim_memb(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --  Get/set memory (word/long) MSB first
   --  (memw addr value)
   --  (memw addr)
   --  (meml addr value)
   --  (meml addr)
   procedure sim_memw(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   procedure sim_meml(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --  Get/set memory (word/long) LSB first
   --  (memlw addr value)
   --  (memlw addr)
   --  (memll addr value)
   --  (memll addr)
   procedure sim_memlw(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   procedure sim_memll(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Get/set memory limits
   --  (mem-max)
   --  (mem-limit value)
   --  (mem-limit)
   --
   procedure sim_mem_max(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   procedure sim_mem_limit(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
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
   --  Send an interrupt
   --  (send-int value)
   procedure sim_send_int(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
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
   --  Select which simulator CPU to use.  This will need to be updated as more
   --  simulators are added.
   --  (sim-cpu cpu)
   procedure sim_cpu(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Attach I/O devices at a specified address and bus
   --  (attach device address bus [device specific])
   procedure sim_attach(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Attach a file to a disk drive
   --  (disk-open <device> <drive> <file>)
   procedure sim_disk_open(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Close a file attached to a disk drive
   --  (disk-close <device> <drive>)
   procedure sim_disk_close(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Set the geometry of a disk drive
   --  (disk-geom <device> <drive> <geometry)
   procedure sim_disk_geom(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Attach a file to a tape drive
   --  (tape-open <device> <drive> <file>)
   procedure sim_tape_open(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Close a file attached to a tape drive
   --  (tape-close <device> <drive>)
   procedure sim_tape_close(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Attach a file to a printer
   --  (print-open <device> <file>)
   procedure sim_print_open(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Close a file attached to a printer
   --  (print-close <device>)
   procedure sim_print_close(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Set the CLI pause count
   --  (set-pause-count <integer>)
   procedure sim_pause_count(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
   --  Set the CLI pause character
   --  (set-pause-char <integer>)
   procedure sim_pause_char(e : out BBS.lisp.element_type; s : BBS.lisp.cons_index);
   --
end;
