# Sim-CPU
CPU Simulations written in Ada

This is spun off of the Pi-Mainframe repository.  Moving simulation into
a separate repository will allow simulators to be developed more independently
as well as used in other applications.

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/bbs_simcpu.json)]
(https://alire.ada.dev/crates/bbs_simcpu.html)


[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/simcpucli.json)]
(https://alire.ada.dev/crates/simcpucli.html)


[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/loadcpm.json)]
(https://alire.ada.dev/crates/loadcpm.html)


## Implementation
The root of the simulators is the abstract object "simulator" defined in the
BBS.Sim_CPU package.  It defined the external interface that all simulators
must implement.  It is expected to evolve as some actual CPU simulators get
implemented.

## Simulators
The following simulators are implemented.

### Simple Example
This is not really a CPU simulator.  Its main purpose is to blink the lights
in the Pi-Mainframe in interesting patterns.

### Intel 8080
[More information](https://github.com/BrentSeidel/Sim-CPU/tree/main/src/i8080/README.md)

All 8080/8085 and Z-80 instructions have been implemented.

### Motorola 68000
[More information](https://github.com/BrentSeidel/Sim-CPU/tree/main/src/m68000/README.md)

All the 68000 CPU instructions have been implemented.

### MOS Technologies 6502
This is currently under development.  Some of the instructions have been implemented
and tested, but there are still more to go.  I/O and interrupts also have not
yet been implemented.

## CLI
A command line interface is provided for development of both the simulators
and software that runs on the simulator.

The following commands are currently provided:
-  BREAK <addr>
    -    Set a breakpoint (currently only one can be active at a time)
-  CONTINUE
    -    Continue execution
-  DEP <addr> <value>
    -    Deposit value to a memory location
-  DISK <subcommand>
    -    Assorted commands for disk images
-  DUMP <addr>
    -    Display a region of memory
-  EXIT or QUIT
    -    EXIT the program
-  GO <addr>
    -    Start execution at a specified address
-  LISP
    -    Enter Lisp interpreter
-  LIST
    -    List attached I/O devices
-  LOAD <filename>
    -    Load data from a file into memory
-  REG
    -    Display register values
-  RUN
    -    Execute instructions until halt or breakpoint
-  STEP
    -    Execute one instruction
-  TRACE <level>
    -    Print information for each instruction executed
-  UNBREAK <addr>
    -    Remove a breakpoint

### Lisp
For more information on the embedded [tiny-Lisp](https://github.com/BrentSeidel/Ada-Lisp)
interpreter.

The following additional Lisp words are implemented for controlling the
simulation.  With use, more words may be added.
-  Execute one instruction
    -  (sim-step)
-  Get/set memory (byte/word/long)
    -  (memb addr value)
    -  (memb addr)
    -  (memw addr value)
    -  (memw addr)
    -  (meml addr value)
    -  (meml addr)
-  Set execution address
    -  (go address)
-  Read register value (index is simulator dependent)
    -  (reg-val index)
-  Return number of registers
    -  (num-reg)
-  Return or set simulator halted state
    -  (halted state)
    -  (halted)
