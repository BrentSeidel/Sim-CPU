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
[More information](https://github.com/BrentSeidel/Sim-CPU/tree/main/src/msc6502/README.md)

All of the instructions have been implemented and tested.  I/O and interrupts also have
been implemented and tested.

## CLI
A command line interface is provided for development of both the simulators
and software that runs on the simulator.

See the [PDF documentation](https://github.com/BrentSeidel/Sim-CPU/blob/main/Docs/sim.pdf) for more details on the CLI.

### Lisp
For more information on the embedded [tiny-Lisp](https://github.com/BrentSeidel/Ada-Lisp)
interpreter.

See the [PDF documentation](https://github.com/BrentSeidel/Sim-CPU/blob/main/Docs/sim.pdf) for more details on the Lisp extensions.
