# Sim-CPU
CPU Simulations written in Ada

This is spun off of the Pi-Mainframe repository.  Moving simulation into
a separate repository will allow simulators to be developed more independently
as well as used in other applications.

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
This is an instruction set simulator.  No effort has been made for timing or
cycle accuracy.  Software that does not depend on instruction timing will
probably work.

This is currently under development.  All instructions have been implemented
and have had basic sanity checks.  It is expected that there some bugs still
remain.  In particular, I'm not entirely sure that I have the flags correct.
Since I wrote both the implementation and test, any misunderstanding would
show up in both.  I would appreciate other eyes to take a look at this (and
everything else), and write issues, if necessary.

Interrupts have not been implemented and I am working on trying to figure out
a way that can be applied generically.

I am now starting work on I/O devices...

I would like to be able to get CP/M running on this.

