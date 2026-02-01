;
;  Configuration file for OS68k
;
(print "Loading OS68k experimental operating system for 68000")
(sim-cpu "68000")
(attach "CLK" #x400 "MEM" 64)
(attach "TEL" #x402 "MEM" 2171 65)
(attach "TEL" #x404 "MEM" 2172 66)
(attach "TEL" #x406 "MEM" 2173 67)
(attach "MUX" #x408 "MEM" 3141 68)
(sim-load "Tasks.S")
(sim-load "OS68k.S")
(go #x2000)
