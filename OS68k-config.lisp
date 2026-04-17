;
;  Configuration file for OS68k
;
(print "Loading OS68k experimental operating system for 68000")
(sim-cpu "68000")
(attach "CLK" #x400 "MEM" 64)
(attach "TEL" #x402 "MEM" 65 2171)
(attach "TEL" #x404 "MEM" 66 2172)
(attach "TEL" #x406 "MEM" 67 2173)
(attach "MUX" #x408 "MEM" 68 3141)
(sim-load "Tasks.S")
(sim-load "OS68k.S")
(go #x2000)
