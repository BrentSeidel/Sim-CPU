;
;  Tests for KW11 attached to PDP-11
;
(sim-cpu "PDP-11/TEST")
;
;  KW11 always attaches to memory address 777546 and vector 100.
;
(attach "KW11" #o777546 "MEM" #o100)
(set-pause-count 1)
;
(terpri)
(print "Starting KW11 test.")
(print "  Loading memory.")
;
;  Load memory
;
;  Vectors
;
(memlw #o000100 #o000000)  ;  Vector PC

;
;  Run test
;
(print "  Telnet to port 2171 on localhost.")
(print "  Enter RUN command to the simulator.")
(print "  A message should appear on the telnet screen.")
(print "  Once the message appears, characters should be echoed")
(print "  Until `A` is entered")
(go #o001000)
