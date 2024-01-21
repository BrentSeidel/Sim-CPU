;
;  Load software
;
(memw #x1000 #x4280) ; CLR.L D0
(memw #x1002 #x4a80) ; TST.L D0
(memw #x1004 #x4a40) ; TST.W D0
(memw #x1006 #x4a00) ; TST.B D0
(memw #x1008 #x203c) ; MOVE.L #$80000000,D0
(memw #x100a #x8000)
(memw #x100c #x0000)
(memw #x100e #x4a80) ; TST.L D0
(memw #x1010 #x4a40) ; TST.W D0
(memw #x1012 #x4a00) ; TST.B D0
(memw #x1014 #x203c) ; MOVE.L #$8000,D0
(memw #x1016 #x0000)
(memw #x1018 #x8000)
(memw #x101a #x4a80) ; TST.L D0
(memw #x101c #x4a40) ; TST.W D0
(memw #x101e #x4a00) ; TST.B D0
(memw #x1020 #x203c) ; MOVE.L #$80,D0
(memw #x1022 #x0000)
(memw #x1024 #x0080)
(memw #x1026 #x4a80) ; TST.L D0
(memw #x1028 #x4a40) ; TST.W D0
(memw #x102a #x4a00) ; TST.B D0
;
;  Define function
;
(defun test-reg (reg-num expected)
  (print "Reg " reg-num " expected " expected " actual " (reg-val reg-num))
  (if (= expected (reg-val reg-num))
    (print " Pass")
    (print " *** FAIL ***"))
  (terpri))
;
;  Execute test
;
(go #x1000)
(print "Testing TAS instruction")
(terpri)
(sim-step)
(test-reg 0 0)
(sim-step)
(test-reg 18 #x2004)
(sim-step)
(test-reg 18 #x2004)
(sim-step)
(test-reg 18 #x2004)
;
(sim-step)
(test-reg 0 #x80000000)
(sim-step)
(test-reg 18 #x2008)
(sim-step)
(test-reg 18 #x2004)
(sim-step)
(test-reg 18 #x2004)
;
(sim-step)
(test-reg 0 #x8000)
(sim-step)
(test-reg 18 #x2000)
(sim-step)
(test-reg 18 #x2008)
(sim-step)
(test-reg 18 #x2004)
;
(sim-step)
(test-reg 0 #x80)
(sim-step)
(test-reg 18 #x2000)
(sim-step)
(test-reg 18 #x2000)
(sim-step)
(test-reg 18 #x2008)
