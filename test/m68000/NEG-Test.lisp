;
;  Load software
;
(memw #x1000 #x4280) ; CLR.L D0
(memw #x1002 #x4480) ; NEG.L D0
(memw #x1004 #x203c) ; MOVE.L #$80000000,D0
(memw #x1006 #x8000)
(memw #x1008 #x0000)
(memw #x100a #x4480) ; NEG.L D0
(memw #x100c #x7001) ; MOVE.L #1,D0
(memw #x100e #x4480) ; NEG.L D0
(memw #x1010 #x4480) ; NEG.L D0
;
(memw #x1012 #x4280) ; CLR.L D0
(memw #x1014 #x4440) ; NEG.W D0
(memw #x1016 #x203c) ; MOVE.L #$8000,D0
(memw #x1018 #x0000)
(memw #x101a #x8000)
(memw #x101c #x4440) ; NEG.W D0
(memw #x101e #x7001) ; MOVE.L #1,D0
(memw #x1020 #x4440) ; NEG.W D0
(memw #x1022 #x4440) ; NEG.W D0
;
(memw #x1024 #x4280) ; CLR.L D0
(memw #x1026 #x4400) ; NEG.B D0
(memw #x1028 #x203c) ; MOVE.L #$80,D0
(memw #x102a #x0000)
(memw #x102c #x0080)
(memw #x102e #x4400) ; NEG.B D0
(memw #x1030 #x7001) ; MOVE.L #1,D0
(memw #x1032 #x4400) ; NEG.B D0
(memw #x1034 #x4400) ; NEG.B D0
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
(print "Testing NEG.L")
(terpri)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 #x80000000)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #x80000000)
(test-reg 18 #x201b)
(sim-step)
(test-reg 0 1)
(test-reg 18 #x2010)
(sim-step)
(test-reg 0 #xffffffff)
(test-reg 18 #x2019)
(sim-step)
(test-reg 0 1)
(test-reg 18 #x2011)
;
(print "Testing NEG.W")
(terpri)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2014)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 #x8000)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x8000)
(test-reg 18 #x201b)
(sim-step)
(test-reg 0 1)
(test-reg 18 #x2010)
(sim-step)
(test-reg 0 #xffff)
(test-reg 18 #x2019)
(sim-step)
(test-reg 0 1)
(test-reg 18 #x2011)
;
(print "Testing NEG.B")
(terpri)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2014)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 #x80)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x80)
(test-reg 18 #x201b)
(sim-step)
(test-reg 0 1)
(test-reg 18 #x2010)
(sim-step)
(test-reg 0 #xff)
(test-reg 18 #x2019)
(sim-step)
(test-reg 0 1)
(test-reg 18 #x2011)
