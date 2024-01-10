;
;  Load software
;
(memw #x1000 #x4280) ; CLR.L D0
(memw #x1002 #x4680) ; NOT.L D0
(memw #x1004 #x4680) ; NOT.L D0
(memw #x1006 #x203c) ; MOVE #$55555555,D0
(memw #x1008 #x5555)
(memw #x100a #x5555)
(memw #x100c #x4680) ; NOT.L D0
(memw #x100e #x4680) ; NOT.L D0
;
(memw #x1010 #x4280) ; CLR.L D0
(memw #x1012 #x4640) ; NOT.W D0
(memw #x1014 #x4640) ; NOT.W D0
(memw #x1016 #x203c) ; MOVE #$55555555,D0
(memw #x1018 #x5555)
(memw #x101a #x5555)
(memw #x101c #x4640) ; NOT.W D0
(memw #x101e #x4640) ; NOT.W D0
;
(memw #x1020 #x4280) ; CLR.L D0
(memw #x1022 #x4600) ; NOT.B D0
(memw #x1024 #x4600) ; NOT.B D0
(memw #x1026 #x203c) ; MOVE #$55555555,D0
(memw #x1028 #x5555)
(memw #x102a #x5555)
(memw #x102c #x4600) ; NOT.B D0
(memw #x102e #x4600) ; NOT.B D0
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
(print "Testing NOT.L")
(terpri)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 #xffffffff)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 #x55555555)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #xaaaaaaaa)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #x55555555)
(test-reg 18 #x2000)
;
(print "Testing NOT.W")
(terpri)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 #xffff)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 #x55555555)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x5555aaaa)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #x55555555)
(test-reg 18 #x2000)
;
(print "Testing NOT.B")
(terpri)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 #xff)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 #x55555555)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x555555aa)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #x55555555)
(test-reg 18 #x2000)
