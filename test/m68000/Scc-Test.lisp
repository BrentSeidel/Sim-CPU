;
;  Load software
;
(memw #x1000 #x50c0) ; ST D0
(memw #x1002 #x51c0) ; SF D0
;
(memw #x1004 #x44fc) ; MOVE #0,CCR
(memw #x1006 #x0000)
(memw #x1008 #x55c0) ; SCS D0
(memw #x100a #x54c0) ; SCC D0
(memw #x100c #x59c0) ; SVS D0
(memw #x100e #x58c0) ; SVC D0
(memw #x1010 #x57c0) ; SEQ D0
(memw #x1012 #x56c0) ; SNE D0
(memw #x1014 #x5bc0) ; SMI D0
(memw #x1016 #x5ac0) ; SPL D0
;
(memw #x1018 #x44fc) ; MOVE #1,CCR
(memw #x101a #x0001)
(memw #x101c #x55c0) ; SCS D0
(memw #x101e #x54c0) ; SCC D0
;
(memw #x1020 #x44fc) ; MOVE #2,CCR
(memw #x1022 #x0002)
(memw #x1024 #x59c0) ; SVS D0
(memw #x1026 #x58c0) ; SVC D0
;
(memw #x1028 #x44fc) ; MOVE #4,CCR
(memw #x102a #x0004)
(memw #x102c #x57c0) ; SEQ D0
(memw #x102e #x56c0) ; SNE D0
;
(memw #x1030 #x44fc) ; MOVE #8,CCR
(memw #x1032 #x0008)
(memw #x1034 #x5bc0) ; SMI D0
(memw #x1036 #x5ac0) ; SPL D0
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
(print "Testing Scc instructions")
(terpri)
(sim-step)
(test-reg 0 #xff)
(sim-step)
(test-reg 0 0)
;
(sim-step)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 0)
(sim-step)
(test-reg 0 #xff)
(sim-step)
(test-reg 0 0)
(sim-step)
(test-reg 0 #xff)
(sim-step)
(test-reg 0 0)
(sim-step)
(test-reg 0 #xff)
(sim-step)
(test-reg 0 0)
(sim-step)
(test-reg 0 #xff)
;
(sim-step)
(test-reg 18 #x2001)
(sim-step)
(test-reg 0 #xff)
(sim-step)
(test-reg 0 0)
;
(sim-step)
(test-reg 18 #x2002)
(sim-step)
(test-reg 0 #xff)
(sim-step)
(test-reg 0 0)
;
(sim-step)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 #xff)
(sim-step)
(test-reg 0 0)
;
(sim-step)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xff)
(sim-step)
(test-reg 0 0)
