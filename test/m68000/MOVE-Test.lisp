;
;  Load software
;
(memw #x1000 #x203c) ; MOVE.L #$DEADBEEF,D0
(memw #x1002 #xdead)
(memw #x1004 #xbeef)
(memw #x1006 #x323c) ; MOVE.W #$ABBA,D1
(memw #x1008 #xabba)
(memw #x100a #x1001) ; MOVE.B D1,D0
(memw #x100c #x2040) ; MOVE.L D0,A0
(memw #x100e #x3041) ; MOVE.W D1,A0
;
;  Define function
;
(defun test-reg (reg-num expected)
  (print "Reg " reg-num " expected " expected " actual " (reg-val reg-num))
  (if (= expected (reg-val reg-num))
    (print " Pass")
    (print " *** FAIL ***"))
  (terpri))
(defun test-memw (address expected)
  (print "Memory " address " expected " expected " actual " (memw address))
  (if (= expected (memw address))
    (print " Pass")
    (print " *** FAIL ***"))
  (terpri))
;
;  Execute test
;
(go #x1000)
(sim-step)
(test-reg 0 #xdeadbeef)
(test-reg 18 #x2008)
(sim-step)
(test-reg 1 #xabba)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xdeadbeba)
(test-reg 18 #x2008)
(sim-step)
(test-reg 8 #xdeadbeba)
(test-reg 18 #x2008)
(sim-step)
(test-reg 8 #xdeadabba)
(test-reg 18 #x2008)
