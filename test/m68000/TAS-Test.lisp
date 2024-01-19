;
;  Load software
;
(memw #x1000 #x7aff) ; MOVE.L #$ffffffff,D5
(memw #x1002 #x4ac5) ; TAS D5
(memw #x1004 #x1a3c) ; MOVE.B #0,D5
(memw #x1006 #x0000)
(memw #x1008 #x4ac5) ; TAS D5
(memw #x100a #x1a3c) ; MOVE.B #$80,D5
(memw #x100c #x0080)
(memw #x100e #x4ac5) ; TAS D5
(memw #x1010 #x5345) ; SUBQ #1,D5
(memw #x1012 #x4ac5) ; TAS D5
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
(test-reg 5 #xffffffff)
(test-reg 18 #x2008)
(sim-step)
(test-reg 5 #xffffffff)
(test-reg 18 #x2008)
(sim-step)
(test-reg 5 #xffffff00)
(test-reg 18 #x2004)
(sim-step)
(test-reg 5 #xffffff80)
(test-reg 18 #x2004)
(sim-step)
(test-reg 5 #xffffff80)
(test-reg 18 #x2008)
(sim-step)
(test-reg 5 #xffffff80)
(test-reg 18 #x2008)
(sim-step)
(test-reg 5 #xffffff7f)
(test-reg 18 #x2008)
(sim-step)
(test-reg 5 #xffffffff)
(test-reg 18 #x2000)
