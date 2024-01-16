;
;  Load software
;
(memw #x0020 #x0000) ; Privilege violation exception vector
(memw #x0022 #x100c)
;
(memw #x1000 #x4e72) ; STOP #$55aa
(memw #x1002 #x55aa)
(memw #x1004 #x4e72) ; STOP #$aa55
(memw #x1006 #xaa55)
;PRIVIOL:
(memw #x100c #x4e73) ; RTE
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
(sim-step)
(test-reg 18 #x55aa)
(if (halted)
  (print "Simulation halted - Pass")
  (print "Simulation not halted - *** FAIL ***"))
(terpri)
(halted nil)
(sim-step)
(test-reg 17 #x100c)
(test-reg 18 #x35aa)
