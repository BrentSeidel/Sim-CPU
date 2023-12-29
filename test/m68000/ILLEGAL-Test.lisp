;
;  Load software
;
(memw (* 4 4) #x2000)
(memw #x1000 #x4AFC) ; ILLEGAL
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
(print "No exceptions have occured");
(sim-step)
(print "Illegal instruction exception has occured.")
;(test-reg 17 #x2000) ; Once exceptions have been implemented, this can be activiated
