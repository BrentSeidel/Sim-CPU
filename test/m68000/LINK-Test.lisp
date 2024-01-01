;
;  Load software
;
(memw #x1000 #x0680) ; ADD.L $STACK,D0
(memw #x1002 #x0000)
(memw #x1004 #x2000)
(memw #x1006 #xc18f) ; EXG D0,SP
(memw #x1008 #xddfc) ; ADD.L #$FFFF,A6
(memw #x100a #x0000)
(memw #x100c #xffff)
(memw #x100e #x4e56) ; LINK A6,#-$20
(memw #x1010 #xffe0)
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
(test-reg 0 #x2000)
(sim-step)
(test-reg 16 #x2000)
(sim-step)
(test-reg 14 #xffff)
(sim-step)
(test-reg 14 #x1ffc)
(test-reg 16 #x1fdc)
(if (= #xffff (meml #x1ffc))
  (print "Value on stack")
  (print "*** FAIL ***"))
(terpri)
