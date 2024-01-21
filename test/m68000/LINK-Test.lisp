;
;  Load software
;
(memw #x1000 #x203c) ; ADD.L $STACK,D0
(memw #x1002 #x0000)
(memw #x1004 #x2000)
(memw #x1006 #xc18f) ; EXG D0,SP
(memw #x1008 #x2c7c) ; ADD.L #$FFFF,A6
(memw #x100a #x0000)
(memw #x100c #xffff)
(memw #x100e #x4e56) ; LINK A6,#-$20
(memw #x1010 #xffe0)
(memw #x1012 #x4e5e) ; UNLK A6
;
;  Define function
;
(defun test-reg (reg-num expected)
  (print "Reg " reg-num " expected " expected " actual " (reg-val reg-num))
  (if (= expected (reg-val reg-num))
    (print " Pass")
    (print " *** FAIL ***"))
  (terpri))
(defun test-meml (address expected)
  (print "Memory " address " expected " expected " actual " (meml address))
  (if (= expected (meml address))
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
(test-meml #x1ffc #xffff)
;
(sim-step)
(test-reg 14 #xffff)
(test-reg 16 #x2000)
