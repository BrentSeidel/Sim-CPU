;
;  Load software
;
(memw #x1000 #x41f8) ; LEA START,A0
(memw #x1002 #x1000)
(memw #x1004 #x43d0) ; LEA (A0),A1
(memw #x1006 #x45e9) ; LEA 2(A1),A2
(memw #x1008 #x0002)
(memw #x100a #x47fa) ; LEA START(PC),A3
(memw #x100c #xfff4)
(memw #x100e #x5440) ; ADDQ #2,D0
(memw #x1010 #x49f1) ; LEA 2(A1,D0),A4
(memw #x1012 #x0002)
(memw #x1014 #x4bfb) ; LEA START(PC,D0),A5
(memw #x1016 #x00ea)
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
(test-reg 8 #x1000)
(sim-step)
(test-reg 9 #x1000)
(sim-step)
(test-reg 10 #x1002)
(sim-step)
(test-reg 11 #x1000)
(sim-step)
(test-reg 0 2)
(sim-step)
(test-reg 12 #x1004)
(sim-step)
(test-reg 13 #x1002)
