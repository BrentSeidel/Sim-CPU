;
;  Load software
;
(memw #x1000 #x0680) ; ADD.L #$0F0F0F0F,D0
(memw #x1002 #x0f0f)
(memw #x1004 #x0f0f)
(memw #x1006 #x0681) ; ADD.L #$00FF00FF,D1
(memw #x1008 #x00ff)
(memw #x100a #x00ff)
(memw #x100c #xd1fc) ; ADDA.L #$55AA55AA,A0
(memw #x100e #x55aa)
(memw #x1010 #x55aa)
(memw #x1012 #xd3fc) ; ADDA.L #$5A5A5A5A,A1
(memw #x1014 #x5a5a)
(memw #x1016 #x5a5a)
(memw #x1018 #xc141) ; EXG D0,D1
(memw #x101a #xc142) ; EXG D0,D2
(memw #x101c #xc149) ; EXG A0,A1
(memw #x101e #xc14a) ; EXG A0,A2
(memw #x1020 #xc389) ; EXG A1,D1
(memw #x1022 #xc58a) ; EXG D2,A2
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
(print "Loading initial values")
(terpri)
(sim-step)
(test-reg 0 #x0f0f0f0f)
(sim-step)
(test-reg 1 #x00ff00ff)
(sim-step)
(test-reg 8 #x55aa55aa)
(sim-step)
(test-reg 9 #x5a5a5a5a)
(test-reg 2 0)
(test-reg 10 0)
(print "Exchanging data registers")
(terpri)
(sim-step)
(test-reg 0 #x00ff00ff)
(test-reg 1 #x0f0f0f0f)
(sim-step)
(test-reg 0 0)
(test-reg 2 #x00ff00ff)
(print "Exchanding address registers")
(terpri)
(sim-step)
(test-reg 8 #x5a5a5a5a)
(test-reg 9 #x55aa55aa)
(sim-step)
(test-reg 8 0)
(test-reg 10 #x5a5a5a5a)
(print "Exchanging address and data registers")
(terpri)
(sim-step)
(test-reg 1 #x55aa55aa)
(test-reg 9 #x0f0f0f0f)
(sim-step)
(test-reg 2 #x5a5a5a5a)
(test-reg 10 #x00ff00ff)
