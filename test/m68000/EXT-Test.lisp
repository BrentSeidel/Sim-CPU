;
;  Load software
;
(memw #x1000 #x0640) ; ADD.W #$0F0F,D0
(memw #x1002 #x0f0f)
(memw #x1004 #x0641) ; ADD.W #$8F0F,D1
(memw #x1006 #x8f0f)
(memw #x1008 #x0602) ; ADD.B #$0F,D2
(memw #x100a #x000f)
(memw #x100c #x0603) ; ADD.B #$FF,D3
(memw #x100e #x00ff)
(memw #x1010 #x48c0) ; EXT.L D0
(memw #x1012 #x48c1) ; EXT.L D1
(memw #x1014 #x4882) ; EXT.W D2
(memw #x1016 #x4883) ; EXT.W D3
(memw #x1018 #x4881) ; EXT.W D1
(memw #x101a #x48c1) ; EXT.L D1
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
(test-reg 0 #x0f0f)
(sim-step)
(test-reg 1 #x8f0f)
(sim-step)
(test-reg 2 #x0f)
(sim-step)
(test-reg 3 #xff)
(print "Extending word values")
(terpri)
(sim-step)
(test-reg 0 #x0f0f)
(sim-step)
(test-reg 1 #xffff8f0f)
(print "Extending byte values")
(terpri)
(sim-step)
(test-reg 2 #x0f)
(sim-step)
(test-reg 3 #xffff)
(print "Extras")
(terpri)
(sim-step)
(test-reg 1 #xffff000f)
(sim-step)
(test-reg 1 #x0f)
