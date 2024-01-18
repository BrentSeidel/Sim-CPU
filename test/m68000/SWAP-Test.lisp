;
;  Load software
;
(memw #x1000 #x2a3c) ; MOVE.L #$5555aaaa,D5
(memw #x1002 #x5555)
(memw #x1004 #xaaaa)
(memw #x1006 #x4845) ; SWAP D5
(memw #x1008 #x4845) ; SWAP D5
(memw #x100a #x4286) ; CLR.L D6
(memw #x100c #x4846) ; SWAP D6
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
(test-reg 5 #x5555aaaa)
(test-reg 18 #x2000)
(sim-step)
(test-reg 5 #xaaaa5555)
(test-reg 18 #x2008)
(sim-step)
(test-reg 5 #x5555aaaa)
(test-reg 18 #x2000)
(sim-step)
(test-reg 6 0)
(test-reg 18 #x2004)
(sim-step)
(test-reg 6 0)
(test-reg 18 #x2004)

