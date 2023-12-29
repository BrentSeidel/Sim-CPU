;
;  Load software
;
(memw #x1000 #x0640) ; ADD.W #$2000,D0
(memw #x1002 #x2000)
(memw #x1004 #xc18f) ; EXG D0,SP
(memw #x1006 #x4ef9) ; JMP L1
(memw #x1008 #x0000)
(memw #x100a #x1010)
(memw #x100c #x6000) ; BRA FAIL
(memw #x100e #x0020)
(memw #x1010 #xd3fc) ; ADD.L #L2,A1
(memw #x1012 #x0000)
(memw #x1014 #x101c)
(memw #x1016 #x4ed1) ; JMP (A1)
(memw #x1018 #x6000) ; BRA FAIL
(memw #x101a #x0014)
(memw #x101c #x4efa) ; JMP L3(PC)
(memw #x101e #x0006)
(memw #x1020 #x6000) ; BRA FAIL
(memw #x1022 #x000c)
(memw #x1024 #x4eb9) ; JSR PASS
(memw #x1026 #x0000)
(memw #x1028 #x1030)
;
(memw #x102e #x60fe) ; BRA FAIL
(memw #x1030 #x60fe) ; BRA PASS
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
(sim-step)
(test-reg 16 #x2000)
(sim-step)
(test-reg 17 #x1010)
(sim-step)
(sim-step)
(test-reg 17 #x101c)
(sim-step)
(test-reg 17 #x1024)
(sim-step)
(test-reg 16 #x1ffc)
(test-reg 17 #x1030)
