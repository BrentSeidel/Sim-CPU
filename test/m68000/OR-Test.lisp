;
(memw #x0020 #x0000) ; Privilege violation vector
(memw #x0022 #x1100)
;
(memw #x1000 #x203c) ; MOVE.L #$0f0f0f0f,D0
(memw #x1002 #x0f0f)
(memw #x1004 #x0f0f)
(memw #x1006 #x223c) ; MOVE.L #$00ff00ff,D1
(memw #x1008 #x00ff)
(memw #x100a #x00ff)
(memw #x100c #x2400) ; MOVE.L D0,D2
(memw #x100e #x2601) ; MOVE.L D1,D3
;
(memw #x1010 #x8600) ; OR.B D0,D3
(memw #x1012 #x8441) ; OR.W D1,D2
(memw #x1014 #x8682) ; OR.L D2,D3
;
(memw #x1016 #x0080) ; ORI.L #55000000,D0
(memw #x1018 #x5500)
(memw #x101a #x0000)
(memw #x101c #x0040) ; ORI.W #5500,D0
(memw #x101e #x5500)
(memw #x1020 #x0000) ; ORI.B #55,D0
(memw #x1022 #x0055)
;
(memw #x1024 #x003c) ; ORI #$8,CCR
(memw #x1026 #x0008)
(memw #x1028 #x003c) ; ORI #$4,CCR
(memw #x102a #x0004)
;
(memw #x102c #x007c) ; ORI #$1000,SR
(memw #x102e #x1000)
(memw #x1030 #x0a7c) ; EORI #$2000,SR
(memw #x1032 #x2000)
(memw #x1034 #x007c) ; ORI #$2000,SR
(memw #x1036 #x2000)
;
(memw #x1100 #x4e73) ; RTE
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
(go #x1000)
(sim-step)
(test-reg 0 #x0f0f0f0f)
(sim-step)
(test-reg 1 #x00ff00ff)
(sim-step)
(test-reg 2 #x0f0f0f0f)
(sim-step)
(test-reg 3 #x00ff00ff)
;
(print "Test OR instructions")
(terpri)
(sim-step)
(test-reg 3 #x00ff00ff)
(test-reg 18 #x2008)
(sim-step)
(test-reg 2 #x0f0f0fff)
(test-reg 18 #x2000)
(sim-step)
(test-reg 3 #x0fff0fff)
(test-reg 18 #x2000)
;
(print "Test ORI instructions")
(terpri)
(sim-step)
(test-reg 0 #x5f0f0f0f)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x5f0f5f0f)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x5f0f5f5f)
(test-reg 18 #x2000)
;
(print "Test ORI to CCR instruction")
(terpri)
(sim-step)
(test-reg 18 #x2008)
(sim-step)
(test-reg 18 #x200c)
;
(print "Test ORI to SR instruction")
(terpri)
(sim-step)
(test-reg 18 #x300c)
(sim-step)
(test-reg 18 #x100c)
(print "Next instruction should have a privilege violation exception")
(terpri)
(sim-step)
