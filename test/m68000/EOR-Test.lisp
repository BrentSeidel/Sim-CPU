;
;  Load software
;
(memw #x0020 #x0000) ; Privalege violation vector
(memw #x0022 #x1048)
;
(memw #x1000 #x0680) ; ADD.L #$0F0F0F0F,D0
(memw #x1002 #x0f0f)
(memw #x1004 #x0f0f)
(memw #x1006 #x0681) ; ADD.L #$00FF00FF,D1
(memw #x1008 #x00ff)
(memw #x100a #x00ff)
(memw #x100c #xd480) ; ADD.L D0,D2
(memw #x100e #xd681) ; ADD.L D1,D3
(memw #x1010 #xb103) ; EOR.B D0,D3
(memw #x1012 #xb342) ; EOR.W D1,D2
(memw #x1014 #xb583) ; EOR.L D2,D3
(memw #x1016 #x0a80) ; EORI.L #$55555555,D0
(memw #x1018 #x5555)
(memw #x101a #x5555)
(memw #x101c #x0a41) ; EORI.W #$AAAA,D1
(memw #x101e #xaaaa)
(memw #x1020 #x0a02) ; EORI.B #$A5,D2
(memw #x1022 #x00a5)
(memw #x1024 #x4280) ; CLR.L D0
(memw #x1026 #x0640) ; ADD.W #$FFFF,D0
(memw #x1028 #xffff)
(memw #x102a #x0a3c) ; EORI #$08,CCR
(memw #x102c #x0008)
(memw #x102e #x0a3c) ; EORI #$F7,CCR
(memw #x1030 #x00f7)
(memw #x1032 #x4240) ; CLR.W D0
(memw #x1034 #x0a3c) ; EORI #$04,CCR
(memw #x1036 #x0004)
(memw #x1038 #x0a3c) ; EORI #$FB,CCR
(memw #x103a #x00fb)
(memw #x103c #x0a7c) ; EORI #$2000,SR
(memw #x103e #x2000)
(memw #x1040 #x0a7c) ; EORI #$2000,SR
(memw #x1042 #x2000)
(memw #x1044 #xffff)
(memw #x1046 #xffff)
(memw #x1048 #x4e73) ; RTE
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
(test-reg 0 #x0f0f0f0f)
(sim-step)
(test-reg 1 #x00ff00ff)
(sim-step)
(test-reg 2 #x0f0f0f0f)
(sim-step)
(test-reg 3 #x00ff00ff)
(print "Testing EOR instructions")
(terpri)
(sim-step)
(test-reg 3 #x00ff00f0)
(test-reg 18 #x2008)
(sim-step)
(test-reg 2 #x0f0f0ff0)
(test-reg 18 #x2000)
(sim-step)
(test-reg 3 #x0ff00f00)
(test-reg 18 #x2000)
(print "Testing EORI instructions")
(terpri)
(sim-step)
(test-reg 0 #x5a5a5a5a)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 #x00ffaa55)
(test-reg 18 #x2008)
(sim-step)
(test-reg 2 #x0f0f0f55)
(test-reg 18 #x2000)
(sim-step)
(sim-step)
(test-reg 18 #x2008)
(print "Testing EORI to CCR instructions")
(terpri)
(sim-step)
(test-reg 18 #x2000)
(sim-step)
(test-reg 18 #x20f7)
(sim-step)
(test-reg 18 #x20f4)
(sim-step)
(test-reg 18 #x20f0)
(sim-step)
(test-reg 18 #x200b)
(sim-step)
(test-reg 18 #x000b)
(print "No exception has occured yet")
(terpri)
(sim-step)
(print "A privalege violation exception has occured")
(terpri)
(test-reg 18 #x000b)

