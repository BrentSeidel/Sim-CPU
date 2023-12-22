;
;  Load software
;
(memw #x0014 0)
(memw #x0016 #x1070) ; Div by 0 exception vector
(memw #x1000 #x4280) ; CLR.L D0
(memw #x1002 #x0680) ; ADD.L #$12345678,D0
(memw #x1004 #x1234)
(memw #x1006 #x5678)
(memw #x1008 #x81fc) ; DIVS #0,D0
(memw #x100a #x0000)
(memw #x100c #x81fc) ; DIVS #0,D0
(memw #x100e #x0001)
(memw #x1010 #x4280) ; CLR.L D0
(memw #x1012 #x0640) ; ADD.W #$1234,D0
(memw #x1014 #x1234)
(memw #x1016 #x81fc) ; DIVS #$12,D0
(memw #x1018 #x0012)
(memw #x101a #x4280) ; CLR.L D0
(memw #x101c #x0640) ; ADD.W #$1234,D0
(memw #x101e #x1234)
(memw #x1020 #x81fc) ; DIVS #$FF00,D0
(memw #x1022 #xff00)
(memw #x1024 #x4280) ; CLR.L D0
(memw #x1026 #x0640) ; ADD.W #$FF00,D0
(memw #x1028 #xFF00)
(memw #x102a #x81fc) ; DIVS #$12,D0
(memw #x102c #x0012)
(memw #x102e #x4280) ; CLR.L D0
(memw #x1030 #x0640) ; ADD.W #$FF00,D0
(memw #x1032 #xFF00)
(memw #x1034 #x81fc) ; DIVS #$FFF0,D0
(memw #x1036 #xFFF0)
(memw #x1038 #x4280) ; CLR.L D0
(memw #x103a #x0680) ; ADD.L #$12345678
(memw #x103c #x1234)
(memw #x103e #x5678)
(memw #x1040 #x80fc) ; DIVU #0,D0
(memw #x1042 #x0000)
(memw #x1044 #x80fc) ; DIVU #1,D0
(memw #x1046 #x0001)
(memw #x1048 #x4280) ; CLR.L D0
(memw #x104a #x0640) ; ADD.W #$1234,D0
(memw #x104c #x1234)
(memw #x104e #x80fc) ; DIVU #$12,D0
(memw #x1050 #x0012)
(memw #x1052 #x4280) ; CLR.L D0
(memw #x1054 #x0640) ; ADD.W #$1235
(memw #x1056 #x1234)
(memw #x1058 #x80fc) ; DIVU #$FF00,D0
(memw #x105a #xFF00)
(memw #x105c #x4280) ; CLR.L D0
(memw #x105e #x0640) ; ADD.W #$FF00,D0
(memw #x1060 #xFF00)
(memw #x1062 #x80fc) ; DIVU #$12,D0
(memw #x1064 #x0012)
(memw #x1066 #x4280) ; CLR.L D0
(memw #x1068 #x0640) ; ADD.W #$FF00,D0
(memw #x106a #xff00)
(memw #x106c #x80fc) ; DIVU #$FFF0,D0
(memw #x106e #xfff0)
(memw #x1070 #x4e73) ; RTE
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
; Testing signed division
(print "Testing DIVS instruction")
(terpri)
(sim-step)
(sim-step)
(print "No exception messsage yet")
(terpri)
(sim-step)
(print "Exception message has been printed")
(terpri)
(sim-step)
(test-reg 0 #x12345678)
(test-reg 18 #x2002)
; Tested overflow
(sim-step)
(sim-step)
(test-reg 0 #x1234)
(sim-step)
(test-reg 0 #x00100102)
(test-reg 18 #x2000)
(sim-step)
(sim-step)
(test-reg 0 #x1234)
(sim-step)
(test-reg 0 #x0034ffee)
(test-reg 18 #x2008)
(sim-step)
(sim-step)
(test-reg 0 #xff00)
(sim-step)
(test-reg 0 #x000c0e2a)
(test-reg 18 #x2000)
(sim-step)
(sim-step)
(test-reg 0 #xff00)
(sim-step)
(test-reg 0 #xf010)
(test-reg 18 #x2008)
; Testing unsigned division
(print "Testing DIVU instruction")
(terpri)
(sim-step)
(sim-step)
(print "No exception messsage yet")
(terpri)
(sim-step)
(print "Exception message has been printed")
(terpri)
(sim-step)
(test-reg 0 #x12345678)
(test-reg 18 #x2002)
; Tested overflow
(sim-step)
(sim-step)
(test-reg 0 #x1234)
(sim-step)
(test-reg 0 #x00100102)
(test-reg 18 #x2000)
(sim-step)
(sim-step)
(test-reg 0 #x1234)
(sim-step)
(test-reg 0 #x12340000)
(test-reg 18 #x2004)
(sim-step)
(sim-step)
(test-reg 0 #xff00)
(sim-step)
(test-reg 0 #x000c0e2a)
(test-reg 18 #x2000)
(sim-step)
(sim-step)
(test-reg 0 #xff00)
(sim-step)
(test-reg 0 #xff000000)
(test-reg 18 #x2004)
