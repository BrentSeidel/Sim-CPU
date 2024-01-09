;
;  Load software
;
(memw #x0014 0)
(memw #x0016 #x108e) ; Div by 0 exception vector
;
;  Setup
;
(memw #x1000 #x2a3c) ; MOVE.L #$00001234,D5
(memw #x1002 #x0000)
(memw #x1004 #x1234)
(memw #x1006 #x2c3c) ; MOVE.L #$123456578,D6
(memw #x1008 #x1234)
(memw #x100a #x5678)
(memw #x100c #x2e3c) ; MOVE.L #$0000FF00,D7
(memw #x100e #x0000)
(memw #x1010 #xff00)
;
;  DIVS
;
(memw #x1012 #x2006) ; MOVE.L D6,D0
(memw #x1014 #x81fc) ; DIVS #0,D0
(memw #x1016 #x0000)
(memw #x1018 #x81fc) ; DIVS #0,D0
(memw #x101a #x0001)
(memw #x101c #x2005) ; MOVE.L D5,D0
(memw #x101e #x81fc) ; DIVS #$12,D0
(memw #x1020 #x0012)
(memw #x1022 #x2005) ; MOVE.L D5,D0
(memw #x1024 #x81fc) ; DIVS #$FF00,D0
(memw #x1026 #xff00)
(memw #x1028 #x2007) ; MOVE.L D7,D0
(memw #x102a #x81fc) ; DIVS #$12,D0
(memw #x102c #x0012)
(memw #x102e #x2007) ; MOVE.L #$FF00,D0
(memw #x1030 #x81fc) ; DIVS #$FFF0,D0
(memw #x1032 #xFFF0)
;
; DIVU
;
(memw #x1034 #x2006) ; MOVE.L D6,D0
(memw #x1036 #x80fc) ; DIVU #0,D0
(memw #x1038 #x0000)
(memw #x103a #x80fc) ; DIVU #1,D0
(memw #x103c #x0001)
(memw #x103e #x2005) ; MOVE.L D5,D0
(memw #x1040 #x80fc) ; DIVU #$12,D0
(memw #x1042 #x0012)
(memw #x1044 #x2005) ; MOVE.L D5,D0
(memw #x1046 #x80fc) ; DIVU #$FF00,D0
(memw #x1048 #xFF00)
(memw #x104a #x2007) ; MOVE.L D7,D0
(memw #x104c #x80fc) ; DIVU #$12,D0
(memw #x104e #x0012)
(memw #x1050 #x2007) ; MOVE.L D7,D0
(memw #x1052 #x80fc) ; DIVU #$FFF0,D0
(memw #x1054 #xfff0)
;
;  MULS
;
(memw #x1056 #x4280) ; CLR.L D0
(memw #x1058 #x303c) ; MOVE.W #-1,D0
(memw #x105a #xffff)
(memw #x105c #xc1fc) ; MULS #-1,D0
(memw #x105e #xffff)
(memw #x1060 #x303c) ; MOVE.W #-2,D0
(memw #x1062 #xfffe)
(memw #x1064 #xc1fc) ; MULS #2,D0
(memw #x1066 #x0002)
(memw #x1068 #x303c) ; MOVE.W #6,D0
(memw #x106a #x0006)
(memw #x106c #xc1fc) ; MULS #24,D0
(memw #x106e #x0018)
;
;  MULU
;
(memw #x1070 #x4280) ; CLR.L D0
(memw #x1072 #x303c) ; MOVE.W #-1,D0
(memw #x1074 #xffff)
(memw #x1076 #xc0fc) ; MULU #-1,D0
(memw #x1078 #xffff)
(memw #x107a #x303c) ; MOVE.W #-2,D0
(memw #x107c #xfffe)
(memw #x107e #xc0fc) ; MULU #2,D0
(memw #x1080 #x0002)
(memw #x1082 #x303c) ; MOVE.W #6,D0
(memw #x1084 #x0006)
(memw #x1086 #xc0fc) ; MULU #24,D0
(memw #x1088 #x0018)
;
(memw #x108e #x4e73) ; RTE
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
; Setup
(sim-step)
(test-reg 5 #x1234)
(sim-step)
(test-reg 6 #x12345678)
(sim-step)
(test-reg 7 #xff00)
; Testing signed division
(print "Testing DIVS instruction")
(terpri)
(sim-step)
(test-reg 0 #x12345678)
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
(test-reg 0 #x1234)
(sim-step)
(test-reg 0 #x00100102)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x1234)
(sim-step)
(test-reg 0 #x0034ffee)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xff00)
(sim-step)
(test-reg 0 #x000c0e2a)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #xff00)
(sim-step)
(test-reg 0 #xf010)
(test-reg 18 #x2008)
; Testing unsigned division
(print "Testing DIVU instruction")
(terpri)
(sim-step)
(test-reg 0 #x12345678)
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
(test-reg 0 #x1234)
(sim-step)
(test-reg 0 #x00100102)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x1234)
(sim-step)
(test-reg 0 #x12340000)
(test-reg 18 #x2004)
(sim-step)
(test-reg 0 #xff00)
(sim-step)
(test-reg 0 #x000c0e2a)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #xff00)
(sim-step)
(test-reg 0 #xff000000)
(test-reg 18 #x2004)
;  Testing signed multiplication
(print "Testing MULS instruction")
(terpri)
(sim-step)
(test-reg 0 0)
(sim-step)
(test-reg 0 #xffff)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 1)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #xfffe)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xfffffffc)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xffff0006)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x90)
(test-reg 18 #x2000)
;  Testing unsigned multiplication
(print "Testing MULU instruction")
(terpri)
(sim-step)
(test-reg 0 0)
(sim-step)
(test-reg 0 #xffff)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xfffe0001)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xfffefffe)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #x0001fffc)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x00010006)
(sim-step)
(test-reg 0 #x00000090)
(test-reg 18 #x2000)
