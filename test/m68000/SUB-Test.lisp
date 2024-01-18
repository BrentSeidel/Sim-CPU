2
lisp
;
;  Test for subtraction instructions
;
; Setup
(memw #x1000 #x203c) ; MOVE.L #$87654321,D0
(memw #x1002 #x8765)
(memw #x1004 #x4321)
(memw #x1006 #x223c) ; MOVE.L #$12345678,D1
(memw #x1008 #x1234)
(memw #x100a #x5678)
(memw #x100c #x2c00) ; MOVE.L D0,D6
(memw #x100e #x2e01) ; MOVE.L D1,D7)
; Test SUB.L
(memw #x1010 #x9280) ; SUB.L D0,D1
(memw #x1012 #x2207) ; MOVE.L D7,D1
(memw #x1014 #x9081) ; SUB.L D1,D0
(memw #x1016 #x9281) ; SUB.L D1,D1
; Test SUB.W
(memw #x1018 #x2006) ; MOVE.L D6,D0
(memw #x101a #x2207) ; MOVE.L D7,D1
(memw #x101c #x9240) ; SUB.W D0,D1
(memw #x101e #x2207) ; MOVE.L D7,D1
(memw #x1020 #x9041) ; SUB.W D1,D0
(memw #x1022 #x9241) ; SUB.W D1,D1
; Test SUB.B
(memw #x1024 #x2006) ; MOVE.L D6,D0
(memw #x1026 #x2207) ; MOVE.L D7,D1
(memw #x1028 #x9200) ; SUB.B D0,D1
(memw #x102a #x2207) ; MOVE.L D7,D1
(memw #x102c #x9001) ; SUB.B D1,D0
(memw #x102e #x9201) ; SUB.B D1,D1
; Test SUBA.L
(memw #x1030 #x2046) ; MOVE.L D6,A0
(memw #x1032 #x2247) ; MOVE.L D7,A1
(memw #x1034 #x93c8) ; SUB.L A0,A1
(memw #x1036 #x2247) ; MOVE.L D7,A1
(memw #x1038 #x91c9) ; SUB.L A1,A0
(memw #x103a #x93c9) ; SUB.L A1,A1
; Test SUBA.W
(memw #x103c #x2046) ; MOVE.L D6,A0
(memw #x103e #x2247) ; MOVE.L D7,A1
(memw #x1040 #x92c8) ; SUB.W A0,A1
(memw #x1042 #x2247) ; MOVE.L D7,A1
(memw #x1044 #x90c9) ; SUB.W A1,A0
(memw #x1046 #x92c9) ; SUB.W A1,A1
; Test SUBX.L
(memw #x1048 #x44fc) ; MOVE #0,CCR
(memw #x104a #x0000)
(memw #x104c #x2006) ; MOVE.L D6,D0
(memw #x104e #x2207) ; MOVE.L D7,D1
(memw #x1050 #x9380) ; SUBX.L D0,D1
(memw #x1052 #x44fc) ; MOVE #$10,CCR
(memw #x1054 #x0010)
(memw #x1056 #x2207) ; MOVE.L D7,D1
(memw #x1058 #x9380) ; SUBX.L D0,D1
; Test SUBX.W
(memw #x105a #x44fc) ; MOVE #0,CCR
(memw #x105c #x0000)
(memw #x105e #x2006) ; MOVE.L D6,D0
(memw #x1060 #x2207) ; MOVE.L D7,D1
(memw #x1062 #x9340) ; SUBX.W D0,D1
(memw #x1064 #x44fc) ; MOVE #$10,CCR
(memw #x1066 #x0010)
(memw #x1068 #x2207) ; MOVE.L D7,D1
(memw #x106a #x9340) ; SUBX.W D0,D1
; Test SUBX.B
(memw #x106c #x44fc) ; MOVE #0,CCR
(memw #x106e #x0000)
(memw #x1070 #x2006) ; MOVE.L D6,D0
(memw #x1072 #x2207) ; MOVE.L D7,D1
(memw #x1074 #x9300) ; SUBX.B D0,D1
(memw #x1076 #x44fc) ; MOVE #$10,CCR
(memw #x1078 #x0010)
(memw #x107a #x2207) ; MOVE.L D7,D1
(memw #x107c #x9300) ; SUBX.B D0,D1
; Test SUBI.L
(memw #x107e #x203c) ; MOVE.L #$10000,D0
(memw #x1080 #x0001)
(memw #x1082 #x0000)
(memw #x1084 #x0480) ; SUBI.L #$20000,D0
(memw #x1086 #x0002)
(memw #x1088 #x0000)
(memw #x108a #x203c) ; MOVE.L #$20000,D0
(memw #x108c #x0002)
(memw #x108e #x0000)
(memw #x1090 #x0480) ; SUBI.L #$10000,D0
(memw #x1092 #x0001)
(memw #x1094 #x0000)
;
(memw #x1096 #x303c) ; MOVE.W #$1000,D0
(memw #x1098 #x1000)
(memw #x109a #x0440) ; SUBI.W #$2000,D0
(memw #x109c #x2000)
(memw #x109e #x303c) ; MOVE.W #$2000,D0
(memw #x10a0 #x2000)
(memw #x10a2 #x0440) ; SUBI.W #$1000,D0
(memw #x10a4 #x1000)
;
(memw #x10a6 #x103c) ; MOVE.B #$10,D0
(memw #x10a8 #x0010)
(memw #x10aa #x0400) ; SUBI.B #$20,D0
(memw #x10ac #x0020)
(memw #x10ae #x103c) ; MOVE.B #$20,D0
(memw #x10b0 #x0020)
(memw #x10b2 #x0400) ; SUBI.B #$10,D0
(memw #x10b4 #x0010)
;
(memw #x10b6 #x4e72) ; STOP #2000
(memw #x10b8 #x2000)
;
;  Define some functions for the test
;
(defun test-reg (reg-num expected)
  (print "Reg " reg-num " expected " expected " actual " (reg-val reg-num))
  (if (= expected (reg-val reg-num))
    (print " Pass")
    (print " *** FAIL ***"))
  (terpri))
(defun test-mask (expected mask)
  (print "CCR expected " expected ", masked " (and expected mask) ", actual " (and (reg-val 18) mask))
  (if (= (and expected mask) (and (reg-val 18) mask))
    (print " Pass")
    (print " *** FAIL ***"))
  (terpri))
(defun test-memw (address expected)
  (print "Memory " address " expected " expected " actual " (memw address))
  (if (= expected (memw address))
    (print " Pass")
    (print " *** FAIL ***"))
  (terpri))
;
(go #x1000)
(print "Test setup")
(terpri)
(sim-step)
(test-reg 0 #x87654321)
(test-reg 18 #x2008)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2000)
(sim-step)
(test-reg 6 #x87654321)
(test-reg 18 #x2008)
(sim-step)
(test-reg 7 #x12345678)
(test-reg 18 #x2000)
;
(print "Test SUB.L")
(terpri)
(sim-step)
(test-reg 1 #x8acf1357)
(test-reg 18 #x201b)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2010)
(sim-step)
(test-reg 0 #x7530eca9)
(test-reg 18 #x2002)
(sim-step)
(test-reg 1 0)
(test-reg 18 #x2004)
;
(print "Test SUB.W")
(terpri)
(sim-step)
(test-reg 0 #x87654321)
(test-reg 18 #x2008)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 #x12341357)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x8765eca9)
(test-reg 18 #x2019)
(sim-step)
(test-reg 1 #x12340000)
(test-reg 18 #x2004)
;
(print "Test SUB.B")
(terpri)
(sim-step)
(test-reg 0 #x87654321)
(test-reg 18 #x2008)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 #x12345657)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x876543a9)
(test-reg 18 #x2019)
(sim-step)
(test-reg 1 #x12345600)
(test-reg 18 #x2004)
;
(print "Test SUBA.L")
(terpri)
(sim-step)
(test-reg 8 #x87654321)
(test-reg 18 #x2004)
(sim-step)
(test-reg 9 #x12345678)
(test-reg 18 #x2004)
(sim-step)
(test-reg 9 #x8acf1357)
(test-reg 18 #x2004)
(sim-step)
(test-reg 9 #x12345678)
(test-reg 18 #x2004)
(sim-step)
(test-reg 8 #x7530eca9)
(test-reg 18 #x2004)
(sim-step)
(test-reg 9 0)
(test-reg 18 #x2004)
;
(print "Test SUBA.W")
(terpri)
(sim-step)
(test-reg 8 #x87654321)
(test-reg 18 #x2004)
(sim-step)
(test-reg 9 #x12345678)
(test-reg 18 #x2004)
(sim-step)
(test-reg 9 #x12341357)
(test-reg 18 #x2004)
(sim-step)
(test-reg 9 #x12345678)
(test-reg 18 #x2004)
(sim-step)
(test-reg 8 #x8765eca9)
(test-reg 18 #x2004)
(sim-step)
(test-reg 9 #x12340000)
(test-reg 18 #x2004)
;
(print "Test SUBX.L")
(terpri)
(sim-step)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x87654321)
(test-reg 18 #x2008)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 #x8acf1357)
(test-reg 18 #x201b)
(sim-step)
(test-reg 18 #x2010)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2010)
(sim-step)
(test-reg 1 #x8acf1356)
(test-reg 18 #x201b)
;
(print "Test SUBX.W")
(terpri)
(sim-step)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x87654321)
(test-reg 18 #x2008)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 #x12341357)
(test-reg 18 #x2000)
(sim-step)
(test-reg 18 #x2010)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2010)
(sim-step)
(test-reg 1 #x12341356)
(test-reg 18 #x2000)
;
(print "Test SUBX.B")
(terpri)
(sim-step)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x87654321)
(test-reg 18 #x2008)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 #x12345657)
(test-reg 18 #x2000)
(sim-step)
(test-reg 18 #x2010)
(sim-step)
(test-reg 1 #x12345678)
(test-reg 18 #x2010)
(sim-step)
(test-reg 1 #x12345656)
(test-reg 18 #x2000)
;
(print "Test SUBI.L")
(terpri)
(sim-step)
(test-reg 0 #x00010000)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #xffff0000)
(test-reg 18 #x2019)
(sim-step)
(test-reg 0 #x00020000)
(test-reg 18 #x2010)
(sim-step)
(test-reg 0 #x00010000)
(test-reg 18 #x2000)
;
(print "Test SUBI.W")
(terpri)
(sim-step)
(test-reg 0 #x00011000)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x0001f000)
(test-reg 18 #x2019)
(sim-step)
(test-reg 0 #x00012000)
(test-reg 18 #x2010)
(sim-step)
(test-reg 0 #x00011000)
(test-reg 18 #x2000)
;
(print "Test SUBI.B")
(terpri)
(sim-step)
(test-reg 0 #x00011010)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x000110f0)
(test-reg 18 #x2019)
(sim-step)
(test-reg 0 #x00011020)
(test-reg 18 #x2010)
(sim-step)
(test-reg 0 #x00011010)
(test-reg 18 #x2000)
(exit)
exit
