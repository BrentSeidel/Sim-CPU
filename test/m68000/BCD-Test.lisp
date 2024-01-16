;
;  Script for testing BCD related instructions
;
;  ABCD
;
(memw #x1000 #x103c) ; MOVE.B #$46,D0
(memw #x1002 #x0046)
(memw #x1004 #x123c) ; MOVE.B #$47,D1
(memw #x1006 #x0047)
(memw #x1008 #xc300) ; ABCD D0,D1
;
(memw #x100a #x103c) ; MOVE.B #$46,D0
(memw #x100c #x0046)
(memw #x100e #x123c) ; MOVE.B #$47,D1
(memw #x1010 #x0047)
(memw #x1012 #x44fc) ; MOVE #$10,CCR
(memw #x1014 #x0010)
(memw #x1016 #xc300) ; ABCD D0,D1
;
(memw #x1018 #x103c) ; MOVE.B #$99,D0
(memw #x101a #x0099)
(memw #x101c #x123c) ; MOVE.B #$98,D1
(memw #x101e #x0098)
(memw #x1020 #x44fc) ; MOVE #0,CCR
(memw #x1022 #x0000)
(memw #x1024 #xc300) ; ABCD D0,D1
;
(memw #x1026 #x103c) ; MOVE.B #$99,D0
(memw #x1028 #x0099)
(memw #x102a #x123c) ; MOVE.B #$98,D1
(memw #x102c #x0098)
(memw #x102e #x44fc) ; MOVE #$10,CCR
(memw #x1030 #x0010)
(memw #x1032 #xc300) ; ABCD D0,D1
;
;  NBCD
;
(memw #x1034 #x103c) ; MOVE.B #$0,D0
(memw #x1036 #x0000)
(memw #x1038 #x4800) ; NBCD D0
;
(memw #x103a #x103c) ; MOVE.B #$99,D0
(memw #x103c #x0099)
(memw #x103e #x44fc) ; MOVE #$10,CCR
(memw #x1040 #x0010)
(memw #x1042 #x4800) ; NBCD D0
;
(memw #x1044 #x103c) ; MOVE.B #1,D0
(memw #x1046 #x0001)
(memw #x1048 #x44fc) ; MOVE #$10,CCR
(memw #x104a #x0010)
(memw #x104c #x4800) ; NBCD D0
;
(memw #x104e #x103c) ; MOVE.B #1,D0
(memw #x1050 #x0001)
(memw #x1052 #x44fc) ; MOVE #0,CCR
(memw #x1054 #x0000)
(memw #x1056 #x4800) ; NBCD D0
;
;  SBCD
;
(memw #x1058 #x44fc) ; MOVE #0,CCR
(memw #x105a #x0000)
(memw #x105c #x303c) ; MOVE #$46,D0
(memw #x105e #x0046)
(memw #x1060 #x323c) ; MOVE #$47,D1
(memw #x1062 #x0047)
(memw #x1064 #x8300) ; SBCD D0,D1
;
(memw #x1066 #x44fc) ; MOVE #0,CCR
(memw #x1068 #x0000)
(memw #x106a #x303c) ; MOVE #$46,D0
(memw #x106c #x0046)
(memw #x106e #x323c) ; MOVE #$47,D1
(memw #x1070 #x0047)
(memw #x1072 #x8101) ; SBCD D1,D0
;
(memw #x1074 #x44fc) ; MOVE #10,CCR
(memw #x1076 #x0010)
(memw #x1078 #x303c) ; MOVE #$46,D0
(memw #x107a #x0046)
(memw #x107c #x323c) ; MOVE #$47,D1
(memw #x107e #x0047)
(memw #x1080 #x8300) ; SBCD D0,D1
;
(memw #x1082 #x44fc) ; MOVE #10,CCR
(memw #x1084 #x0010)
(memw #x1086 #x303c) ; MOVE #$46,D0
(memw #x1088 #x0046)
(memw #x108a #x323c) ; MOVE #$47,D1
(memw #x108c #x0047)
(memw #x108e #x8101) ; SBCD D0,D1
;
;  Define function
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
;
; Run test
;
(go #x1000)
;
(print "Testing ABCD")
(terpri)
(sim-step)
(test-reg 0 #x46)
(sim-step)
(test-reg 1 #x47)
(sim-step)
(test-reg 1 #x93)
(test-mask #x2000 #xfff5)
;
(sim-step)
(test-reg 0 #x46)
(sim-step)
(test-reg 1 #x47)
(sim-step)
(test-reg 18 #x2010)
(sim-step)
(test-reg 1 #x94)
(test-mask  #x2000 #xfff5)
;
(sim-step)
(test-reg 0 #x99)
(sim-step)
(test-reg 1 #x98)
(sim-step)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 #x97)
(test-mask #x201b #xfff5)
;
(sim-step)
(test-reg 0 #x99)
(sim-step)
(test-reg 1 #x98)
(sim-step)
(test-reg 18 #x2010)
(sim-step)
(test-reg 1 #x98)
(test-mask #x201b #xfff5)
;
(print "Testing NBCD")
(terpri)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2014)
(sim-step)
(test-reg 0 #x99)
(test-mask #x201b #xfff5)
;
(sim-step)
(test-reg 0 #x99)
(sim-step)
(test-reg 18 #x2010)
(sim-step)
(test-reg 0 0)
(test-mask #x201b #xfff5)
;
(sim-step)
(test-reg 0 1)
(sim-step)
(test-reg 18 #x2010)
(sim-step)
(test-reg 0 #x98)
(test-mask #x201b #xfff5)
;
(sim-step)
(test-reg 0 1)
(sim-step)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x99)
(test-mask #x201b #xfff5)
;
(print "Testing SBCD")
(terpri)
(sim-step)
(sim-step)
(sim-step)
(test-reg 0 #x46)
(test-reg 1 #x47)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 1)
(test-mask #x200a #xfff5)
;
(sim-step)
(sim-step)
(sim-step)
(test-reg 0 #x46)
(test-reg 1 #x47)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x99)
(test-mask #x201b #xfff5)
;
(sim-step)
(sim-step)
(sim-step)
(test-reg 0 #x46)
(test-reg 1 #x47)
(test-reg 18 #x2010)
(sim-step)
(test-reg 1 0)
(test-mask #x200a #xfff5)
;
(sim-step)
(sim-step)
(sim-step)
(test-reg 0 #x46)
(test-reg 1 #x47)
(test-reg 18 #x2010)
(sim-step)
(test-reg 0 #x98)
(test-mask #x201b #xfff5)
