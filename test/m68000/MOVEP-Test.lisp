2
lisp
;
;  Load software
;
;
(memw #x1000 #x203c) ; MOVE.L #$12345678,D0
(memw #x1002 #x1234)
(memw #x1004 #x5678)
(memw #x1006 #x207c) ; MOVE DEST,A0
(memw #x1008 #x0000)
(memw #x100a #x1500)
;
(memw #x100c #x01c8) ; MOVEP.L D0,0(A0)
(memw #x100e #x0000)
(memw #x1010 #x0348) ; MOVEP.L 0(A0),D1
(memw #x1012 #x0000)
(memw #x1014 #x0188) ; MOVEP.W D0,9(A0)
(memw #x1016 #x0009)
(memw #x1018 #x0508) ; MOVEP.W 9(A0),D2
(memw #x101a #x0009)
;
(memw #x101c #x7801) ; MOVEQ #1,D4
(memw #x101e #x7aff) ; MOVEQ #$FF,D5
(memw #x1020 #x7000) ; MOVEQ #0,D0
; DEST
(meml #x1500 0) ; DC.L 0
(meml #x1504 0) ; DC.L 0
(meml #x1508 0) ; DC.L 0
(meml #x150c 0) ; DC.L 0
;
;  Define function
;
(defun test-reg (reg-num expected)
  (print "Reg " reg-num " expected " expected " actual " (reg-val reg-num))
  (if (= expected (reg-val reg-num))
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
;  Execute test
;
(go #x1000)
(sim-step)
(test-reg 0 #x12345678)
(sim-step)
(test-reg 8 #x1500)
(print "Testing basic long MOVEP")
(terpri)
(sim-step)
(test-memw #x1500 #x1200)
(test-memw #x1502 #x3400)
(test-memw #x1504 #x5600)
(test-memw #x1506 #x7800)
(sim-step)
(test-reg 1 #x12345678)
;
(print "Testing word MOVEP")
(terpri)
(sim-step)
(test-memw #x1508 #x0056)
(test-memw #x150a #x0078)
(sim-step)
(test-reg 2 #x5678)
;
(print "Testing MOVEQ instructions")
(terpri)
(sim-step)
(test-reg 4, 1)
(test-reg 18 #x2000)
(sim-step)
(test-reg 5 #xffffffff)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 0)
(test-reg 18 #x2004)
;
(exit)
exit
