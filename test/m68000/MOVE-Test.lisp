2
lisp
;
;  Load software
;
(memw #x0020 #x0000) ; Exception vector for privilege violation
(memw #x0022 #x1050)
;
(memw #x1000 #x203c) ; MOVE.L #$DEADBEEF,D0
(memw #x1002 #xdead)
(memw #x1004 #xbeef)
(memw #x1006 #x323c) ; MOVE.W #$ABBA,D1
(memw #x1008 #xabba)
(memw #x100a #x1001) ; MOVE.B D1,D0
(memw #x100c #x2040) ; MOVE.L D0,A0
(memw #x100e #x3041) ; MOVE.W D1,A0
;
(memw #x1010 #x327c) ; MOVE #SRC,A1
(memw #x1012 #x1040)
(memw #x1014 #x347c) ; MOVE #DEST,A2
(memw #x1016 #x1030)
(memw #x1018 #x24d9) ; MOVE.L (A1)+,(A2)+
(memw #x101a #x34d9) ; MOVE.W (A1)+,(A2)+
(memw #x101c #x14d9) ; MOVE.B (A1)+,(A2)+
;
(memw #x101e #x44c0) ; MOVE D0,CCR
;
(memw #x1020 #x367c) ; MOVE.W #USRSTACK,A3
(memw #x1022 #x3000)
(memw #x1024 #x4e63) ; MOVE A3,USP
(memw #x1026 #x4e6c) ; MOVE USP,A4
(memw #x1028 #x6000) ; BRA CONTINUE
(memw #x102a #x0028)
;
(meml #x1030 0) ; DC.L 0
(meml #x1034 0) ; DC.L 0
(meml #x1038 0) ; DC.L 0
(meml #x103c 0) ; DC.L 0
(meml #x1040 #x12345678) ; DC.L $12345678
(meml #x1044 #x9abcdef0) ; DC.L $9ABCDEF0
(meml #x1048 0) ; DC.L 0
(meml #x104c 0) ; DC.L 0
;
; PRIV
(memw #x1050 #x4e73) ; RTE
; CONTINUE:
(memw #x1052 #x40c0) ; MOVE SR,D0
(memw #x1054 #x0a40) ; EORI #$2000,D0
(memw #x1056 #x2000)
(memw #x1058 #x46c0) ; MOVE D0,SR
(memw #x105a #x5249) ; ADDQ.L #1,A1
(memw #x105c #x46d9) ; MOVE (A1)+,SR
(memw #x105e #x4e63) ; MOVE A3,USP
(memw #x1060 #x4e6c) ; MOVE USP,A4
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
(print "Testing basic MOVE")
(terpri)
(sim-step)
(test-reg 0 #xdeadbeef)
(test-reg 18 #x2008)
(sim-step)
(test-reg 1 #xabba)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xdeadbeba)
(test-reg 18 #x2008)
(sim-step)
(test-reg 8 #xdeadbeba)
(test-reg 18 #x2008)
(sim-step)
(test-reg 8 #xffffabba)
(test-reg 18 #x2008)
;
(print "Testing post-increment MOVE")
(terpri)
(sim-step)
(test-reg 9 #x1040)
(sim-step)
(test-reg 10 #x1030)
(sim-step)
(test-reg 9 #x1044)
(test-reg 10 #x1034)
(test-reg 18 #x2000)
(test-memw #x1030 #x1234)
(test-memw #x1032 #x5678)
(sim-step)
(test-reg 9 #x1046)
(test-reg 10 #x1036)
(test-reg 18 #x2008)
(test-memw #x1034 #x9abc)
(sim-step)
(test-reg 9 #x1047)
(test-reg 10 #x1037)
(test-reg 18 #x2008)
(test-memw #x1036 #xde00)
;
(print "Testing MOVE to CCR")
(terpri)
(sim-step)
(test-reg 18 #x20ba)
;
(print "Testing MOVE to/from USP")
(terpri)
(sim-step)
(test-reg 11 #x3000)
(sim-step)
(test-reg 15 #x3000)
(sim-step)
(test-reg 12 #x3000)
(sim-step)
;
(print "Testing MOVE from/to SR")
(terpri)
(sim-step)
(test-reg 0 #xdead20ba)
(sim-step)
(test-reg 0 #xdead00ba)
(test-reg 18 #x20b0)
(sim-step)
(test-reg 18 #x00ba)
(sim-step)
(test-reg 9 #x1048)
(print "A privilege violation exception occurs on the next step")
(terpri)
(sim-step)
(test-reg 9 #x1048) ; Post increment does not occur
(print "A privilege violation exception occurs on the next step")
(terpri)
(sim-step)
(print "A privilege violation exception occurs on the next step")
(terpri)
(sim-step)
(exit)
exit
