;
;  Load software
;
; Vectors
(memw #x0010 #x0000) ; Vector for illegal instruction
(memw #x0012 #x1036)
(memw #x0020 #x0000) ; Vector for privilege violation
(memw #x0022 #x103e)
;
(memw #x1000 #x2e7c) ; MOVE.L #STACK2,A7
(memw #x1002 #x0000)
(memw #x1004 #x1300)
(memw #x1006 #x207c) ; MOVE.L #STACK1,A0
(memw #x1008 #x0000)
(memw #x100a #x1200)
(memw #x100c #x4e60) ; MOVE A0,USP
;
(memw #x100e #x44fc) ; MOVE #0,CCR
(memw #x1010 #x0000)
(memw #x1012 #x4eb9) ; JSR SUB1
(memw #x1014 #x0000)
(memw #x1016 #x1026)
;
(memw #x1018 #x4eb9) ; JSR SUB2
(memw #x101a #x0000)
(memw #x101c #x102e)
;
(memw #x101e #x4afc) ; ILLEGAL
(memw #x1020 #x46fc) ; MOVE #0,SR
(memw #x1022 #x0000)
(memw #x1024 #x4e73) ; RTE
;SUB1
(memw #x1026 #x701f) ; MOVEQ #$1f,D0
(memw #x1028 #x3f00) ; MOVE.W D0,-(SP)
(memw #x102a #x7201) ; MOVEQ #1,D1
(memw #x102c #x4e77) ; RTR
;SUB2
(memw #x102e #x7202) ; MOVEQ #2,D1
(memw #x1030 #x4e75)
;ILLINST
(memw #x1036 #x7004) ; MOVEQ #4,D0
(memw #x1038 #x54af) ; ADDQ.L #2,2(SP)
(memw #x103a #x0002)
(memw #x103c #x4e73) ; RTE
;PRIVIOL
(memw #x103e #x7008) ; MOVEQ #8,D0
(memw #x1040 #x4e73) ; RTE
;
;  Define functions
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
(test-reg 16 #x1300)
(sim-step)
(test-reg 8 #x1200)
(sim-step)
(test-reg 15 #x1200)
;
(sim-step)
(test-reg 17 #x1012)
(test-reg 18 #x2000)
(sim-step)
(test-reg 16 #x12fc)
(test-reg 17 #x1026)
(test-memw #x12fc #x0000)
(test-memw #x12fe #x1018)
(sim-step)
(test-reg 0 #x001f)
(sim-step)
(test-reg 16 #x12fa)
(test-memw #x12fa #x001f)
(sim-step)
(test-reg 1 1)
(test-reg 17 #x102c)
(sim-step)
(test-reg 1 1)
(test-reg 16 #x1300)
(test-reg 17 #x1018)
(test-reg 18 #x201f)
;
(sim-step)
(test-reg 17 #x102e)
(sim-step)
(test-reg 1 2)
(test-reg 17 #x1030)
(sim-step)
(test-reg 1 2)
(test-reg 17 #x101e)
;
(sim-step)
(test-reg 17 #x1036)
(sim-step)
(test-reg 0 4)
(test-reg 17 #x1038)
(test-memw #x12fc #x0000)
(test-memw #x12fe #x101e)
(sim-step)
(test-memw #x12fc #x0000)
(test-memw #x12fe #x1020)
(sim-step)
(test-reg 0 4)
(test-reg 17 #x1020)
;
(sim-step)
(test-reg 18 0)
(test-reg 17 #x1024)
(sim-step)
(test-reg 17 #x103e)
(sim-step)
(test-reg 0 8)
(test-reg 17 #x1040)
(sim-step)
(test-reg 0 8)
(test-reg 17 #x1026)
