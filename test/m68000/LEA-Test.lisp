;
;  Load software
;
(memw #x1000 #x41f8) ; LEA START,A0
(memw #x1002 #x1000)
(memw #x1004 #x43d0) ; LEA (A0),A1
(memw #x1006 #x45e9) ; LEA 2(A1),A2
(memw #x1008 #x0002)
(memw #x100a #x47fa) ; LEA START(PC),A3
(memw #x100c #xfff4)
(memw #x100e #x5440) ; ADDQ #2,D0
(memw #x1010 #x49f1) ; LEA 2(A1,D0),A4
(memw #x1012 #x0002)
(memw #x1014 #x4bfb) ; LEA START(PC,D0),A5
(memw #x1016 #x00ea)
;
(memw #x1018 #x2e7c) ; MOVE.L #SUPER,A7
(memw #x101a #x0000)
(memw #x101c #x1300)
(memw #x101e #x207c) ; MOVE.L #USER,A0
(memw #x1020 #x0000)
(memw #x1022 #x1200)
(memw #x1024 #x4e60) ; MOVE A0,USP
(memw #x1026 #x41f8) ; LEA START,A0
(memw #x1028 #x1000)
;
(memw #x102a #x4850) ; PEA (A0)
(memw #x102c #x0a7c) ; EORI #$2000,SR
(memw #x102e #x2000)
(memw #x1030 #x4850) ; PEA (A0)
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
(print "Testing LEA instruction")
(terpri)
(sim-step)
(test-reg 8 #x1000)
(sim-step)
(test-reg 9 #x1000)
(sim-step)
(test-reg 10 #x1002)
(sim-step)
(test-reg 11 #x1000)
(sim-step)
(test-reg 0 2)
(sim-step)
(test-reg 12 #x1004)
(sim-step)
(test-reg 13 #x1002)
;
(print "Setting up stack pointers")
(terpri)
(sim-step)
(test-reg 16 #x1300) ; Test SSP
(sim-step)
(test-reg 8 #x1200)
(sim-step)
(test-reg 15 #x1200) ; Test USP
(sim-step)
(test-reg 8 #x1000)
;
(print "Testing PEA instruction")
(terpri)
(sim-step)
(test-reg 16 #x12fc)
(test-memw #x12fc 0)
(test-memw #x12fe #x1000)
(sim-step)
(test-reg 18 0) ; Drop privileges
(sim-step)
(test-reg 15 #x11fc)
(test-memw #x12fc 0)
(test-memw #x12fe #x1000)
