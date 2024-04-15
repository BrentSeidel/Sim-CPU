2
lisp
;
;  Lisp test cases for 68000 simulator
;
;  This is loosely based on the Tiny-Lisp test cases.
;
;  This is a collection of functions used in the various test scripts.
;
;-------------------------------------------------------------------------------
;  Support functions.  Load these first.
;
;  Initialize global counters
;
(setq *PASS-COUNT* 0)
(setq *FAIL-COUNT* 0)
;
;  Check if a register holds the correct value
;
(defun test-reg (reg-num expected)
  (print "Reg " reg-num " expected ")
  (print-hex expected)
  (print " actual ")
  (print-hex (reg-val reg-num))
  (if (= expected (reg-val reg-num))
    (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print " PASS"))
    (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print " *** FAIL ***")))
  (terpri))
;
;  Check a masked status register value
;
(defun test-mask (expected mask)
  (print "CCR expected ")
  (print-hex expected)
  (print ", masked ")
  (print-hex (and expected mask))
  (print ", actual ")
  (print-hex (and (reg-val 18) mask))
  (if (= (and expected mask) (and (reg-val 18) mask))
    (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print " PASS"))
    (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print " *** FAIL ***")))
  (terpri))
;
;  Check a longword in memory
;
(defun test-meml (address expected)
  (print "Memory ")
  (print-hex address)
  (print " expected ")
  (print-hex expected)
  (print " actual ")
  (print-hex (meml address))
  (if (= expected (meml address))
    (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print " PASS"))
    (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print " *** FAIL ***")))
  (terpri))
;
;  Check a word in memory
;
(defun test-memw (address expected)
  (print "Memory ")
  (print-hex address)
  (print " expected ")
  (print-hex expected)
  (print " actual ")
  (print-hex (memw address))
  (if (= expected (memw address))
    (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print " PASS"))
    (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print " *** FAIL ***")))
  (terpri))
;
;  Print summary results
;
(defun summary ()
  (print "Test cases passed: " *PASS-COUNT*)
  (terpri)
  (print "Test cases failed: " *FAIL-COUNT*)
  (terpri)
  (print "Total test cases:  " (+ *PASS-COUNT* *FAIL-COUNT*)))
;===============================================================================
;  Start of test cases
;
;-------------------------------------------------------------------------------
;  Test ADD instructions
;
; Load memory
;
(memw #x1000 #x1234)
(memw #x1002 #x5678)
;
(memw #x1004 #xd0b8) ; ADD.L (DATA),D0
(memw #x1006 #x1000)
(memw #x1008 #xd279) ; ADD.W (DATA).L,D1
(memw #x100a #x0000)
(memw #x100c #x1000)
(memw #x100e #xd400) ; ADD.B D0,D2
(memw #x1010 #xd3c1) ; ADD.L D1,A1
(memw #x1012 #xd0c0) ; ADD.W D0,A0
(memw #x1014 #xd3c0) ; ADD.L D0,A1
(memw #x1016 #xd1c9) ; ADD.L A1,A0
(memw #x1018 #x0603) ; ADD #$46,D3
(memw #x101a #x0046)
(memw #x101c #x0604)
(memw #x101e #x0047)
;
(memw #x1020 #xc903) ; ABCD D3,D4
(memw #x1022 #xd4fc) ; ADD.W #$1000,A2
(memw #x1024 #x1000)
(memw #x1026 #xda92) ; ADD.L (A2),D5
(memw #x1028 #x0686) ; ADD.L #$87654321,D6
(memw #x102a #x8765)
(memw #x102c #x4321)
;
(memw #x102e #x5280) ; ADDQ.L #1,D0
(memw #x1030 #x5452) ; ADDQ.W #2,(A2)
(memw #x1032 #x5638) ; ADDQ.B #3,(DATA)
(memw #x1034 #x1000)
;
(memw #x1036 #xd6fc) ; ADD.W #$2000,A3
(memw #x1038 #x2000)
(memw #x103a #x521b) ; ADDQ.B #1,(A3)+
(memw #x103c #x541b) ; ADDQ.B #2,(A3)+
(memw #x103e #x561b) ; ADDQ.B #3,(A3)+
(memw #x1040 #x581b) ; ADDQ.B #4,(A3)+
(memw #x1042 #x5a1b) ; ADDQ.B #5,(A3)+
(memw #x1044 #x5c1b) ; ADDQ.B #6,(A3)+
(memw #x1046 #x5e1b) ; ADDQ.B #7,(A3)+
(memw #x1048 #x501b) ; ADDQ.B #8,(A3)+
(memw #x104a #x5263) ; ADDQ.W #1,-(A3)
(memw #x104c #x5463) ; ADDQ.W #2,-(A3)
(memw #x104e #x5663) ; ADDQ.W #3,-(A3)
(memw #x1050 #x5863) ; ADDQ.W #4,-(A3)
(memw #x1052 #x529b) ; ADDQ.L #1,(A3)+
(memw #x1054 #x549b) ; ADDQ.L #4,(A3)+
;
(memw #x1056 #xd300) ; ADDX.B D0,D1
(memw #x1058 #xd701) ; ADDX.W D1,D3
(memw #x105a #xdd85) ; ADDX.L D5,D6
;
;  Run test
;
(print "==>  Testing ADD instructions")
(sim-init)
(go #x1004)
(sim-step) ; ADD.L (DATA),D0
(test-reg 0 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; ADD.W (DATA).L,D1
(test-reg 1 #x1234)
(test-mask #x00 #xff)
(sim-step) ; ADD.B D0,D2
(test-reg 2 #x78)
(test-mask #x00 #xff)
(sim-step) ; ADD.L D1,A1
(test-reg 9 #x1234)
(test-mask #x00 #xff)
(sim-step) ; ADD.W D0,A0
(test-reg 8 #x5678)
(test-mask #x00 #xff)
(sim-step) ; ADD.L D0,A1
(test-reg 9 #x123468ac)
(test-mask #x00 #xff)
(sim-step) ; ADD.L A1,A0
(test-reg 8 #x1234bf24)
(test-mask #x00 #xff)
(sim-step) ; ADD #$46,D3
(test-reg 3 #x46)
(test-mask #x00 #xff)
(sim-step) ; ADD #$47,D4
(test-reg 4 #x47)
(test-mask #x00 #xff)
;
(sim-step) ; ABCD D3,D4
(test-reg 4 #x93)
(test-mask #x00 #xf5)
(sim-step) ; ADD.W #$1000,A2
(test-reg 10 #x1000)
(test-mask #x00 #xf5)
(sim-step) ; ADD.L (A2),D5
(test-reg 5 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; ADD.L #$87654321,D6
(test-reg 6 #x87654321)
(test-mask #x08 #xff)
;
(sim-step) ; ADDQ.L #1,D0
(test-reg 0 #x12345679)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.W #2,(A2)
(test-memw #x1000 #x1236)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #3,(DATA)
(test-memw #x1000 #x1536)
(test-mask #x00 #xff)
;
(sim-step) ; ADDQ.B #1,(A3)+
(test-reg 11 #x2000)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #2,(A3)+
(test-reg 11 #x2001)
(test-memw #x2000 #x0100)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #3,(A3)+
(test-reg 11 #x2002)
(test-memw #x2000 #x0102)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #4,(A3)+
(test-reg 11 #x2003)
(test-memw #x2002 #x0300)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #5,(A3)+
(test-reg 11 #x2004)
(test-memw #x2002 #x0304)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #6,(A3)+
(test-reg 11 #x2005)
(test-memw #x2004 #x0500)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #7,(A3)+
(test-reg 11 #x2006)
(test-memw #x2004 #x0506)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #8,(A3)+
(test-reg 11 #x2007)
(test-memw #x2006 #x0700)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.W #1,-(A3)
(test-reg 11 #x2008)
(test-memw #x2006 #x0708)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.W #2,-(A3)
(test-reg 11 #x2006)
(test-memw #x2006 #x0709)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.W #3,-(A3)
(test-reg 11 #x2004)
(test-memw #x2004 #x0508)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.W #4,-(A3)
(test-reg 11 #x2002)
(test-memw #x2002 #x0307)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.L #1,(A3)+
(test-reg 11 #x2000)
(test-memw #x2000 #x0106)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.L #1,(A3)+
(test-reg 11 #x2004)
(test-memw #x2000 #x0106)
(test-memw #x2002 #x0308)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.L #4,(A3)+
(test-reg 11 #x2008)
(test-memw #x2004 #x0508)
(test-memw #x2006 #x070b)
(test-mask #x00 #xff)
;
(sim-step) ; ADDX.B D0,D1
(test-reg 1 #x12ad)
(test-mask #x0a #xff)
(sim-step) ; ADDX.W D1,D3
(test-reg 3 #xf3)
(test-mask #x08 #xff)
(sim-step) ; ADDX.L D5,D6
(test-reg 6 #x99999999)
(test-mask #x08 #xff)
;
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
