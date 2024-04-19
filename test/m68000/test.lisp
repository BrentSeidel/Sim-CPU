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
(meml #x100a #x00001000)
(memw #x100e #xd400) ; ADD.B D0,D2
(memw #x1010 #xd3c1) ; ADD.L D1,A1
(memw #x1012 #xd0c0) ; ADD.W D0,A0
(memw #x1014 #xd3c0) ; ADD.L D0,A1
(memw #x1016 #xd1c9) ; ADD.L A1,A0
(memw #x1018 #x0603) ; ADD #$46,D3
(memw #x101a #x0046)
(memw #x101c #x0604) ; ADD #$47,D4
(memw #x101e #x0047)
;
(memw #x1020 #xc903) ; ABCD D3,D4
(memw #x1022 #xd4fc) ; ADD.W #$1000,A2
(memw #x1024 #x1000)
(memw #x1026 #xda92) ; ADD.L (A2),D5
(memw #x1028 #x0686) ; ADD.L #$87654321,D6
(meml #x102a #x87654321)
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
;-------------------------------------------------------------------------------
;  Test AND instructions
;
; Load memory
;
(memw #x1000 #x0680) ; ADD.L #$0F0F0F0F,D0
(meml #x1002 #x0f0f0f0f)
(memw #x1006 #x0681) ; ADD.L #$00FF00FF,D1
(meml #x1008 #x00ff00ff)
(memw #x100c #xd480) ; ADD.L D0,D2
(memw #x100e #xd681) ; ADD.L D1,D3
;
(memw #x1010 #xc600) ; AND.B D0,D3
(memw #x1012 #xc441) ; AND.W D1,D2
(memw #x1014 #xc682) ; AND.L D2,D3
;
(memw #x1016 #x0280) ; ANDI.L #0,D0
(meml #x1018 #x00000000)
(memw #x101c #x0241) ; ANDI.W #0,D1
(memw #x101e #x0000)
(memw #x1020 #x0202) ; ANDI.B #0,D2
(memw #x1022 #x0000)
(memw #x1024 #xc0bc) ; AND.L #$0,D0
(meml #x1026 #x00000000)
;
(memw #x102a #x0640) ; ADD.W #$FFFF,D0
(memw #x102c #xffff)
(memw #x102e #x023c) ; ANDI #$08,CCR
(memw #x1030 #x0008)
(memw #x1032 #x023c) ; ANDI #$F7,CCR
(memw #x0134 #x00f7)
(memw #x1036 #x0240) ; ANDI.W #0,D0
(memw #x1038 #x0000)
(memw #x103a #x023c) ; ANDI #$04,CCR
(memw #x103c #x0004)
(memw #x103e #x023c) ; ANDI #$FB,CCR
(memw #x1040 #x00fb)
;
(print "==>  Testing AND instructions")
(sim-init)
(go #x1000)
;  Setup
(sim-step) ; ADD.L #$0F0F0F0F,D0
(test-reg 0 #x0f0f0f0f)
(sim-step) ; ADD.L #$00FF00FF,D1
(test-reg 1 #x00ff00ff)
(sim-step) ; ADD.L D0,D2
(test-reg 2 #x0f0f0f0f)
(sim-step) ; ADD.L D1,D3
(test-reg 3 #x00ff00ff)
; Testing AND
(sim-step) ; AND.B D0,D3
(test-reg 3 #x00ff000f)
(test-mask #x00 #xff)
(sim-step) ; AND.W D1,D2
(test-reg 2 #x0f0f000f)
(test-mask #x00 #xff)
(sim-step) ; AND.L D2,D3
(test-reg 3 #x000f000f)
(test-mask #x00 #xff)
; Testing ANDI
(sim-step) ; ANDI.L #0,D0
(test-reg 0 #x00000000)
(test-mask #x04 #xff)
(sim-step) ; ANDI.W #0,D1
(test-reg 1 #x00ff0000)
(test-mask #x04 #xff)
(sim-step) ; ANDI.B #0,D2
(test-reg 2 #x0f0f0000)
(test-mask #x04 #xff)
(sim-step) ; AND.L #$0,D0
(test-reg 0 #x00000000)
(test-mask #x04 #xff)
; Testing ANDI to CCR
(sim-step) ; ADD.W #$FFFF,D0
(test-reg 0 #x0000ffff)
(test-mask #x08 #xff)
(sim-step) ; ANDI #$08,CCR
(test-mask #x08 #xff)
(sim-step) ; ANDI #$F7,CCR
(test-mask #x00 #xff)
(sim-step) ; ANDI.W #0,D0
(test-reg 0 #x00000000)
(test-mask #x04 #xff)
(sim-step) ; ANDI #$04,CCR
(test-mask #x04 #xff)
(sim-step) ; ANDI #$FB,CCR
(test-mask #x00 #xff)
;-------------------------------------------------------------------------------
;  Test BCD instructions
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
; Run test
;
(print "==>  Testing BCD instructions")
(sim-init)
(go #x1000)
;
(print "Testing ABCD")
(terpri)
(sim-step) ; MOVE.B #$46,D0
(test-reg 0 #x46)
(sim-step) ; MOVE.B #$47,D1
(test-reg 1 #x47)
(sim-step) ; ABCD D0,D1
(test-reg 1 #x93)
(test-mask #x00 #xf5)
;
(sim-step) ; MOVE.B #$46,D0
(test-reg 0 #x46)
(sim-step) ; MOVE.B #$47,D1
(test-reg 1 #x47)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xFF)
(sim-step) ; MOVE #$10,CCR
(test-reg 1 #x94)
(test-mask  #x00 #xf5)
;
(sim-step) ; MOVE.B #$99,D0
(test-reg 0 #x99)
(sim-step) ; MOVE.B #$98,D1
(test-reg 1 #x98)
(sim-step) ; MOVE #0,CCR
(test-mask #x00 #xff)
(sim-step) ; ABCD D0,D1
(test-reg 1 #x97)
(test-mask #x1b #xf5)
;
(sim-step) ; MOVE.B #$99,D0
(test-reg 0 #x99)
(sim-step) ; MOVE.B #$98,D1
(test-reg 1 #x98)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xff)
(sim-step) ; ABCD D0,D1
(test-reg 1 #x98)
(test-mask #x1b #xf5)
;
(print "Testing NBCD")
(terpri)
(sim-step) ; MOVE.B #$0,D0
(test-reg 0 0)
(test-mask #x14 #xff)
(sim-step) ; NBCD D0
(test-reg 0 #x99)
(test-mask #x15 #xf5)
;
(sim-step) ; MOVE.B #$99,D0
(test-reg 0 #x99)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xff)
(sim-step) ; NBCD D0
(test-reg 0 0)
(test-mask #x11 #xf5)
;
(sim-step) ; MOVE.B #1,D0
(test-reg 0 1)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xff)
(sim-step) ; NBCD D0
(test-reg 0 #x98)
(test-mask #x11 #xf5)
;
(sim-step) ; MOVE.B #1,D0
(test-reg 0 1)
(sim-step) ; MOVE #0,CCR
(test-mask #x00 #xff)
(sim-step) ; NBCD D0
(test-reg 0 #x99)
(test-mask #x11 #xf5)
;
(print "Testing SBCD")
(terpri)
(sim-step) ; MOVE #0,CCR
(sim-step) ; MOVE #$46,D0
(sim-step) ; MOVE #$47,D1
(test-reg 0 #x46)
(test-reg 1 #x47)
(test-mask #x00 #xff)
(sim-step) ; SBCD D0,D1
(test-reg 1 1)
(test-mask #x00 #xf5)
;
(sim-step) ; MOVE #0,CCR
(sim-step) ; MOVE #$46,D0
(sim-step) ; MOVE #$47,D1
(test-reg 0 #x46)
(test-reg 1 #x47)
(test-mask #x00 #xff)
(sim-step) ; SBCD D1,D0
(test-reg 0 #x99)
(test-mask #x11 #xf5)
;
(sim-step) ; MOVE #10,CCR
(sim-step) ; MOVE #$46,D0
(sim-step) ; MOVE #$47,D1
(test-reg 0 #x46)
(test-reg 1 #x47)
(test-mask #x10 #xff)
(sim-step) ; SBCD D0,D1
(test-reg 1 0)
(test-mask #x00 #xf5)
;
(sim-step) ; MOVE #10,CCR
(sim-step) ; MOVE #$46,D0
(sim-step) ; MOVE #$47,D1
(test-reg 0 #x46)
(test-reg 1 #x47)
(test-mask #x10 #xff)
(sim-step) ; SBCD D0,D1
(test-reg 0 #x98)
(test-mask #x11 #xf5)
;-------------------------------------------------------------------------------
;  Test BCD instructions
;
; Load memory
;
(memw #x1000 #x5555) ; DC.W $5555
;
(memw #x1002 #x0878) ; BCHG #3,(DATA)
(memw #x1004 #x0003)
(memw #x1006 #x1000)
(memw #x1008 #x0878) ; BCHG #32,(DATA)
(memw #x100a #x0020)
(memw #x100c #x1000)
(memw #x100e #x0840) ; BCHG #2,D0
(memw #x1010 #x0002)
(memw #x1012 #x0141) ; BCHG D0,D1
(memw #x1014 #x0178) ; BCHG D0,(DATA)
(memw #x1016 #x1000)
;
(memw #x1018 #x0181) ; BCLR D0,D1
(memw #x101a #x0880) ; BCLR #2,D0
(memw #x101c #x0002)
(memw #x101e #x0880) ; BCLR #2,D0
(memw #x1020 #x0002)
(memw #x1022 #x08b8) ; BCLR #2,(DATA)
(memw #x1024 #x0002)
(memw #x1026 #x1000)
;
(memw #x1028 #x08c0) ; BSET #2,D0
(memw #x102a #x0002)
(memw #x102c #x08c0) ; BSET #2,D0
(memw #x102e #x0002)
(memw #x1030 #x01c1) ; BSET D0,D1
(memw #x1032 #x01f8) ; BSET D0,(DATA)
(memw #x1034 #x1000)
(memw #x1036 #x08f8) ; BSET #1,(DATA)
(memw #x1038 #x0001)
(memw #x103a #x1000)
;
(memw #x103c #x0800) ; BTST #1,D0
(memw #x103e #x0001)
(memw #x1040 #x0800) ; BTST #2,D0
(memw #x1042 #x0002)
(memw #x1044 #x0101) ; BTST D0,D1
(memw #x1046 #x0102) ; BTST D0,D2
(memw #x1048 #x0138) ; BTST D0,(DATA)
(memw #x104a #x1000)
(memw #x104c #x0838) ; BTST #2,(DATA)
(memw #x104e #x0002)
(memw #x1050 #x1000)
;
(print "==>  Testing bit instructions")
(terpri)
(sim-init)
(go #x1002)
(print "Test BCHG instruction")
(terpri)
(sim-step) ; BCHG #3,(DATA)
(test-memw #x1000 #x5d55)
(test-mask #x04 #x04)
(sim-step) ; BCHG #32,(DATA)
(test-memw #x1000 #x5c55)
(test-mask #x00 #x04)
(sim-step) ; BCHG #2,D0
(test-reg 0 #x00000004)
(test-mask #x04 #x04)
(sim-step) ; BCHG D0,D1
(test-reg 1 #x00000010)
(test-mask #x04 #x04)
(sim-step) ; BCHG D0,(DATA)
(test-memw #x1000 #x4c55)
(test-mask #x00 #x04)
;
(print "Test BCLR instruction")
(terpri)
(sim-step) ; BCLR D0,D1
(test-reg 1 #x00000000)
(test-mask #x00 #x04)
(sim-step) ; BCLR #2,D0
(test-reg 0 #x00000000)
(test-mask #x00 #x04)
(sim-step) ; BCLR #2,D0
(test-reg 0 #x00000000)
(test-mask #x04 #x04)
(sim-step) ; BCLR #2,(DATA)
(test-memw #x1000 #x4855)
(test-mask #x00 #x04)
;
(print "Test BSET instruction")
(terpri)
(sim-step) ; BSET #2,D0
(test-reg 0 #x00000004)
(test-mask #x04 #x04)
(sim-step) ; BSET #2,D0
(test-reg 0 #x00000004)
(test-mask #x00 #x04)
(sim-step) ; BSET D0,D1
(test-reg 1 #x00000010)
(test-mask #x04 #x04)
(sim-step) ; BSET D0,(DATA)
(test-memw #x1000 #x5855)
(test-mask #x04 #x04)
(sim-step) ; BSET #1,(DATA)
(test-memw #x1000 #x5a55)
(test-mask #x04 #x04)
;
(print "Test BTST instruction")
(terpri)
(sim-step) ; BTST #1,D0
(test-mask #x04 #x04)
(sim-step) ; BTST #2,D0
(test-mask #x00 #x04)
(sim-step) ; BTST D0,D1
(test-mask #x00 #x04)
(sim-step) ; BTST D0,D2
(test-mask #x04 #x04)
(sim-step) ; BTST D0,(DATA)
(test-mask #x00 #x04)
(sim-step) ; BTST #2,(DATA)
(test-mask #x04 #x04)
;-------------------------------------------------------------------------------
;  Test branch instructions
;
; Load memory
;
(memw #x1000 #x6000) ; START: BRA NEXT1
(memw #x1002 #x0006)
(memw #x1004 #x6800) ; NEXT4: BVC NEXT5
(memw #x1006 #x0032)
(memw #x1008 #x6500) ; NEXT1: BCS FAIL
(memw #x100a #x0048)
(memw #x100c #x6700) ; BEQ FAIL
(memw #x100e #x0044)
(memw #x1010 #x6b00) ; BMI FAIL
(memw #x1012 #x0040)
(memw #x1014 #x6900) ; BVS FAIL
(memw #x1016 #x003c)
(memw #x1018 #x6f00) ; BLE FAIL
(memw #x101a #x0038)
(memw #x101c #x6300) ; BLS FAIL
(memw #x101e #x0034)
(memw #x1020 #x6d00) ; BLT FAIL
(memw #x1022 #x0030)
(memw #x1024 #x6400) ; BCC NEXT2
(memw #x1026 #x001a)
;
(memw #x1028 #x5440) ; NEXT3: ADDQ.W #2,D0
(memw #x102a #x6900) ; BVS FAIL
(memw #x102c #x0026)
(memw #x102e #x6400) ; BCC FAIL
(memw #x1030 #x0022)
(memw #x1032 #x65d0) ; BCS NEXT4
(memw #x1034 #x6000) ; BRA FAIL
(memw #x1036 #x001c)
(memw #x1038 #x6700) ; NEXT5: BEQ NEXT6
(memw #x103a #x0010)
(memw #x103c #x6000) ; NEXT7: BRA PASS
(memw #x103e #x0012)
;
(memw #x1040 #x0640) ; NEXT2: ADD.W #$FFFE,D0
(memw #x1042 #xfffe)
(memw #x1044 #x6a00) ; BPL FAIL
(memw #x1046 #x000c)
(memw #x1048 #x6bde) ; BMI NEXT3
;
(memw #x104a #xdefc) ; NEXT6: ADD #STACK,SP
(memw #x104c #x2000)
(memw #x104e #x61ec) ; BSR NEXT7
;
(memw #x1050 #x60fe) ; PASS: BRA PASS
(memw #x1052 #x60fe) ; FAIL: BRA FAIL
;
(print "==> Testing branch instructions")
(terpri)
(sim-init)
(go #x1000)
(test-mask #x00 #xff)
(sim-step) ; START: BRA NEXT1
(test-reg 17 #x1008)
(sim-step) ; NEXT1: BCS FAIL
(test-reg 17 #x100c)
(sim-step) ; BEQ FAIL
(test-reg 17 #x1010)
(sim-step) ; BMI FAIL
(test-reg 17 #x1014)
(sim-step) ; BVS FAIL
(test-reg 17 #x1018)
(sim-step) ; BLE FAIL
(test-reg 17 #x101c)
(sim-step) ; BLS FAIL
(test-reg 17 #x1020)
(sim-step) ; BLT FAIL
(test-reg 17 #x1024)
(sim-step) ; BCC NEXT2
(test-reg 17 #x1040)
(sim-step) ; NEXT2: ADD.W #$FFFE,D0
(test-reg 17 #x1044)
(test-mask #x08 #xff)
(sim-step) ; BPL FAIL
(test-reg 17 #x1048)
(sim-step) ; BMI NEXT3
(test-reg 17 #x1028)
(sim-step) ; NEXT3: ADDQ.W #2,D0
(test-reg 17 #x102a)
(test-mask #x15 #xff)
(sim-step) ; BVS FAIL
(test-reg 17 #x102e)
(sim-step) ; BCC FAIL
(test-reg 17 #x1032)
(sim-step) ; BCS NEXT4
(test-reg 17 #x1004)
(sim-step) ; NEXT4: BVC NEXT5
(test-reg 17 #x1038)
(sim-step) ; NEXT5: BEQ NEXT6
(test-reg 17 #x104a)
(sim-step) ; NEXT6: ADD #STACK,SP
(test-reg 16 #x2000)
(test-reg 17 #x104e)
(sim-step) ; BSR NEXT7
(test-reg 16 #x1ffc)
(test-reg 17 #x103c)
(test-meml #x1ffc #x1050)
(sim-step) ; NEXT7: BRA PASS
(test-reg 17 #x1050)
;-------------------------------------------------------------------------------
;  Test CHK instruction
;
; Load memory
;
(memw #x0018 #x0000) ;  CHK exception vector
(memw #x001a #x2000)
;  Test program
(memw #x1000 #x0640) ; ADD.W #100,D0
(memw #x1002 #x0064)
(memw #x1004 #x0641) ; ADD.W #2000,D1
(memw #x1006 #x07d0)
(memw #x1008 #x0642) ; ADD.W #$FFFF,D2
(memw #x100a #xffff)
;
(memw #x100c #x41bc) ; CHK #1000,D0
(memw #x100e #x03e8)
(memw #x1010 #x43bc) ; CHK #1000,D1
(memw #x1012 #x03e8)
(memw #x1014 #x45bc) ; CHK #1000,D2
(memw #x1016 #x03e8)
;  Exception handler
(memw #x2000 #x4e73) ; RTE
;
(print "==> Testing CHK instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; ADD.W #100,D0
(test-reg 0 #x00000064)
(sim-step) ; ADD.W #2000,D1
(test-reg 1 #x000007d0)
(sim-step) ; ADD.W #$FFFF,D2
(test-reg 2 #x0000ffff)
;
(sim-step) ; CHK #1000,D0
(test-reg 17 #x1010)
(sim-step) ; CHK #1000,D1
(test-reg 17 #x2000)
(sim-step) ; RTE
(test-reg 17 #x1014)
(sim-step) ; CHK #1000,D2
(test-reg 17 #x2000)
(sim-step) ; RTE
(test-reg 17 #x1018)
;-------------------------------------------------------------------------------
;  Test CLR instruction
;
; Load memory
;
(memw #x1000 #x0680) ; ADD.L #$FFFFFFFF,D0
(meml #x1002 #xffffffff)
(memw #x1006 #x4200) ; CLR.B D0
(memw #x1008 #x4240) ; CLR.W D0
(memw #x100a #x4280) ; CLR.L D0
;
(print "== Testing CLR instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; ADD.L #$FFFFFFFF,D0
(test-reg 0 #xffffffff)
(test-mask #x08 #xff)
(sim-step) ; CLR.B D0
(test-reg 0 #xffffff00)
(test-mask #x04 #xff)
(sim-step) ; CLR.W D0
(test-reg 0 #xffff0000)
(test-mask #x04 #xff)
(sim-step) ; CLR.L D0
(test-reg 0 #x00000000)
(test-mask #x04 #xff)
;-------------------------------------------------------------------------------
;  Test CLR instruction
;
; Load memory
;
(memw #x1000 #x0680) ; ADD.L #$12345678,D0
(meml #x1002 #x12345678)
(memw #x1006 #xd1c0) ; ADD.L D0,A0
(memw #x1008 #x0641) ; ADD.W #$1234,D1
(memw #x100a #x1234)
(memw #x100c #xd2c1) ; ADD.W D1,A1
;
(memw #x100e #xb280) ; CMP.L D0,D1
(memw #x1010 #xb081) ; CMP.L D1,D0
(memw #x1012 #xb080) ; CMP.L D0,D0
;
(memw #x1014 #xb240) ; CMP.W D0,D1
(memw #x1016 #xb041) ; CMP.W D1,D0
(memw #x1018 #xb040) ; CMP.W D0,D0
;
(memw #x101a #xb200) ; CMP.B D0,D1
(memw #x101c #xb001) ; CMP.B D1,D0
(memw #x101e #xb000) ; CMP.B D0,D0
;
(memw #x1020 #xb3c8) ; CMP.L A0,A1
(memw #x1022 #xb1c9) ; CMP.L A1,A0
(memw #x1024 #xb1c8) ; CMP.L A0,A0
;
(memw #x1026 #xb2c8) ; CMP.W A0,A1
(memw #x1028 #xb0c9) ; CMP.W A1,A0
(memw #x102a #xb0c8) ; CMP.W A0,A0
;
(memw #x102c #xb2bc) ; CMP.L #$12345678,D1
(meml #x102e #x12345678)
(memw #x1032 #xb0bc) ; CMP.L #$0,D0
(meml #x1034 #x00000000)
(memw #x1038 #xb0bc) ; CMP.L #$12345678,D0
(meml #x103a #x12345678)
;
(memw #x103e #xb27c) ; CMP.W #$5678,D1
(memw #x1040 #x5678)
(memw #x1042 #xb27c) ; CMP.W #$0,D1
(memw #x1044 #x0000)
(memw #x1046 #xb27c) ; CMP.W #$1234,D1
(memw #x1048 #x1234)
;
(memw #x104a #xb03c) ; CMP.B #$FF,D0
(memw #x104c #x00ff)
(memw #x104e #xb03c) ; CMP.B #$77,D0
(memw #x1050 #x0077)
(memw #x1052 #xb03c) ; CMP.B #$78,D0
(memw #x1054 #x0078)
;
(memw #x1056 #xd4fc) ; ADD #DATA1,A2
(memw #x1058 #x1070)
(memw #x105a #xd6fc) ; ADD #DATA2,A3
(memw #x105c #x1086)
;
(memw #x105e #xb78a) ; CMP.L (A2)+,(A3)+
(memw #x1060 #xb78a) ; CMP.L (A2)+,(A3)+
(memw #x1062 #xb78a) ; CMP.L (A2)+,(A3)+
;
(memw #x1064 #xb74a) ; CMP.W (A2)+,(A3)+
(memw #x1066 #xb74a) ; CMP.W (A2)+,(A3)+
(memw #x1068 #xb74a) ; CMP.W (A2)+,(A3)+
;
(memw #x106a #xb70a) ; CMP.B (A2)+,(A3)+
(memw #x106c #xb70a) ; CMP.B (A2)+,(A3)+
(memw #x106e #xb70a) ; CMP.B (A2)+,(A3)+
;
(meml #x1070 #x12345678) ;DATA1: DC.L $123456578
(meml #x1074 #x00001234) ; DC.L $1234
(meml #x1078 #x12345678) ; DC.L $12345678
(memw #x107c #x5678) ; DC.W $5678
(memw #x107e #x1234) ; DC.W $1234
(memw #x1080 #x1234) ; DC.W $1234
(memw #x1082 #x3412) ; DC.B $34, $12
(memw #x1084 #x5600) ; DC.B $56
;
(meml #x1086 #x00001234) ;DATA2: DC.L $1234
(meml #x108a #x12345678) ; DC.L $12345678
(meml #x108e #x12345678) ; DC.L $12345678
(memw #x1092 #x1234) ; DC.W $1234
(memw #x1094 #x5678) ; DC.W $5678
(memw #x1096 #x1234) ; DC.W $1234
(memw #x1098 #x1234) ; DC.B $12, $34
(memw #x109a #x5600) ; DC.B $56
;
(print "==> Testing CMP instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; ADD.L #$12345678,D0
(test-reg 0 #x12345678)
(sim-step) ; ADD.L D0,A0
(test-reg 8 #x12345678)
(sim-step) ; ADD.W #$1234,D1
(test-reg 1 #x1234)
(sim-step) ; ADD.W D1,A1
(test-reg 9 #x1234)
;
(print "CMP.L")
(terpri)
(sim-step) ; CMP.L D0,D1
(test-mask #x09 #xff)
(sim-step) ; CMP.L D1,D0
(test-mask #x00 #xff)
(sim-step) ; CMP.L D0,D0
(test-mask #x04 #xff)
;
(print "CMP.W")
(terpri)
(sim-step) ; CMP.W D0,D1
(test-mask #x09 #xff)
(sim-step) ; CMP.W D1,D0
(test-mask #x00 #xff)
(sim-step) ; CMP.W D0,D0
(test-mask #x04 #xff)
;
(print "CMP.B")
(terpri)
(sim-step) ; CMP.B D0,D1
(test-mask #x09 #xff)
(sim-step) ; CMP.B D1,D0
(test-mask #x00 #xff)
(sim-step) ; CMP.B D0,D0
(test-mask #x04 #xff)
;
(print "CMPA.L")
(terpri)
(sim-step) ; CMPA.L A0,A1
(test-mask #x09 #xff)
(sim-step) ; CMPA.L A1,A0
(test-mask #x00 #xff)
(sim-step) ; CMPA.L A0,A0
(test-mask #x04 #xff)
;
(print "CMPA.W")
(terpri)
(sim-step) ; CMPA.W A0,A1
(test-mask #x09 #xff)
(sim-step) ; CMPA.W A1,A0
(test-mask #x00 #xff)
(sim-step) ; CMPA.W A0,A0
;
;  This, oddly enough is the expected result
;
(test-mask #x00 #xff)
;
(print "CMPI.L")
(terpri)
(sim-step) ; CMP.L #$12345678,D1
(test-mask #x09 #xff)
(sim-step) ; CMP.L #$0,D0
(test-mask #x00 #xff)
(sim-step) ; CMP.L #$12345678,D0
(test-mask #x04 #xff)
;
(print "CMPI.W")
(terpri)
(sim-step) ; CMP.W #$5678,D1
(test-mask #x09 #xff)
(sim-step) ; CMP.W #$0,D1
(test-mask #x00 #xff)
(sim-step) ; CMP.W #$1234,D1
(test-mask #x04 #xff)
;
(print "CMPI.B")
(terpri)
(sim-step) ; CMP.B #$FF,D0
(test-mask #x01 #xff)
(sim-step) ; CMP.B #$77,D0
(test-mask #x00 #xff)
(sim-step) ; CMP.B #$78,D0
(test-mask #x04 #xff)
;
(sim-step) ; ADD #DATA1,A2
(sim-step) ; ADD #DATA2,A3
(test-reg 10 #x1070)
(test-reg 11 #x1086)
;
(print "CMPM.L")
(terpri)
(sim-step) ; CMP.L (A2)+,(A3)+
(test-reg 10 #x1074)
(test-reg 11 #x108a)
(test-mask #x09 #xff)
(sim-step) ; CMP.L (A2)+,(A3)+
(test-reg 10 #x1078)
(test-reg 11 #x108e)
(test-mask #x00 #xff)
(sim-step) ; CMP.L (A2)+,(A3)+
(test-reg 10 #x107c)
(test-reg 11 #x1092)
(test-mask #x04 #xff)
;
(print "CMPM.W")
(terpri)
(sim-step) ; CMP.W (A2)+,(A3)+
(test-reg 10 #x107e)
(test-reg 11 #x1094)
(test-mask #x09 #xff)
(sim-step) ; CMP.W (A2)+,(A3)+
(test-reg 10 #x1080)
(test-reg 11 #x1096)
(test-mask #x00 #xff)
(sim-step) ; CMP.W (A2)+,(A3)+
(test-reg 10 #x1082)
(test-reg 11 #x1098)
(test-mask #x04 #xff)
;
(print "CMPM.B")
(terpri)
(sim-step) ; CMP.B (A2)+,(A3)+
(test-reg 10 #x1083)
(test-reg 11 #x1099)
(test-mask #x09 #xff)
(sim-step) ; CMP.B (A2)+,(A3)+
(test-reg 10 #x1084)
(test-reg 11 #x109a)
(test-mask #x00 #xff)
(sim-step) ; CMP.B (A2)+,(A3)+
(test-reg 10 #x1085)
(test-reg 11 #x109b)
(test-mask #x04 #xff)
;-------------------------------------------------------------------------------
;  Test DBcc instructions
;  Note that only DBF, DBT, and DBEQ variants are tested.
;
;  Load memory
;
(memw #x1000 #x0640) ; ADDI.W #$FFFE,D0
(memw #x1002 #xFFFE)
(memw #x1004 #x5841) ; ADDQ.W #4,D1
(memw #x1006 #x5240) ; L1: ADDQ.W #1,D0
(memw #x1008 #x50c9) ; DBT D1,L1
(memw #x100a #xFFFC)
(memw #x100c #x5240) ; L2: ADDQ.W #1,D0
(memw #x100e #x51c9) ; DBF D1,L2
(memw #x1010 #xFFFC)
(memw #x1012 #x0640) ; ADD.W #$FFF8,D0
(memw #x1014 #xFFF8)
(memw #x1016 #x5240) ; L3: ADDQ.W #1,D0
(memw #x1018 #x57c9) ; DBEQ D1,L3
(memw #x101a #xFFFC)
;
;  Execute test
;
(print "==> Testing DBcc instructions")
(terpri)
(sim-init)
(go #x1000)
(print "Testing DBT instruction")
(terpri)
(test-reg 17 #x1000)
(sim-step) ; ADDI.W #$FFFE,D0
(test-reg 0 #xFFFE)
(test-reg 17 #x1004)
(sim-step) ; ADDQ.W #4,D1
(test-reg 1 4)
(test-reg 17 #x1006)
(sim-step) ; L1: ADDQ.W #1,D0
(test-reg 0 #xFFFF)
(test-reg 17 #x1008)
(sim-step) ; DBT D1,L1
(test-reg 17 #x100c)
(test-reg 0 #xFFFF)
(test-reg 1 4)
(print "Testing DBF instruction")
(terpri)
(sim-step) ; L2: ADDQ.W #1,D0
(test-reg 0 0)
(test-reg 17 #x100e)
(sim-step) ; DBF D1,L2
(test-reg 0 0)
(test-reg 1 3)
(test-reg 17 #x100c)
(sim-step) ; L2: ADDQ.W #1,D0
(test-reg 0 1)
(test-reg 17 #x100e)
(sim-step) ; DBF D1,L2
(test-reg 0 1)
(test-reg 1 2)
(test-reg 17 #x100c)
(sim-step) ; L2: ADDQ.W #1,D0
(test-reg 0 2)
(test-reg 17 #x100e)
(sim-step) ; DBF D1,L2
(test-reg 0 2)
(test-reg 1 1)
(test-reg 17 #x100c)
(sim-step) ; L2: ADDQ.W #1,D0
(test-reg 0 3)
(test-reg 17 #x100e)
(sim-step) ; DBF D1,L2
(test-reg 0 3)
(test-reg 1 0)
(test-reg 17 #x100c)
(sim-step) ; L2: ADDQ.W #1,D0
(test-reg 0 4)
(test-reg 17 #x100e)
(sim-step) ; DBF D1,L2
(test-reg 0 4)
(test-reg 1 #xFFFF)
(test-reg 17 #x1012)
(print "Testing DBEQ instruction")
(terpri)
(sim-step) ; ADD.W #$FFF8,D0
(test-reg 0 #xFFFC)
(test-reg 17 #x1016)
(sim-step) ; L3: ADDQ.W #1,D0
(test-reg 0 #xFFFD)
(test-reg 17 #x1018)
(sim-step) ; DBEQ D1,L3
(test-reg 1 #xFFFE)
(test-reg 17 #x1016)
(sim-step) ; L3: ADDQ.W #1,D0
(test-reg 0 #xFFFE)
(test-reg 17 #x1018)
(sim-step) ; DBEQ D1,L3
(test-reg 1 #xFFFD)
(test-reg 17 #x1016)
(sim-step) ; L3: ADDQ.W #1,D0
(test-reg 0 #xFFFF)
(test-reg 17 #x1018)
(sim-step) ; DBEQ D1,L3
(test-reg 1 #xFFFC)
(test-reg 17 #x1016)
(sim-step) ; L3: ADDQ.W #1,D0
(test-reg 0 0)
(test-reg 17 #x1018)
(sim-step) ; DBEQ D1,L3
(test-reg 1 #xFFFC)
(test-reg 17 #x101C)
;-------------------------------------------------------------------------------
;  Test DIV and MUL instructions
;
;
;  Load memory
;
(memw #x0014 0)
(memw #x0016 #x108e) ; Div by 0 exception vector
;
;  Setup
;
(memw #x1000 #x2a3c) ; MOVE.L #$00001234,D5
(meml #x1002 #x00001234)
(memw #x1006 #x2c3c) ; MOVE.L #$123456578,D6
(meml #x1008 #x12345678)
(memw #x100c #x2e3c) ; MOVE.L #$0000FF00,D7
(meml #x100e #x0000ff00)
;  DIVS
(memw #x1012 #x2006) ; MOVE.L D6,D0
(memw #x1014 #x81fc) ; DIVS #0,D0
(memw #x1016 #x0000)
(memw #x1018 #x81fc) ; DIVS #1,D0
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
; DIVU
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
;  MULS
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
;  MULU
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
;  Execute test
;
(print "==> Testing DIV and MUL instructions")
(terpri)
(sim-init)
(go #x1000)
; Setup
(sim-step) ; MOVE.L #$00001234,D5
(test-reg 5 #x1234)
(sim-step) ; MOVE.L #$123456578,D6
(test-reg 6 #x12345678)
(sim-step) ; MOVE.L #$0000FF00,D7
(test-reg 7 #xff00)
;
(print "Testing DIVS instruction")
(terpri)
(sim-step) ; MOVE.L D6,D0
(test-reg 0 #x12345678)
(sim-step) ; DIVS #0,D0
(test-reg 17 #x108e)
(print "In div by 0 exception handler")
(terpri)
(sim-step) ; RTE
(test-reg 17 #x1018)
(sim-step) ; DIVS #1,D0
(test-reg 0 #x12345678)
(test-mask #x02 #xff)
(sim-step) ; MOVE.L D5,D0
(test-reg 0 #x1234)
(sim-step) ; DIVS #$12,D0
(test-reg 0 #x00100102)
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D5,D0
(test-reg 0 #x1234)
(sim-step) ; DIVS #$FF00,D0
(test-reg 0 #x0034ffee)
(test-mask #x08 #xff)
(sim-step) ; MOVE.L D7,D0
(test-reg 0 #xff00)
(sim-step) ; DIVS #$12,D0
(test-reg 0 #x000c0e2a)
(test-mask #x00 #xff)
(sim-step) ; MOVE.L #$FF00,D0
(test-reg 0 #xff00)
(sim-step) ; DIVS #$FFF0,D0
(test-reg 0 #xf010)
(test-mask #x08 #xff)
(print "Testing DIVU instruction")
(terpri)
(sim-step) ; MOVE.L D6,D0
(test-reg 0 #x12345678)
(sim-step) ; DIVU #0,D0
(test-reg 17 #x108e)
(print "In div by 0 exception handler")
(terpri)
(sim-step) ; RTE
(test-reg 17 #x103a)
(sim-step) ; DIVU #1,D0
(test-reg 0 #x12345678)
(test-mask #x02 #xff)
(sim-step) ; MOVE.L D5,D0
(test-reg 0 #x1234)
(sim-step) ; DIVU #$12,D0
(test-reg 0 #x00100102)
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D5,D0
(test-reg 0 #x1234)
(sim-step) ; DIVU #$FF00,D0
(test-reg 0 #x12340000)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L D7,D0
(test-reg 0 #xff00)
(sim-step) ; DIVU #$12,D0
(test-reg 0 #x000c0e2a)
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D7,D0
(test-reg 0 #xff00)
(sim-step) ; DIVU #$FFF0,D0
(test-reg 0 #xff000000)
(test-mask #x04 #xff)
(print "Testing MULS instruction")
(terpri)
(sim-step) ; CLR.L D0
(test-reg 0 0)
(sim-step) ; MOVE.W #-1,D0
(test-reg 0 #xffff)
(test-mask #x08 #xff)
(sim-step) ; MULS #-1,D0
(test-reg 0 1)
(test-mask #x00 #xff)
(sim-step) ; MOVE.W #-2,D0
(test-reg 0 #xfffe)
(test-mask #x08 #xff)
(sim-step) ; MULS #2,D0
(test-reg 0 #xfffffffc)
(test-mask #x08 #xff)
(sim-step) ; MOVE.W #6,D0
(test-reg 0 #xffff0006)
(test-mask #x00 #xff)
(sim-step) ; MULS #24,D0
(test-reg 0 #x90)
(test-mask #x00 #xff)
(print "Testing MULU instruction")
(terpri)
(sim-step) ; CLR.L D0
(test-reg 0 0)
(sim-step) ; MOVE.W #-1,D0
(test-reg 0 #xffff)
(test-mask #x08 #xff)
(sim-step) ; MULU #-1,D0
(test-reg 0 #xfffe0001)
(test-mask #x08 #xff)
(sim-step) ; MOVE.W #-2,D0
(test-reg 0 #xfffefffe)
(test-mask #x08 #xff)
(sim-step) ; MULU #2,D0
(test-reg 0 #x0001fffc)
(test-mask #x00 #xff)
(sim-step) ; MOVE.W #6,D0
(test-reg 0 #x00010006)
(sim-step) ; MULU #24,D0
(test-reg 0 #x00000090)
(test-mask #x00 #xff)
;-------------------------------------------------------------------------------
;  Test EOR instructions
;
;  Load memory
;
(memw #x0020 #x0000) ; Privalege violation vector
(memw #x0022 #x1048)
;
(memw #x1000 #x0680) ; ADD.L #$0F0F0F0F,D0
(meml #x1002 #x0f0f0f0f)
(memw #x1006 #x0681) ; ADD.L #$00FF00FF,D1
(meml #x1008 #x00ff00ff)
(memw #x100c #xd480) ; ADD.L D0,D2
(memw #x100e #xd681) ; ADD.L D1,D3
;
(memw #x1010 #xb103) ; EOR.B D0,D3
(memw #x1012 #xb342) ; EOR.W D1,D2
(memw #x1014 #xb583) ; EOR.L D2,D3
;
(memw #x1016 #x0a80) ; EORI.L #$55555555,D0
(meml #x1018 #x55555555)
(memw #x101c #x0a41) ; EORI.W #$AAAA,D1
(memw #x101e #xaaaa)
(memw #x1020 #x0a02) ; EORI.B #$A5,D2
(memw #x1022 #x00a5)
(memw #x1024 #x4280) ; CLR.L D0
(memw #x1026 #x0640) ; ADD.W #$FFFF,D0
(memw #x1028 #xffff)
;
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
;  Execute test
;
(print "==> Testing EOR instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; ADD.L #$0F0F0F0F,D0
(test-reg 0 #x0f0f0f0f)
(sim-step) ; ADD.L #$00FF00FF,D1
(test-reg 1 #x00ff00ff)
(sim-step) ; ADD.L D0,D2
(test-reg 2 #x0f0f0f0f)
(sim-step) ; ADD.L D1,D3
(test-reg 3 #x00ff00ff)
(print "Testing EOR instructions")
(terpri)
(sim-step) ; EOR.B D0,D3
(test-reg 3 #x00ff00f0)
(test-mask #x08 #xff)
(sim-step) ; EOR.W D1,D2
(test-reg 2 #x0f0f0ff0)
(test-mask #x00 #xff)
(sim-step) ; EOR.L D2,D3
(test-reg 3 #x0ff00f00)
(test-mask #x00 #xff)
(print "Testing EORI instructions")
(terpri)
(sim-step) ; EORI.L #$55555555,D0
(test-reg 0 #x5a5a5a5a)
(test-mask #x00 #xff)
(sim-step) ; EORI.W #$AAAA,D1
(test-reg 1 #x00ffaa55)
(test-mask #x08 #xff)
(sim-step) ; EORI.B #$A5,D2
(test-reg 2 #x0f0f0f55)
(test-mask #x00 #xff)
(sim-step) ; CLR.L D0
(sim-step) ; ADD.W #$FFFF,D0
(test-mask #x08 #xff)
(print "Testing EORI to CCR instructions")
(terpri)
(sim-step) ; EORI #$08,CCR
(test-mask #x00 #xff)
(sim-step) ; EORI #$F7,CCR
(test-mask #xf7 #xff)
(sim-step) ; CLR.W D0
(test-mask #xf4 #xff)
(sim-step) ; EORI #$04,CCR
(test-mask #xf0 #xff)
(sim-step) ; EORI #$FB,CCR
(test-mask #x0b #xff)
(sim-step) ; EORI #$2000,SR
(test-mask #x0b #xf0ff)
(test-reg 17 #x1040)
(sim-step) ; EORI #$2000,SR
(print "In privilege violation handler")
(terpri)
(test-mask #x200b #xf0ff)
(test-reg 17 #x1048)
(sim-step)  ; RTE
(test-mask #x0b #xf0ff)
(test-reg 17 #x1044)
;-------------------------------------------------------------------------------
;  Test EXG instructions
;
;  Load memory
;
(memw #x1000 #x0680) ; ADD.L #$0F0F0F0F,D0
(memlw #x1002 #x0f0f0f0f)
(memw #x1006 #x0681) ; ADD.L #$00FF00FF,D1
(meml #x1008 #x00ff00ff)
(memw #x100c #xd1fc) ; ADDA.L #$55AA55AA,A0
(meml #x100e #x55aa55aa)
(memw #x1012 #xd3fc) ; ADDA.L #$5A5A5A5A,A1
(meml #x1014 #x5a5a5a5a)
(memw #x1018 #xc141) ; EXG D0,D1
(memw #x101a #xc142) ; EXG D0,D2
(memw #x101c #xc149) ; EXG A0,A1
(memw #x101e #xc14a) ; EXG A0,A2
(memw #x1020 #xc389) ; EXG A1,D1
(memw #x1022 #xc58a) ; EXG D2,A2
;
;  Execute test
;
(print "==> Testing EXG instruction")
(sim-init)
(go #x1000)
(print "Loading initial values")
(terpri)
(sim-step) ; ADD.L #$0F0F0F0F,D0
(test-reg 0 #x0f0f0f0f)
(sim-step) ; ADD.L #$00FF00FF,D1
(test-reg 1 #x00ff00ff)
(sim-step) ; ADDA.L #$55AA55AA,A0
(test-reg 8 #x55aa55aa)
(sim-step) ; ADDA.L #$5A5A5A5A,A1
(test-reg 9 #x5a5a5a5a)
(test-reg 2 0)
(test-reg 10 0)
(print "Exchanging data registers")
(terpri)
(sim-step) ; EXG D0,D1
(test-reg 0 #x00ff00ff)
(test-reg 1 #x0f0f0f0f)
(sim-step) ; EXG D0,D2
(test-reg 0 0)
(test-reg 2 #x00ff00ff)
(print "Exchanding address registers")
(terpri)
(sim-step) ; EXG A0,A1
(test-reg 8 #x5a5a5a5a)
(test-reg 9 #x55aa55aa)
(sim-step) ; EXG A0,A2
(test-reg 8 0)
(test-reg 10 #x5a5a5a5a)
(print "Exchanging address and data registers")
(terpri)
(sim-step) ; EXG A1,D1
(test-reg 1 #x55aa55aa)
(test-reg 9 #x0f0f0f0f)
(sim-step) ; EXG D2,A2
(test-reg 2 #x5a5a5a5a)
(test-reg 10 #x00ff00ff)
;-------------------------------------------------------------------------------
;  Test EXT instructions
;
;  Load memory
;
(memw #x1000 #x0640) ; ADD.W #$0F0F,D0
(memw #x1002 #x0f0f)
(memw #x1004 #x0641) ; ADD.W #$8F0F,D1
(memw #x1006 #x8f0f)
(memw #x1008 #x0602) ; ADD.B #$0F,D2
(memw #x100a #x000f)
(memw #x100c #x0603) ; ADD.B #$FF,D3
(memw #x100e #x00ff)
(memw #x1010 #x48c0) ; EXT.L D0
(memw #x1012 #x48c1) ; EXT.L D1
(memw #x1014 #x4882) ; EXT.W D2
(memw #x1016 #x4883) ; EXT.W D3
(memw #x1018 #x4881) ; EXT.W D1
(memw #x101a #x48c1) ; EXT.L D1
;
;  Execute test
;
(print "==> Testing EXT instruction")
(terpri)
(sim-init)
(go #x1000)
(print "Loading initial values")
(terpri)
(sim-step) ; ADD.W #$0F0F,D0
(test-reg 0 #x0f0f)
(sim-step) ; ADD.W #$8F0F,D1
(test-reg 1 #x8f0f)
(sim-step) ; ADD.B #$0F,D2
(test-reg 2 #x0f)
(sim-step) ; ADD.B #$FF,D3
(test-reg 3 #xff)
(print "Extending word values")
(terpri)
(sim-step) ; EXT.L D0
(test-reg 0 #x0f0f)
(sim-step) ; EXT.L D1
(test-reg 1 #xffff8f0f)
(print "Extending byte values")
(terpri)
(sim-step) ; EXT.W D2
(test-reg 2 #x0f)
(sim-step) ; EXT.W D3
(test-reg 3 #xffff)
(print "Extras")
(terpri)
(sim-step) ; EXT.W D1
(test-reg 1 #xffff000f)
(sim-step) ; EXT.L D1
(test-reg 1 #x0f)
;-------------------------------------------------------------------------------
;  Test ILLEGAL instructions
;
;  Load memory
;
(meml (* 4 4) #x2000)
(memw #x1000 #x2e7c) ; MOVE #$3000,SP
(meml #x1002 #x00003000)
(memw #x1006 #x4AFC) ; ILLEGAL
(memw #x2000 #x4e73) ; RTE
;
;  Execute test
;
(print "==> Testing ILLEGAL instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOVE #$3000,SP
(test-reg 16 #x3000)
(test-reg 17 #x1006)
(sim-step) ; ILLEGAL
(test-reg 16 #x2ffa)
(test-reg 17 #x2000)
(test-memw #x2ffa #x2700)
(test-meml #x2ffc #x1006)
(sim-step) ; RTE
(test-reg 16 #x3000)
(test-reg 17 #x1006)
;-------------------------------------------------------------------------------
;  Test JMP/JSR instructions
;
;  Load memory
;
(memw #x1000 #x0640) ; ADD.W #$2000,D0
(memw #x1002 #x2000)
(memw #x1004 #xc18f) ; EXG D0,SP
(memw #x1006 #x4ef9) ; JMP L1
(meml #x1008 #x00001010)
(memw #x100c #x6000) ; BRA FAIL
(memw #x100e #x0020)
(memw #x1010 #xd3fc) ; L1: ADD.L #L2,A1
(meml #x1012 #x0000101c)
(memw #x1016 #x4ed1) ; JMP (A1)
(memw #x1018 #x6000) ; BRA FAIL
(memw #x101a #x0014)
(memw #x101c #x4efa) ; L2: JMP L3(PC)
(memw #x101e #x0006)
(memw #x1020 #x6000) ; BRA FAIL
(memw #x1022 #x000c)
(memw #x1024 #x4eb9) ; L3: JSR PASS
(meml #x1026 #x00001030)
;
(memw #x102e #x60fe) ; BRA FAIL
(memw #x1030 #x60fe) ; BRA PASS
;
;  Execute test
;
(print "==> Testing JMP/JSR instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; ADD.W #$2000,D0
(sim-step) ; EXG D0,SP
(test-reg 16 #x2000)
(sim-step) ; JMP L1
(test-reg 17 #x1010)
(sim-step) ; L1: ADD.L #L2,A1
(sim-step) ; JMP (A1)
(test-reg 17 #x101c)
(sim-step) ; L2: JMP L3(PC)
(test-reg 17 #x1024)
(sim-step) ; L3: JSR PASS
(test-reg 16 #x1ffc)
(test-reg 17 #x1030)
(test-meml #x1ffc #x102a)
;-------------------------------------------------------------------------------
;  Test LEA/PEA instructions
;
;  Load memory
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
(meml #x101a #x00001300)
(memw #x101e #x207c) ; MOVE.L #USER,A0
(meml #x1020 #x00001200)
(memw #x1024 #x4e60) ; MOVE A0,USP
(memw #x1026 #x41f8) ; LEA START,A0
(memw #x1028 #x1000)
;
(memw #x102a #x4850) ; PEA (A0)
(memw #x102c #x0a7c) ; EORI #$2000,SR
(memw #x102e #x2000)
(memw #x1030 #x4850) ; PEA (A0)
;
;  Execute test
;
(print "==> Testing LEA/PEA instructions")
(terpri)
(sim-init)
(go #x1000)
(print "Testing LEA instruction")
(terpri)
(sim-step) ; LEA START,A0
(test-reg 8 #x1000)
(sim-step) ; LEA (A0),A1
(test-reg 9 #x1000)
(sim-step) ; LEA 2(A1),A2
(test-reg 10 #x1002)
(sim-step) ; LEA START(PC),A3
(test-reg 11 #x1000)
(sim-step) ; ADDQ #2,D0
(test-reg 0 2)
(sim-step) ; LEA 2(A1,D0),A4
(test-reg 12 #x1004)
(sim-step) ; LEA START(PC,D0),A5
(test-reg 13 #x1002)
;
(print "Setting up stack pointers")
(terpri)
(sim-step) ; MOVE.L #SUPER,A7
(test-reg 16 #x1300) ; Test SSP
(sim-step) ; MOVE.L #USER,A0
(test-reg 8 #x1200)
(sim-step) ; MOVE A0,USP
(test-reg 15 #x1200) ; Test USP
(sim-step) ; LEA START,A0
(test-reg 8 #x1000)
;
(print "Testing PEA instruction")
(terpri)
(sim-step) ; PEA (A0)
(test-reg 16 #x12fc)
(test-meml #x12fc #x00001000)
(sim-step) ; EORI #$2000,SR
(test-mask #x0000 #xf000) ; Drop privileges
(sim-step) ; PEA (A0)
(test-reg 15 #x11fc)
(test-meml #x12fc #x00001000)
;-------------------------------------------------------------------------------
;  Test LINK/UNLK instructions
;
;  Load memory
;
(memw #x1000 #x203c) ; ADD.L $STACK,D0
(meml #x1002 #x00002000)
(memw #x1006 #xc18f) ; EXG D0,SP
(memw #x1008 #x2c7c) ; ADD.L #$FFFF,A6
(meml #x100a #x0000ffff)
(memw #x100e #x4e56) ; LINK A6,#-$20
(memw #x1010 #xffe0)
(memw #x1012 #x4e5e) ; UNLK A6
;
;  Execute test
;
(print "==> Testing LINK/UNLK instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; ADD.L $STACK,D0
(test-reg 0 #x2000)
(sim-step) ; EXG D0,SP
(test-reg 16 #x2000)
(sim-step) ; ADD.L #$FFFF,A6
(test-reg 14 #xffff)
(sim-step) ; LINK A6,#-$20
(test-reg 14 #x1ffc)
(test-reg 16 #x1fdc)
(test-meml #x1ffc #xffff)
(sim-step) ; UNLK A6
(test-reg 14 #xffff)
(test-reg 16 #x2000)
;-------------------------------------------------------------------------------
;  Test MOVE instructions
;
;  Load memory
;
(memw #x0020 #x0000) ; Exception vector for privilege violation
(memw #x0022 #x1050)
;
(memw #x1000 #x203c) ; MOVE.L #$DEADBEEF,D0
(meml #x1002 #xdeadbeef)
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
;  Execute test
;
(print "==> Testing MOVE instructions")
(terpri)
(sim-init)
(go #x1000)
(print "Testing basic MOVE")
(terpri)
(sim-step) ; MOVE.L #$DEADBEEF,D0
(test-reg 0 #xdeadbeef)
(test-mask #x08 #xff)
(sim-step) ; MOVE.W #$ABBA,D1
(test-reg 1 #xabba)
(test-mask #x08 #xff)
(sim-step) ; MOVE.B D1,D0
(test-reg 0 #xdeadbeba)
(test-mask #x08 #xff)
(sim-step) ; MOVE.L D0,A0
(test-reg 8 #xdeadbeba)
(test-mask #x08 #xff)
(sim-step) ; MOVE.W D1,A0
(test-reg 8 #xffffabba)
(test-mask #x08 #xff)
;
(print "Testing post-increment MOVE")
(terpri)
(sim-step) ; MOVE #SRC,A1
(test-reg 9 #x1040)
(sim-step) ; MOVE #DEST,A2
(test-reg 10 #x1030)
(sim-step) ; MOVE.L (A1)+,(A2)+
(test-reg 9 #x1044)
(test-reg 10 #x1034)
(test-mask #x00 #xff)
(test-memw #x1030 #x1234)
(test-memw #x1032 #x5678)
(sim-step) ; MOVE.W (A1)+,(A2)+
(test-reg 9 #x1046)
(test-reg 10 #x1036)
(test-mask #x08 #xff)
(test-memw #x1034 #x9abc)
(sim-step) ; MOVE.B (A1)+,(A2)+
(test-reg 9 #x1047)
(test-reg 10 #x1037)
(test-mask #x08 #xff)
(test-memw #x1036 #xde00)
;
(print "Testing MOVE to CCR")
(terpri)
(sim-step) ; MOVE D0,CCR
(test-mask #xba #xff)
;
(print "Testing MOVE to/from USP")
(terpri)
(sim-step) ; MOVE.W #USRSTACK,A3
(test-reg 11 #x3000)
(sim-step) ; MOVE A3,USP
(test-reg 15 #x3000)
(sim-step) ; MOVE USP,A4
(test-reg 12 #x3000)
(sim-step) ; BRA CONTINUE
;
(print "Testing MOVE from/to SR")
(terpri)
(test-reg 17 #x1052)
(sim-step) ; MOVE SR,D0
(test-reg 0 #xdead27ba)
(sim-step) ; EORI #$2000,D0
(test-reg 0 #xdead07ba)
(test-mask #xb0 #xff)
(sim-step) ; MOVE D0,SR
(test-reg 9 #x1047)
(test-mask #xba #xff)
(sim-step) ; ADDQ.L #1,A1
(test-reg 9 #x1048)
(terpri)
(sim-step) ; MOVE (A1)+,SR
(test-reg 9 #x1048) ; Post increment does not occur
(test-reg 17 #x1050)
(print "In privilege violation exception handler")
(terpri)
(sim-step) ; RTE
(test-reg 17 #x105e)
(sim-step) ; MOVE A3,USP
(test-reg 17 #x1050)
(print "In privilege violation exception handler")
(terpri)
(sim-step) ; RTE
(test-reg 17 #x1060)
(sim-step) ; MOVE USP,A4
(test-reg 17 #x1050)
(print "In privilege violation exception handler")
(terpri)
(sim-step) ; RTE
(test-reg 17 #x1062)
;-------------------------------------------------------------------------------
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
exit
