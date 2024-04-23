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
;  Definitions for registers
;
(setq D0 0)
(setq D1 1)
(setq D2 2)
(setq D3 3)
(setq D4 4)
(setq D5 5)
(setq D6 6)
(setq D7 7)
(setq A0 8)
(setq A1 9)
(setq A2 10)
(setq A3 11)
(setq A4 12)
(setq A5 13)
(setq A6 14)
(setq USP 15)
(setq SSP 16)
(setq PC 17)
(setq SR 18)
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
  (print-hex (and (reg-val SR) mask))
  (if (= (and expected mask) (and (reg-val SR) mask))
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
(test-reg D0 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; ADD.W (DATA).L,D1
(test-reg D1 #x1234)
(test-mask #x00 #xff)
(sim-step) ; ADD.B D0,D2
(test-reg D2 #x78)
(test-mask #x00 #xff)
(sim-step) ; ADD.L D1,A1
(test-reg A1 #x1234)
(test-mask #x00 #xff)
(sim-step) ; ADD.W D0,A0
(test-reg A0 #x5678)
(test-mask #x00 #xff)
(sim-step) ; ADD.L D0,A1
(test-reg A1 #x123468ac)
(test-mask #x00 #xff)
(sim-step) ; ADD.L A1,A0
(test-reg A0 #x1234bf24)
(test-mask #x00 #xff)
(sim-step) ; ADD #$46,D3
(test-reg D3 #x46)
(test-mask #x00 #xff)
(sim-step) ; ADD #$47,D4
(test-reg D4 #x47)
(test-mask #x00 #xff)
;
(sim-step) ; ABCD D3,D4
(test-reg D4 #x93)
(test-mask #x00 #xf5)
(sim-step) ; ADD.W #$1000,A2
(test-reg A2 #x1000)
(test-mask #x00 #xf5)
(sim-step) ; ADD.L (A2),D5
(test-reg D5 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; ADD.L #$87654321,D6
(test-reg D6 #x87654321)
(test-mask #x08 #xff)
;
(sim-step) ; ADDQ.L #1,D0
(test-reg D0 #x12345679)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.W #2,(A2)
(test-memw #x1000 #x1236)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #3,(DATA)
(test-memw #x1000 #x1536)
(test-mask #x00 #xff)
;
(sim-step) ; ADDQ.B #1,(A3)+
(test-reg A3 #x2000)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #2,(A3)+
(test-reg A3 #x2001)
(test-memw #x2000 #x0100)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #3,(A3)+
(test-reg A3 #x2002)
(test-memw #x2000 #x0102)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #4,(A3)+
(test-reg A3 #x2003)
(test-memw #x2002 #x0300)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #5,(A3)+
(test-reg A3 #x2004)
(test-memw #x2002 #x0304)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #6,(A3)+
(test-reg A3 #x2005)
(test-memw #x2004 #x0500)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #7,(A3)+
(test-reg A3 #x2006)
(test-memw #x2004 #x0506)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.B #8,(A3)+
(test-reg A3 #x2007)
(test-memw #x2006 #x0700)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.W #1,-(A3)
(test-reg A3 #x2008)
(test-memw #x2006 #x0708)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.W #2,-(A3)
(test-reg A3 #x2006)
(test-memw #x2006 #x0709)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.W #3,-(A3)
(test-reg A3 #x2004)
(test-memw #x2004 #x0508)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.W #4,-(A3)
(test-reg A3 #x2002)
(test-memw #x2002 #x0307)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.L #1,(A3)+
(test-reg A3 #x2000)
(test-memw #x2000 #x0106)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.L #1,(A3)+
(test-reg A3 #x2004)
(test-memw #x2000 #x0106)
(test-memw #x2002 #x0308)
(test-mask #x00 #xff)
(sim-step) ; ADDQ.L #4,(A3)+
(test-reg A3 #x2008)
(test-memw #x2004 #x0508)
(test-memw #x2006 #x070b)
(test-mask #x00 #xff)
;
(sim-step) ; ADDX.B D0,D1
(test-reg D1 #x12ad)
(test-mask #x0a #xff)
(sim-step) ; ADDX.W D1,D3
(test-reg D3  #xf3)
(test-mask #x08 #xff)
(sim-step) ; ADDX.L D5,D6
(test-reg D6 #x99999999)
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
(test-reg D0 #x0f0f0f0f)
(sim-step) ; ADD.L #$00FF00FF,D1
(test-reg D1 #x00ff00ff)
(sim-step) ; ADD.L D0,D2
(test-reg D2  #x0f0f0f0f)
(sim-step) ; ADD.L D1,D3
(test-reg D3  #x00ff00ff)
; Testing AND
(sim-step) ; AND.B D0,D3
(test-reg D3  #x00ff000f)
(test-mask #x00 #xff)
(sim-step) ; AND.W D1,D2
(test-reg D2  #x0f0f000f)
(test-mask #x00 #xff)
(sim-step) ; AND.L D2,D3
(test-reg D3  #x000f000f)
(test-mask #x00 #xff)
; Testing ANDI
(sim-step) ; ANDI.L #0,D0
(test-reg D0 #x00000000)
(test-mask #x04 #xff)
(sim-step) ; ANDI.W #0,D1
(test-reg D1 #x00ff0000)
(test-mask #x04 #xff)
(sim-step) ; ANDI.B #0,D2
(test-reg D2  #x0f0f0000)
(test-mask #x04 #xff)
(sim-step) ; AND.L #$0,D0
(test-reg D0 #x00000000)
(test-mask #x04 #xff)
; Testing ANDI to CCR
(sim-step) ; ADD.W #$FFFF,D0
(test-reg D0 #x0000ffff)
(test-mask #x08 #xff)
(sim-step) ; ANDI #$08,CCR
(test-mask #x08 #xff)
(sim-step) ; ANDI #$F7,CCR
(test-mask #x00 #xff)
(sim-step) ; ANDI.W #0,D0
(test-reg D0 #x00000000)
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
(test-reg D0 #x46)
(sim-step) ; MOVE.B #$47,D1
(test-reg D1 #x47)
(sim-step) ; ABCD D0,D1
(test-reg D1 #x93)
(test-mask #x00 #xf5)
;
(sim-step) ; MOVE.B #$46,D0
(test-reg D0 #x46)
(sim-step) ; MOVE.B #$47,D1
(test-reg D1 #x47)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xFF)
(sim-step) ; MOVE #$10,CCR
(test-reg D1 #x94)
(test-mask  #x00 #xf5)
;
(sim-step) ; MOVE.B #$99,D0
(test-reg D0 #x99)
(sim-step) ; MOVE.B #$98,D1
(test-reg D1 #x98)
(sim-step) ; MOVE #0,CCR
(test-mask #x00 #xff)
(sim-step) ; ABCD D0,D1
(test-reg D1 #x97)
(test-mask #x1b #xf5)
;
(sim-step) ; MOVE.B #$99,D0
(test-reg D0 #x99)
(sim-step) ; MOVE.B #$98,D1
(test-reg D1 #x98)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xff)
(sim-step) ; ABCD D0,D1
(test-reg D1 #x98)
(test-mask #x1b #xf5)
;
(print "Testing NBCD")
(terpri)
(sim-step) ; MOVE.B #$0,D0
(test-reg D0 0)
(test-mask #x14 #xff)
(sim-step) ; NBCD D0
(test-reg D0 #x99)
(test-mask #x15 #xf5)
;
(sim-step) ; MOVE.B #$99,D0
(test-reg D0 #x99)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xff)
(sim-step) ; NBCD D0
(test-reg D0 0)
(test-mask #x11 #xf5)
;
(sim-step) ; MOVE.B #1,D0
(test-reg D0 1)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xff)
(sim-step) ; NBCD D0
(test-reg D0 #x98)
(test-mask #x11 #xf5)
;
(sim-step) ; MOVE.B #1,D0
(test-reg D0 1)
(sim-step) ; MOVE #0,CCR
(test-mask #x00 #xff)
(sim-step) ; NBCD D0
(test-reg D0 #x99)
(test-mask #x11 #xf5)
;
(print "Testing SBCD")
(terpri)
(sim-step) ; MOVE #0,CCR
(sim-step) ; MOVE #$46,D0
(sim-step) ; MOVE #$47,D1
(test-reg D0 #x46)
(test-reg D1 #x47)
(test-mask #x00 #xff)
(sim-step) ; SBCD D0,D1
(test-reg D1 1)
(test-mask #x00 #xf5)
;
(sim-step) ; MOVE #0,CCR
(sim-step) ; MOVE #$46,D0
(sim-step) ; MOVE #$47,D1
(test-reg D0 #x46)
(test-reg D1 #x47)
(test-mask #x00 #xff)
(sim-step) ; SBCD D1,D0
(test-reg D0 #x99)
(test-mask #x11 #xf5)
;
(sim-step) ; MOVE #10,CCR
(sim-step) ; MOVE #$46,D0
(sim-step) ; MOVE #$47,D1
(test-reg D0 #x46)
(test-reg D1 #x47)
(test-mask #x10 #xff)
(sim-step) ; SBCD D0,D1
(test-reg D1 0)
(test-mask #x00 #xf5)
;
(sim-step) ; MOVE #10,CCR
(sim-step) ; MOVE #$46,D0
(sim-step) ; MOVE #$47,D1
(test-reg D0 #x46)
(test-reg D1 #x47)
(test-mask #x10 #xff)
(sim-step) ; SBCD D0,D1
(test-reg D0 #x98)
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
(test-reg D0 #x00000004)
(test-mask #x04 #x04)
(sim-step) ; BCHG D0,D1
(test-reg D1 #x00000010)
(test-mask #x04 #x04)
(sim-step) ; BCHG D0,(DATA)
(test-memw #x1000 #x4c55)
(test-mask #x00 #x04)
;
(print "Test BCLR instruction")
(terpri)
(sim-step) ; BCLR D0,D1
(test-reg D1 #x00000000)
(test-mask #x00 #x04)
(sim-step) ; BCLR #2,D0
(test-reg D0 #x00000000)
(test-mask #x00 #x04)
(sim-step) ; BCLR #2,D0
(test-reg D0 #x00000000)
(test-mask #x04 #x04)
(sim-step) ; BCLR #2,(DATA)
(test-memw #x1000 #x4855)
(test-mask #x00 #x04)
;
(print "Test BSET instruction")
(terpri)
(sim-step) ; BSET #2,D0
(test-reg D0 #x00000004)
(test-mask #x04 #x04)
(sim-step) ; BSET #2,D0
(test-reg D0 #x00000004)
(test-mask #x00 #x04)
(sim-step) ; BSET D0,D1
(test-reg D1 #x00000010)
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
(test-reg PC #x1008)
(sim-step) ; NEXT1: BCS FAIL
(test-reg PC #x100c)
(sim-step) ; BEQ FAIL
(test-reg PC #x1010)
(sim-step) ; BMI FAIL
(test-reg PC #x1014)
(sim-step) ; BVS FAIL
(test-reg PC #x1018)
(sim-step) ; BLE FAIL
(test-reg PC #x101c)
(sim-step) ; BLS FAIL
(test-reg PC #x1020)
(sim-step) ; BLT FAIL
(test-reg PC #x1024)
(sim-step) ; BCC NEXT2
(test-reg PC #x1040)
(sim-step) ; NEXT2: ADD.W #$FFFE,D0
(test-reg PC #x1044)
(test-mask #x08 #xff)
(sim-step) ; BPL FAIL
(test-reg PC #x1048)
(sim-step) ; BMI NEXT3
(test-reg PC #x1028)
(sim-step) ; NEXT3: ADDQ.W #2,D0
(test-reg PC #x102a)
(test-mask #x15 #xff)
(sim-step) ; BVS FAIL
(test-reg PC #x102e)
(sim-step) ; BCC FAIL
(test-reg PC #x1032)
(sim-step) ; BCS NEXT4
(test-reg PC #x1004)
(sim-step) ; NEXT4: BVC NEXT5
(test-reg PC #x1038)
(sim-step) ; NEXT5: BEQ NEXT6
(test-reg PC #x104a)
(sim-step) ; NEXT6: ADD #STACK,SP
(test-reg SSP #x2000)
(test-reg PC #x104e)
(sim-step) ; BSR NEXT7
(test-reg SSP #x1ffc)
(test-reg PC #x103c)
(test-meml #x1ffc #x1050)
(sim-step) ; NEXT7: BRA PASS
(test-reg PC #x1050)
;-------------------------------------------------------------------------------
;  Test CHK instruction
;
; Load memory
;
(meml #x0018 #x00002000) ;  CHK exception vector
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
(test-reg D0 #x00000064)
(sim-step) ; ADD.W #2000,D1
(test-reg D1 #x000007d0)
(sim-step) ; ADD.W #$FFFF,D2
(test-reg D2  #x0000ffff)
;
(sim-step) ; CHK #1000,D0
(test-reg PC #x1010)
(sim-step) ; CHK #1000,D1
(test-reg PC #x2000)
(sim-step) ; RTE
(test-reg PC #x1014)
(sim-step) ; CHK #1000,D2
(test-reg PC #x2000)
(sim-step) ; RTE
(test-reg PC #x1018)
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
(test-reg D0 #xffffffff)
(test-mask #x08 #xff)
(sim-step) ; CLR.B D0
(test-reg D0 #xffffff00)
(test-mask #x04 #xff)
(sim-step) ; CLR.W D0
(test-reg D0 #xffff0000)
(test-mask #x04 #xff)
(sim-step) ; CLR.L D0
(test-reg D0 #x00000000)
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
(test-reg D0 #x12345678)
(sim-step) ; ADD.L D0,A0
(test-reg A0 #x12345678)
(sim-step) ; ADD.W #$1234,D1
(test-reg D1 #x1234)
(sim-step) ; ADD.W D1,A1
(test-reg A1 #x1234)
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
(test-reg A2 #x1070)
(test-reg A3 #x1086)
;
(print "CMPM.L")
(terpri)
(sim-step) ; CMP.L (A2)+,(A3)+
(test-reg A2 #x1074)
(test-reg A3 #x108a)
(test-mask #x09 #xff)
(sim-step) ; CMP.L (A2)+,(A3)+
(test-reg A2 #x1078)
(test-reg A3 #x108e)
(test-mask #x00 #xff)
(sim-step) ; CMP.L (A2)+,(A3)+
(test-reg A2 #x107c)
(test-reg A3 #x1092)
(test-mask #x04 #xff)
;
(print "CMPM.W")
(terpri)
(sim-step) ; CMP.W (A2)+,(A3)+
(test-reg A2 #x107e)
(test-reg A3 #x1094)
(test-mask #x09 #xff)
(sim-step) ; CMP.W (A2)+,(A3)+
(test-reg A2 #x1080)
(test-reg A3 #x1096)
(test-mask #x00 #xff)
(sim-step) ; CMP.W (A2)+,(A3)+
(test-reg A2 #x1082)
(test-reg A3 #x1098)
(test-mask #x04 #xff)
;
(print "CMPM.B")
(terpri)
(sim-step) ; CMP.B (A2)+,(A3)+
(test-reg A2 #x1083)
(test-reg A3 #x1099)
(test-mask #x09 #xff)
(sim-step) ; CMP.B (A2)+,(A3)+
(test-reg A2 #x1084)
(test-reg A3 #x109a)
(test-mask #x00 #xff)
(sim-step) ; CMP.B (A2)+,(A3)+
(test-reg A2 #x1085)
(test-reg A3 #x109b)
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
(test-reg PC #x1000)
(sim-step) ; ADDI.W #$FFFE,D0
(test-reg D0 #xFFFE)
(test-reg PC #x1004)
(sim-step) ; ADDQ.W #4,D1
(test-reg D1 4)
(test-reg PC #x1006)
(sim-step) ; L1: ADDQ.W #1,D0
(test-reg D0 #xFFFF)
(test-reg PC #x1008)
(sim-step) ; DBT D1,L1
(test-reg PC #x100c)
(test-reg D0 #xFFFF)
(test-reg D1 4)
(print "Testing DBF instruction")
(terpri)
(sim-step) ; L2: ADDQ.W #1,D0
(test-reg D0 0)
(test-reg PC #x100e)
(sim-step) ; DBF D1,L2
(test-reg D0 0)
(test-reg D1 3)
(test-reg PC #x100c)
(sim-step) ; L2: ADDQ.W #1,D0
(test-reg D0 1)
(test-reg PC #x100e)
(sim-step) ; DBF D1,L2
(test-reg D0 1)
(test-reg D1 2)
(test-reg PC #x100c)
(sim-step) ; L2: ADDQ.W #1,D0
(test-reg D0 2)
(test-reg PC #x100e)
(sim-step) ; DBF D1,L2
(test-reg D0 2)
(test-reg D1 1)
(test-reg PC #x100c)
(sim-step) ; L2: ADDQ.W #1,D0
(test-reg D0 3)
(test-reg PC #x100e)
(sim-step) ; DBF D1,L2
(test-reg D0 3)
(test-reg D1 0)
(test-reg PC #x100c)
(sim-step) ; L2: ADDQ.W #1,D0
(test-reg D0 4)
(test-reg PC #x100e)
(sim-step) ; DBF D1,L2
(test-reg D0 4)
(test-reg D1 #xFFFF)
(test-reg PC #x1012)
(print "Testing DBEQ instruction")
(terpri)
(sim-step) ; ADD.W #$FFF8,D0
(test-reg D0 #xFFFC)
(test-reg PC #x1016)
(sim-step) ; L3: ADDQ.W #1,D0
(test-reg D0 #xFFFD)
(test-reg PC #x1018)
(sim-step) ; DBEQ D1,L3
(test-reg D1 #xFFFE)
(test-reg PC #x1016)
(sim-step) ; L3: ADDQ.W #1,D0
(test-reg D0 #xFFFE)
(test-reg PC #x1018)
(sim-step) ; DBEQ D1,L3
(test-reg D1 #xFFFD)
(test-reg PC #x1016)
(sim-step) ; L3: ADDQ.W #1,D0
(test-reg D0 #xFFFF)
(test-reg PC #x1018)
(sim-step) ; DBEQ D1,L3
(test-reg D1 #xFFFC)
(test-reg PC #x1016)
(sim-step) ; L3: ADDQ.W #1,D0
(test-reg D0 0)
(test-reg PC #x1018)
(sim-step) ; DBEQ D1,L3
(test-reg D1 #xFFFC)
(test-reg PC #x101C)
;-------------------------------------------------------------------------------
;  Test DIV and MUL instructions
;
;
;  Load memory
;
(meml #x0014 #x0000108e) ; Div by 0 exception vector
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
(test-reg D5 #x1234)
(sim-step) ; MOVE.L #$123456578,D6
(test-reg D6 #x12345678)
(sim-step) ; MOVE.L #$0000FF00,D7
(test-reg D7 #xff00)
;
(print "Testing DIVS instruction")
(terpri)
(sim-step) ; MOVE.L D6,D0
(test-reg D0 #x12345678)
(sim-step) ; DIVS #0,D0
(test-reg PC #x108e)
(print "In div by 0 exception handler")
(terpri)
(sim-step) ; RTE
(test-reg PC #x1018)
(sim-step) ; DIVS #1,D0
(test-reg D0 #x12345678)
(test-mask #x02 #xff)
(sim-step) ; MOVE.L D5,D0
(test-reg D0 #x1234)
(sim-step) ; DIVS #$12,D0
(test-reg D0 #x00100102)
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D5,D0
(test-reg D0 #x1234)
(sim-step) ; DIVS #$FF00,D0
(test-reg D0 #x0034ffee)
(test-mask #x08 #xff)
(sim-step) ; MOVE.L D7,D0
(test-reg D0 #xff00)
(sim-step) ; DIVS #$12,D0
(test-reg D0 #x000c0e2a)
(test-mask #x00 #xff)
(sim-step) ; MOVE.L #$FF00,D0
(test-reg D0 #xff00)
(sim-step) ; DIVS #$FFF0,D0
(test-reg D0 #xf010)
(test-mask #x08 #xff)
(print "Testing DIVU instruction")
(terpri)
(sim-step) ; MOVE.L D6,D0
(test-reg D0 #x12345678)
(sim-step) ; DIVU #0,D0
(test-reg PC #x108e)
(print "In div by 0 exception handler")
(terpri)
(sim-step) ; RTE
(test-reg PC #x103a)
(sim-step) ; DIVU #1,D0
(test-reg D0 #x12345678)
(test-mask #x02 #xff)
(sim-step) ; MOVE.L D5,D0
(test-reg D0 #x1234)
(sim-step) ; DIVU #$12,D0
(test-reg D0 #x00100102)
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D5,D0
(test-reg D0 #x1234)
(sim-step) ; DIVU #$FF00,D0
(test-reg D0 #x12340000)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L D7,D0
(test-reg D0 #xff00)
(sim-step) ; DIVU #$12,D0
(test-reg D0 #x000c0e2a)
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D7,D0
(test-reg D0 #xff00)
(sim-step) ; DIVU #$FFF0,D0
(test-reg D0 #xff000000)
(test-mask #x04 #xff)
(print "Testing MULS instruction")
(terpri)
(sim-step) ; CLR.L D0
(test-reg D0 0)
(sim-step) ; MOVE.W #-1,D0
(test-reg D0 #xffff)
(test-mask #x08 #xff)
(sim-step) ; MULS #-1,D0
(test-reg D0 1)
(test-mask #x00 #xff)
(sim-step) ; MOVE.W #-2,D0
(test-reg D0 #xfffe)
(test-mask #x08 #xff)
(sim-step) ; MULS #2,D0
(test-reg D0 #xfffffffc)
(test-mask #x08 #xff)
(sim-step) ; MOVE.W #6,D0
(test-reg D0 #xffff0006)
(test-mask #x00 #xff)
(sim-step) ; MULS #24,D0
(test-reg D0 #x90)
(test-mask #x00 #xff)
(print "Testing MULU instruction")
(terpri)
(sim-step) ; CLR.L D0
(test-reg D0 0)
(sim-step) ; MOVE.W #-1,D0
(test-reg D0 #xffff)
(test-mask #x08 #xff)
(sim-step) ; MULU #-1,D0
(test-reg D0 #xfffe0001)
(test-mask #x08 #xff)
(sim-step) ; MOVE.W #-2,D0
(test-reg D0 #xfffefffe)
(test-mask #x08 #xff)
(sim-step) ; MULU #2,D0
(test-reg D0 #x0001fffc)
(test-mask #x00 #xff)
(sim-step) ; MOVE.W #6,D0
(test-reg D0 #x00010006)
(sim-step) ; MULU #24,D0
(test-reg D0 #x00000090)
(test-mask #x00 #xff)
;-------------------------------------------------------------------------------
;  Test EOR instructions
;
;  Load memory
;
(meml #x0020 #x00001048) ; Privalege violation vector
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
(test-reg D0 #x0f0f0f0f)
(sim-step) ; ADD.L #$00FF00FF,D1
(test-reg D1 #x00ff00ff)
(sim-step) ; ADD.L D0,D2
(test-reg D2  #x0f0f0f0f)
(sim-step) ; ADD.L D1,D3
(test-reg D3  #x00ff00ff)
(print "Testing EOR instructions")
(terpri)
(sim-step) ; EOR.B D0,D3
(test-reg D3  #x00ff00f0)
(test-mask #x08 #xff)
(sim-step) ; EOR.W D1,D2
(test-reg D2  #x0f0f0ff0)
(test-mask #x00 #xff)
(sim-step) ; EOR.L D2,D3
(test-reg D3  #x0ff00f00)
(test-mask #x00 #xff)
(print "Testing EORI instructions")
(terpri)
(sim-step) ; EORI.L #$55555555,D0
(test-reg D0 #x5a5a5a5a)
(test-mask #x00 #xff)
(sim-step) ; EORI.W #$AAAA,D1
(test-reg D1 #x00ffaa55)
(test-mask #x08 #xff)
(sim-step) ; EORI.B #$A5,D2
(test-reg D2  #x0f0f0f55)
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
(test-reg PC #x1040)
(sim-step) ; EORI #$2000,SR
(print "In privilege violation handler")
(terpri)
(test-mask #x200b #xf0ff)
(test-reg PC #x1048)
(sim-step)  ; RTE
(test-mask #x0b #xf0ff)
(test-reg PC #x1044)
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
(test-reg D0 #x0f0f0f0f)
(sim-step) ; ADD.L #$00FF00FF,D1
(test-reg D1 #x00ff00ff)
(sim-step) ; ADDA.L #$55AA55AA,A0
(test-reg A0 #x55aa55aa)
(sim-step) ; ADDA.L #$5A5A5A5A,A1
(test-reg A1 #x5a5a5a5a)
(test-reg D2  0)
(test-reg A2 0)
(print "Exchanging data registers")
(terpri)
(sim-step) ; EXG D0,D1
(test-reg D0 #x00ff00ff)
(test-reg D1 #x0f0f0f0f)
(sim-step) ; EXG D0,D2
(test-reg D0 0)
(test-reg D2  #x00ff00ff)
(print "Exchanding address registers")
(terpri)
(sim-step) ; EXG A0,A1
(test-reg A0 #x5a5a5a5a)
(test-reg A1 #x55aa55aa)
(sim-step) ; EXG A0,A2
(test-reg A0 0)
(test-reg A2 #x5a5a5a5a)
(print "Exchanging address and data registers")
(terpri)
(sim-step) ; EXG A1,D1
(test-reg D1 #x55aa55aa)
(test-reg A1 #x0f0f0f0f)
(sim-step) ; EXG D2,A2
(test-reg D2  #x5a5a5a5a)
(test-reg A2 #x00ff00ff)
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
(test-reg D0 #x0f0f)
(sim-step) ; ADD.W #$8F0F,D1
(test-reg D1 #x8f0f)
(sim-step) ; ADD.B #$0F,D2
(test-reg D2  #x0f)
(sim-step) ; ADD.B #$FF,D3
(test-reg D3  #xff)
(print "Extending word values")
(terpri)
(sim-step) ; EXT.L D0
(test-reg D0 #x0f0f)
(sim-step) ; EXT.L D1
(test-reg D1 #xffff8f0f)
(print "Extending byte values")
(terpri)
(sim-step) ; EXT.W D2
(test-reg D2  #x0f)
(sim-step) ; EXT.W D3
(test-reg D3  #xffff)
(print "Extras")
(terpri)
(sim-step) ; EXT.W D1
(test-reg D1 #xffff000f)
(sim-step) ; EXT.L D1
(test-reg D1 #x0f)
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
(test-reg SSP #x3000)
(test-reg PC #x1006)
(sim-step) ; ILLEGAL
(test-reg SSP #x2ffa)
(test-reg PC #x2000)
(test-memw #x2ffa #x2700)
(test-meml #x2ffc #x1006)
(sim-step) ; RTE
(test-reg SSP #x3000)
(test-reg PC #x1006)
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
(test-reg SSP #x2000)
(sim-step) ; JMP L1
(test-reg PC #x1010)
(sim-step) ; L1: ADD.L #L2,A1
(sim-step) ; JMP (A1)
(test-reg PC #x101c)
(sim-step) ; L2: JMP L3(PC)
(test-reg PC #x1024)
(sim-step) ; L3: JSR PASS
(test-reg SSP #x1ffc)
(test-reg PC #x1030)
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
(test-reg A0 #x1000)
(sim-step) ; LEA (A0),A1
(test-reg A1 #x1000)
(sim-step) ; LEA 2(A1),A2
(test-reg A2 #x1002)
(sim-step) ; LEA START(PC),A3
(test-reg A3 #x1000)
(sim-step) ; ADDQ #2,D0
(test-reg D0 2)
(sim-step) ; LEA 2(A1,D0),A4
(test-reg A4  #x1004)
(sim-step) ; LEA START(PC,D0),A5
(test-reg A5 #x1002)
;
(print "Setting up stack pointers")
(terpri)
(sim-step) ; MOVE.L #SUPER,A7
(test-reg SSP #x1300) ; Test SSP
(sim-step) ; MOVE.L #USER,A0
(test-reg A0 #x1200)
(sim-step) ; MOVE A0,USP
(test-reg USP #x1200) ; Test USP
(sim-step) ; LEA START,A0
(test-reg A0 #x1000)
;
(print "Testing PEA instruction")
(terpri)
(sim-step) ; PEA (A0)
(test-reg SSP #x12fc)
(test-meml #x12fc #x00001000)
(sim-step) ; EORI #$2000,SR
(test-mask #x0000 #xf000) ; Drop privileges
(sim-step) ; PEA (A0)
(test-reg USP #x11fc)
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
(test-reg D0 #x2000)
(sim-step) ; EXG D0,SP
(test-reg SSP #x2000)
(sim-step) ; ADD.L #$FFFF,A6
(test-reg A6 #xffff)
(sim-step) ; LINK A6,#-$20
(test-reg A6 #x1ffc)
(test-reg SSP #x1fdc)
(test-meml #x1ffc #xffff)
(sim-step) ; UNLK A6
(test-reg A6 #xffff)
(test-reg SSP #x2000)
;-------------------------------------------------------------------------------
;  Test MOVE instructions
;
;  Load memory
;
(meml #x0020 #x00001050) ; Exception vector for privilege violation
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
(test-reg D0 #xdeadbeef)
(test-mask #x08 #xff)
(sim-step) ; MOVE.W #$ABBA,D1
(test-reg D1 #xabba)
(test-mask #x08 #xff)
(sim-step) ; MOVE.B D1,D0
(test-reg D0 #xdeadbeba)
(test-mask #x08 #xff)
(sim-step) ; MOVE.L D0,A0
(test-reg A0 #xdeadbeba)
(test-mask #x08 #xff)
(sim-step) ; MOVE.W D1,A0
(test-reg A0 #xffffabba)
(test-mask #x08 #xff)
;
(print "Testing post-increment MOVE")
(terpri)
(sim-step) ; MOVE #SRC,A1
(test-reg A1 #x1040)
(sim-step) ; MOVE #DEST,A2
(test-reg A2 #x1030)
(sim-step) ; MOVE.L (A1)+,(A2)+
(test-reg A1 #x1044)
(test-reg A2 #x1034)
(test-mask #x00 #xff)
(test-memw #x1030 #x1234)
(test-memw #x1032 #x5678)
(sim-step) ; MOVE.W (A1)+,(A2)+
(test-reg A1 #x1046)
(test-reg A2 #x1036)
(test-mask #x08 #xff)
(test-memw #x1034 #x9abc)
(sim-step) ; MOVE.B (A1)+,(A2)+
(test-reg A1 #x1047)
(test-reg A2 #x1037)
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
(test-reg A3 #x3000)
(sim-step) ; MOVE A3,USP
(test-reg USP #x3000)
(sim-step) ; MOVE USP,A4
(test-reg A4  #x3000)
(sim-step) ; BRA CONTINUE
;
(print "Testing MOVE from/to SR")
(terpri)
(test-reg PC #x1052)
(sim-step) ; MOVE SR,D0
(test-reg D0 #xdead27ba)
(sim-step) ; EORI #$2000,D0
(test-reg D0 #xdead07ba)
(test-mask #x20b0 #xf0ff)
(sim-step) ; MOVE D0,SR (clear privilege bit)
(test-reg A1 #x1047)
(test-mask #x00ba #xf0ff)
(sim-step) ; ADDQ.L #1,A1
(test-reg A1 #x1048)
(terpri)
(sim-step) ; MOVE (A1)+,SR
(test-reg A1 #x1048) ; Post increment does not occur
(test-reg PC #x1050)
(print "In privilege violation exception handler")
(terpri)
(sim-step) ; RTE
(test-reg PC #x105e)
(sim-step) ; MOVE A3,USP
(test-reg PC #x1050)
(print "In privilege violation exception handler")
(terpri)
(sim-step) ; RTE
(test-reg PC #x1060)
(sim-step) ; MOVE USP,A4
(test-reg PC #x1050)
(print "In privilege violation exception handler")
(terpri)
(sim-step) ; RTE
(test-reg PC #x1062)
;-------------------------------------------------------------------------------
;  Test MOVEM instructions
;
;  Load memory
;
(memw #x1000 #x4cf9) ; MOVEM.L REG,D0-D7/A0-A7
(memw #x1002 #xffff)
(memw #x1004 #x0000)
(memw #x1006 #x1100)
(memw #x1008 #x48b9) ; MOVEM.W D0-D7/A0-A7,BUFF
(memw #x100a #xffff)
(memw #x100c #x0000)
(memw #x100e #x1200)
;
(memw #x1010 #x2e7c) ; MOVE.L #STACK,SP
(memw #x1012 #x0000)
(memw #x1014 #x1300)
(memw #x1016 #x48e7) ; MOVEM.L D0-D7/A0-A6,-(SP)
(memw #x1018 #xfffe)
;
(memw #x101a #x4cb9) ; MOVEM.W REG,D0-D7/A0-A7
(memw #x101c #xffff)
(memw #x101e #x0000)
(memw #x1020 #x1100)
(memw #x1022 #x48f9) ; MOVEM.L D0-D7/A0-A7,REG
(memw #x1024 #xffff)
(memw #x1026 #x0000)
(memw #x1028 #x1200)
;
(memw #x102a #x2e7c) ; MOVE.L #$12c4,SP
(memw #x102c #x0000)
(memw #x102e #x12c4)
(memw #x1030 #x4cdf) ; MOVEM.L (SP)+,D0-D7/A0-A6
(memw #x1032 #x7fff)
;
(meml #x1100 #x0000ffff) ; REG:
(meml #x1104 #x0010eeee)
(meml #x1108 #x0020dddd)
(meml #x110c #x0030cccc)
(meml #x1110 #x0040bbbb)
(meml #x1114 #x0050aaaa)
(meml #x1118 #x00609999)
(meml #x111c #x00708888)
(meml #x1120 #x00807777)
(meml #x1124 #x00906666)
(meml #x1128 #x00a05555)
(meml #x112c #x00b04444)
(meml #x1130 #x00c03333)
(meml #x1134 #x00d02222)
(meml #x1138 #x00e01111)
(meml #x113c #x00f00000)
;
(meml #x1200 0) ; BUFF:
(meml #x1204 0)
(meml #x1208 0)
(meml #x120c 0)
(meml #x1210 0)
(meml #x1214 0)
(meml #x1218 0)
(meml #x121c 0)
(meml #x1220 0)
(meml #x1224 0)
(meml #x1228 0)
(meml #x122c 0)
(meml #x1230 0)
(meml #x1234 0)
(meml #x1238 0)
(meml #x123c 0)
;
;  Execute test
;
(print "==> Testing MOVEM instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOVEM.L REG,D0-D7/A0-A7
(test-reg D0 #x0000ffff)
(test-reg D1 #x0010eeee)
(test-reg D2 #x0020dddd)
(test-reg D3 #x0030cccc)
(test-reg D4 #x0040bbbb)
(test-reg D5 #x0050aaaa)
(test-reg D6 #x00609999)
(test-reg D7 #x00708888)
(test-reg A0 #x00807777)
(test-reg A1 #x00906666)
(test-reg A2 #x00a05555)
(test-reg A3 #x00b04444)
(test-reg A4  #x00c03333)
(test-reg A5 #x00d02222)
(test-reg A6 #x00e01111)
(test-reg SSP #x00f00000)
(sim-step) ; MOVEM.W D0-D7/A0-A7,BUFF
(test-meml #x1200 #xffffeeee)
(test-meml #x1204 #xddddcccc)
(test-meml #x1208 #xbbbbaaaa)
(test-meml #x120c #x99998888)
(test-meml #x1210 #x77776666)
(test-meml #x1214 #x55554444)
(test-meml #x1218 #x33332222)
(test-meml #x121c #x11110000)
(test-meml #x1220 #x00000000)
(test-meml #x1224 #x00000000)
(test-meml #x1228 #x00000000)
(test-meml #x122c #x00000000)
(test-meml #x1230 #x00000000)
(test-meml #x1234 #x00000000)
(test-meml #x1238 #x00000000)
(test-meml #x123c #x00000000)
;
(sim-step) ; MOVE.L #STACK,SP
(test-reg SSP #x1300)
(sim-step) ; MOVEM.L D0-D7/A0-A6,-(SP)
(test-reg SSP #x12c4)
(test-meml #x12c4 #x0000ffff)
(test-meml #x12c8 #x0010eeee)
(test-meml #x12cc #x0020dddd)
(test-meml #x12d0 #x0030cccc)
(test-meml #x12d4 #x0040bbbb)
(test-meml #x12d8 #x0050aaaa)
(test-meml #x12dc #x00609999)
(test-meml #x12e0 #x00708888)
(test-meml #x12e4 #x00807777)
(test-meml #x12e8 #x00906666)
(test-meml #x12ec #x00a05555)
(test-meml #x12f0 #x00b04444)
(test-meml #x12f4 #x00c03333)
(test-meml #x12f8 #x00d02222)
(test-meml #x12fc #x00e01111)
;
(sim-step) ; MOVEM.W REG,D0-D7/A0-A7
(test-reg D0 #x00000000)
(test-reg D1 #xFFFFFFFF)
(test-reg D2 #x00000010)
(test-reg D3 #xFFFFEEEE)
(test-reg D4 #x00000020)
(test-reg D5 #xFFFFDDDD)
(test-reg D6 #x00000030)
(test-reg D7 #xFFFFCCCC)
(test-reg A0 #x00000040)
(test-reg A1 #xFFFFBBBB)
(test-reg A2 #x00000050)
(test-reg A3 #xFFFFAAAA)
(test-reg A4  #x00000060)
(test-reg A5 #xFFFF9999)
(test-reg A6 #x00000070)
(test-reg SSP #xFFFF8888)
(sim-step) ; MOVEM.L D0-D7/A0-A7,REG
(test-meml #x1200 #x00000000)
(test-meml #x1204 #xFFFFFFFF)
(test-meml #x1208 #x00000010)
(test-meml #x120c #xFFFFEEEE)
(test-meml #x1210 #x00000020)
(test-meml #x1214 #xFFFFDDDD)
(test-meml #x1218 #x00000030)
(test-meml #x121c #xFFFFCCCC)
(test-meml #x1220 #x00000040)
(test-meml #x1224 #xFFFFBBBB)
(test-meml #x1228 #x00000050)
(test-meml #x122c #xFFFFAAAA)
(test-meml #x1230 #x00000060)
(test-meml #x1234 #xFFFF9999)
(test-meml #x1238 #x00000070)
(test-meml #x123c #xFFFF8888)
;
(sim-step) ; MOVE.L #$12c4,SP
(test-reg SSP #x12c4)
(sim-step) ; MOVEM.L (SP)+,D0-D7/A0-A6
(test-reg D0 #x0000ffff)
(test-reg D1 #x0010eeee)
(test-reg D2  #x0020dddd)
(test-reg D3  #x0030cccc)
(test-reg D4 #x0040bbbb)
(test-reg D5 #x0050aaaa)
(test-reg D6 #x00609999)
(test-reg D7 #x00708888)
(test-reg A0 #x00807777)
(test-reg A1 #x00906666)
(test-reg A2 #x00a05555)
(test-reg A3 #x00b04444)
(test-reg A4  #x00c03333)
(test-reg A5 #x00d02222)
(test-reg A6 #x00e01111)
(test-reg SSP #x00001300)
;-------------------------------------------------------------------------------
;  Test MOVEP/MOVEQ instructions
;
;  Load memory
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
; DEST:
(meml #x1500 0) ; DC.L 0
(meml #x1504 0) ; DC.L 0
(meml #x1508 0) ; DC.L 0
(meml #x150c 0) ; DC.L 0
;
;  Execute test
;
(print "==> Testing MOVEP and MOVEQ instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOVE.L #$12345678,D0
(test-reg D0 #x12345678)
(sim-step) ; MOVE DEST,A0
(test-reg A0 #x1500)
(print "Testing basic long MOVEP")
(terpri)
(sim-step) ; MOVEP.L D0,0(A0)
(test-mask #x00 #xff)
(test-memw #x1500 #x1200)
(test-memw #x1502 #x3400)
(test-memw #x1504 #x5600)
(test-memw #x1506 #x7800)
(sim-step) ; MOVEP.L D0,0(A0)
(test-mask #x00 #xff)
(test-reg D1 #x12345678)
;
(print "Testing word MOVEP")
(terpri)
(sim-step) ; MOVEP.W D0,9(A0)
(test-mask #x00 #xff)
(test-memw #x1508 #x0056)
(test-memw #x150a #x0078)
(sim-step) ; MOVEP.W 9(A0),D2
(test-mask #x00 #xff)
(test-reg D2  #x5678)
;
(print "Testing MOVEQ instructions")
(terpri)
(sim-step) ; MOVEQ #1,D4
(test-reg D4 1)
(test-mask #x00 #xff)
(sim-step) ; MOVEQ #$FF,D5
(test-reg D5 #xffffffff)
(test-mask #x08 #xff)
(sim-step) ; MOVEQ #0,D0
(test-reg D0 0)
(test-mask #x04 #xff)
;-------------------------------------------------------------------------------
;  Test NEG instructions
;
;  Load memory
;
(memw #x1000 #x4280) ; CLR.L D0
(memw #x1002 #x4480) ; NEG.L D0
(memw #x1004 #x203c) ; MOVE.L #$80000000,D0
(meml #x1006 #x80000000)
(memw #x100a #x4480) ; NEG.L D0
(memw #x100c #x7001) ; MOVE.L #1,D0
(memw #x100e #x4480) ; NEG.L D0
(memw #x1010 #x4480) ; NEG.L D0
;
(memw #x1012 #x4280) ; CLR.L D0
(memw #x1014 #x4440) ; NEG.W D0
(memw #x1016 #x203c) ; MOVE.L #$8000,D0
(meml #x1018 #x00008000)
(memw #x101c #x4440) ; NEG.W D0
(memw #x101e #x7001) ; MOVE.L #1,D0
(memw #x1020 #x4440) ; NEG.W D0
(memw #x1022 #x4440) ; NEG.W D0
;
(memw #x1024 #x4280) ; CLR.L D0
(memw #x1026 #x4400) ; NEG.B D0
(memw #x1028 #x203c) ; MOVE.L #$80,D0
(meml #x102a #x00000080)
(memw #x102e #x4400) ; NEG.B D0
(memw #x1030 #x7001) ; MOVE.L #1,D0
(memw #x1032 #x4400) ; NEG.B D0
(memw #x1034 #x4400) ; NEG.B D0
;
;  Execute test
;
(print "==> Testing NEG instructions")
(terpri)
(sim-init)
(go #x1000)
(print "Testing NEG.L")
(terpri)
(sim-step) ; CLR.L D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; NEG.L D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L #$80000000,D0
(test-reg D0 #x80000000)
(test-mask #x08 #xff)
(sim-step) ; NEG.L D0
(test-reg D0 #x80000000)
(test-mask #x1b #xff)
(sim-step) ; MOVE.L #1,D0
(test-reg D0 1)
(test-mask #x10 #xff)
(sim-step) ; NEG.L D0
(test-reg D0 #xffffffff)
(test-mask #x19 #xff)
(sim-step) ; NEG.L D0
(test-reg D0 1)
(test-mask #x11 #xff)
;
(print "Testing NEG.W")
(terpri)
(sim-step) ; CLR.L D0
(test-reg D0 0)
(test-mask #x14 #xff)
(sim-step) ; NEG.W D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L #$8000,D0
(test-reg D0 #x8000)
(test-mask #x00 #xff)
(sim-step) ; NEG.W D0
(test-reg D0 #x8000)
(test-mask #x1b #xff)
(sim-step) ; MOVE.L #1,D0
(test-reg D0 1)
(test-mask #x10 #xff)
(sim-step) ; NEG.W D0
(test-reg D0 #xffff)
(test-mask #x19 #xff)
(sim-step) ; NEG.W D0
(test-reg D0 1)
(test-mask #x11 #xff)
;
(print "Testing NEG.B")
(terpri)
(sim-step) ; CLR.L D0
(test-reg D0 0)
(test-mask #x14 #xff)
(sim-step) ; NEG.B D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L #$80,D0
(test-reg D0 #x80)
(test-mask #x00 #xff)
(sim-step) ; NEG.B D0
(test-reg D0 #x80)
(test-mask #x1b #xff)
(sim-step) ; MOVE.L #1,D0
(test-reg D0 1)
(test-mask #x10 #xff)
(sim-step) ; NEG.B D0
(test-reg D0 #xff)
(test-mask #x19 #xff)
(sim-step) ; NEG.B D0
(test-reg D0 1)
(test-mask #x11 #xff)
;-------------------------------------------------------------------------------
;  Test NEGX and NOP instructions
;
;  Load memory
;
(memw #x1000 #x4280) ; CLR.L D0
(memw #x1002 #x4080) ; NEGX.L D0
(memw #x1004 #x203c) ; MOVE.L #$80000000,D0
(meml #x1006 #x80000000)
(memw #x100a #x4080) ; NEGX.L D0
(memw #x100c #x7001) ; MOVE.L #1,D0
(memw #x100e #x4080) ; NEGX.L D0
(memw #x1010 #x4080) ; NEGX.L D0
;
(memw #x1012 #x4280) ; CLR.L D0
(memw #x1014 #x4040) ; NEGX.W D0
(memw #x1016 #x203c) ; MOVE.L #$8000,D0
(meml #x1018 #x00008000)
(memw #x101c #x4040) ; NEGX.W D0
(memw #x101e #x7001) ; MOVE.L #1,D0
(memw #x1020 #x4040) ; NEGX.W D0
(memw #x1022 #x4040) ; NEGX.W D0
;
(memw #x1024 #x4280) ; CLR.L D0
(memw #x1026 #x4000) ; NEGX.B D0
(memw #x1028 #x203c) ; MOVE.L #$80,D0
(meml #x102a #x00000080)
(memw #x102e #x4000) ; NEGX.B D0
(memw #x1030 #x7001) ; MOVE.L #1,D0
(memw #x1032 #x4000) ; NEGX.B D0
(memw #x1034 #x4000) ; NEGX.B D0
;
(memw #x1036 #x4e71) ; NOP
;
;  Execute test
;
(print "==> Testing NEGX and NOP instructions")
(terpri)
(sim-init)
(go #x1000)
(print "Testing NEGX.L")
(terpri)
(sim-step) ; CLR.L D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; NEGX.L D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L #$80000000,D0
(test-reg D0 #x80000000)
(test-mask #x08 #xff)
(sim-step) ; NEGX.L D0
(test-reg D0 #x80000000)
(test-mask #x1b #xff)
(sim-step) ; MOVE.L #1,D0
(test-reg D0 1)
(test-mask #x10 #xff)
(sim-step) ; NEGX.L D0
(test-reg D0 #xfffffffe)
(test-mask #x19 #xff)
(sim-step) ; NEGX.L D0
(test-reg D0 1)
(test-mask #x11 #xff)
;
(print "Testing NEGX.W")
(terpri)
(sim-step) ; CLR.L D0
(test-reg D0 0)
(test-mask #x14 #xff)
(sim-step) ; NEGX.W D0
(test-reg D0 #xffff)
(test-mask #x19 #xff)
(sim-step) ; MOVE.L #$8000,D0
(test-reg D0 #x8000)
(test-mask #x10 #xff)
(sim-step) ; NEGX.W D0
(test-reg D0 #x7fff)
(test-mask #x11 #xff)
(sim-step) ; MOVE.L #1,D0
(test-reg D0 1)
(test-mask #x10 #xff)
(sim-step) ; NEGX.W D0
(test-reg D0 #xfffe)
(test-mask #x19 #xff)
(sim-step) ; NEGX.W D0
(test-reg D0 1)
(test-mask #x11 #xff)
;
(print "Testing NEGX.B")
(terpri)
(sim-step)
(test-reg D0 0)
(test-mask #x14 #xff)
(sim-step)
(test-reg D0 #xff)
(test-mask #x19 #xff)
(sim-step)
(test-reg D0 #x80)
(test-mask #x10 #xff)
(sim-step)
(test-reg D0 #x7f)
(test-mask #x11 #xff)
(sim-step)
(test-reg D0 1)
(test-mask #x10 #xff)
(sim-step)
(test-reg D0 #xfe)
(test-mask #x19 #xff)
(sim-step)
(test-reg D0 1)
(test-mask #x11 #xff)
;
(print "Executing NOP instruction")
(terpri)
(test-reg D0 #x1)
(test-reg D1 #x0)
(test-reg D2 #x0)
(test-reg D3 #x0)
(test-reg D4 #x0)
(test-reg D5 #x0)
(test-reg D6 #x0)
(test-reg D7 #x0)
(test-reg A0 #x0)
(test-reg A1 #x0)
(test-reg A2 #x0)
(test-reg A3 #x0)
(test-reg A4  #x0)
(test-reg A5 #x0)
(test-reg A6 #x0)
(test-reg USP #x0)
(test-reg SSP #x0)
(test-reg PC #x1036)
(test-mask #x11 #xff)
(sim-step)
(test-reg D0 #x1)
(test-reg D1 #x0)
(test-reg D2 #x0)
(test-reg D3 #x0)
(test-reg D4 #x0)
(test-reg D5 #x0)
(test-reg D6 #x0)
(test-reg D7 #x0)
(test-reg A0 #x0)
(test-reg A1 #x0)
(test-reg A2 #x0)
(test-reg A3 #x0)
(test-reg A4  #x0)
(test-reg A5 #x0)
(test-reg A6 #x0)
(test-reg USP #x0)
(test-reg SSP #x0)
(test-reg PC #x1038)
(test-mask #x11 #xff)
;-------------------------------------------------------------------------------
;  Test NOT instructions
;
;  Load memory
;
(memw #x1000 #x4280) ; CLR.L D0
(memw #x1002 #x4680) ; NOT.L D0
(memw #x1004 #x4680) ; NOT.L D0
(memw #x1006 #x203c) ; MOVE.L #$55555555,D0
(meml #x1008 #x55555555)
(memw #x100c #x4680) ; NOT.L D0
(memw #x100e #x4680) ; NOT.L D0
;
(memw #x1010 #x4280) ; CLR.L D0
(memw #x1012 #x4640) ; NOT.W D0
(memw #x1014 #x4640) ; NOT.W D0
(memw #x1016 #x203c) ; MOVE.L #$55555555,D0
(meml #x1018 #x55555555)
(memw #x101c #x4640) ; NOT.W D0
(memw #x101e #x4640) ; NOT.W D0
;
(memw #x1020 #x4280) ; CLR.L D0
(memw #x1022 #x4600) ; NOT.B D0
(memw #x1024 #x4600) ; NOT.B D0
(memw #x1026 #x203c) ; MOVE.L #$55555555,D0
(meml #x1028 #x55555555)
(memw #x102c #x4600) ; NOT.B D0
(memw #x102e #x4600) ; NOT.B D0
;
;  Execute test
;
(print "==> Testing NOT instructions")
(terpri)
(sim-init)
(go #x1000)
(print "Testing NOT.L")
(terpri)
(sim-step) ; CLR.L D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; NOT.L D0
(test-reg D0 #xffffffff)
(test-mask #x08 #xff)
(sim-step) ; NOT.L D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; MOVE #$55555555,D0
(test-reg D0 #x55555555)
(test-mask #x00 #xff)
(sim-step) ; NOT.L D0
(test-reg D0 #xaaaaaaaa)
(test-mask #x08 #xff)
(sim-step) ; NOT.L D0
(test-reg D0 #x55555555)
(test-mask #x00 #xff)
;
(print "Testing NOT.W")
(terpri)
(sim-step) ; CLR.L D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; NOT.W D0
(test-reg D0 #xffff)
(test-mask #x08 #xff)
(sim-step) ; NOT.W D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L #$55555555,D0
(test-reg D0 #x55555555)
(test-mask #x00 #xff)
(sim-step) ; NOT.W D0
(test-reg D0 #x5555aaaa)
(test-mask #x08 #xff)
(sim-step) ; NOT.W D0
(test-reg D0 #x55555555)
(test-mask #x00 #xff)
;
(print "Testing NOT.B")
(terpri)
(sim-step) ; CLR.L D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; NOT.W D0
(test-reg D0 #xff)
(test-mask #x08 #xff)
(sim-step) ; NOT.W D0
(test-reg D0 0)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L #$55555555,D0
(test-reg D0 #x55555555)
(test-mask #x00 #xff)
(sim-step) ; NOT.W D0
(test-reg D0 #x555555aa)
(test-mask #x08 #xff)
(sim-step) ; NOT.W D0
(test-reg D0 #x55555555)
(test-mask #x00 #xff)
;-------------------------------------------------------------------------------
;  Test OR instructions
;
;  Load memory
;
(meml #x0020 #x00001100) ; Privilege violation vector
;
(memw #x1000 #x203c) ; MOVE.L #$0f0f0f0f,D0
(meml #x1002 #x0f0f0f0f)
(memw #x1006 #x223c) ; MOVE.L #$00ff00ff,D1
(meml #x1008 #x00ff00ff)
(memw #x100c #x2400) ; MOVE.L D0,D2
(memw #x100e #x2601) ; MOVE.L D1,D3
;
(memw #x1010 #x8600) ; OR.B D0,D3
(memw #x1012 #x8441) ; OR.W D1,D2
(memw #x1014 #x8682) ; OR.L D2,D3
;
(memw #x1016 #x0080) ; ORI.L #55000000,D0
(meml #x1018 #x55000000)
(memw #x101c #x0040) ; ORI.W #5500,D0
(memw #x101e #x5500)
(memw #x1020 #x0000) ; ORI.B #55,D0
(memw #x1022 #x0055)
;
(memw #x1024 #x003c) ; ORI #$8,CCR
(memw #x1026 #x0008)
(memw #x1028 #x003c) ; ORI #$4,CCR
(memw #x102a #x0004)
;
(memw #x102c #x007c) ; ORI #$1000,SR
(memw #x102e #x1000)
(memw #x1030 #x0a7c) ; EORI #$2000,SR
(memw #x1032 #x2000)
(memw #x1034 #x007c) ; ORI #$2000,SR
(memw #x1036 #x2000)
;
(memw #x1100 #x4e73) ; RTE
;
;  Execute test
;
(print "==> Testing OR instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOVE.L #$0f0f0f0f,D0
(test-reg D0 #x0f0f0f0f)
(sim-step) ; MOVE.L #$00ff00ff,D1
(test-reg D1 #x00ff00ff)
(sim-step) ; MOVE.L D0,D2
(test-reg D2  #x0f0f0f0f)
(sim-step) ; MOVE.L D1,D3
(test-reg D3  #x00ff00ff)
;
(print "Test OR instructions")
(terpri)
(sim-step) ; OR.B D0,D3
(test-reg D3  #x00ff00ff)
(test-mask #x08 #xff)
(sim-step) ; OR.W D1,D2
(test-reg D2  #x0f0f0fff)
(test-mask #x00 #xff)
(sim-step) ; OR.L D2,D3
(test-reg D3  #x0fff0fff)
(test-mask #x00 #xff)
;
(print "Test ORI instructions")
(terpri)
(sim-step) ; ORI.L #55000000,D0
(test-reg D0 #x5f0f0f0f)
(test-mask #x00 #xff)
(sim-step) ; ORI.W #5500,D0
(test-reg D0 #x5f0f5f0f)
(test-mask #x00 #xff)
(sim-step) ; ORI.B #55,D0
(test-reg D0 #x5f0f5f5f)
(test-mask #x00 #xff)
;
(print "Test ORI to CCR instruction")
(terpri)
(sim-step) ; ORI #$8,CCR
(test-mask #x08 #xff)
(sim-step) ; ORI #$4,CCR
(test-mask #x0c #xff)
;
(print "Test ORI to SR instruction")
(terpri)
(sim-step) ; ORI #$1000,SR
(test-mask #x300c #xf0ff)
(sim-step) ; EORI #$2000,SR
(test-mask #x100c #xf0ff)
(sim-step) ; ORI #$2000,SR
(test-reg PC #x1100)
(print "In privilege violation exception handler")
(terpri)
(sim-step) ; RTE
(test-reg PC #x1038)
;-------------------------------------------------------------------------------
;  Test RTE, RTR, and RTS instructions
;
;  Load memory
;
(meml #x0010 #x00001036) ; Vector for illegal instruction
(meml #x0020 #x0000103e) ; Vector for privilege violation
;
(memw #x1000 #x2e7c) ; MOVE.L #STACK2,A7
(meml #x1002 #x00001300)
(memw #x1006 #x207c) ; MOVE.L #STACK1,A0
(meml #x1008 #x00001200)
(memw #x100c #x4e60) ; MOVE A0,USP
;
(memw #x100e #x44fc) ; MOVE #0,CCR
(memw #x1010 #x0000)
(memw #x1012 #x4eb9) ; JSR SUB1
(meml #x1014 #x00001026)
;
(memw #x1018 #x4eb9) ; JSR SUB2
(meml #x101a #x0000102e)
;
(memw #x101e #x4afc) ; ILLEGAL
(memw #x1020 #x46fc) ; MOVE #0,SR
(memw #x1022 #x0000)
(memw #x1024 #x4e73) ; RTE
;
(memw #x1026 #x701f) ; SUB1: MOVEQ #$1f,D0
(memw #x1028 #x3f00) ; MOVE.W D0,-(SP)
(memw #x102a #x7201) ; MOVEQ #1,D1
(memw #x102c #x4e77) ; RTR
;
(memw #x102e #x7202) ; SUB2: MOVEQ #2,D1
(memw #x1030 #x4e75) ; RTS
;
(memw #x1036 #x7004) ; ILLINST: MOVEQ #4,D0
(memw #x1038 #x54af) ; ADDQ.L #2,2(SP)
(memw #x103a #x0002)
(memw #x103c #x4e73) ; RTE
;
(memw #x103e #x7008) ; PRIVIOL: MOVEQ #8,D0
(memw #x1040 #x4e73) ; RTE
;
;  Execute test
;
(print "==> Testing RTE, RTR, and RTS instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOVE.L #STACK2,A7
(test-reg SSP #x1300)
(sim-step) ; MOVE.L #STACK1,A0
(test-reg A0 #x1200)
(sim-step) ; MOVE A0,USP
(test-reg USP #x1200)
;
(sim-step) ; MOVE #0,CCR
(test-reg PC #x1012)
(test-mask #x00 #xff)
(sim-step) ; JSR SUB1
(test-reg SSP #x12fc)
(test-reg PC #x1026)
(test-meml #x12fc #x00001018)
(sim-step) ; SUB1: MOVEQ #$1f,D0
(test-reg D0 #x001f)
(sim-step) ; MOVE.W D0,-(SP)
(test-reg SSP #x12fa)
(test-memw #x12fa #x001f)
(sim-step) ; MOVEQ #1,D1
(test-reg D1 1)
(test-reg PC #x102c)
(sim-step) ; RTR
(test-reg D1 1)
(test-reg SSP #x1300)
(test-reg PC #x1018)
(test-mask #x1f #xff)
;
(sim-step) ; JSR SUB2
(test-reg PC #x102e)
(sim-step) ; SUB2: MOVEQ #2,D1
(test-reg D1 2)
(test-reg PC #x1030)
(sim-step) ; RTS
(test-reg D1 2)
(test-reg PC #x101e)
;
(sim-step) ; ILLEGAL
(test-reg PC #x1036)
(sim-step) ; ILLINST: MOVEQ #4,D0
(test-reg D0 4)
(test-reg PC #x1038)
(test-memw #x12fc #x0000)
(test-memw #x12fe #x101e)
(sim-step) ; ADDQ.L #2,2(SP)
(test-memw #x12fc #x0000)
(test-memw #x12fe #x1020)
(sim-step) ; RTE
(test-reg D0 4)
(test-reg PC #x1020)
;
(sim-step) ; MOVE #0,SR
(test-reg SR 0)
(test-reg PC #x1024)
(sim-step) ; RTE
(test-reg PC #x103e)
(sim-step) ; PRIVIOL: MOVEQ #8,D0
(test-reg D0 8)
(test-reg PC #x1040)
(sim-step) ; RTE
(test-reg D0 8)
(test-reg PC #x1026)
;-------------------------------------------------------------------------------
;  Test Scc instructions
;
;  Load memory
;
(memw #x1000 #x50c0) ; ST D0
(memw #x1002 #x51c0) ; SF D0
;
(memw #x1004 #x44fc) ; MOVE #0,CCR
(memw #x1006 #x0000)
(memw #x1008 #x55c0) ; SCS D0
(memw #x100a #x54c0) ; SCC D0
(memw #x100c #x59c0) ; SVS D0
(memw #x100e #x58c0) ; SVC D0
(memw #x1010 #x57c0) ; SEQ D0
(memw #x1012 #x56c0) ; SNE D0
(memw #x1014 #x5bc0) ; SMI D0
(memw #x1016 #x5ac0) ; SPL D0
;
(memw #x1018 #x44fc) ; MOVE #1,CCR
(memw #x101a #x0001)
(memw #x101c #x55c0) ; SCS D0
(memw #x101e #x54c0) ; SCC D0
;
(memw #x1020 #x44fc) ; MOVE #2,CCR
(memw #x1022 #x0002)
(memw #x1024 #x59c0) ; SVS D0
(memw #x1026 #x58c0) ; SVC D0
;
(memw #x1028 #x44fc) ; MOVE #4,CCR
(memw #x102a #x0004)
(memw #x102c #x57c0) ; SEQ D0
(memw #x102e #x56c0) ; SNE D0
;
(memw #x1030 #x44fc) ; MOVE #8,CCR
(memw #x1032 #x0008)
(memw #x1034 #x5bc0) ; SMI D0
(memw #x1036 #x5ac0) ; SPL D0
;
;  Execute test
;
(print "==> Testing Scc instructions")
(terpri)
(sim-init)
(go #x1000)
(terpri)
(sim-step) ; ST D0
(test-reg D0 #xff)
(sim-step) ; SF D0
(test-reg D0 0)
;
(sim-step) ; MOVE #0,CCR
(test-mask #x00 #xff)
(sim-step) ; SCS D0
(test-reg D0 0)
(sim-step) ; SCC D0
(test-reg D0 #xff)
(sim-step) ; SVS D0
(test-reg D0 0)
(sim-step) ; SVC D0
(test-reg D0 #xff)
(sim-step) ; SEQ D0
(test-reg D0 0)
(sim-step) ; SNE D0
(test-reg D0 #xff)
(sim-step) ; SMI D0
(test-reg D0 0)
(sim-step) ; SPL D0
(test-reg D0 #xff)
;
(sim-step) ; MOVE #1,CCR
(test-mask #x01 #xff)
(sim-step) ; SCS D0
(test-reg D0 #xff)
(sim-step) ; SCC D0
(test-reg D0 0)
;
(sim-step) ; MOVE #2,CCR
(test-mask #x02 #xff)
(sim-step) ; SVS D0
(test-reg D0 #xff)
(sim-step) ; SVC D0
(test-reg D0 0)
;
(sim-step) ; MOVE #4,CCR
(test-mask #x04 #xff)
(sim-step) ; SEQ D0
(test-reg D0 #xff)
(sim-step) ; SNE D0
(test-reg D0 0)
;
(sim-step) ; MOVE #8,CCR
(test-mask #x08 #xff)
(sim-step) ; SMI D0
(test-reg D0 #xff)
(sim-step) ; SPL D0
(test-reg D0 0)
;-------------------------------------------------------------------------------
;  Test shift instructions
;
;  Load memory
;
(memw #x1000 #x5555) ; DATA: DC.W $5555
;
(memw #x1002 #xe1f8) ; ASL DATA
(memw #x1004 #x1000)
(memw #x1006 #xe0f8) ; ASR DATA
(memw #x1008 #x1000)
;
(memw #x100a #x203c) ; MOVE.L #$55555555,D0
(meml #x100c #x55555555)
(memw #x1010 #x7203) ; MOVEQ #3,D1
(memw #x1012 #xe300) ; ASL.B #1,D0
(memw #x1014 #xe340) ; ASL.W #1,D0
(memw #x1016 #xe380) ; ASL.L #1,D0
(memw #x1018 #xe2a0) ; ASR.L D1,D0
(memw #x101a #xe260) ; ASR.W D1,D0
(memw #x101c #xe220) ; ASR.B D1,D0
;
(memw #x101e #x203c) ; MOVE.L #$55555555,D0
(meml #x1020 #x55555555)
(memw #x1024 #xe308) ; LSL.B #1,D0
(memw #x1026 #xe348) ; LSL.W #1,D0
(memw #x1028 #xe388) ; LSL.L #1,D0
(memw #x102a #xe2a8) ; LSR.L D1,D0
(memw #x102c #xe268) ; LSR.W D1,D0
(memw #x102e #xe228) ; LSR.B D1,D0
;
(memw #x1030 #xe7f8) ; ROL DATA
(memw #x1032 #x1000)
(memw #x1034 #xe7f8) ; ROL DATA
(memw #x1036 #x1000)
(memw #x1038 #xe6f8) ; ROR DATA
(memw #x103a #x1000)
(memw #x103c #xe6f8) ; ROR DATA
(memw #x103e #x1000)
;
(memw #x1040 #x203c) ; MOVE.L #$55555555,D0
(meml #x1042 #x55555555)
(memw #x1046 #xe318) ; ROL.B #1,D0
(memw #x1048 #xe358) ; ROL.W #1,D0
(memw #x104a #xe398) ; ROL.L #1,D0
(memw #x104c #xe2b8) ; ROR.L D1,D0
(memw #x104e #xe278) ; ROL.W D1,D0
(memw #x1050 #xe238) ; ROL.B D1,D0
;
(memw #x1052 #xe5f8) ; ROXL DATA
(memw #x1054 #x1000)
(memw #x1056 #xe5f8) ; ROXL DATA
(memw #x1058 #x1000)
(memw #x105a #xe5f8) ; ROXL DATA
(memw #x105c #x1000)
(memw #x105e #xe4f8) ; ROXR DATA
(memw #x1060 #x1000)
(memw #x1062 #xe4f8) ; ROXR DATA
(memw #x1064 #x1000)
(memw #x1066 #xe4f8) ; ROXR DATA
(memw #x1068 #x1000)
;
(memw #x106a #x203c) ; MOVE.L #$55555555,D0
(meml #x106c #x55555555)
(memw #x1070 #x7203) ; MOVEQ #3,D1
(memw #x1072 #x44fc) ; MOVE #$10,CCR
(memw #x1074 #x0010)
;
(memw #x1076 #xe710) ; ROXL.B #3,D0
(memw #x1078 #xe750) ; ROXL.W #3,D0
(memw #x107a #xe790) ; ROXL.L #3,D0
(memw #x107c #xe2b0) ; ROXR.L D1,D0
(memw #x107e #xe270) ; ROXR.W D1,D0
(memw #x1080 #xe230) ; ROXR.B D1,D0
;
;  Execute test
;
(print "==> Testing shift instructions")
(terpri)
(sim-init)
(go #x1002)
(test-memw #x1000 #x5555)
(print "Testing memory ASL/R")
(terpri)
(sim-step) ; ASL DATA
(test-memw #x1000 #xaaaa)
(test-mask #x0a #xff)
(sim-step) ; ASR DATA
(test-memw #x1000 #xd555)
(test-mask #x08 #xff)
(print "Testing fixed count ASL/R")
(terpri)
(sim-step) ; MOVE.L #$55555555,D0
(test-reg D0 #x55555555)
(test-mask #x00 #xff)
(sim-step) ; MOVEQ #3,D1
(test-reg D1 3)
(test-mask #x00 #xff)
(sim-step) ; ASL.B #1,D0
(test-reg D0 #x555555aa)
(test-mask #x0a #xff)
(sim-step) ; ASL.W #1,D0
(test-reg D0 #x5555ab54)
(test-mask #x0a #xff)
(sim-step) ; ASL.L #1,D0
(test-reg D0 #xaaab56a8)
(test-mask #x0a #xff)
(print "Testing register count ASL/R")
(terpri)
(sim-step) ; ASR.L D1,D0
(test-reg D0 #xf5556ad5)
(test-mask #x08 #xff)
(sim-step) ; ASR.W D1,D0
(test-reg D0 #xf5550d5a)
(test-mask #x11 #xff)
(sim-step) ; ASR.B D1,D0
(test-reg D0 #xf5550d0b)
(test-mask #x00 #xff)
(print "Testing LSL/LSR")
(terpri)
(sim-step) ; MOVE.L #$55555555,D0
(test-reg D0 #x55555555)
(print "Testing fixed count LSL/R")
(terpri)
(sim-step) ; LSL.B #1,D0
(test-reg D0 #x555555aa)
(test-mask #x08 #xff)
(sim-step) ; LSL.W #1,D0
(test-reg D0 #x5555ab54)
(test-mask #x08 #xff)
(sim-step) ; LSL.L #1,D0
(test-reg D0 #xaaab56a8)
(test-mask #x08 #xff)
(print "Testing register count LSL/R")
(terpri)
(sim-step) ; LSR.L D1,D0
(test-reg D0 #x15556ad5)
(test-mask #x00 #xff)
(sim-step) ; LSR.W D1,D0
(test-reg D0 #x15550d5a)
(test-mask #x11 #xff)
(sim-step) ; LSR.B D1,D0
(test-reg D0 #x15550d0b)
(test-mask #x00 #xff)
(print "Testing memory ROL/R")
(terpri)
(sim-step) ; ROL DATA
(test-memw #x1000 #xaaab)
(test-mask #x09 #xff)
(sim-step) ; ROL DATA
(test-memw #x1000 #x5557)
(test-mask #x01 #xff)
(sim-step) ; ROR DATA
(test-memw #x1000 #xaaab)
(test-mask #x09 #xff)
(sim-step) ; ROR DATA
(test-memw #x1000 #xd555)
(test-mask #x09 #xff)
(print "Testing fixed count ROL/R")
(terpri)
(sim-step) ; MOVE.L #$55555555,D0
(test-reg D0 #x55555555)
(sim-step) ; ROL.B #1,D0
(test-reg D0 #x555555aa)
(test-mask #x08 #xff)
(sim-step) ; ROL.W #1,D0
(test-reg D0 #x5555ab54)
(test-mask #x08 #xff)
(sim-step) ; ROL.L #1,D0
(test-reg D0 #xaaab56a8)
(test-mask #x08 #xff)
(sim-step) ; ROR.L D1,D0
(print "Testing register count ROL/R")
(terpri)
(test-reg D0 #x15556ad5)
(test-mask #x00 #xff)
(sim-step) ; ROL.W D1,D0
(test-reg D0 #x1555ad5a)
(test-mask #x09 #xff)
(sim-step) ; ROL.B D1,D0
(test-reg D0 #x1555ad4b)
(test-mask #x00 #xff)
(print "Testing memory ROXL/R")
(terpri)
(sim-step) ; ROXL DATA
(test-memw #x1000 #xaaaa)
(test-mask #x19 #xff)
(sim-step) ; ROXL DATA
(test-memw #x1000 #x5555)
(test-mask #x11 #xff)
(sim-step) ; ROXL DATA
(test-memw #x1000 #xaaab)
(test-mask #x08 #xff)
(sim-step) ; ROXR DATA
(test-memw #x1000 #x5555)
(test-mask #x11 #xff)
(sim-step) ; ROXR DATA
(test-memw #x1000 #xaaaa)
(test-mask #x19 #xff)
(sim-step) ; ROXR DATA
(test-memw #x1000 #xd555)
(test-mask #x08 #xff)
(print "Testing register ROXL/R setup")
(terpri)
(sim-step) ; MOVE.L #$55555555,D0
(test-reg D0 #x55555555)
(test-mask #x00 #xff)
(sim-step) ; MOVEQ #3,D1
(test-reg D1 3)
(test-mask #x00 #xff)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xff)
(print "Testing fixed count ROXL")
(terpri)
(sim-step) ; ROXL.B #3,D0
(test-reg D0 #x555555ad)
(test-mask #x08 #xff)
(sim-step) ; ROXL.W #3,D0
(test-reg D0 #x5555ad69)
(test-mask #x08 #xff)
(sim-step) ; ROXL.L #3,D0
(test-reg D0 #xaaad6b49)
(test-mask #x08 #xff)
(print "Testing register count ROXR")
(terpri)
(sim-step) ; ROXR.L D1,D0
(test-reg D0 #x5555ad69)
(test-mask #x00 #xff)
(sim-step) ; ROXR.W D1,D0
(test-reg D0 #x555555ad)
(test-mask #x00 #xff)
(sim-step) ; ROXR.B D1,D0
(test-reg D0 #x55555555)
(test-mask #x11 #xff)
;-------------------------------------------------------------------------------
;  Test STOP instruction
;
;  Load memory
;
(memw #x0020 #x0000) ; Privilege violation exception vector
(memw #x0022 #x100c)
;
(memw #x1000 #x4e72) ; STOP #$55aa
(memw #x1002 #x55aa)
(memw #x1004 #x4e72) ; STOP #$aa55
(memw #x1006 #xaa55)
;
(memw #x100c #x4e73) ; PRIVIOL: RTE
;
;  Execute test
;
(print "==> Testing STOP instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; STOP #$55aa
(test-reg SR #x55aa)
(if (halted)
   (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print " Simulation halted - PASS"))
   (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print "Simulation not halted - *** FAIL ***")))
(terpri)
(halted nil)
(sim-step) ; STOP #$aa55
(test-reg PC #x100c)
(test-reg SR #x35aa)
(sim-step) ; RTE
(test-reg PC #x1008)
(test-reg SR #x55aa)
;-------------------------------------------------------------------------------
;  Test SUB instructions
;
;  Load memory
;
; Setup
(memw #x1000 #x203c) ; MOVE.L #$87654321,D0
(meml #x1002 #x87654321)
(memw #x1006 #x223c) ; MOVE.L #$12345678,D1
(meml #x1008 #x12345678)
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
(meml #x1080 #x00010000)
(memw #x1084 #x0480) ; SUBI.L #$20000,D0
(meml #x1086 #x00020000)
(memw #x108a #x203c) ; MOVE.L #$20000,D0
(meml #x108c #x00020000)
(memw #x1090 #x0480) ; SUBI.L #$10000,D0
(meml #x1092 #x00010000)
; Test SUBI.W
(memw #x1096 #x303c) ; MOVE.W #$1000,D0
(memw #x1098 #x1000)
(memw #x109a #x0440) ; SUBI.W #$2000,D0
(memw #x109c #x2000)
(memw #x109e #x303c) ; MOVE.W #$2000,D0
(memw #x10a0 #x2000)
(memw #x10a2 #x0440) ; SUBI.W #$1000,D0
(memw #x10a4 #x1000)
; Test SUBI.B
(memw #x10a6 #x103c) ; MOVE.B #$10,D0
(memw #x10a8 #x0010)
(memw #x10aa #x0400) ; SUBI.B #$20,D0
(memw #x10ac #x0020)
(memw #x10ae #x103c) ; MOVE.B #$20,D0
(memw #x10b0 #x0020)
(memw #x10b2 #x0400) ; SUBI.B #$10,D0
(memw #x10b4 #x0010)
; Test SUBQ.L
(memw #x10b6 #x7004) ; MOVE.L #4,D0
(memw #x10b8 #x5b80) ; SUBQ.L #5,D0
(memw #x10ba #x7004) ; MOVE.L #4,D0
(memw #x10bc #x5580) ; SUBQ.L #2.D0
; Test SUBQ.W
(memw #x10be #x7004) ; MOVE.L #4,D0
(memw #x10c0 #x5b40) ; SUBQ.W #5,D0
(memw #x10c2 #x7004) ; MOVE.L #4.D0
(memw #x10c4 #x5540) ; SUBQ.W #2,D0
; Test SUBQ.B
(memw #x10c6 #x7004) ; MOVE.L #4,D0
(memw #x10c8 #x5b00) ; SUBQ.B #5,D0
(memw #x10ca #x7004) ; MOVE.L #4,D0
(memw #x10cc #x5500) ; SUBQ.B #2,D0
;
;  Execute test
;
(print "==> Testing SUB instruction")
(terpri)
(sim-init)
(go #x1000)
(print "Test setup")
(terpri)
(sim-step) ; MOVE.L #$87654321,D0
(test-reg D0 #x87654321)
(test-mask #x08 #xff)
(sim-step) ; MOVE.L #$12345678,D1
(test-reg D1 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D0,D6
(test-reg D6 #x87654321)
(test-mask #x08 #xff)
(sim-step) ; MOVE.L D1,D7)
(test-reg D7 #x12345678)
(test-mask #x00 #xff)
;
(print "Test SUB.L")
(terpri)
(sim-step) ; SUB.L D0,D1
(test-reg D1 #x8acf1357)
(test-mask #x1b #xff)
(sim-step) ; MOVE.L D7,D1
(test-reg D1 #x12345678)
(test-mask #x10 #xff)
(sim-step) ; SUB.L D1,D0
(test-reg D0 #x7530eca9)
(test-mask #x02 #xff)
(sim-step) ; SUB.L D1,D1
(test-reg D1 0)
(test-mask #x04 #xff)
;
(print "Test SUB.W")
(terpri)
(sim-step) ; MOVE.L D6,D0
(test-reg D0 #x87654321)
(test-mask #x08 #xff)
(sim-step) ; MOVE.L D7,D1
(test-reg D1 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; SUB.W D0,D1
(test-reg D1 #x12341357)
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D7,D1
(test-reg D1 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; SUB.W D1,D0
(test-reg D0 #x8765eca9)
(test-mask #x19 #xff)
(sim-step) ; SUB.W D1,D1
(test-reg D1 #x12340000)
(test-mask #x04 #xff)
;
(print "Test SUB.B")
(terpri)
(sim-step) ; MOVE.L D6,D0
(test-reg D0 #x87654321)
(test-mask #x08 #xff)
(sim-step) ; MOVE.L D7,D1
(test-reg D1 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; SUB.B D0,D1
(test-reg D1 #x12345657)
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D7,D1
(test-reg D1 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; SUB.B D1,D0
(test-reg D0 #x876543a9)
(test-mask #x19 #xff)
(sim-step) ; SUB.B D1,D1
(test-reg D1 #x12345600)
(test-mask #x04 #xff)
;
(print "Test SUBA.L")
(terpri)
(sim-step) ; MOVE.L D6,A0
(test-reg A0 #x87654321)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L D7,A1
(test-reg A1 #x12345678)
(test-mask #x04 #xff)
(sim-step) ; SUB.L A0,A1
(test-reg A1 #x8acf1357)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L D7,A1
(test-reg A1 #x12345678)
(test-mask #x04 #xff)
(sim-step) ; SUB.L A1,A0
(test-reg A0 #x7530eca9)
(test-mask #x04 #xff)
(sim-step) ; SUB.L A1,A1
(test-reg A1 0)
(test-mask #x04 #xff)
;
(print "Test SUBA.W")
(terpri)
(sim-step) ; MOVE.L D6,A0
(test-reg A0 #x87654321)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L D7,A1
(test-reg A1 #x12345678)
(test-mask #x04 #xff)
(sim-step) ; SUB.W A0,A1
(test-reg A1 #x12341357)
(test-mask #x04 #xff)
(sim-step) ; MOVE.L D7,A1
(test-reg A1 #x12345678)
(test-mask #x04 #xff)
(sim-step) ; SUB.W A1,A0
(test-reg A0 #x8765eca9)
(test-mask #x04 #xff)
(sim-step) ; SUB.W A1,A1
(test-reg A1 #x12340000)
(test-mask #x04 #xff)
;
(print "Test SUBX.L")
(terpri)
(sim-step) ; MOVE #0,CCR
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D6,D0
(test-reg D0 #x87654321)
(test-mask #x08 #xff)
(sim-step) ; MOVE.L D7,D1
(test-reg D1 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; SUBX.L D0,D1
(test-reg D1 #x8acf1357)
(test-mask #x1b #xff)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xff)
(sim-step) ; MOVE.L D7,D1
(test-reg D1 #x12345678)
(test-mask #x10 #xff)
(sim-step) ; SUBX.L D0,D1
(test-reg D1 #x8acf1356)
(test-mask #x1b #xff)
;
(print "Test SUBX.W")
(terpri)
(sim-step) ; MOVE #0,CCR
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D6,D0
(test-reg D0 #x87654321)
(test-mask #x08 #xff)
(sim-step) ; MOVE.L D7,D1
(test-reg D1 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; SUBX.W D0,D1
(test-reg D1 #x12341357)
(test-mask #x00 #xff)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xff)
(sim-step) ; MOVE.L D7,D1
(test-reg D1 #x12345678)
(test-mask #x10 #xff)
(sim-step) ; SUBX.W D0,D1
(test-reg D1 #x12341356)
(test-mask #x00 #xff)
;
(print "Test SUBX.B")
(terpri)
(sim-step) ; MOVE #0,CCR
(test-mask #x00 #xff)
(sim-step) ; MOVE.L D6,D0
(test-reg D0 #x87654321)
(test-mask #x08 #xff)
(sim-step) ; MOVE.L D7,D1
(test-reg D1 #x12345678)
(test-mask #x00 #xff)
(sim-step) ; SUBX.B D0,D1
(test-reg D1 #x12345657)
(test-mask #x00 #xff)
(sim-step) ; MOVE #$10,CCR
(test-mask #x10 #xff)
(sim-step) ; MOVE.L D7,D1
(test-reg D1 #x12345678)
(test-mask #x10 #xff)
(sim-step) ; SUBX.B D0,D1
(test-reg D1 #x12345656)
(test-mask #x00 #xff)
;
(print "Test SUBI.L")
(terpri)
(sim-step) ; MOVE.L #$10000,D0
(test-reg D0 #x00010000)
(test-mask #x00 #xff)
(sim-step) ; SUBI.L #$20000,D0
(test-reg D0 #xffff0000)
(test-mask #x19 #xff)
(sim-step) ; MOVE.L #$20000,D0
(test-reg D0 #x00020000)
(test-mask #x10 #xff)
(sim-step) ; SUBI.L #$10000,D0
(test-reg D0 #x00010000)
(test-mask #x00 #xff)
;
(print "Test SUBI.W")
(terpri)
(sim-step) ; MOVE.W #$1000,D0
(test-reg D0 #x00011000)
(test-mask #x00 #xff)
(sim-step) ; SUBI.W #$2000,D0
(test-reg D0 #x0001f000)
(test-mask #x19 #xff)
(sim-step) ; MOVE.W #$2000,D0
(test-reg D0 #x00012000)
(test-mask #x10 #xff)
(sim-step) ; SUBI.W #$1000,D0
(test-reg D0 #x00011000)
(test-mask #x00 #xff)
;
(print "Test SUBI.B")
(terpri)
(sim-step) ; MOVE.B #$10,D0
(test-reg D0 #x00011010)
(test-mask #x00 #xff)
(sim-step) ; SUBI.B #$20,D0
(test-reg D0 #x000110f0)
(test-mask #x19 #xff)
(sim-step) ; MOVE.B #$20,D0
(test-reg D0 #x00011020)
(test-mask #x10 #xff)
(sim-step) ; SUBI.B #$10,D0
(test-reg D0 #x00011010)
(test-mask #x00 #xff)
;
(print "Test SUBQ.L")
(terpri)
(sim-step) ; MOVE.L #4,D0
(test-reg D0 4)
(sim-step) ; SUBQ.L #5,D0
(test-reg D0 #xffffffff)
(test-mask #x19 #xff)
(sim-step) ; MOVE.L #4,D0
(test-reg D0 4)
(sim-step) ; SUBQ.L #2.D0
(test-reg D0 2)
(test-mask #x00 #xff)
;
(print "Test SUBQ.W")
(terpri)
(sim-step) ; MOVE.L #4,D0
(test-reg D0 4)
(sim-step) ; SUBQ.W #5,D0
(test-reg D0 #xffff)
(test-mask #x19 #xff)
(sim-step) ; MOVE.L #4.D0
(test-reg D0 4)
(sim-step) ; SUBQ.W #2,D0
(test-reg D0 2)
(test-mask #x00 #xff)
;
(print "Test SUBQ.B")
(terpri)
(sim-step) ; MOVE.L #4,D0
(test-reg D0 4)
(sim-step) ; SUBQ.B #5,D0
(test-reg D0 #xff)
(test-mask #x19 #xff)
(sim-step) ; MOVE.L #4,D0
(test-reg D0 4)
(sim-step) ; SUBQ.B #2,D0
(test-reg D0 2)
(test-mask #x00 #xff)
;-------------------------------------------------------------------------------
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
