1
lisp
;
;  Lisp test cases for 8080 simulator
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
(setq RA 0)
(setq RPSW 1)
(setq RB 2)
(setq RC 3)
(setq RBC 4)
(setq RD 5)
(setq RE 6)
(setq RDE 7)
(setq RH 8)
(setq RL 9)
(setq RHL 10)
(setq RSP 11)
(setq RPC 12)
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
;  Status register bits are S|Z|0|AC|0|P|1|C
;                           7 6 5  4 3 2 1 0
;
(defun test-mask (expected mask)
  (print "PSW expected ")
  (print-hex expected)
  (print ", masked ")
  (print-hex (and expected mask))
  (print ", actual ")
  (print-hex (and (reg-val RPSW) mask))
  (if (= (and expected mask) (and (reg-val RPSW) mask))
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
;  Check a byte in memory
;
(defun test-memb (address expected)
  (print "Memory ")
  (print-hex address)
  (print " expected ")
  (print-hex expected)
  (print " actual ")
  (print-hex (memb address))
  (if (= expected (memb address))
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
;  Test ADD/ADC instructions
;
; Load memory
;
;  Initialize registers
(memw #x0100 #x06de) ; MVI B,DE
(memw #x0102 #x0ead) ; MVI C,AD
(memw #x0104 #x16be) ; MVI D,BE
(memw #x0106 #x1eef) ; MVI E,EF
(memw #x0108 #x2610) ; MVI H,10
(memw #x010a #x2e01) ; MVI L,01
(memw #x010c #x3612) ; MVI M,12
(memw #x010e #x3e34) ; MVI A,34
;
;  Test ADD x instructions
(memb #x0110 #x80) ; ADD B
(memb #x0111 #x81) ; ADD C
(memb #x0112 #x82) ; ADD D
(memb #x0113 #x83) ; ADD E
(memb #x0114 #x84) ; ADD H
(memb #x0115 #x85) ; ADD L
(memb #x0116 #x86) ; ADD M (address in HL (12A), contents 0E)
(memb #x0117 #x87) ; ADD A
;
;  Test ADC x instructions
(memb #x0118 #x88) ; ADC B
(memb #x0119 #x89) ; ADC C
(memb #x011a #x8a) ; ADC D
(memb #x011b #x8b) ; ADC E
(memb #x011c #x8c) ; ADC H
(memb #x011d #x8d) ; ADC L
(memb #x011e #x8e) ; ADC M (address in HL (12A), contents 0E)
(memb #x011f #x8f) ; ADC A
;
;  Execute test
;
(print "==> Testing ADD/ADC instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; MVI B,DE
(test-reg RB #xde)
(sim-step) ; MVI C,AD
(test-reg RC #xad)
(sim-step) ; MVI D,BE
(test-reg RD #xbe)
(sim-step) ; MVI E,EF
(test-reg RE #xef)
(sim-step) ; MVI H,10
(test-reg RH #x10)
(sim-step) ; MVI L,01
(test-reg RL #x01)
(sim-step) ; MVI M,12
(test-memb #x1001 #x12)
(sim-step) ; MVI A,34
(test-reg RA #x34)
;
(print "Testing ADD instructions")
(terpri)
(sim-step) ; Verify ADD B
; Verify that A is 12, PC is 111, A,P,&C flags are set
(test-reg ra #x12)
(test-reg rpc #x0111)
(test-reg rpsw #x17)
(sim-step) ; Verify ADD C
; Verify that A is BF, PC is 112, S flag is set
(test-reg ra #xbf)
(test-reg rpc #x0112)
(test-reg rpsw #x82)
(sim-step) ; Verify ADD D
; Verify that A is 7D, PC is 113, A,P,&C flags are set
(test-reg RA #x7d)
(test-reg RPC #x113)
(test-reg RPSW #x17)
(sim-step) ; Verify ADD E
; Verify that A is 6C, PC is 114, A,P,&C flags are set
(test-reg RA #x6c)
(test-reg RPC #x114)
(test-reg RPSW #x17)
(sim-step) ; Verify ADD H
; Verify that A is 7C, PC is 115, no flags are set
(test-reg RA #x7c)
(test-reg RPC #x115)
(test-reg RPSW #x02)
(sim-step) ; Verify ADD L
; Verify that A is 7D, PC is 116, P flag is set
(test-reg RA #x7d)
(test-reg RPC #x116)
(test-reg RPSW #x06)
(sim-step) ; Verify ADD M
; Verify that A is 8F, PC is 117, S flag is set
(test-reg RA #x8f)
(test-reg RPC #x117)
(test-reg RPSW #x82)
(sim-step) ; Verify ADD A
; Verify that A is 1E, PC is 118, A,P,&C flags are set
(test-reg RA #x1e)
(test-reg RPC #x118)
(test-reg RPSW #x17)
;
(print "Verifying ADC instructions")
(terpri)
(sim-step) ; Verify ADC B
; Verify that A is FD, PC is 119, S&A flags are set
(test-reg RA #xfd)
(test-reg RPC #x119)
(test-reg RPSW #x92)
(sim-step) ; Verify ADC C
; Verify that A is AA, PC is 11A, S,A,P,&C flags are set
(test-reg RA #xaa)
(test-reg RPC #x11a)
(test-reg RPSW #x97)
(sim-step) ; Verify ADC D
; Verify that A is 69, PC is 11B, A,P,&C flags are set
(test-reg RA #x69)
(test-reg RPC #x11b)
(test-reg RPSW #x17)
(sim-step) ; Verify ADC E
; Verify that A is 59, PC is 11C, A,P,&C flags are set
(test-reg RA #x59)
(test-reg RPC #x11c)
(test-reg RPSW #x17)
(sim-step) ; Verify ADC H
; Verify that A is 6A, PC is 11D, P flag is set
(test-reg RA #x6a)
(test-reg RPC #x11d)
(test-reg RPSW #x06)
(sim-step) ; Verify ADC L
; Verify that A is 6B, PC is 11E, no flags are set
(test-reg RA #x6b) ; Actually d4
(test-reg RPC #x11e)
(test-reg RPSW #x02)
(sim-step) ; Verify ADC M
; Verify that A is 7D, PC is 11F, P flag is set
(test-reg RA #x7d)
(test-reg RPC #x11f)
(test-reg RPSW #x06)
(sim-step) ; Verify ADC A
; Verify that A is FA, PC is 120, S,A,&P flags are set
(test-reg RA #xfa)
(test-reg RPC #x120)
(test-reg RPSW #x96)
;-------------------------------------------------------------------------------
;  Test ALU Immediate instructions
;
; Load memory
;
(memw #x0100 #x06de) ; MVI B,DE
(memw #x0102 #x0ead) ; MVI C,AD
(memw #x0104 #x16be) ; MVI D,BE
(memw #x0106 #x1eef) ; MVI E,EF
(memw #x0108 #x2610) ; MVI H,10
(memw #x010a #x2e01) ; MVI L,01
(memw #x010c #x3612) ; MVI M,12
(memw #x010e #x3e34) ; MVI A,34
;
;  Test immediate instructions
(memw #x0110 #xc60f) ; ADI 0F
(memw #x0112 #xce88) ; ACI 88
(memw #x0114 #xd6d2) ; SUI D2
(memw #x0116 #xde22) ; SBI 22
(memw #x0118 #xe6f0) ; ANI F0
(memw #x011a #xeeff) ; XRI FF
(memw #x011c #xf6d0) ; ORI F0
(memw #x011e #xfe00) ; CPI 00
;
;  Execute test
;
(print "==> Testing ALU Immediate instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; MVI B,DE
(test-reg RB #xde)
(sim-step) ; MVI C,AD
(test-reg RC #xad)
(sim-step) ; MVI D,BE
(test-reg RD #xbe)
(sim-step) ; MVI E,EF
(test-reg RE #xef)
(sim-step) ; MVI H,10
(test-reg RH #x10)
(sim-step) ; MVI L,01
(test-reg RL #x01)
(sim-step) ; MVI M,12
(test-memb #x1001 #x12)
(sim-step) ; MVI A,34
(test-reg RA #x34)
;
(sim-step) ;  Verify ADI 0F
; Verify that A is 43, A&P flags are set, and PC is 112
(test-reg RA #x43)
(test-reg RPC #x112)
(test-reg RPSW #x12)
(sim-step) ;  Verify ACI 88
; Verify that A is CB, S flag is set, and PC is 114
(test-reg RA #xcb)
(test-reg RPC #x114)
(test-reg RPSW #x82)
(sim-step) ;  Verify SUI D2
; Verify that A is F9, flags S,P,&C are set, and PC is 116
(test-reg RA #xf9)
(test-reg RPC #x116)
(test-reg RPSW #x87)
(sim-step) ;  Verify SBI 22
; Verify that A is D6, flag S is set, and PC is 118
(test-reg RA #xd6)
(test-reg RPC #x118)
(test-reg RPSW #x82)
(sim-step) ;  Verify ANI F0
; Verify that A is C0, flag S is set, and PC is 11A
(test-reg RA #xd0)
(test-reg RPC #x11a)
(test-reg RPSW #x82)
(sim-step) ; Verify XRI FF
; Verify that A is 2F and PC is 11C
(test-reg RA #x2f)
(test-reg RPC #x11c)
(test-reg RPSW #x02)
(sim-step) ; Verify ORI F0
; Verify that A is FF, flags S&P are set, and PC is 11E
(test-reg RA #xff)
(test-reg RPC #x11e)
(test-reg RPSW #x86)
(sim-step) ; Verify CPI 00
; Verify that A is unchanged, flags S&P are set, PC is 120
(test-reg RA #xff)
(test-reg RPC #x120)
(test-reg RPSW #x86)
;-------------------------------------------------------------------------------
;  Test ANA instructions
;
; Load memory
;
(memw #x0100 #x06de) ; MVI B,DE
(memw #x0102 #x0ead) ; MVI C,AD
(memw #x0104 #x16be) ; MVI D,BE
(memw #x0106 #x1eef) ; MVI E,EF
(memw #x0108 #x2610) ; MVI H,10
(memw #x010a #x2e01) ; MVI L,01
(memw #x010c #x3612) ; MVI M,12
(memw #x010e #x3e34) ; MVI A,34
;  Test ANA instructions
(memb #x0110 #xa0) ; ANA B
(memb #x0111 #xa1) ; ANA C
(memb #x0112 #x2f) ; CMA
(memb #x0113 #xa2) ; ANA D
(memb #x0114 #xa3) ; ANA E
(memb #x0115 #xa4) ; ANA H
(memb #x0116 #x2f) ; CMA
(memb #x0117 #xa5) ; ANA L
(memb #x0118 #x2f) ; CMA
(memb #x0119 #xa6) ; ANA M (Address 1001, contents 12)
(memb #x011a #xa7) ; ANA A (effectively a NOP)
;
;  Execute test
;
(print "==> Testing ANA instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; MVI B,DE
(test-reg RB #xde)
(sim-step) ; MVI C,AD
(test-reg RC #xad)
(sim-step) ; MVI D,BE
(test-reg RD #xbe)
(sim-step) ; MVI E,EF
(test-reg RE #xef)
(sim-step) ; MVI H,10
(test-reg RH #x10)
(sim-step) ; MVI L,01
(test-reg RL #x01)
(sim-step) ; MVI M,12
(test-memb #x1001 #x12)
(sim-step) ; MVI A,34
(test-reg RA #x34)
;
(sim-step) ; Verify ANA B
; Verify that register A is 14, flag P is set, and PC is 111
(test-reg RA #x14)
(test-reg RPC #x111)
(test-reg RPSW #x06)
(sim-step); Verify ANA C
; Verify that register A is 04, no flags are set, and PC is 112
(test-reg RA #x04)
(test-reg RPC #x112)
(test-reg RPSW #x02)
(sim-step); Complement A
(test-reg RA #xFB)
(sim-step) ; Verify ANA D
; Verify that register A is BA, S flag is set, and PC is 114
(test-reg RA #xba)
(test-reg RPC #x114)
(test-reg RPSW #x82)
(sim-step) ; Verify ANA E
; Verify that register A is AA, S&P flags are set, and PC is 115
(test-reg RA #xaa)
(test-reg RPC #x115)
(test-reg RPSW #x86)
(sim-step) ; Verify ANA H
; Verify that register A is 00, Z&P flags are set, and PC is 116
(test-reg RA #x00)
(test-reg RPC #x116)
(test-reg RPSW #x46)
(sim-step) ; Complement A
(test-reg RA #xff)
(sim-step) ; Verify ANA L
; Verify that register A is 01, all flags clear, and PC is 118
(test-reg RA #x01)
(test-reg RPC #x118)
(test-reg RPSW #x02)
(sim-step) ; Complement A
(test-reg RA #xFE)
(sim-step) ; Verify ANA M
; Verify that register A is 12, P flag is set, and PC is 11A
(test-reg RA #x12)
(test-reg RPC #x11a)
(test-reg RPSW #x06)
(sim-step) ; Verify ANA A
; Verify that registers are unchanged, and PC is 11B
(test-reg RA #x12)
(test-reg RPC #x11b)
(test-reg RPSW #x06)
;
;
;  Status register bits are S|Z|0|AC|0|P|1|C
;                           7 6 5  4 3 2 1 0
;-------------------------------------------------------------------------------
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
