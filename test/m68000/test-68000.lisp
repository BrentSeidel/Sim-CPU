;
;  Lisp test cases for 68000 simulator
(sim-cpu "68000")
(trace 33) ; Instructions and exceptions
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
;-------------------------------------------------------------------------------
;  Test MOVE SR,r instruction when not privileged
;
;  This specifically tests that MOVE SR,r is not privileged for the 68000/68008
;
;  Load memory
;
(meml #x0020 #x00001050) ; Exception vector for privilege violation
(memw #x1000 #x40c0) ; MOVE SR,D0
(memw #x1002 #x0a40) ; EORI #$2000,D0
(memw #x1004 #x2000)
(memw #x1006 #x46c0) ; MOVE D0,SR
(memw #x1008 #x40c0) ; MOVE SR,D0
;
; PRIV
(memw #x1050 #x4e73) ; RTE
;
;  Execute test
;
(print "==> Testing MOVE SR,r instruction privilege")
(terpri)
(sim-init)
(go #x1000)
;
(print "Testing MOVE from/to SR")
(terpri)
(test-reg PC #x1000)
(sim-step) ; MOVE SR,D0
(test-reg D0 #x2700)
(sim-step) ; EORI #$2000,D0
(test-reg D0 #x0700)
(sim-step) ; MOVE D0,SR (clear privilege bit)
(test-reg PC #x1008)
(sim-step) ; MOVE SR,D0 (privileged in 68010 and later)
(test-reg PC #x100a)
;
;-------------------------------------------------------------------------------
;  Test 680x0 instructions that are not implmented for 68000/68008
;
;  Load memory (vectors)
;
(meml #x0000 #x00004000) ; Reset SSP
(meml #x0004 #x00001000) ; Reset PC
(meml #x0008 #x00003000) ; Exception vector 2 for bus error
(meml #x000C #x00003010) ; Exception vector 3 for odd address
(meml #x0010 #x00003020) ; Exception vector 4 for illegal instruction
(meml #x0014 #x00003030) ; Exception vector 5 for division by 0
(meml #x0018 #x00003040) ; Exception vector 6 for CHK instruction
(meml #x001C #x00003050) ; Exception vector 7 for TRAPV instruction
(meml #x0020 #x00003060) ; Exception vector 8 for privilege violation
(meml #x0024 #x00003070) ; Exception vector 9 for trace
(meml #x0028 #x00003080) ; Exception vector 10 for A-line instructions
(meml #x002C #x00003090) ; Exception vector 11 for F-line instructions
;   ex_14_format    : constant byte := 14;  --  68010 only (and later?)
;   ex_24_spurious  : constant byte := 24;
;   ex_32_TRAP_base : constant byte := 32;  --  Add trap number to get exception
;
;  Load memory (test)
;
(memw #x1000 #x2e7c) ; MOVE #$4000,SP
(meml #x1002 #x00004000)
(memw #x1006 #x42c0) ; MOVE CCR,D0
(memw #x1008 #x4848) ; BKPT #0
(memw #x100a #x4e7b) ; MOVEC to ctrl
(memw #x100c #x0e00) ; MOVES.B to/from D0
(memw #x100e #x4e74) ; RTD
;
;  Load memory (handlers)
;
(memw #x3000 #x4e73) ; Bus error (2)
(memw #x3010 #x4e73) ; Odd address (3)
(memw #x3020 #x54af) ; ADDQ.L #2,2(SP)  --   Illegal instruction (4)
(memw #x3022 #x0002)
(memw #x3024 #x4e73) ; RTE
(memw #x3030 #x4e73) ; Division by 0 (5)
(memw #x3040 #x4e73) ; CHK instruction (6)
(memw #x3050 #x4e73) ; TRAPV instruction (7)
(memw #x3060 #x4e73) ; Privilege violation (8)
(memw #x3070 #x4e73) ; Trace (9)
(memw #x3080 #x4e73) ; A-line instruction (10)
(memw #x3090 #x4e73) ; F-line instruction (11)
;
;  Execute test
;
(print "==> Testing MOVE CCR,r instruction not implemented")
(terpri)
(sim-init)
(go #x1000)
;
(sim-step) ; MOVE.L #3$000,SP
(test-reg SSP #x4000)
(test-reg PC #x1006)
(sim-step) ; MOVE CCR,D0
(test-reg SSP #x3ffa)
(test-reg PC #x3020)
(test-memw #x3ffa #x2700)
(test-meml #x3ffc #x1006)
(sim-step) ; ADDQ.L #2,2(SP)
(test-reg PC #x3024)
(sim-step) ; RTE
(test-reg PC #x1008)
(sim-step) ; BKPT #0
(test-reg PC #x3020)
(sim-step) ; ADDQ.L #2,2(SP)
(test-reg PC #x3024)
(sim-step) ; RTE
(test-reg PC #x100a)
(sim-step) ; MOVEC to ctrl
(test-reg PC #x3020)
(sim-step) ; ADDQ.L #2,2(SP)
(test-reg PC #x3024)
(sim-step) ; RTE
(test-reg PC #x100c)
(sim-step) ; MOVES.B to/from D0
(test-reg PC #x3020)
(sim-step) ; ADDQ.L #2,2(SP)
(test-reg PC #x3024)
(sim-step) ; RTE
(test-reg PC #x100e)
(sim-step) ; RTD
(test-reg PC #x3020)
(sim-step) ; ADDQ.L #2,2(SP)
(test-reg PC #x3024)
(sim-step) ; RTE
(test-reg PC #x1010)
;
;-------------------------------------------------------------------------------
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
