;
;  Lisp test cases for 68010 simulator
(sim-cpu "68010")
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
(setq DFC 19)
(setq SFC 20)
(setq VBR 21)
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
;  Test instructions changed for 68010
;  New instructions are BKPT, MOVE from CCR, MOVEC, MOVES, and RTD.
;  Changed instruction is MOVE from SR is privileged
;-------------------------------------------------------------------------------
;  Test MOVE SR,r instruction when not privileged
;
;  This specifically tests that MOVE SR,r is privileged for the 68010
;
;  Load memory
;
(meml #x0020 #x00001050) ; Exception vector for privilege violation
;
(memw #x1000 #x2e7c) ; MOVE #$3000,SP
(meml #x1002 #x00003000)
(memw #x1006 #x40c0) ; MOVE SR,D0
(memw #x1008 #x0a40) ; EORI #$2000,D0
(memw #x100a #x2000)
(memw #x100c #x46c0) ; MOVE D0,SR
(memw #x100e #x40c0) ; MOVE SR,D0
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
(sim-step) ; MOVE #$3000,SP
(test-reg SSP #x3000)
(sim-step) ; MOVE SR,D0
(test-reg D0 #x2700)
(sim-step) ; EORI #$2000,D0
(test-reg D0 #x0700)
(sim-step) ; MOVE D0,SR (clear privilege bit)
(test-reg PC #x100e)
(sim-step) ; MOVE SR,D0 (privileged in 68010 and later)
(test-reg PC #x1050)
(test-reg SSP #x2ff8)
(test-memw #x2ff8 #x0700)
(test-meml #x2ffa #x1010)
(test-memw #x2ffe #x0008)
;-------------------------------------------------------------------------------
;  Test instructions new to 68010
;  New instructions are BKPT, MOVE from CCR, MOVEC, MOVES, and RTD.
;
;  Since the bus object doesn't provide test visibility to the processor
;  mode and addressing mode, MOVES can't really be tested.  All that the
;  test covers here is if it can read and write to memory and that the
;  proper stack pointer is read/written.
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
(memw #x1006 #x40c0) ; MOVE SR,D0
(memw #x1008 #x0a40) ; EORI #$2000,D0
(memw #x100a #x2000)
(memw #x100c #x46c0) ; MOVE D0,SR  --  Drop privileges
(memw #x100e #x203c) ; MOVE.L #$FFFFFFFF,D0
(meml #x1010 #xFFFFFFFF)
(memw #x1014 #x223c) ; MOVE.L #$12345678,D1
(meml #x1016 #x12345678)
(memw #x101a #x44c0) ; MOV D0,CCR
(memw #x101c #x42c1) ; MOV CCR,D0
;
(memw #x101e #x4848) ; BKPT #0
;
(memw #x1020 #x203c) ; MOVE.L #$5000,D0
(meml #x1022 #x00005000)
(memw #x1026 #x4e7b) ; MOVEC D0,#$801
(memw #x1028 #x0801)
(memw #x102a #x4AFC) ; ILLEGAL
(memw #x102c #x7005) ; MOVEQ #5,D0
(memw #x102e #x4e7b) ; MOVEC D0,#0
(memw #x1030 #x0000)
(memw #x1032 #x4e7b) ; MOVEC D0,#1
(memw #x1034 #x0001)
(memw #x1036 #x70FF) ; MOVEQ #FF,D0
(memw #x1038 #x4e7b) ; MOVEC D0,#$800
(memw #x103a #x0800)
;
;  Sim-init here to clear registers and restore supervisor mode.
;
(memw #x103c #x2e7c) ; MOVE #$4000,SP
(meml #x103e #x00004000)
(memw #x1042 #x7005) ; MOVEQ #5,D0
(memw #x1044 #x4e7b) ; MOVEC D0,#0
(memw #x1046 #x0000)
(memw #x1048 #x4e7b) ; MOVEC D0,#1
(memw #x104a #x0001)
(memw #x104c #x207c) ; MOVEA.L #$7000,A0
(meml #x104e #x00007000)
(memw #x1052 #x2a3c) ; MOVE.L #$12345678,D5
(meml #x1054 #x12345678)
(memw #x1058 #x0e98) ; MOVES.L D5,(A0)+
(memw #x105a #x5800)
(memw #x105c #x0e58) ; MOVES.W D5,(A0)+
(memw #x105e #x5800)
(memw #x1060 #x0e18) ; MOVES.B D5,(A0)+
(memw #x1062 #x5800)
(memw #x1064 #x207c) ; MOVEA.L #$7000,A0
(meml #x1066 #x00007000)
(memw #x106a #x0e98) ; MOVES.L (A0)+,D0
(memw #x106c #x0000)
(memw #x106e #x0e58) ; MOVES.W (A0)+,D1
(memw #x1070 #x1000)
(memw #x1072 #x0e18) ; MOVES.B (A0)+,D2
(memw #x1074 #x2000)
;
(memw #x1076 #x4eb9) ; JSR #x107e
(meml #x1078 #x0000107e)
(memw #x107c #x6004) ; BRA 4
(memw #x107e #x4e74) ; RTD #$10
(memw #x1080 #x0010)
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
;  Load memory (vectors move with VBR)
;
(meml #x5000 #x00004000) ; Reset SSP
(meml #x5004 #x00001000) ; Reset PC
(meml #x5008 #x00006000) ; Exception vector 2 for bus error
(meml #x500C #x00006010) ; Exception vector 3 for odd address
(meml #x5010 #x00006020) ; Exception vector 4 for illegal instruction
(meml #x5014 #x00006030) ; Exception vector 5 for division by 0
(meml #x5018 #x00006040) ; Exception vector 6 for CHK instruction
(meml #x501C #x00006050) ; Exception vector 7 for TRAPV instruction
(meml #x5020 #x00006060) ; Exception vector 8 for privilege violation
(meml #x5024 #x00006070) ; Exception vector 9 for trace
(meml #x5028 #x00006080) ; Exception vector 10 for A-line instructions
(meml #x502C #x00006090) ; Exception vector 11 for F-line instructions
;   ex_14_format    : constant byte := 14;  --  68010 only (and later?)
;   ex_24_spurious  : constant byte := 24;
;   ex_32_TRAP_base : constant byte := 32;  --  Add trap number to get exception
;
;  Load memory (handlers from moved vectors)
;
(memw #x6000 #x4e73) ; Bus error (2)
(memw #x6010 #x4e73) ; Odd address (3)
(memw #x6020 #x54af) ; ADDQ.L #2,2(SP)  --   Illegal instruction (4)
(memw #x6022 #x0002)
(memw #x6024 #x4e73) ; RTE
(memw #x6030 #x4e73) ; Division by 0 (5)
(memw #x6040 #x4e73) ; CHK instruction (6)
(memw #x6050 #x4e73) ; TRAPV instruction (7)
(memw #x6060 #x4e73) ; Privilege violation (8)
(memw #x6070 #x4e73) ; Trace (9)
(memw #x6080 #x4e73) ; A-line instruction (10)
(memw #x6090 #x4e73) ; F-line instruction (11)
;
;  Test memory area
;
(meml #x7000 0)
(meml #x7004 0)
(meml #x7008 0)
;
;  Execute test
;
(print "==> Testing new instructions for 68010")
(terpri)
(sim-init)
(go #x1000)
;
(print "Testing MOVE from/to CCR")
(terpri)
(sim-step) ; MOVE #$4000,SP
(test-reg SSP #x4000)
(sim-step) ; MOVE SR,D0
(test-reg D0 #x2700)
(test-reg SR #x2700)
(sim-step) ; EORI #$2000,D0
(test-reg D0 #x0700)
(sim-step) ; MOVE D0,SR (clear privilege bit)
(test-reg PC #x100e)
(test-reg SR #x0700)
(sim-step) ; MOVE.L #$FFFFFFFF,D0
(test-reg PC #x1014)
(test-reg D0 #xffffffff)
(sim-step) ; MOVE.L #$12345678,D1
(test-reg D1 #x12345678)
(test-reg PC #x101a)
(sim-step) ; MOV D0,CCR
(test-reg PC #x101c)
(test-reg SR #x07ff)
(sim-step) ; MOV CCR,D1
(test-reg D1 #x1234ffff)
(test-reg PC #x101e)
;
(print "Testing BKPT")
(terpri)
(sim-step) ; BKPT
(test-reg PC #x3020)
(sim-step) ; ADDQ.L #2,2(SP)
(test-reg PC #x3024)
(sim-step) ; RTE
(test-reg PC #x1020)
;
(print "Testing MOVEC")
(terpri)
(sim-step) ; MOVE.L #$5000,D0
(test-reg D0 #x5000)
(test-reg PC #x1026)
(test-reg USP 0)
(test-reg VBR 0)
(test-reg DFC 0)
(test-reg SFC 0)
(sim-step) ; MOVEC D0,#$801
(test-reg PC #x102a)
(test-reg VBR #x5000)
(test-reg USP 0)
(test-reg DFC 0)
(test-reg SFC 0)
(sim-step) ; ILLEGAL
(test-reg PC #x6020)
(sim-step) ; ADDQ.L #2,2(SP)
(test-reg PC #x6024)
(sim-step) ; RTE
(test-reg PC #x102c)
(test-reg USP 0)
(test-reg VBR #x5000)
(test-reg DFC 0)
(test-reg SFC 0)
(sim-step) ; MOVEQ #5,D0
(test-reg D0 #x0005)
(test-reg PC #x102e)
(sim-step) ; MOVEC D0,#0
(test-reg USP 0)
(test-reg VBR #x5000)
(test-reg DFC 0)
(test-reg SFC 5)
(test-reg PC #x1032)
(sim-step) ; MOVEC D0,#1
(test-reg USP 0)
(test-reg VBR #x5000)
(test-reg DFC 5)
(test-reg SFC 5)
(test-reg PC #x1036)
(sim-step) ; MOVEQ #FF,D0
(test-reg D0 #xffffffff)
(test-reg PC #x1038)
(sim-step) ; MOVEC D0,#$800
(test-reg USP #xffffffff)
(test-reg VBR #x5000)
(test-reg DFC 5)
(test-reg SFC 5)
(test-reg PC #x103c)
;
(print "Testing MOVES")
(terpri)
(sim-init)
(go #x103c)
(sim-step) ; MOVE #$4000,SP
(test-reg SSP #x4000)
(test-reg PC #x1042)
(test-reg VBR 0)
(test-reg DFC 0)
(test-reg SFC 0)
(test-reg SR #x2700)
(sim-step) ; MOVEQ #5,D0
(test-reg D0 5)
(test-reg PC #x1044)
(sim-step) ; MOVEC D0,#0
(test-reg DFC 0)
(test-reg SFC 5)
(test-reg PC #x1048)
(sim-step) ; MOVEC D0,#1
(test-reg DFC 5)
(test-reg SFC 5)
(test-reg PC #x104c)
(sim-step) ; MOVEA.L #$7000,A0
(test-reg A0 #x7000)
(test-reg PC #x1052)
(sim-step) ; MOVE.L #$12345678,D5
(test-reg D5 #x12345678)
(test-reg PC #x1058)
(sim-step) ; MOVES.L D5,(A0)+
(test-reg A0 #x7004)
(test-reg PC #x105c)
(test-meml #x7000 #x12345678)
(sim-step) ; MOVES.W D5,(A0)+
(test-reg A0 #x7006)
(test-reg PC #x1060)
(test-memw #x7004 #x5678)
(sim-step) ; MOVES.B D5,(A0)+
(test-reg A0 #x7007)
(test-reg PC #x1064)
(test-memb #x7006 #x78)
(sim-step) ; MOVEA.L #$7000,A0
(test-reg A0 #x7000)
(test-reg PC #x106a)
(test-reg D0 5)
(test-reg D1 0)
(test-reg D2 0)
(sim-step) ; MOVES.L (A0)+,D0
(test-reg D0 #x12345678)
(test-reg A0 #x7004)
(test-reg PC #x106e)
(sim-step) ; MOVES.W (A0)+,D1
(test-reg D1 #x5678)
(test-reg A0 #x7006)
(test-reg PC #x1072)
(sim-step) ; MOVES.B (A0)+,D2
(test-reg D2 #x78)
(test-reg A0 #x7007)
(test-reg PC #x1076)
;
(print "Testing RTD")
(terpri)
(sim-step) ; JSR #x107e
(test-reg SSP #x3ffc)
(test-reg PC #x107e)
(test-meml #x3ffc #x107c)
(sim-step) ; RTD #$10
(test-reg PC #x107c)
(test-reg SSP #x4010)
(sim-step) ; BRA 4
(test-reg PC #x1082)
;
(memw #x1076 #x4eb9) ; JSR #x107e
(meml #x1078 #x0000107e)
(memw #x107c #x6004) ; BRA 4
(memw #x107e #x4e74) ; RTD #$10
(memw #x1080 #x0010)
;
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
