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
;-------------------------------------------------------------------------------
;  Test CALL and RET instructions
;
; Load memory
;
;  Returns
(memb #x0010 #xc0) ; RNZ
(memb #x0011 #xd0) ; RNC
(memb #x0012 #xe0) ; RPO
(memb #x0013 #xf0) ; RP
(memb #x0014 #xc8) ; RZ
(memb #x0015 #xd8) ; RC
(memb #x0016 #xe8) ; RPE
(memb #x0017 #xf8) ; RM
(memb #x0018 #xc9) ; RET
; Target for branches that should not be taken
(memb #x00ff #x76) ; HLT
; Initialize SP to 2000
(memb #x0100 #x31) ; LXI SP,2000
(memw #x0101 #x0020)
; Ordinary call and return
(memb #x0103 #xcd) ; CALL 18
(memw #x0104 #x1800)
;  Test conditional CALLs and RETs
; No flags set
(memb #x0106 #xc4) ; CNZ 10
(memw #x0107 #x1000)
(memb #x0109 #xd4) ; CNC 11
(memw #x010a #x1100)
(memb #x010c #xe4) ; CPO 12
(memw #x010d #x1200)
(memb #x010f #xf4) ; CP 13
(memw #x0110 #x1300)
(memb #x0112 #xcc) ; CZ FF
(memw #x0113 #xff00)
(memb #x0115 #xdc) ; CC FF
(memw #x0116 #xff00)
(memb #x0118 #xec) ; CPE FF
(memw #x0119 #xff00)
(memb #x011b #xfc) ; CM FF
(memw #x011c #xff00)
(memb #x011e #xcd) ; CALL 14
(memw #x011f #x1400)
(memb #x0121 #xaf) ; XRA A
(memb #x0122 #xc4) ; CNZ FF
(memw #x0123 #xff00)
(memb #x0125 #xd4) ; CNC 10
(memw #x0126 #x1000)
(memb #x0128 #xe4) ; CPO FF
(memw #x0129 #xff00)
(memb #x012b #xf4) ; CP 12
(memw #x012c #x1200)
(memb #x012e #xcc) ; CZ 14
(memw #x012f #x1400)
(memb #x0131 #xdc) ; CC FF
(memw #x0132 #xff00)
(memb #x0134 #xec) ; CPE 15
(memw #x0135 #x1500)
(memb #x0137 #xfc) ; CM FF
(memw #x0138 #xff00)
(memb #x013a #xcd) ; CALL 17
(memw #x013b #x1700)
(memw #x013d #x3eff) ; MVI A FF
(memb #x013f #x87) ; ADD A
(memb #x0140 #xc4) ; CNZ 10
(memw #x0141 #x1000)
(memb #x0143 #xd4) ; CNC FF
(memw #x0144 #xff00)
(memb #x0146 #xe4) ; CPO 11
(memw #x0147 #x1100)
(memb #x0149 #xf4) ; CP FF
(memw #x014a #xff00)
(memb #x014c #xcc) ; CZ FF
(memw #x014d #xff00)
(memb #x014f #xdc) ; CC 13
(memw #x0150 #x1300)
(memb #x0152 #xec) ; CPE 15
(memw #x0153 #x1500)
(memb #x0155 #xfc) ; CM 16
(memw #x0156 #x1600)
;
;  Execute test
;
(print "==> Testing CALL-RET instructions")
(terpri)
(sim-init)
(go #x0100)
; Set SP
(sim-step) ; LXI SP,2000
(test-reg RSP #x2000)
(test-reg RPSW #x02)
(test-reg RA #x00)
(sim-step) ; CALL 18  ; Verify CALL
; Verify that PC is 18, SP is 1FFE, and the stack contains 06 01
(test-reg RPC #x18)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x0601)
(sim-step) ; RET  ; Verify RET
; Verify that PC is 106, SP is 2000
(test-reg RPC #x0106)
(test-reg RSP #x2000)
(sim-step) ; CNZ 10  ; Verify CNZ taken
; Verify that PC is 10, SP is 1FFE
(test-reg RPC #x0010)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x0901)
(sim-step) ; RNZ  ; Verify RNZ taken
; Verify that PC is 109, SP is 2000
(test-reg RPC #x0109)
(test-reg RSP #x2000)
(sim-step) ; CNC 11  ; Verify CNC taken
; Verify that PC is 11, SP is 1FFE
(test-reg RPC #x0011)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x0c01)
(sim-step) ; RNC  ; Verify RNC taken
; Verify that PC is 10C, SP is 2000
(test-reg RPC #x010c)
(test-reg RSP #x2000)
(sim-step) ; CPO 12  ; Verify CPO taken
; Verify that PC is 12, SP is 1FFE
(test-reg RPC #x0012)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x0f01)
(sim-step) ; RPO  ; Verify RPO taken
; Verify that PC is 10F, SP is 2000
(test-reg RPC #x010f)
(test-reg RSP #x2000)
(sim-step) ; CP 13  ; Verify CP taken
; Verify that PC is 13, SP is 1FFE
(test-reg RPC #x0013)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x1201)
(sim-step) ; RP  ; Verify RP taken
; Verify that PC is 112, SP is 2000
(test-reg RPC #x0112)
(test-reg RSP #x2000)
(sim-step) ; CZ FF  ; Verify CZ not taken
; Verify that PC is 115, SP is 2000
(test-reg RPC #x0115)
(test-reg RSP #x2000)
(sim-step) ; CC FF  ; Verify CC not taken
; Verify that PC is 118, SP is 2000
(test-reg RPC #x0118)
(test-reg RSP #x2000)
(sim-step) ; CPE FF  ; Verify CPE not taken
; Verify that PC is 11B, SP is 2000
(test-reg RPC #x11B)
(test-reg RSP #x2000)
(sim-step) ; CM FF  ; Verify CM not taken
; Verify that PC is 11E, SP is 2000
(test-reg RPC #x011e)
(test-reg RSP #x2000)
; Verify returns not taken
(sim-step) ; CALL 14
; Verify that PC is 14, SP is 1FFE
(test-reg RPC #x0014)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2101)
(sim-step) ; RZ  ; RZ not taken
; Verify that PC is 15, SP is 1FFE
(test-reg RPC #x0015)
(test-reg RSP #x1ffe)
(sim-step) ; RC  ; RC not taken
; Verify that PC is 16, SP is 1FFE
(test-reg RPC #x0016)
(test-reg RSP #x1ffe)
(sim-step) ; RPE  ; RPE not taken
; Verify that PC is 17, SP is 1FFE
(test-reg RPC #x0017)
(test-reg RSP #x1ffe)
(sim-step) ; RM  ; RM not taken
; Verify that PC is 18, SP is 1FFE
(test-reg RPC #x0018)
(test-reg RSP #x1ffe)
; Normal return
(sim-step) ; RET
; Verify that PC is 121, SP is 2000
(test-reg RPC #x0121)
(test-reg RSP #x2000)
;
; Set flags Z&P
(sim-step) ; XRA A
(test-reg RA #x00)
(test-reg RPSW #x46) ; Z&P flags set
(sim-step) ; CNZ FF  ;  Verify CNZ not taken
; Verify that PC is 125, SP is 2000
(test-reg RPC #x0125)
(test-reg RSP #x2000)
(sim-step) ; CNC 10  ; Verify CNC taken
; Verify that PC is 10, SP is 1FFE
(test-reg RPC #x0010)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2801)
(sim-step) ; RNZ  ; Verify RNZ not taken
; Verify that PC is 11, SP is 1FFE
(test-reg RPC #x0011)
(test-reg RSP #x1ffe)
(sim-step) ; RNC  ; Verify RNC taken
; Verify that PC is 128, SP is 2000
(test-reg RPC #x0128)
(test-reg RSP #x2000)
(sim-step) ; CPO FF  ; Verify that CPO not taken
; Verify that PC is 12B, SP is 2000
(test-reg RPC #x012b)
(test-reg RSP #x2000)
(sim-step)  ; CP 12  ; Verify that CP taken
; Verify that PC is 12, SP is 1FFE
(test-reg RPC #x0012)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2e01)
(sim-step) ; RPO  ; Verify that RPO not taken
; Verify that PC is 13, SP is 1FFE
(test-reg RPC #x0013)
(test-reg RSP #x1ffe)
(sim-step) ; RP  ; Verify that RP taken
; Verify that PC is 12E, SP is 2000
(test-reg RPC #x012e)
(test-reg RSP #x2000)
(sim-step) ; CZ 14  ; Verify that CZ taken
; Verify that PC is 14, SP is 1FFE
(test-reg RPC #x0014)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x3101)
(sim-step) ; RZ  ; Verify that RZ taken
; Verify that PC is 131, SP is 2000
(test-reg RPC #x0131)
(test-reg RSP #x2000)
(sim-step) ; CC FF  ; Verify that CC not taken
; Verify that PC is 134, SP is 2000
(test-reg RPC #x0134)
(test-reg RSP #x2000)
(sim-step) ; CPE 15  ; Verify that CPE taken
; Verify that PC is 15, SP is 1FFE
(test-reg RPC #x0015)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x3701)
(sim-step) ; RC  ; Verify that RC not taken
; Verify that PC is 16, SP is 1FFE
(test-reg RPC #x0016)
(test-reg RSP #x1ffe)
(sim-step) ; RPE  ; Verify that RPE taken
; Verify that PC is 137, SP is 2000
(test-reg RPC #x0137)
(test-reg RSP #x2000)
(sim-step) ; CM FF  ; Verify that CM not taken
; Verify that PC is 13A, SP is 2000
(test-reg RPC #x013a)
(test-reg RSP #x2000)
; Call to verify RM not taken
(sim-step) ; CALL 17
(test-reg RPC #x0017)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x3d01)
(sim-step) ; RM
; Verify that PC is 18, SP is 1FFE
(test-reg RPC #x0018)
(test-reg RSP #x1ffe)
(sim-step) ; RET
; Verify that PC is 13D, SP is 2000
(test-reg RPC #x013d)
(test-reg RSP #x2000)
; Set flags S&C
(sim-step) ; MVI A FF
(test-reg RA #xff)
(test-reg RPSW #x46)
(sim-step) ; ADD A
(test-reg RA #xfe)
; Verify flags S&C set
(test-reg RPSW #x93)
;
(sim-step) ; CNZ 10  ; Verify CNZ taken
; Verify PC is 10, SP is 1FFE
(test-reg RPC #x0010)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x4301)
(sim-step) ; RNZ  ; Verify RNZ taken
; Verify PC is 143, SP is 2000
(test-reg RPC #x0143)
(test-reg RSP #x2000)
(sim-step) ; CNC FF  ; Verify CNC not taken
; Verify PC is 146, SP is 2000
(test-reg RPC #x0146)
(test-reg RSP #x2000)
(sim-step) ; CPO 11  ; Verify CPO taken
; Verify PC is 11, SP is 1FFE
(test-reg RPC #x0011)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x4901)
(sim-step) ; RNC  ; Verify RNC not taken
; Verify PC is 12, SP is 1FFE
(test-reg RPC #x0012)
(test-reg RSP #x1ffe)
(sim-step) ; RPO  ; Verify RPO taken
; Verify PC is 149, SP is 2000
(test-reg RPC #x0149)
(test-reg RSP #x2000)
(sim-step) ; CP FF  ; Verify CP not taken
; Verify PC is 14C, SP is 2000
(test-reg RPC #x014c)
(test-reg RSP #x2000)
(sim-step) ; CZ FF  ; Verify CZ not taken
; Verify PC is 14F, SP is 2000
(test-reg RPC #x014f)
(test-reg RSP #x2000)
(sim-step) ; CC 13  ; Verify CC taken
; Verify PC is 13, SP is 1FFE
(test-reg RPC #x0013)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x5201)
(sim-step) ; RP  ; Verify RP not taken
; Verify PC is 14, SP is 1FFE
(test-reg RPC #x0014)
(test-reg RSP #x1ffe)
(sim-step) ; RZ  ; Verify RZ not taken
; Verify PC is 15, SP is 1FFE
(test-reg RPC #x0015)
(test-reg RSP #x1ffe)
(sim-step) ; RC  ; Verify RC taken
; Verify PC is 152, SP is 2000
(test-reg RPC #x0152)
(test-reg RSP #x2000)
(sim-step) ; CPE 15  ; Verify CPE not taken
; Verify PC is 155, SP is 2000
(test-reg RPC #x0155)
(test-reg RSP #x2000)
(sim-step) ; CM 16  ; Verify CM taken
; Verify PC is 16, SP is 1FFE
(test-reg RPC #x0016)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x5801)
(sim-step) ; RPE  ; Verify RPE not taken
; Verify PC is 17, SP is 1FFE
(test-reg RPC #x0017)
(test-reg RSP #x1ffe)
(sim-step) ; RM  ; Verify RM taken
; Verify PC is 158, SP is 2000
(test-reg RPC #x0158)
(test-reg RSP #x2000)
;-------------------------------------------------------------------------------
;  Test CALL and RET instructions
;
; Load memory
;
;  Initialize register pairs
(memb #x0100 #x01) ; LXI B,5678
(memw #x0101 #x7856)
(memb #x0103 #x11) ; LXI D,9ABC
(memw #x0104 #xbc9a)
(memb #x0106 #x21) ; LXI H,DEF0
(memw #x0107 #xf0de)
(memb #x0109 #x31) ; LXI SP,2000
(memw #x010a #x0020)
(memb #x010c #x09) ; DAD B
(memb #x010d #x19) ; DAD D
(memb #x010e #x29) ; DAD H
(memb #x010f #x39) ; DAD SP
;
;  Execute test
;
(print "==> Testing CALL-RET instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LXI B,5678
(test-reg RBC #x5678)
(sim-step) ; LXI D,9ABC
(test-reg RDE #x9abc)
(sim-step) ; LXI H,DEF0
(test-reg RHL #xdef0)
(sim-step) ; LXI SP,2000
(test-reg RSP #x2000)
(sim-step) ; DAD B  ;  Verify DAD B
; Verify that HL is 3568 and Carry is set
(test-reg RHL #x3568)
(test-reg RPSW #x03)
(sim-step) ; DAD D  ;  Verify DAD D
; Verify that HL is D024 and Carry is clear
(test-reg RHL #xd024)
(test-reg RPSW #x02)
(sim-step) ; DAD H  ;  Verify DAD H
; Verify that HL is A048 and Carry is set
(test-reg RHL #xa048)
(test-reg RPSW #x03)
(sim-step) ; DAD SP  ;  Verify DAD SP
; Verify that HL is C048 and Carry is clear
(test-reg RHL #xc048)
(test-reg RPSW #x02)
;-------------------------------------------------------------------------------
;  Test CMP instructions
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
;  Test CMP x instructions
(memb #x0110 #xb8) ; CMP B
(memb #x0111 #xb9) ; CMP C
(memb #x0112 #xba) ; CMP D
(memb #x0113 #xbb) ; CMP E
(memb #x0114 #xbc) ; CMP H
(memb #x0115 #xbd) ; CMP L
(memb #x0116 #xbe) ; CMP M (address in HL (12A), contents 0E)
(memb #x0117 #xbf) ; CMP A
;
;  Execute test
;
(print "==> Testing CMP instructions")
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
(sim-step) ; CMP B  ; Verify CMP B
; Verify that A is 34, PC is 111, A,P,&C flags are set
(test-reg RA #x34)
(test-reg RPC #x0111)
(test-mask #x15 #xfd)
(sim-step) ; CMP C  ; Verify CMP C
; Verify that A is 34, PC is 112, S,A,P,&C flags are set
(test-reg RA #x34)
(test-reg RPC #x0112)
(test-mask #x95 #xfd)
(sim-step) ; CMP D  ; Verify CMP D
; Verify that A is 34, PC is 113, A&C flags are set
(test-reg RA #x34)
(test-reg RPC #x0113)
(test-mask #x11 #xfd)
(sim-step) ;CMP E  ; Verify CMP E
; Verify that A is 34, PC is 114, A&C flags are set
(test-reg RA #x34)
(test-reg RPC #x0114)
(test-mask #x11 #xfd)
(sim-step) ; CMP H  ; Verify CMP H
; Verify that A is 34, PC is 115, P flag is set
(test-reg RA #x34)
(test-reg RPC #x0115)
(test-mask #x04 #xfd)
(sim-step) ; CMP L  ; Verify CMP L
; Verify that A is 34, PC is 116, P flag is set
(test-reg RA #x34)
(test-reg RPC #x0116)
(test-mask #x04 #xfd)
(sim-step) ; CMP M  ; Verify CMP M
; Verify that A is 34, PC is 117, P flag is set
(test-reg RA #x34)
(test-reg RPC #x0117)
(test-mask #x04 #xfd)
(sim-step) ; CMP A  ; Verify CMP A
; Verify that A is 34, PC is 118, Z&P flags are set
(test-reg RA #x34)
(test-reg RPC #x0118)
(test-mask #x44 #xfd)
;-------------------------------------------------------------------------------
;  Test INR-DCR instructions
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
(memb #x0110 #x04) ; INR B
(memb #x0111 #x0c) ; INR C
(memb #x0112 #x14) ; INR D
(memb #x0113 #x1c) ; INR E
(memb #x0114 #x24) ; INR H
(memb #x0115 #x2c) ; INR L
(memb #x0116 #x21) ; LXI H,1001
(memw #x0117 #x0110)
(memb #x0119 #x34) ; INR M
(memb #x011a #x3c) ; INR A
(memb #x011b #x05) ; DCR B
(memb #x011c #x0d) ; DCR C
(memb #x011d #x15) ; DCR D
(memb #x011e #x1d) ; DCR E
(memb #x011f #x25) ; DCR H
(memb #x0120 #x2d) ; DCR L
(memb #x0121 #x21) ; LXI H,1001
(memw #x0122 #x0110)
(memb #x0124 #x35) ; DCR M
(memb #x0125 #x3d) ; DCR A
;
;  Execute test
;
(print "==> Testing INR-DCR instructions")
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
(sim-step) ; INR B  ;  Verify INR B
; Verify that B is DF, flags are S
(test-reg RB #xdf)
(test-mask #x80 #xfd)
(sim-step) ; INR C  ;  Verify INR C
; Verify that C is AE, flags are S
(test-reg RC #xae)
(test-mask #x80 #xfd)
(sim-step) ; INR D  ;  Verify INR D
; Verify that D is BF, flags are S
(test-reg RD #xbf)
(test-mask #x80 #xfd)
(sim-step) ; INR E  ;  Verify INR E
; Verify that E is F0, flags are S,A,P
(test-reg RE #xf0)
(test-mask #x94 #xfd)
(sim-step) ; INH H  ;  Verify INR H
; Verify that H is 11, flags are P
(test-reg RH #x11)
(test-mask #x04 #xfd)
(sim-step) ; INR L  ;  Verify INR L
; Verify that L is 2, flags are all clear
(test-reg RL #x02)
(test-mask #x00 #xfd)
(sim-step) ; LXI H,1001  ; Restore HL
(test-reg RHL #x1001)
(sim-step) ; INR M ;  Verify INR M
; Verify that location 1001 is 13, flags are clear
(test-memb #x1001 #x13)
(test-mask #x00 #xfd)
(sim-step) ; INR A  ;  Verify INR A
; Verify that A is 35, flags are P
(test-reg RA #x35)
(test-mask #x04 #xfd)
(sim-step) ; DCR B  ;  Verify DCR B
; Verify that B is DE, flags are S&P
(test-reg RB #xde)
(test-mask #x84 #xfd)
(sim-step) ; DCR C  ;  Verify DCR C
; Verify that C is AD, flags are S
(test-reg RC #xad)
(test-mask #x80 #xfd)
(sim-step) ; DCR D  ;  Verify DCR D
; Verify that D is BE, flags are S&P
(test-reg RD #xbe)
(test-mask #x84 #xfd)
(sim-step) ; DCR E  ;  Verify DCR E
; Verify that E is EF, flags are S&A
(test-reg RE #xef)
(test-mask #x90 #xfd)
(sim-step) ; DCR H  ;  Verify DCR H
; Verify that H is 0F, flags are A&P
(test-reg RH #x0f)
(test-mask #x14 #xfd)
(sim-step) ; DCR L  ;  Verify DCR L
; Verify that L is 00, flags are Z&P
(test-reg RL #x00)
(test-mask #x44 #xfd)
(sim-step) ; LXI H,1001  ; Restore HL
(test-reg RHL #x1001)
(sim-step) ; DCR M  ;  Verify DCR M
; Verify that location 1001 is 12, flags are P
(test-memb #x1001 #x12)
(test-mask #x04 #xfd)
(sim-step) ; DCR A  ;  Verify DCR A
; Verify that A is 34, flags are clear
(test-reg RA #x34)
(test-mask #x00 #xfd)
;-------------------------------------------------------------------------------
;  Test INX-DCX instructions
;
; Load memory
;
(memb #x0100 #x01) ; LXI B,5678
(memw #x0101 #x7856)
(memb #x0103 #x11) ; LXI D,9ABC
(memw #x0104 #xbc9a)
(memb #x0106 #x21) ; LXI H,DEF0
(memw #x0107 #xf0de)
(memb #x0109 #x31) ; LXI SP,2000
(memw #x010a #x0020)
;
(memb #x010c #x03) ; INX B
(memb #x010d #x13) ; INX D
(memb #x010e #x23) ; INX H
(memb #x010f #x33) ; INX SP
(memb #x0110 #x0b) ; DCX B
(memb #x0111 #x1b) ; DCX D
(memb #x0112 #x2b) ; DCX H
(memb #x0113 #x3b) ; DCX SP
;
;  Execute test
;
(print "==> Testing INX-DCX instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LXI B,5678
(test-reg RBC #x5678)
(sim-step) ; LXI D,9ABC
(test-reg RDE #x9abc)
(sim-step) ; LXI H,DEF0
(test-reg RHL #xdef0)
(sim-step) ; LXI SP,2000
(test-reg RSP #x2000)
;
(sim-step) ; INX B  ;  Verify INX B
; Verify that BC contains 5679
(test-reg RBC #x5679)
(sim-step) ; INX D  ;  Verify INX D
; Verify that DE contains 9ABD
(test-reg RDE #x9abd)
(sim-step) ; INX H  ;  Verify INX H
; Verify that HL contains DEF1
(test-reg RHL #xdef1)
(sim-step) ; INX SP  ;  Verify INX SP
; Verify that SP contains 2001
(test-reg RSP #x2001)
(sim-step) ; DCX B  ;  Verify DCX B
; Verify that BC contains 5678
(test-reg RBC #x5678)
(sim-step) ; DCX D  ;  Verify DCX D
; Verify that DE contains 9ABC
(test-reg RDE #x9abc)
(sim-step) ; DCX H  ;  Verify DCX H
; Verify that HL contains DEF0
(test-reg RHL #xdef0)
(sim-step) ; DCX SP  ;  Verify DCX SP
; Verify that SP contains 2000
(test-reg RSP #x2000)
;-------------------------------------------------------------------------------
;  Test JMP instructions
;
; Load memory
;
; Jump  Taken  Not-Taken
; JNZ     V       V
; JZ      V       V
; JNC     V       V
; JC      V       V
; JPO     V       V
; JPE     V       V
; JP      V       V
; JM      V       V

(memb #x00ff #x76) ; HLT
(memb #x0100 #xc3) ; JMP 104
(memw #x0101 #x0401)
(memb #x0103 #x76) ; HLT
(memb #x0104 #xad) ; XRA A (Z&P flags set)
(memb #x0105 #xc2) ; JNZ FF
(memw #x0106 #xff00)
(memb #x0108 #xe2) ; JPO FF
(memw #x0109 #xff00)
(memb #x010b #xda) ; JC FF
(memw #x010c #xff00)
(memb #x010e #xfa) ; JM FF
(memw #x010f #xff00)
(memb #x0111 #xca) ; JZ 115
(memw #x0112 #x1501)
(memb #x0114 #x76) ; HLT
(memb #x0115 #xea) ; JPE 119
(memw #x0116 #x1901)
(memb #x0118 #x76) ; HLT
(memb #x0119 #xf2) ; JP 11D
(memw #x011a #x1d01)
(memb #x011c #x76) ; HLT
(memb #x011d #xd2) ; JNC 121
(memw #x011e #x2101)
(memb #x0120 #x76) ; HLT
(memb #x0121 #x3c) ; INR A (No flags set)
(memb #x0122 #xca) ; JZ FF
(memw #x0123 #xff00)
(memb #x0125 #xda) ; JC FF
(memw #x0126 #xff00)
(memb #x0128 #xea) ; JPE FF
(memw #x0129 #xff00)
(memb #x012b #xfa) ; JM FF
(memw #x012c #xff00)
(memb #x012e #xc2) ; JNZ 132
(memw #x012f #x3201)
(memb #x0131 #x76) ; HLT
(memb #x0132 #xd2) ; JNC 136
(memw #x0133 #x3601)
(memb #x0135 #x76) ; HLT
(memb #x0136 #xe2) ; JPO 13A
(memw #x0137 #x3a01)
(memb #x0139 #x76) ; HLT
(memb #x013a #xf2) ; JP 13E
(memw #x013b #x3e01)
(memb #x013d #x76) ; HLT
(memw #x013e #x3e80) ; MVI A,80
(memb #x0140 #xb7) ; ORA A (S flag should be only one set)
(memb #x0141 #xca) ; JZ FF
(memw #x0142 #xff00)
(memb #x0144 #xda) ; JC FF
(memw #x0145 #xff00)
(memb #x0147 #xea) ; JPE FF
(memb #x0148 #xff00)
(memb #x014a #xf2) ; JP FF
(memw #x014b #xff00)
(memb #x014d #xc2) ; JNZ 151
(memw #x014e #x5101)
(memb #x0150 #x76) ; HLT
(memb #x0151 #xd2) ; JNC 155
(memw #x0152 #x5501)
(memb #x0154 #x76) ; HLT
(memb #x0155 #xe2) ; JPO 159
(memw #x0156 #x5901)
(memb #x0158 #x76) ; HLT
(memb #x0159 #xfa) ; JM 15D
(memw #x015a #x5d01)
(memb #x015c #x76) ; HLT
(memb #x015d #x87) ; ADD A (Z,P,&C flags set)
(memb #x015e #xc2) ; JNZ FF
(memw #x015f #xff00)
(memb #x0161 #xe2) ; JPO FF
(memw #x0162 #xff00)
(memb #x0164 #xd2) ; JNC FF
(memw #x0165 #xff00)
(memb #x0167 #xfa) ; JM FF
(memw #x0168 #xff00)
(memb #x016a #xca) ; JZ 16D
(memw #x016b #x6e01)
(memb #x016d #x76) ; HLT
(memb #x016e #xea) ; JPE 172
(memw #x016f #x7201)
(memb #x0171 #x76) ; HLT
(memb #x0172 #xf2) ; JP 176
(memw #x0173 #x7601)
(memb #x0175 #x76) ; HLT
(memb #x0176 #xda) ; JC 17A
(memw #x0177 #x7a01)
(memb #x0179 #x76) ; HLT
;
;  Execute test
;
(print "==> Testing JMP instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; JMP 104  ; Verify JMP
(test-reg RPC #x0104) ; Verify PC is 104
(sim-step)  ; XRA A  ; Change flags (Z&P set)
(test-mask #x44 #xfd)
(sim-step)  ; JNZ FF  ; Verify JNZ not taken
(test-reg RPC #x0108) ; Verify PC is 108
(sim-step)  ; JPO FF  ; Verify JPE not taken
(test-reg RPC #x010b) ; Verify PC is 10B
(sim-step)  ; JC FF  ; Verify JC not taken
(test-reg RPC #x010e) ; Verify PC is 10E
(sim-step)  ; JM FF  ; Verify JM not taken
(test-reg RPC #x0111) ; Verify PC is 111
(sim-step)  ; JZ 115  ; Verify JZ taken
(test-reg RPC #x0115) ; Verify PC is 115
(sim-step)  ; JPE 119  ; Verify JPE taken
(test-reg RPC #x0119) ; Verify PC is 119
(sim-step)  ; JP 11D  ; Verify JP taken
(test-reg RPC #x011d) ; Verify PC is 11D
(sim-step)  ; JNC 121  ; Verify JNC taken
(test-reg RPC #x0121) ; Verify PC is 121
(sim-step) ; INR A  ; Change flags (none set)
(test-mask #x00 #xfd)
(sim-step)  ; JZ FF  ; Verify JZ not taken
(test-reg RPC #x0125) ; Verify PC is 125
(sim-step)  ; JC FF  ; Verify JC not taken
(test-reg RPC #x0128) ; Verify PC is 128
(sim-step)  ; JPE FF  ; Verify JPE not taken
(test-reg RPC #x012b) ; Verify PC is 12B
(sim-step)  ; JM FF  ; Verify JM not taken
(test-reg RPC #x012e) ; Verify PC is 12E
(sim-step)  ; JNZ 132  ; Verify JNZ taken
(test-reg RPC #x0132) ; Verify PC is 132
(sim-step)  ; JNC 136  ; Verify JNC taken
(test-reg RPC #x0136) ; Verify PC is 136
(sim-step)  ; JPO 13A  ; Verify JPO taken
(test-reg RPC #x013a) ; Verify PC is 13A
(sim-step)  ; JP 13E  ; Verify JP taken
(test-reg RPC #x013e) ; Verify PC is 13E
(sim-step) ; MVI A,80
(test-reg RA #x80)
(sim-step) ; ORA A  ;  Change flags (S set)
(test-mask #x80 #xfd)
(sim-step)  ; JZ FF  ; Verify JZ not taken
(test-reg RPC #x0144) ; Verify PC is 144
(sim-step)  ; JC FF  ; Verify JC not taken
(test-reg RPC #x0147) ; Verify PC is 147
(sim-step)  ; JPE FF  ; Verify JPE not taken
(test-reg RPC #x014a) ; Verify PC is 14A
(sim-step)  ; JP FF  ; Verify JP not taken
(test-reg RPC #x014d) ; Verify PC is 14D
(sim-step)  ; JNZ 151  ; Verify JNZ taken
(test-reg RPC #x0151) ; Verify PC is 151
(sim-step)  ; JNC 155  ; Verify JNC taken
(test-reg RPC #x0155) ; Verify PC is 155
(sim-step)  ; JPO 159  ; Verify JPO taken
(test-reg RPC #x0159) ; Verify PC is 159
(sim-step)  ; JM 15D  ; Verify JM taken
(test-reg RPC #x015d) ; Verify PC is 15D
(sim-step)  ; ADD A  ; Change flags (Z,P,&C set)
(test-mask #x45 #xfd)
(sim-step)  ; JNZ FF  ; Verify JNZ not taken
(test-reg RPC #x0161) ; Verify PC is 161
(sim-step)  ; JPO FF  ; Verify JPO not taken
(test-reg RPC #x0164) ; Verify PC is 164
(sim-step)  ; JNC FF  ; Verify JNC not taken
(test-reg RPC #x0167) ; Verify PC is 167
(sim-step)  ; JM FF  ; Verify JM not taken
(test-reg RPC #x016a) ; Verify PC is 16A
(sim-step)  ; JZ 16D  ; Verify JZ taken
(test-reg RPC #x016e) ; Verify PC is 16E
(sim-step)  ; JPE 172  ; Verify JPE taken
(test-reg RPC #x0172) ; Verify PC is 172
(sim-step)  ; JP 176  ; Verify JP taken
(test-reg RPC #x0176) ; Verify PC is 176
(sim-step)  ; JC 17A  ; Verify JC taken
(test-reg RPC #x017a) ; Verify PC is 17A
;-------------------------------------------------------------------------------
;  Test LDAX-STAX instructions
;
; Load memory
;
(memb #x0100 #x01) ; LXI B,2020
(memw #x0101 #x2020)
(memb #x0103 #x11) ; LXI D,2021
(memw #x0104 #x2120)
(memw #x0106 #x3e34) ; MVI A,34
(memb #x0108 #x02) ; STAX B
(memw #x0109 #x3e56) ; MVI A,56
(memb #x010b #x12) ; STAX D
(memb #x010c #x0a) ; LDAX B
(memb #x010d #x1a) ; LDAX D
;
;  Execute test
;
(print "==> Testing JMP instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LXI B,2020
(test-reg RBC #x2020)
(sim-step)  ; LXI D,2021
(test-reg RDE #x2021)
(sim-step) ; MVI A,34
(test-reg RA #x34)
(sim-step) ; STAX B
(test-memb #x2020 #x34) ; Verify that 34 is in memory location 2020
(test-reg RPC #x0109) ; Verify that PC is 109
(sim-step) ; MVI A,56
(test-reg RA #x56)
(sim-step) ; STAX D
(test-memb #x2021 #x56) ; Verify that 56 is in memory location 2021
(test-reg RPC #x010c) ; Verify that PC is 10C
(sim-step) ; LDAX B
(test-reg RA #x34) ; Verify that register A is 34 and PC is 10D
(test-reg RPC #x010d)
(sim-step) ; LDAX D
(test-reg RA #x56) ; Verify that register A is 56 and PC is 10E
(test-reg RPC #x010e)
;-------------------------------------------------------------------------------
;  Test LXI instructions
;
; Load memory
;
(memb #x0100 #x01) ; LXI B,5678
(memw #x0101 #x7856)
(memb #x0103 #x11) ; LXI D,9ABC
(memw #x0104 #xbc9a)
(memb #x0106 #x21) ; LXI H,DEF0
(memw #x0107 #xf0de)
(memb #x0109 #x31) ; LXI SP,2000
(memw #x010a #x0020)
;
;  Execute test
;
(print "==> Testing LXI instructions")
(terpri)
(sim-init)
(test-reg RBC #x0000)
(test-reg RDE #x0000)
(test-reg RHL #x0000)
(test-reg RSP #x0000)
(test-reg RPSW #x02)
(test-reg RA #x00)
(go #x0100)
(test-reg RPC #x0100)
(sim-step) ; Verify LXI B,5678
; Verify that BC is 5678, PC is 103, and other registers unchanged
(test-reg RBC #x5678)
(test-reg RDE #x0000)
(test-reg RHL #x0000)
(test-reg RSP #x0000)
(test-reg RPSW #x02)
(test-reg RA #x00)
(test-reg RPC #x0103)
(sim-step) ; Verify LXI D,5678
; Verify that DE is 9ABC, PC is 106, and other registers unchanged
(test-reg RBC #x5678)
(test-reg RDE #x9abc)
(test-reg RHL #x0000)
(test-reg RSP #x0000)
(test-reg RPSW #x02)
(test-reg RA #x00)
(test-reg RPC #x0106)
(sim-step) ; Verify LXI H,DEF0
; Verify that HL is DEF0, PC is 109, and other registers unchanged
(test-reg RBC #x5678)
(test-reg RDE #x9abc)
(test-reg RHL #xdef0)
(test-reg RSP #x0000)
(test-reg RPSW #x02)
(test-reg RA #x00)
(test-reg RPC #x0109)
(sim-step) ; Verify LXI SP,2000
; Verify that SP is 2000, PC is 10C, and other registers unchanged
(test-reg RBC #x5678)
(test-reg RDE #x9abc)
(test-reg RHL #xdef0)
(test-reg RSP #x2000)
(test-reg RPSW #x02)
(test-reg RA #x00)
(test-reg RPC #x010c)
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
