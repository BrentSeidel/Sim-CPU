;
;  Lisp test cases for Z-80 variant of the 8080 simulator
(sim-cpu "Z80")
(trace 1)
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
(setq RAP 13)  ; Extra Z-80 registers start here
(setq RPSWP 14)
(setq RBP 15)
(setq RCP 16)
(setq RBCP 17)
(setq RDP 18)
(setq REP 19)
(setq RDEP 20)
(setq DHP 21)
(setq RLP 22)
(setq RHLP 23)
(setq RIX 24)
(setq RIY 25)
(setq RI 26)
(setq RM 27)
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
;  PSW mask for 8080/8085 is D5, for Z-80 is D7
(setq MPSW #xD7)
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
;  Check last output
;
(defun test-out (addr data)
  (print "Output port ")
  (print-hex (last-out-addr))
  (print " expected ")
  (print-hex addr)
  (if (= (last-out-addr) addr)
    (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print " PASS"))
    (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print " *** FAIL ***")))
  (terpri)
  (print "Output data ")
  (print-hex (last-out-data))
  (print " expected ")
  (print-hex data)
  (if (= (last-out-data) data)
    (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print " PASS"))
    (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print " *** FAIL ***")))
  (terpri))
;
;  Check for interrupts disabled or enabled
;
(defun test-int-dis ()
   (if (= (int-state) 0)
      (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print "Interrupts not Enabled - PASS"))
      (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print "Interrupts Enabled - *** FAIL ***"))))
;
(defun test-int-en ()
   (if (= (int-state) 0)
      (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print "Interrupts not Enabled - *** FAIL ***"))
      (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print "Interrupts Enabled - PASS"))))
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
(memw #x0100 #x06de) ; LD B,DE
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3612) ; LD M,12
(memw #x010e #x3e34) ; LD A,34
;
;  Test ADD x instructions
(memb #x0110 #x80) ; ADD A,B
(memb #x0111 #x81) ; ADD A,C
(memb #x0112 #x82) ; ADD A,D
(memb #x0113 #x83) ; ADD A,E
(memb #x0114 #x84) ; ADD A,H
(memb #x0115 #x85) ; ADD A,L
(memb #x0116 #x86) ; ADD A,(HL) (address in HL (12A), contents 0E)
(memb #x0117 #x87) ; ADD A,A
;
;  Test ADC x instructions
(memb #x0118 #x88) ; ADC A,B
(memb #x0119 #x89) ; ADC A,C
(memb #x011a #x8a) ; ADC A,D
(memb #x011b #x8b) ; ADC A,E
(memb #x011c #x8c) ; ADC A,H
(memb #x011d #x8d) ; ADC A,L
(memb #x011e #x8e) ; ADC A,(HL) (address in HL (12A), contents 0E)
(memb #x011f #x8f) ; ADC A,A
;
;  Execute test
;
(print "==> Testing ADD/ADC instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,DE
(test-reg RB #xde)
(sim-step) ; LD C,AD
(test-reg RC #xad)
(sim-step) ; LD D,BE
(test-reg RD #xbe)
(sim-step) ; LD E,EF
(test-reg RE #xef)
(sim-step) ; LD H,10
(test-reg RH #x10)
(sim-step) ; LD L,01
(test-reg RL #x01)
(sim-step) ; LD M,12
(test-memb #x1001 #x12)
(sim-step) ; LD A,34
(test-reg RA #x34)
;
(print "Testing ADD instructions")
(terpri)
(sim-step) ; Verify ADD A,B
; Verify that A is 12, PC is 111, A&C flags are set
(test-reg RA #x12)
(test-reg RPC #x0111)
(test-mask #x11 MPSW)
(sim-step) ; Verify ADD A,C
; Verify that A is BF, PC is 112, S flag is set
(test-reg RA #xbf)
(test-reg RPC #x0112)
(test-mask #x80 MPSW)
(sim-step) ; Verify ADD A,D
; Verify that A is 7D, PC is 113, A,P,&C flags are set
(test-reg RA #x7d)
(test-reg RPC #x113)
(test-mask #x15 MPSW)
(sim-step) ; Verify ADD A,E
; Verify that A is 6C, PC is 114, A&C flags are set
(test-reg RA #x6c)
(test-reg RPC #x114)
(test-mask #x11 MPSW)
(sim-step) ; Verify ADD A,H
; Verify that A is 7C, PC is 115, no flags are set
(test-reg RA #x7c)
(test-reg RPC #x115)
(test-mask #x00 MPSW)
(sim-step) ; Verify ADD A,L
; Verify that A is 7D, PC is 116, no flags are set
(test-reg RA #x7d)
(test-reg RPC #x116)
(test-mask #x00 MPSW)
(sim-step) ; Verify ADD A,(HL)
; Verify that A is 8F, PC is 117, S&P flags are set
(test-reg RA #x8f)
(test-reg RPC #x117)
(test-mask #x84 MPSW)
(sim-step) ; Verify ADD A,A
; Verify that A is 1E, PC is 118, A,P,&C flags are set
(test-reg RA #x1e)
(test-reg RPC #x118)
(test-mask #x15 MPSW)
;
(print "Verifying ADC instructions")
(terpri)
(sim-step) ; Verify ADC A,B
; Verify that A is FD, PC is 119, S&A flags are set
(test-reg RA #xfd)
(test-reg RPC #x119)
(test-mask #x90 MPSW)
(sim-step) ; Verify ADC A,C
; Verify that A is AA, PC is 11A, S,A,&C flags are set
(test-reg RA #xaa)
(test-reg RPC #x11a)
(test-mask #x91 MPSW)
(sim-step) ; Verify ADC A,D
; Verify that A is 69, PC is 11B, A,P,&C flags are set
(test-reg RA #x69)
(test-reg RPC #x11b)
(test-mask #x15 MPSW)
(sim-step) ; Verify ADC A,E
; Verify that A is 59, PC is 11C, A&C flags are set
(test-reg RA #x59)
(test-reg RPC #x11c)
(test-mask #x11 MPSW)
(sim-step) ; Verify ADC A,H
; Verify that A is 6A, PC is 11D, no flags are set
(test-reg RA #x6a)
(test-reg RPC #x11d)
(test-mask #x00 MPSW)
(sim-step) ; Verify ADC A,L
; Verify that A is 6B, PC is 11E, no flags are set
(test-reg RA #x6b) ; Actually d4
(test-reg RPC #x11e)
(test-mask #x00 MPSW)
(sim-step) ; Verify ADC A,(HL)
; Verify that A is 7D, PC is 11F, no flags are set
(test-reg RA #x7d)
(test-reg RPC #x11f)
(test-mask #x00 MPSW)
(sim-step) ; Verify ADC A,A
; Verify that A is FA, PC is 120, S,A,&P flags are set
(test-reg RA #xfa)
(test-reg RPC #x120)
(test-mask #x94 MPSW)
;-------------------------------------------------------------------------------
;  Test ALU Immediate instructions
;
; Load memory
;
(memw #x0100 #x06de) ; LD B,DE
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3612) ; LD (HL),12
(memw #x010e #x3e34) ; LD A,34
;
;  Test immediate instructions
(memw #x0110 #xc60f) ; ADD A,0F
(memw #x0112 #xce88) ; ADC A,88
(memw #x0114 #xd6d2) ; SUB A,D2
(memw #x0116 #xde22) ; SBC A,22
(memw #x0118 #xe6f0) ; AND A,F0
(memw #x011a #xeeff) ; XOR A,FF
(memw #x011c #xf6d0) ; OR A,F0
(memw #x011e #xfe00) ; CP A,00
;
;  Execute test
;
(print "==> Testing ALU Immediate instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,DE
(test-reg RB #xde)
(sim-step) ; LD C,AD
(test-reg RC #xad)
(sim-step) ; LD D,BE
(test-reg RD #xbe)
(sim-step) ; LD E,EF
(test-reg RE #xef)
(sim-step) ; LD H,10
(test-reg RH #x10)
(sim-step) ; LD L,01
(test-reg RL #x01)
(sim-step) ; LD (HL),12
(test-memb #x1001 #x12)
(sim-step) ; LD A,34
(test-reg RA #x34)
;
(sim-step) ;  Verify ADD A,0F
; Verify that A is 43, A flag is set, and PC is 112
(test-reg RA #x43)
(test-reg RPC #x112)
(test-mask #x10 MPSW)
(sim-step) ;  Verify ADC A,88
; Verify that A is CB, S flag is set, and PC is 114
(test-reg RA #xcb)
(test-reg RPC #x114)
(test-mask #x80 MPSW)
(sim-step) ;  Verify SUB A,D2
; Verify that A is F9, flags S,N,&C are set, and PC is 116
(test-reg RA #xf9)
(test-reg RPC #x116)
(test-mask #x83 MPSW)
(sim-step) ;  Verify SBC A,22
; Verify that A is D6, flag S is set, and PC is 118
(test-reg RA #xd6)
(test-reg RPC #x118)
(test-mask #x82 MPSW)
(sim-step) ;  Verify AND A,F0
; Verify that A is C0, flag S is set, and PC is 11A
(test-reg RA #xd0)
(test-reg RPC #x11a)
(test-mask #x80 MPSW)
(sim-step) ; Verify XOR A,FF
; Verify that A is 2F and PC is 11C
(test-reg RA #x2f)
(test-reg RPC #x11c)
(test-mask #x00 MPSW)
(sim-step) ; Verify OR A,F0
; Verify that A is FF, flags S&P are set, and PC is 11E
(test-reg RA #xff)
(test-reg RPC #x11e)
(test-mask #x84 MPSW)
(sim-step) ; Verify CP A,00
; Verify that A is unchanged, flags S&N are set, PC is 120
(test-reg RA #xff)
(test-reg RPC #x120)
(test-mask #x82 MPSW)
;-------------------------------------------------------------------------------
;  Test AND A,r instructions
;
; Load memory
;
(memw #x0100 #x06de) ; LD B,DE
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3612) ; LD (HL),12
(memw #x010e #x3e34) ; LD A,34
;  Test ANA instructions
(memb #x0110 #xa0) ; AND A,B
(memb #x0111 #xa1) ; AND A,C
(memb #x0112 #x2f) ; CPL
(memb #x0113 #xa2) ; AND A,D
(memb #x0114 #xa3) ; AND A,E
(memb #x0115 #xa4) ; AND A,H
(memb #x0116 #x2f) ; CPL
(memb #x0117 #xa5) ; AND A,L
(memb #x0118 #x2f) ; CPL
(memb #x0119 #xa6) ; AND A,(HL) (Address 1001, contents 12)
(memb #x011a #xa7) ; AND A,A (effectively a NOP)
;
;  Execute test
;
(print "==> Testing AND A,r instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,DE
(test-reg RB #xde)
(sim-step) ; LD C,AD
(test-reg RC #xad)
(sim-step) ; LD D,BE
(test-reg RD #xbe)
(sim-step) ; LD E,EF
(test-reg RE #xef)
(sim-step) ; LD H,10
(test-reg RH #x10)
(sim-step) ; LD L,01
(test-reg RL #x01)
(sim-step) ; LD (HL),12
(test-memb #x1001 #x12)
(sim-step) ; LD A,34
(test-reg RA #x34)
;
(sim-step) ; Verify AND A,B
; Verify that register A is 14, flag P is set, and PC is 111
(test-reg RA #x14)
(test-reg RPC #x111)
(test-mask #x04 MPSW)
(sim-step); Verify AND A,C
; Verify that register A is 04, no flags are set, and PC is 112
(test-reg RA #x04)
(test-reg RPC #x112)
(test-mask #x00 MPSW)
(sim-step); Complement A
(test-reg RA #xFB)
(sim-step) ; Verify AND A,D
; Verify that register A is BA, S flag is set, and PC is 114
(test-reg RA #xba)
(test-reg RPC #x114)
(test-mask #x80 MPSW)
(sim-step) ; Verify AND A,E
; Verify that register A is AA, S&P flags are set, and PC is 115
(test-reg RA #xaa)
(test-reg RPC #x115)
(test-mask #x84 MPSW)
(sim-step) ; Verify AND A,H
; Verify that register A is 00, Z&P flags are set, and PC is 116
(test-reg RA #x00)
(test-reg RPC #x116)
(test-mask #x44 MPSW)
(sim-step) ; Complement A
(test-reg RA #xff)
(sim-step) ; Verify AND A,L
; Verify that register A is 01, all flags clear, and PC is 118
(test-reg RA #x01)
(test-reg RPC #x118)
(test-mask #x00 MPSW)
(sim-step) ; Complement A
(test-reg RA #xFE)
(sim-step) ; Verify AND A,(HL)
; Verify that register A is 12, P flag is set, and PC is 11A
(test-reg RA #x12)
(test-reg RPC #x11a)
(test-mask #x04 MPSW)
(sim-step) ; Verify AND A,A
; Verify that registers are unchanged, and PC is 11B
(test-reg RA #x12)
(test-reg RPC #x11b)
(test-mask #x04 MPSW)
;-------------------------------------------------------------------------------
;  Test CALL and RET instructions
;
; Load memory
;
;  Returns
(memb #x0010 #xc0) ; RET NZ
(memb #x0011 #xd0) ; RET NC
(memb #x0012 #xe0) ; RET PO
(memb #x0013 #xf0) ; RET P
(memb #x0014 #xc8) ; RET Z
(memb #x0015 #xd8) ; RET C
(memb #x0016 #xe8) ; RET PE
(memb #x0017 #xf8) ; RET M
(memb #x0018 #xc9) ; RET
; Target for branches that should not be taken
(memb #x00ff #x76) ; HLT
; Initialize SP to 2000
(memb #x0100 #x31) ; LD SP,2000
(memw #x0101 #x0020)
; Ordinary call and return
(memb #x0103 #xcd) ; CALL 18
(memw #x0104 #x1800)
;  Test conditional CALLs and RETs
; No flags set
(memb #x0106 #xc4) ; CALL NZ,10
(memw #x0107 #x1000)
(memb #x0109 #xd4) ; CALL NC,11
(memw #x010a #x1100)
(memb #x010c #xe4) ; CALL PO,12
(memw #x010d #x1200)
(memb #x010f #xf4) ; CALL P,13
(memw #x0110 #x1300)
(memb #x0112 #xcc) ; CALL Z,FF
(memw #x0113 #xff00)
(memb #x0115 #xdc) ; CALL C,FF
(memw #x0116 #xff00)
(memb #x0118 #xec) ; CALL PE,FF
(memw #x0119 #xff00)
(memb #x011b #xfc) ; CALL M,FF
(memw #x011c #xff00)
(memb #x011e #xcd) ; CALL 14
(memw #x011f #x1400)
(memb #x0121 #xaf) ; XOR A,A
(memb #x0122 #xc4) ; CALL NZ,FF
(memw #x0123 #xff00)
(memb #x0125 #xd4) ; CALL NC,10
(memw #x0126 #x1000)
(memb #x0128 #xe4) ; CALL PO,FF
(memw #x0129 #xff00)
(memb #x012b #xf4) ; CALL P,12
(memw #x012c #x1200)
(memb #x012e #xcc) ; CALL Z,14
(memw #x012f #x1400)
(memb #x0131 #xdc) ; CALL C,FF
(memw #x0132 #xff00)
(memb #x0134 #xec) ; CALL PE,15
(memw #x0135 #x1500)
(memb #x0137 #xfc) ; CALL M,FF
(memw #x0138 #xff00)
(memb #x013a #xcd) ; CALL 17
(memw #x013b #x1700)
(memw #x013d #x3eff) ; LD A,FF
(memb #x013f #x87) ; ADD A,A
(memb #x0140 #xc4) ; CALL NZ,10
(memw #x0141 #x1000)
(memb #x0143 #xd4) ; CALL NC,FF
(memw #x0144 #xff00)
(memb #x0146 #xe4) ; CALL PO,11
(memw #x0147 #x1100)
(memb #x0149 #xf4) ; CALL P,FF
(memw #x014a #xff00)
(memb #x014c #xcc) ; CALL Z,FF
(memw #x014d #xff00)
(memb #x014f #xdc) ; CALL C,13
(memw #x0150 #x1300)
(memb #x0152 #xec) ; CALL PE,15
(memw #x0153 #x1500)
(memb #x0155 #xfc) ; CALL M,16
(memw #x0156 #x1600)
;
;  Execute test
;
(print "==> Testing CALL-RET instructions")
(terpri)
(sim-init)
(go #x0100)
; Set SP
(sim-step) ; LD SP,2000
(test-reg RSP #x2000)
(test-mask #x00 MPSW)
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
(sim-step) ; CALL NZ,10  ; Verify CALL NZ taken
; Verify that PC is 10, SP is 1FFE
(test-reg RPC #x0010)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x0901)
(sim-step) ; RET NZ  ; Verify RET NZ taken
; Verify that PC is 109, SP is 2000
(test-reg RPC #x0109)
(test-reg RSP #x2000)
(sim-step) ; CALL NC,11  ; Verify CALL NC taken
; Verify that PC is 11, SP is 1FFE
(test-reg RPC #x0011)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x0c01)
(sim-step) ; RET NC  ; Verify RET NC taken
; Verify that PC is 10C, SP is 2000
(test-reg RPC #x010c)
(test-reg RSP #x2000)
(sim-step) ; CALL PO,12  ; Verify CALL PO taken
; Verify that PC is 12, SP is 1FFE
(test-reg RPC #x0012)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x0f01)
(sim-step) ; RPO  ; Verify RET PO taken
; Verify that PC is 10F, SP is 2000
(test-reg RPC #x010f)
(test-reg RSP #x2000)
(sim-step) ; CALL P,13  ; Verify CALL P taken
; Verify that PC is 13, SP is 1FFE
(test-reg RPC #x0013)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x1201)
(sim-step) ; RET P  ; Verify RET P taken
; Verify that PC is 112, SP is 2000
(test-reg RPC #x0112)
(test-reg RSP #x2000)
(sim-step) ; CALL Z FF  ; Verify CALL Z not taken
; Verify that PC is 115, SP is 2000
(test-reg RPC #x0115)
(test-reg RSP #x2000)
(sim-step) ; CALL C FF  ; Verify CALL C not taken
; Verify that PC is 118, SP is 2000
(test-reg RPC #x0118)
(test-reg RSP #x2000)
(sim-step) ; CALL PE FF  ; Verify CALL PE not taken
; Verify that PC is 11B, SP is 2000
(test-reg RPC #x11B)
(test-reg RSP #x2000)
(sim-step) ; CALL M FF  ; Verify CALL M not taken
; Verify that PC is 11E, SP is 2000
(test-reg RPC #x011e)
(test-reg RSP #x2000)
; Verify returns not taken
(sim-step) ; CALL 14
; Verify that PC is 14, SP is 1FFE
(test-reg RPC #x0014)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2101)
(sim-step) ; RET Z  ; RET Z not taken
; Verify that PC is 15, SP is 1FFE
(test-reg RPC #x0015)
(test-reg RSP #x1ffe)
(sim-step) ; RET C  ; RET C not taken
; Verify that PC is 16, SP is 1FFE
(test-reg RPC #x0016)
(test-reg RSP #x1ffe)
(sim-step) ; RET PE  ; RET PE not taken
; Verify that PC is 17, SP is 1FFE
(test-reg RPC #x0017)
(test-reg RSP #x1ffe)
(sim-step) ; RET M  ; RET M not taken
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
(sim-step) ; XOR A,A
(test-reg RA #x00)
(test-mask #x44 MPSW) ; Z&P flags set
(sim-step) ; CALL NZ,FF  ;  Verify CALL NZ not taken
; Verify that PC is 125, SP is 2000
(test-reg RPC #x0125)
(test-reg RSP #x2000)
(sim-step) ; CALL NC,10  ; Verify CALL NC taken
; Verify that PC is 10, SP is 1FFE
(test-reg RPC #x0010)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2801)
(sim-step) ; RET NZ  ; Verify RET NZ not taken
; Verify that PC is 11, SP is 1FFE
(test-reg RPC #x0011)
(test-reg RSP #x1ffe)
(sim-step) ; RET NC  ; Verify RET NC taken
; Verify that PC is 128, SP is 2000
(test-reg RPC #x0128)
(test-reg RSP #x2000)
(sim-step) ; CALL PO,FF  ; Verify that CALL PO not taken
; Verify that PC is 12B, SP is 2000
(test-reg RPC #x012b)
(test-reg RSP #x2000)
(sim-step)  ; CALL P,12  ; Verify that CALL P taken
; Verify that PC is 12, SP is 1FFE
(test-reg RPC #x0012)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2e01)
(sim-step) ; RRT PO  ; Verify that RET PO not taken
; Verify that PC is 13, SP is 1FFE
(test-reg RPC #x0013)
(test-reg RSP #x1ffe)
(sim-step) ; RET P  ; Verify that RET P taken
; Verify that PC is 12E, SP is 2000
(test-reg RPC #x012e)
(test-reg RSP #x2000)
(sim-step) ; CALL Z,14  ; Verify that CALL Z taken
; Verify that PC is 14, SP is 1FFE
(test-reg RPC #x0014)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x3101)
(sim-step) ; RET Z  ; Verify that RET Z taken
; Verify that PC is 131, SP is 2000
(test-reg RPC #x0131)
(test-reg RSP #x2000)
(sim-step) ; CALL C,FF  ; Verify that CALL C not taken
; Verify that PC is 134, SP is 2000
(test-reg RPC #x0134)
(test-reg RSP #x2000)
(sim-step) ; CALL PE,15  ; Verify that CALL PE taken
; Verify that PC is 15, SP is 1FFE
(test-reg RPC #x0015)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x3701)
(sim-step) ; RET C  ; Verify that RET C not taken
; Verify that PC is 16, SP is 1FFE
(test-reg RPC #x0016)
(test-reg RSP #x1ffe)
(sim-step) ; RET PE  ; Verify that RET PE taken
; Verify that PC is 137, SP is 2000
(test-reg RPC #x0137)
(test-reg RSP #x2000)
(sim-step) ; CALL M,FF  ; Verify that CALL M not taken
; Verify that PC is 13A, SP is 2000
(test-reg RPC #x013a)
(test-reg RSP #x2000)
; Call to verify RET M not taken
(sim-step) ; CALL 17
(test-reg RPC #x0017)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x3d01)
(sim-step) ; RET M
; Verify that PC is 18, SP is 1FFE
(test-reg RPC #x0018)
(test-reg RSP #x1ffe)
(sim-step) ; RET
; Verify that PC is 13D, SP is 2000
(test-reg RPC #x013d)
(test-reg RSP #x2000)
; Set flags S&C
(sim-step) ; LD A,FF
(test-reg RA #xff)
(test-mask #x44 MPSW)
(sim-step) ; ADD A,A
(test-reg RA #xfe)
; Verify flags S&C set
(test-mask #x91 MPSW)
;
(sim-step) ; CALL NZ,10  ; Verify CALL NZ taken
; Verify PC is 10, SP is 1FFE
(test-reg RPC #x0010)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x4301)
(sim-step) ; RET NZ  ; Verify RET NZ taken
; Verify PC is 143, SP is 2000
(test-reg RPC #x0143)
(test-reg RSP #x2000)
(sim-step) ; CALL NC,FF  ; Verify CALL NC not taken
; Verify PC is 146, SP is 2000
(test-reg RPC #x0146)
(test-reg RSP #x2000)
(sim-step) ; CALL PO,11  ; Verify CALL PO taken
; Verify PC is 11, SP is 1FFE
(test-reg RPC #x0011)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x4901)
(sim-step) ; RET NC  ; Verify RET NC not taken
; Verify PC is 12, SP is 1FFE
(test-reg RPC #x0012)
(test-reg RSP #x1ffe)
(sim-step) ; RET PO  ; Verify RET PO taken
; Verify PC is 149, SP is 2000
(test-reg RPC #x0149)
(test-reg RSP #x2000)
(sim-step) ; CALL P,FF  ; Verify CALL P not taken
; Verify PC is 14C, SP is 2000
(test-reg RPC #x014c)
(test-reg RSP #x2000)
(sim-step) ; CALL Z FF  ; Verify CALL Z not taken
; Verify PC is 14F, SP is 2000
(test-reg RPC #x014f)
(test-reg RSP #x2000)
(sim-step) ; CALL C 13  ; Verify CALL C taken
; Verify PC is 13, SP is 1FFE
(test-reg RPC #x0013)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x5201)
(sim-step) ; RET P  ; Verify RET P not taken
; Verify PC is 14, SP is 1FFE
(test-reg RPC #x0014)
(test-reg RSP #x1ffe)
(sim-step) ; RET Z  ; Verify RET Z not taken
; Verify PC is 15, SP is 1FFE
(test-reg RPC #x0015)
(test-reg RSP #x1ffe)
(sim-step) ; RET C  ; Verify RET C taken
; Verify PC is 152, SP is 2000
(test-reg RPC #x0152)
(test-reg RSP #x2000)
(sim-step) ; CALL PE,15  ; Verify CALL PE not taken
; Verify PC is 155, SP is 2000
(test-reg RPC #x0155)
(test-reg RSP #x2000)
(sim-step) ; CALL M,16  ; Verify CALL M taken
; Verify PC is 16, SP is 1FFE
(test-reg RPC #x0016)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x5801)
(sim-step) ; RET PE  ; Verify RET PE not taken
; Verify PC is 17, SP is 1FFE
(test-reg RPC #x0017)
(test-reg RSP #x1ffe)
(sim-step) ; RET M  ; Verify RET M taken
; Verify PC is 158, SP is 2000
(test-reg RPC #x0158)
(test-reg RSP #x2000)
;-------------------------------------------------------------------------------
;  Test ADD word instructions
;
; Load memory
;
;  Initialize register pairs
(memb #x0100 #x01) ; LD BC,5678
(memw #x0101 #x7856)
(memb #x0103 #x11) ; LD DE,9ABC
(memw #x0104 #xbc9a)
(memb #x0106 #x21) ; LD HL,DEF0
(memw #x0107 #xf0de)
(memb #x0109 #x31) ; LD SP,2000
(memw #x010a #x0020)
(memb #x010c #x09) ; ADD HL,BC
(memb #x010d #x19) ; ADD HL,DE
(memb #x010e #x29) ; ADD HL,HL
(memb #x010f #x39) ; ADD HL,SP
;
;  Execute test
;
(print "==> Testing ADD word instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,5678
(test-reg RBC #x5678)
(sim-step) ; LD DE,9ABC
(test-reg RDE #x9abc)
(sim-step) ; LD HL,DEF0
(test-reg RHL #xdef0)
(sim-step) ; LD SP,2000
(test-reg RSP #x2000)
(sim-step) ; ADD HL,BC  ;  Verify ADD HL,BC
; Verify that HL is 3568 and Carry is set
(test-reg RHL #x3568)
(test-mask #x01 MPSW)
(sim-step) ; ADD HL,DE  ;  Verify ADD HL,DE
; Verify that HL is D024 and Carry is clear
(test-reg RHL #xd024)
(test-mask #x00 MPSW)
(sim-step) ; ADD HL,HL  ;  Verify ADD HL,HL
; Verify that HL is A048 and Carry is set
(test-reg RHL #xa048)
(test-mask #x01 MPSW)
(sim-step) ; ADD HL,SP  ;  Verify ADD HL,SP
; Verify that HL is C048 and Carry is clear
(test-reg RHL #xc048)
(test-mask #x00 MPSW)
;-------------------------------------------------------------------------------
;  Test CP instructions
;
; Load memory
;
;  Initialize registers
(memw #x0100 #x06de) ; LD B,DE
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3612) ; LD (HL),12
(memw #x010e #x3e34) ; LD A,34
;
;  Test CMP x instructions
(memb #x0110 #xb8) ; CP A,B
(memb #x0111 #xb9) ; CP A,C
(memb #x0112 #xba) ; CP A,D
(memb #x0113 #xbb) ; CP A,E
(memb #x0114 #xbc) ; CP A,H
(memb #x0115 #xbd) ; CP A,L
(memb #x0116 #xbe) ; CP A,(HL) (address in HL (12A), contents 0E)
(memb #x0117 #xbf) ; CP A,A
;
;  Execute test
;
(print "==> Testing CP instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,DE
(test-reg RB #xde)
(sim-step) ; LD C,AD
(test-reg RC #xad)
(sim-step) ; LD D,BE
(test-reg RD #xbe)
(sim-step) ; LD E,EF
(test-reg RE #xef)
(sim-step) ; LD H,10
(test-reg RH #x10)
(sim-step) ; LD L,01
(test-reg RL #x01)
(sim-step) ; LD (HL),12
(test-memb #x1001 #x12)
(sim-step) ; LD A,34
(test-reg RA #x34)
;
(sim-step) ; CP A,B  ; Verify CP A,B
; Verify that A is 34, PC is 111, A,N,&C flags are set
(test-reg RA #x34)
(test-reg RPC #x0111)
(test-mask #x13 MPSW)
(sim-step) ; CP A,C  ; Verify CP A,C
; Verify that A is 34, PC is 112, S,A,P,N,&C flags are set
(test-reg RA #x34)
(test-reg RPC #x0112)
(test-mask #x97 MPSW)
(sim-step) ; CP A,D  ; Verify CP A,D
; Verify that A is 34, PC is 113, A,N,&C flags are set
(test-reg RA #x34)
(test-reg RPC #x0113)
(test-mask #x13 MPSW)
(sim-step) ; CP A,E  ; Verify CP A,E
; Verify that A is 34, PC is 114, A,N,&C flags are set
(test-reg RA #x34)
(test-reg RPC #x0114)
(test-mask #x13 MPSW)
(sim-step) ; CP A,H  ; Verify CP A,H
; Verify that A is 34, PC is 115, N flag is set
(test-reg RA #x34)
(test-reg RPC #x0115)
(test-mask #x02 MPSW)
(sim-step) ; CP A,L  ; Verify CP A,L
; Verify that A is 34, PC is 116, N flag is set
(test-reg RA #x34)
(test-reg RPC #x0116)
(test-mask #x02 MPSW)
(sim-step) ; CP A,(HL)  ; Verify CP A,(HL)
; Verify that A is 34, PC is 117, N flag is set
(test-reg RA #x34)
(test-reg RPC #x0117)
(test-mask #x02 MPSW)
(sim-step) ; CP A,A  ; Verify CP A,A
; Verify that A is 34, PC is 118, Z&N flags are set
(test-reg RA #x34)
(test-reg RPC #x0118)
(test-mask #x42 MPSW)
;-------------------------------------------------------------------------------
;  Test INC-DEC byte instructions
;
; Load memory
;
(memw #x0100 #x06de) ; LD B,DE
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3612) ; LD (HL),12
(memw #x010e #x3e34) ; LD A,34
;
(memb #x0110 #x04) ; INC B
(memb #x0111 #x0c) ; INC C
(memb #x0112 #x14) ; INC D
(memb #x0113 #x1c) ; INC E
(memb #x0114 #x24) ; INC H
(memb #x0115 #x2c) ; INC L
(memb #x0116 #x21) ; LD HL,1001
(memw #x0117 #x0110)
(memb #x0119 #x34) ; INC (HL)
(memb #x011a #x3c) ; INC A
(memb #x011b #x05) ; DEC B
(memb #x011c #x0d) ; DEC C
(memb #x011d #x15) ; DEC D
(memb #x011e #x1d) ; DEC E
(memb #x011f #x25) ; DEC H
(memb #x0120 #x2d) ; DEC L
(memb #x0121 #x21) ; LD HL,1001
(memw #x0122 #x0110)
(memb #x0124 #x35) ; DEC (HL)
(memb #x0125 #x3d) ; DEC A
;
;  Execute test
;
(print "==> Testing INC-DEC byte instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,DE
(test-reg RB #xde)
(sim-step) ; LD C,AD
(test-reg RC #xad)
(sim-step) ; LD D,BE
(test-reg RD #xbe)
(sim-step) ; LD E,EF
(test-reg RE #xef)
(sim-step) ; LD H,10
(test-reg RH #x10)
(sim-step) ; LD L,01
(test-reg RL #x01)
(sim-step) ; LD (HL),12
(test-memb #x1001 #x12)
(sim-step) ; LD A,34
(test-reg RA #x34)
;
(print "--> INR instructions")
(sim-step) ; INC B  ;  Verify INC B
; Verify that B is DF, flags are S
(test-reg RB #xdf)
(test-mask #x80 MPSW)
(sim-step) ; INC C  ;  Verify INC C
; Verify that C is AE, flags are S
(test-reg RC #xae)
(test-mask #x80 MPSW)
(sim-step) ; INC D  ;  Verify INC D
; Verify that D is BF, flags are S
(test-reg RD #xbf)
(test-mask #x80 MPSW)
(sim-step) ; INC E  ;  Verify INC E
; Verify that E is F0, flags are S,A
(test-reg RE #xf0)
(test-mask #x90 MPSW)
(sim-step) ; INC H  ;  Verify INC H
; Verify that H is 11, flags are all clear
(test-reg RH #x11)
(test-mask #x00 MPSW)
(sim-step) ; INC L  ;  Verify INC L
; Verify that L is 2, flags are all clear
(test-reg RL #x02)
(test-mask #x00 MPSW)
(sim-step) ; LD HL,1001  ; Restore HL
(test-reg RHL #x1001)
(sim-step) ; INC (HL) ;  Verify INC (HL
; Verify that location 1001 is 13, flags are clear
(test-memb #x1001 #x13)
(test-mask #x00 MPSW)
(sim-step) ; INC A  ;  Verify INC A
; Verify that A is 35, flags are P
(test-reg RA #x35)
(test-mask #x04 MPSW)
(print "--> DCR instructions")
(sim-step) ; DEC B  ;  Verify DEC B
; Verify that B is DE, flags are S&N
(test-reg RB #xde)
(test-mask #x82 MPSW)
(sim-step) ; DEC C  ;  Verify DEC C
; Verify that C is AD, flags are S&N
(test-reg RC #xad)
(test-mask #x82 MPSW)
(sim-step) ; DEC D  ;  Verify DEC D
; Verify that D is BE, flags are S&N
(test-reg RD #xbe)
(test-mask #x82 MPSW)
(sim-step) ; DEC E  ;  Verify DEC E
; Verify that E is EF, flags are S,A,&N
(test-reg RE #xef)
(test-mask #x92 MPSW)
(sim-step) ; DEC H  ;  Verify DEC H
; Verify that H is 0F, flags are A&N
(test-reg RH #x0f)
(test-mask #x12 MPSW)
(sim-step) ; DEC L  ;  Verify DEC L
; Verify that L is 00, flags are Z&N
(test-reg RL #x00)
(test-mask #x42 MPSW)
(sim-step) ; LD HL,1001  ; Restore HL
(test-reg RHL #x1001)
(sim-step) ; DEC (HL)  ;  Verify DEC (HL)
; Verify that location 1001 is 12, flags are N
(test-memb #x1001 #x12)
(test-mask #x02 MPSW)
(sim-step) ; DEC A  ;  Verify DEC A
; Verify that A is 34, flags are N
(test-reg RA #x34)
(test-mask #x02 MPSW)
;-------------------------------------------------------------------------------
;  Test INC-DEC word instructions
;
; Load memory
;
(memb #x0100 #x01) ; LD BC,5678
(memw #x0101 #x7856)
(memb #x0103 #x11) ; LD DE,9ABC
(memw #x0104 #xbc9a)
(memb #x0106 #x21) ; LD HL,DEF0
(memw #x0107 #xf0de)
(memb #x0109 #x31) ; LD SP,2000
(memw #x010a #x0020)
;
(memb #x010c #x03) ; INC BC
(memb #x010d #x13) ; INC DE
(memb #x010e #x23) ; INC HL
(memb #x010f #x33) ; INC SP
(memb #x0110 #x0b) ; DEC BC
(memb #x0111 #x1b) ; DEC DE
(memb #x0112 #x2b) ; DEC HL
(memb #x0113 #x3b) ; DEC SP
;
;  Execute test
;
(print "==> Testing INC-DEC word instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,5678
(test-reg RBC #x5678)
(sim-step) ; LD DE,9ABC
(test-reg RDE #x9abc)
(sim-step) ; LD HL,DEF0
(test-reg RHL #xdef0)
(sim-step) ; LD SP,2000
(test-reg RSP #x2000)
;
(sim-step) ; INC BC  ;  Verify INC BC
; Verify that BC contains 5679
(test-reg RBC #x5679)
(sim-step) ; INC DE  ;  Verify INC DE
; Verify that DE contains 9ABD
(test-reg RDE #x9abd)
(sim-step) ; INC HL  ;  Verify INC HL
; Verify that HL contains DEF1
(test-reg RHL #xdef1)
(sim-step) ; INC SP  ;  Verify INC SP
; Verify that SP contains 2001
(test-reg RSP #x2001)
(sim-step) ; DEC BC  ;  Verify DEC BC
; Verify that BC contains 5678
(test-reg RBC #x5678)
(sim-step) ; DEC DE  ;  Verify DEC DE
; Verify that DE contains 9ABC
(test-reg RDE #x9abc)
(sim-step) ; DEC HL  ;  Verify DEC HL
; Verify that HL contains DEF0
(test-reg RHL #xdef0)
(sim-step) ; DEC SP  ;  Verify DEC SP
; Verify that SP contains 2000
(test-reg RSP #x2000)
;-------------------------------------------------------------------------------
;  Test JP instructions
;
; Load memory
;
; Jump  Taken  Not-Taken
;  NZ     V       V
;  Z      V       V
;  NC     V       V
;  C      V       V
;  PO     V       V
;  PE     V       V
;  P      V       V
;  M      V       V
;
(memb #x00ff #x76) ; HLT
(memb #x0100 #xc3) ; JP 104
(memw #x0101 #x0401)
(memb #x0103 #x76) ; HLT
(memb #x0104 #xad) ; XOR A,A (Z&P flags set)
(memb #x0105 #xc2) ; JP NZ,FF
(memw #x0106 #xff00)
(memb #x0108 #xe2) ; JP PO,FF
(memw #x0109 #xff00)
(memb #x010b #xda) ; JP C,FF
(memw #x010c #xff00)
(memb #x010e #xfa) ; JP M,FF
(memw #x010f #xff00)
(memb #x0111 #xca) ; JP Z,115
(memw #x0112 #x1501)
(memb #x0114 #x76) ; HLT
(memb #x0115 #xea) ; JP PE,119
(memw #x0116 #x1901)
(memb #x0118 #x76) ; HLT
(memb #x0119 #xf2) ; JP P,11D
(memw #x011a #x1d01)
(memb #x011c #x76) ; HLT
(memb #x011d #xd2) ; JP NC,121
(memw #x011e #x2101)
(memb #x0120 #x76) ; HLT
(memb #x0121 #x3c) ; INC A (No flags set)
(memb #x0122 #xca) ; JP Z,FF
(memw #x0123 #xff00)
(memb #x0125 #xda) ; JP C,FF
(memw #x0126 #xff00)
(memb #x0128 #xea) ; JP PE,FF
(memw #x0129 #xff00)
(memb #x012b #xfa) ; JP M,FF
(memw #x012c #xff00)
(memb #x012e #xc2) ; JP NZ,132
(memw #x012f #x3201)
(memb #x0131 #x76) ; HLT
(memb #x0132 #xd2) ; JP NC,136
(memw #x0133 #x3601)
(memb #x0135 #x76) ; HLT
(memb #x0136 #xe2) ; JP PO,13A
(memw #x0137 #x3a01)
(memb #x0139 #x76) ; HLT
(memb #x013a #xf2) ; JP P,13E
(memw #x013b #x3e01)
(memb #x013d #x76) ; HLT
(memw #x013e #x3e80) ; LD A,80
(memb #x0140 #xb7) ; OR A,A (S flag should be only one set)
(memb #x0141 #xca) ; JP Z,FF
(memw #x0142 #xff00)
(memb #x0144 #xda) ; JP C,FF
(memw #x0145 #xff00)
(memb #x0147 #xea) ; JP PE,FF
(memb #x0148 #xff00)
(memb #x014a #xf2) ; JP P,FF
(memw #x014b #xff00)
(memb #x014d #xc2) ; JP NZ,151
(memw #x014e #x5101)
(memb #x0150 #x76) ; HLT
(memb #x0151 #xd2) ; JP NC,155
(memw #x0152 #x5501)
(memb #x0154 #x76) ; HLT
(memb #x0155 #xe2) ; JP PO,159
(memw #x0156 #x5901)
(memb #x0158 #x76) ; HLT
(memb #x0159 #xfa) ; JP M,15D
(memw #x015a #x5d01)
(memb #x015c #x76) ; HLT
(memb #x015d #x87) ; ADD A,A (Z,P,&C flags set)
(memb #x015e #xc2) ; JP NZ,FF
(memw #x015f #xff00)
(memb #x0161 #xe2) ; JP PO,FF
(memw #x0162 #xff00)
(memb #x0164 #xd2) ; JP NC,FF
(memw #x0165 #xff00)
(memb #x0167 #xfa) ; JP M,FF
(memw #x0168 #xff00)
(memb #x016a #xca) ; JP Z,16D
(memw #x016b #x6e01)
(memb #x016d #x76) ; HLT
(memb #x016e #xea) ; JP PE,172
(memw #x016f #x7201)
(memb #x0171 #x76) ; HLT
(memb #x0172 #xf2) ; JP P,176
(memw #x0173 #x7601)
(memb #x0175 #x76) ; HLT
(memb #x0176 #xda) ; JP C,17A
(memw #x0177 #x7a01)
(memb #x0179 #x76) ; HLT
;
;  Execute test
;
(print "==> Testing JP instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; JP 104  ; Verify JMP
(test-reg RPC #x0104) ; Verify PC is 104
(sim-step)  ; XOR A,A  ; Change flags (Z&P set)
(test-mask #x44 MPSW)
(sim-step)  ; JP NZ,FF  ; Verify JP NZ not taken
(test-reg RPC #x0108) ; Verify PC is 108
(sim-step)  ; JP PO,FF  ; Verify JP PE not taken
(test-reg RPC #x010b) ; Verify PC is 10B
(sim-step)  ; JP C,FF  ; Verify JP C not taken
(test-reg RPC #x010e) ; Verify PC is 10E
(sim-step)  ; JP M,FF  ; Verify JP M not taken
(test-reg RPC #x0111) ; Verify PC is 111
(sim-step)  ; JP Z,115  ; Verify JP Z taken
(test-reg RPC #x0115) ; Verify PC is 115
(sim-step)  ; JP PE,119  ; Verify JP PE taken
(test-reg RPC #x0119) ; Verify PC is 119
(sim-step)  ; JP P,11D  ; Verify JP P taken
(test-reg RPC #x011d) ; Verify PC is 11D
(sim-step)  ; JP NC,121  ; Verify JP NC taken
(test-reg RPC #x0121) ; Verify PC is 121
(sim-step) ; INC A  ; Change flags (none set)
(test-mask #x00 MPSW)
(sim-step)  ; JP Z,FF  ; Verify JP Z not taken
(test-reg RPC #x0125) ; Verify PC is 125
(sim-step)  ; JP C,FF  ; Verify JP C not taken
(test-reg RPC #x0128) ; Verify PC is 128
(sim-step)  ; JP PE,FF  ; Verify JP PE not taken
(test-reg RPC #x012b) ; Verify PC is 12B
(sim-step)  ; JP M,FF  ; Verify JP M not taken
(test-reg RPC #x012e) ; Verify PC is 12E
(sim-step)  ; JP NZ,132  ; Verify JP NZ taken
(test-reg RPC #x0132) ; Verify PC is 132
(sim-step)  ; JP NC,136  ; Verify JP NC taken
(test-reg RPC #x0136) ; Verify PC is 136
(sim-step)  ; JP PO,13A  ; Verify JP PO taken
(test-reg RPC #x013a) ; Verify PC is 13A
(sim-step)  ; JP P,13E  ; Verify JP P taken
(test-reg RPC #x013e) ; Verify PC is 13E
(sim-step) ; LD A,80
(test-reg RA #x80)
(sim-step) ; OR A,A  ;  Change flags (S set)
(test-mask #x80 MPSW)
(sim-step)  ; JP Z,FF  ; Verify JP Z not taken
(test-reg RPC #x0144) ; Verify PC is 144
(sim-step)  ; JP C,FF  ; Verify JP C not taken
(test-reg RPC #x0147) ; Verify PC is 147
(sim-step)  ; JP PE,FF  ; Verify JP PE not taken
(test-reg RPC #x014a) ; Verify PC is 14A
(sim-step)  ; JP P,FF  ; Verify JP P not taken
(test-reg RPC #x014d) ; Verify PC is 14D
(sim-step)  ; JP NZ,151  ; Verify JP NZ taken
(test-reg RPC #x0151) ; Verify PC is 151
(sim-step)  ; JP NC,155  ; Verify JP NC taken
(test-reg RPC #x0155) ; Verify PC is 155
(sim-step)  ; JP PO,159  ; Verify JP PO taken
(test-reg RPC #x0159) ; Verify PC is 159
(sim-step)  ; JP M,15D  ; Verify JP M taken
(test-reg RPC #x015d) ; Verify PC is 15D
(sim-step)  ; ADD A,A  ; Change flags (Z,P,&C set)
(test-mask #x45 MPSW)
(sim-step)  ; JP NZ,FF  ; Verify JP NZ not taken
(test-reg RPC #x0161) ; Verify PC is 161
(sim-step)  ; JP PO,FF  ; Verify JP PO not taken
(test-reg RPC #x0164) ; Verify PC is 164
(sim-step)  ; JP NC,FF  ; Verify JP NC not taken
(test-reg RPC #x0167) ; Verify PC is 167
(sim-step)  ; JP M,FF  ; Verify JP M not taken
(test-reg RPC #x016a) ; Verify PC is 16A
(sim-step)  ; JP Z,16D  ; Verify JP Z taken
(test-reg RPC #x016e) ; Verify PC is 16E
(sim-step)  ; JP PE,172  ; Verify JP PE taken
(test-reg RPC #x0172) ; Verify PC is 172
(sim-step)  ; JP P,176  ; Verify JP P taken
(test-reg RPC #x0176) ; Verify PC is 176
(sim-step)  ; JP C,17A  ; Verify JP C taken
(test-reg RPC #x017a) ; Verify PC is 17A
;-------------------------------------------------------------------------------
;  Test LD indirect byte instructions
;
; Load memory
;
(memb #x0100 #x01) ; LD BC,2020
(memw #x0101 #x2020)
(memb #x0103 #x11) ; LD DE,2021
(memw #x0104 #x2120)
(memw #x0106 #x3e34) ; LD A,34
(memb #x0108 #x02) ; LD (BC),A
(memw #x0109 #x3e56) ; LD A,56
(memb #x010b #x12) ; LD (DE),A
(memb #x010c #x0a) ; LD A,(BC)
(memb #x010d #x1a) ; LD A,(DE)
;
;  Execute test
;
(print "==> Testing LD indirect byte instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,2020
(test-reg RBC #x2020)
(sim-step)  ; LD DE,2021
(test-reg RDE #x2021)
(sim-step) ; LD A,34
(test-reg RA #x34)
(sim-step) ; LD (BC),A
(test-memb #x2020 #x34) ; Verify that 34 is in memory location 2020
(test-reg RPC #x0109) ; Verify that PC is 109
(sim-step) ; LD A,56
(test-reg RA #x56)
(sim-step) ; LD (DE),A
(test-memb #x2021 #x56) ; Verify that 56 is in memory location 2021
(test-reg RPC #x010c) ; Verify that PC is 10C
(sim-step) ; LD A,(BC)
(test-reg RA #x34) ; Verify that register A is 34 and PC is 10D
(test-reg RPC #x010d)
(sim-step) ; LD A,(DE)
(test-reg RA #x56) ; Verify that register A is 56 and PC is 10E
(test-reg RPC #x010e)
;-------------------------------------------------------------------------------
;  Test LD word instructions
;
; Load memory
;
(memb #x0100 #x01) ; LD BC,5678
(memw #x0101 #x7856)
(memb #x0103 #x11) ; LD DE,9ABC
(memw #x0104 #xbc9a)
(memb #x0106 #x21) ; LD HL,DEF0
(memw #x0107 #xf0de)
(memb #x0109 #x31) ; LD SP,2000
(memw #x010a #x0020)
;
;  Execute test
;
(print "==> Testing LD word instructions")
(terpri)
(sim-init)
(test-reg RBC #x0000)
(test-reg RDE #x0000)
(test-reg RHL #x0000)
(test-reg RSP #x0000)
(test-mask #x00 MPSW)
(test-reg RA #x00)
(go #x0100)
(test-reg RPC #x0100)
(sim-step) ; Verify LD BC,5678
; Verify that BC is 5678, PC is 103, and other registers unchanged
(test-reg RBC #x5678)
(test-reg RDE #x0000)
(test-reg RHL #x0000)
(test-reg RSP #x0000)
(test-mask #x00 MPSW)
(test-reg RA #x00)
(test-reg RPC #x0103)
(sim-step) ; Verify LD DE,5678
; Verify that DE is 9ABC, PC is 106, and other registers unchanged
(test-reg RBC #x5678)
(test-reg RDE #x9abc)
(test-reg RHL #x0000)
(test-reg RSP #x0000)
(test-mask #x00 MPSW)
(test-reg RA #x00)
(test-reg RPC #x0106)
(sim-step) ; Verify LD HL,DEF0
; Verify that HL is DEF0, PC is 109, and other registers unchanged
(test-reg RBC #x5678)
(test-reg RDE #x9abc)
(test-reg RHL #xdef0)
(test-reg RSP #x0000)
(test-mask #x00 MPSW)
(test-reg RA #x00)
(test-reg RPC #x0109)
(sim-step) ; Verify LD SP,2000
; Verify that SP is 2000, PC is 10C, and other registers unchanged
(test-reg RBC #x5678)
(test-reg RDE #x9abc)
(test-reg RHL #xdef0)
(test-reg RSP #x2000)
(test-mask #x00 MPSW)
(test-reg RA #x00)
(test-reg RPC #x010c)
;-------------------------------------------------------------------------------
;  Test LD byte register instructions
;
; Load memory
;
(memw #x0100 #x06de) ; LD B,DE
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3612) ; LD (HL),12
(memw #x010e #x3e34) ; LD A,34
;
(memb #x0110 #x40) ; LD B,B (effectively a NOP)
(memb #x0111 #x41) ; LD B,C
(memb #x0112 #x42) ; LD B,D
(memb #x0113 #x43) ; LD B,E
(memb #x0114 #x44) ; LD B,H
(memb #x0115 #x45) ; LD B,L
(memb #x0116 #x46) ; LD B,(HL) (address 1001 in HL)
(memb #x0117 #x47) ; LD B,A
;  Restore B
(memw #x0118 #x06de) ; LD B,DE
(memb #x011a #x48) ; LD C,B
(memb #x011b #x49) ; LD C,C (effectively a NOP)
(memb #x011c #x4a) ; LD C,D
(memb #x011d #x4b) ; LD C,E
(memb #x011e #x4c) ; LD C,H
(memb #x011f #x4d) ; LD C,L
(memb #x0120 #x4e) ; LD C,(HL) (address 1001 in HL)
(memb #x0121 #x4f) ; LD C,A
; Restore C
(memw #x0122 #x0ead) ; LD C,AD
(memb #x0124 #x50) ; LD D,B
(memb #x0125 #x51) ; LD D,C
(memb #x0126 #x52) ; LD D,D (effectively a NOP)
(memb #x0127 #x53) ; LD D,E
(memb #x0128 #x54) ; LD D,H
(memb #x0129 #x55) ; LD D,L
(memb #x012a #x56) ; LD D,(HL) (address 1001 in HL)
(memb #x012b #x57) ; LD D,A
; Restore D
(memw #x012c #x16be) ; LD D,BE
(memb #x012e #x58) ; LD E,B
(memb #x012f #x59) ; LD E,C
(memb #x0130 #x5a) ; LD E,D
(memb #x0131 #x5b) ; LD E,E (effectively a NOP)
(memb #x0132 #x5c) ; LD E,H
(memb #x0133 #x5d) ; LD E,L
(memb #x0134 #x5e) ; LD E,(HL) (address 1001 in HL)
(memb #x0135 #x5f) ; LD E,A
; Restore E
(memw #x136 #x1eef) ; LD E,EF
(memb #x0138 #x60) ; LD H,B
(memb #x0139 #x61) ; LD H,C
(memb #x013a #x62) ; LD H,D
(memb #x013b #x63) ; LD H,E
(memb #x013c #x64) ; LD H,H (effectively a NOP)
(memb #x013d #x65) ; LD H,L
(memw #x013e #x2610) ; LD H,10
(memb #x0140 #x66) ; LD H,(HL) (address 1001 in HL)
(memb #x0141 #x67) ; LD H,A
; Restore H
(memw #x0142 #x2610) ; LD H,10
(memb #x0144 #x68) ; LD L,B
(memb #x0145 #x69) ; LD L,C
(memb #x0146 #x6a) ; LD L,D
(memb #x0147 #x6b) ; LD L,E
(memb #x0148 #x6c) ; LD L,H
(memb #x0149 #x6d) ; LD L,L (effectively a NOP)
; Restore L
(memw #x014a #x2e01) ; LD L,01
(memb #x014c #x6e) ; LD L,(HL) (address 1001 in HL)
(memb #x014d #x6f) ; LD L,A
; Restore L
(memw #x014e #x2e01) ; LD L,01
(memb #x0150 #x70) ; LD (HL),B (address 1001 in HL)
(memb #x0151 #x71) ; LD (HL),C (address 1001 in HL)
(memb #x0152 #x72) ; LD (HL),D (address 1001 in HL)
(memb #x0153 #x73) ; LD (HL),E (address 1001 in HL)
(memb #x0154 #x74) ; LD (HL),H (address 1001 in HL)
(memb #x0155 #x75) ; LD (HL),L (address 1001 in HL)
; Note that the opcode for LD (HL),(HL) is used for HLT and will be tested
; elsewhere.
(memb #x0156 #x77) ; LD (HL),A (address 1001 in HL)
(memb #x0157 #x78) ; LD A,B
(memb #x0158 #x79) ; LD A,C
(memb #x0159 #x7a) ; LD A,D
(memb #x015a #x7b) ; LD A,E
(memb #x015b #x7c) ; LD A,H
(memb #x015c #x7d) ; LD A,L
(memb #x015d #x7e) ; LD A,(HL) (address 1001 in HL)
(memb #x015e #x7f) ; LD A,A (effectively a NOP)
;
;  Execute test
;
(print "==> Testing LD byte register instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,DE
(sim-step) ; LD C,AD
(test-reg RBC #xdead)
(sim-step) ; LD D,BE
(sim-step) ; LD E,EF
(test-reg RDE #xbeef)
(sim-step) ; LD H,10
(sim-step) ; LD L,01
(test-reg RHL #x1001)
(sim-step) ; LD M,12
(test-memb #x1001 #x12)
(sim-step) ; LD A,34
(test-reg RA #x34)
;
(sim-step) ; LD B,B  ; Verify LD B,B
; Verify that registers are unchanged exept PC is 111
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0111)
(sim-step) ; LD B,C  ; Verify LD B,C
; Verify that B is AD, PC is 112, and other registers are unchanged
(test-reg RBC #xadad)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0112)
(sim-step) ; LD B,D  ; Verify LD B,D
; Verify that B is BE, PC is 113, and other registers are unchanged
(test-reg RBC #xbead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0113)
(sim-step) ; LD B,E  ; Verify LD B,E
; Verify that B is EF, PC is 114, and other registers are unchanged
(test-reg RBC #xefad)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0114)
(sim-step) ; LD B,H  ; Verify LD B,H
; Verify that B is 10, PC is 115, and other registers are unchanged
(test-reg RBC #x10ad)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0115)
(sim-step) ; LD B,L  ; Verify LD B,L
; Verify that B is 01, PC is 116, and other registers are unchanged
(test-reg RBC #x01ad)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0116)
(sim-step) ; LD B,(HL)  ; Verify LD B,(HL)
; Verify that B is 12, PC is 117, and other registers are unchanged
(test-reg RBC #x12ad)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0117)
(sim-step) ; LD B,A  ; Verify LD B,A
; Verify that B is 34, PC is 118, and other registers are unchanged
(test-reg RBC #x34ad)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0118)
(sim-step) ; LD B,DE  ; Restore register B value
(test-reg RBC #xdead)
(sim-step) ; LD C,B  ; Verify LD C,B
; Verify that C is DE, PC is 11B and other registers are unchanged
(test-reg RBC #xdede)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x011b)
(sim-step) ; LD C,C  ; Verify LD C,C
; Verify that registers are unchanged exept PC is 11C
(test-reg RBC #xdede)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x011c)
(sim-step) ; LD C,D  ; Verify LD C,D
; Verify that C is BE, PC is 11D, and other registers are unchanged
(test-reg RBC #xdebe)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x011d)
(sim-step) ; LD C,E  ; Verify LD C,E
; Verify that C is EF, PC is 11E, and other registers are unchanged
(test-reg RBC #xdeef)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x011e)
(sim-step) ; LD C,H  ; Verify LD C,H
; Verify that C is 10, PC is 11F, and other registers are unchanged
(test-reg RBC #xde10)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x011f)
(sim-step) ; LD C,L  ; Verify LD C,L
; Verify that C is 01, PC is 120, and other registers are unchanged
(test-reg RBC #xde01)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0120)
(sim-step) ; LD C,(HL)  ; Verify LD C,(HL)
; Verify that C is 12, PC is 121, and other registers are unchanged
(test-reg RBC #xde12)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0121)
(sim-step) ; LD C,A  ; Verify LD C,A
; Verify that C is 34, PC is 122, and other registers are unchanged
(test-reg RBC #xde34)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0122)
(sim-step) ; LD C,AD  ; Restore register C value
(test-reg RBC #xdead)
(sim-step) ; LD D,B  ; Verify LD D,B
; Verify that D is DE, PC is 125, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xdeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0125)
(sim-step) ; LD D,C  ; Verify LD D,C
; Verify that D is AD, PC is 126, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xadef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0126)
(sim-step) ; LD D,D  ; Verify LD D,D
; Verify that the registers are unchanged except PC is 127
(test-reg RBC #xdead)
(test-reg RDE #xadef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0127)
(sim-step) ; LD D,E  ; Verify LD D,E
; Verify that D is EF, PC is 128, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xefef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0128)
(sim-step); LD D,H  ; Verify LD D,H
; Verify that D is 10, PC is 129, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #x10ef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0129)
(sim-step); LD D,L  ; Verify LD D,L
; Verify that D is 01, PC is 12A, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #x01ef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x012a)
(sim-step); LD D,(HL)  ; Verify LD D,(HL)
; Verify that D is 12, PC is 12B, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #x12ef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x012b)
(sim-step); LD D,A  ; Verify LD D,A
; Verify that D is 34, PC is 12C, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #x34ef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x012c)
(sim-step); LD D,BE  ; Restore register D value
(test-reg RDE #xbeef)
(sim-step) ; MOV E,B  ; Verify MOV E,B
; Verify that E is DE, PC is 12F, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbede)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x012f)
(sim-step) ; LD E,C  ; Verify LD E,C
; Verify that E is AD, PC is 130, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbead)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0130)
(sim-step) ; LD E,D  ; Verify LD E,D
; Verify that E is BE, PC is 131, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbebe)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0131)
(sim-step) ; LD E,E  ; LD MOV E,E
; Verify that the registers are unchanged except PC is 132
(test-reg RBC #xdead)
(test-reg RDE #xbebe)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0132)
(sim-step) ; LD E,H  ; Verify LD E,H
; Verify that E is 10, PC is 133, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbe10)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0133)
(sim-step) ; LD E,L  ; Verify LD E,L
; Verify that E is 01, PC is 134, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbe01)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0134)
(sim-step); LD E,(HL)  ; Verify LD E,(HL)
; Verify that E is 12, PC is 135, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbe12)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0135)
(sim-step) ; LD E,A  ; Verify LD E,A
; Verify that E is 34, PC is 136, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbe34)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0136)
(sim-step)  ; LD E,EF  ; Restore register E value
(test-reg RDE #xbeef)
(sim-step); LD H,B  ; Verify LD H,B
; Verify that H is DE, PC is 139, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #xde01)
(test-reg RSP #x0000)
(test-reg RPC #x0139)
(sim-step) ; LD H,C  ; Verify LD H,C
; Verify that H is AD, PC is 13A, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #xad01)
(test-reg RSP #x0000)
(test-reg RPC #x013a)
(sim-step) ; LD H,D  ; Verify LD H,D
; Verify that H is BE, PC is 13B, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #xbe01)
(test-reg RSP #x0000)
(test-reg RPC #x013b)
(sim-step) ; LD H,E  ; Verify LD H,E
; Verify that H is EF, PC is 13C, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #xef01)
(test-reg RSP #x0000)
(test-reg RPC #x013c)
(sim-step) ; LD H,H  ; Verify LD H,H
; Verify that the registers are unchanged except PC is 13D
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #xef01)
(test-reg RSP #x0000)
(test-reg RPC #x013d)
(sim-step) ; LD H,L  ; Verify LD H,L
; Verify that H is 01, PC is 13E, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x0101)
(test-reg RSP #x0000)
(test-reg RPC #x013e)
(sim-step) ; LD H,10  ; Restore H value
(test-reg RHL #x1001)
(sim-step) ; LD H,(HL)  ; Verify LD H,(HL)
; Verify that H is 12, PC is 141, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1201)
(test-reg RSP #x0000)
(test-reg RPC #x0141)
(sim-step) ; LD H,A  ; Verify LD H,A
; Verify that H is 34, PC is 142, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x3401)
(test-reg RSP #x0000)
(test-reg RPC #x0142)
(sim-step) ; LD H,10  ; Restore register H value
(test-reg RHL #x1001)
(sim-step) ; LD L,B  ; Verify LD L,B
; Verify that L is DE, PC is 145, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x10de)
(test-reg RSP #x0000)
(test-reg RPC #x0145)
(sim-step) ; LD L,C  ; Verify LD L,C
; Verify that L is AD, PC is 146, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x10ad)
(test-reg RSP #x0000)
(test-reg RPC #x0146)
(sim-step) ; LD L,D  ; Verify LD L,D
; Verify that L is BE, PC is 147, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x10be)
(test-reg RSP #x0000)
(test-reg RPC #x0147)
(sim-step) ; LD L,E  ; Verify LD L,E
; Verify that L is EF, PC is 148, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x10ef)
(test-reg RSP #x0000)
(test-reg RPC #x0148)
(sim-step) ; LD L,H  ; Verify LD L,H
; Verify that L is 10, PC is 149, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1010)
(test-reg RSP #x0000)
(test-reg RPC #x0149)
(sim-step) ; LD L,L  ; Verify LD L,L
; Verify that the registers are unchanged except PC is 14A
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1010)
(test-reg RSP #x0000)
(test-reg RPC #x014a)
(sim-step) ; LD L,01  ; Restore L value
(test-reg RHL #x1001)
(sim-step) ; LD L,(HL)  ; Verify LD L,(HL)
; Verify that L is 12, PC is 14D, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1012)
(test-reg RSP #x0000)
(test-reg RPC #x014d)
(sim-step) ; LD L,A  ; Verify LD L,A
; Verify that L is 34, PC is 14E, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1034)
(test-reg RSP #x0000)
(test-reg RPC #x014e)
(sim-step) ; LD L,01  ; Restore register L value
(test-reg RHL #x1001)
(sim-step) ; LD (HL),B  ; Verify LD (HL),B
; Verify that memory location 1001 is DE
(test-memb #x1001 #xde)
(sim-step) ; LD (HL),C  ; Verify LD (HL),C
; Verify that memory location 1001 is AD
(test-memb #x1001 #xad)
(sim-step) ; LD (HL),D  ; Verify LD (HL),D
; Verify that memory location 1001 is BE
(test-memb #x1001 #xbe)
(sim-step) ; LD (HL),E  ; Verify LD (HL),E
; Verify that memory location 1001 is EF
(test-memb #x1001 #xef)
(sim-step) ; LD (HL),H  ; Verify LD (HL),H
; Verify that memory location 1001 is 10
(test-memb #x1001 #x10)
(sim-step) ; LD (HL),L  ; Verify LD (HL),L
; Verify that memory location 1001 is 01
(test-memb #x1001 #x01)
; Note that the opcode for LD (HL),(HL) is used for HLT and is not tested here.
(sim-step) ; LD (HL),A  ; Verify LD (HL),A
; Verify that memory location 1001 is 34
(test-memb #x1001 #x34)
; Verify that register values are unchanged except PC is 157
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0157)
(sim-step) ; LD A,B  ; Verify LD A,B
; Verify that A is DE, PC is 158, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0158)
(test-reg RA #xde)
(sim-step) ; LD A,C  ; Verify LD A,C
; Verify that A is AD, PC is 159, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x0159)
(test-reg RA #xad)
(sim-step) ; LD A,D  ; Verify LD A,D
; Verify that A is BE, PC is 15A, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x015a)
(test-reg RA #xbe)
(sim-step) ; LD A,E  ; Verify LD A,E
; Verify that A is EF, PC is 15B, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x015b)
(test-reg RA #xef)
(sim-step) ; LD A,H  ; Verify LD A,H
; Verify that A is 10, PC is 15C, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x015c)
(test-reg RA #x10)
(sim-step) ; LD A,L  ; Verify LD A,L
; Verify that A is 01, PC is 15D, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x015d)
(test-reg RA #x01)
(sim-step) ; LD A,(HL)  ; Verify LD A,(HL)
; Verify that A is 34, PC is 15E, and other registers are unchanged
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x015e)
(test-reg RA #x34)
(sim-step) ; LD A,A  ; Verify LD A,A
; Verify that the registers are unchanged except PC is 15F
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-reg RSP #x0000)
(test-reg RPC #x015f)
(test-reg RA #x34)
;-------------------------------------------------------------------------------
;  Test LD immediate instructions
;
; Load memory
;
(memw #x0100 #x06de) ; LD B,DE
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3612) ; LD (HL),12
(memw #x010e #x3e34) ; LD A,34
;
;  Execute test
;
(print "==> Testing LD immediate instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,DE  ;  Verify LD B,DE
; Verify that register B is DE and PC is 102
(test-reg RB #xde)
(test-reg RPC #x0102)
(sim-step) ; LD C,AD  ; Verify LD C,AD
; Verify that register C is AD and PC is 104
(test-reg RC #xad)
(test-reg RPC #x0104)
(sim-step) ; LD D,BE  ;  Verify LD D,BE
; Verify that register D is BE and PC is 106
(test-reg RD #xbe)
(test-reg RPC #x0106)
(sim-step) ; LD E,EF  ;  Verify LD E,EF
; Verify that register E is EF and PC is 108
(test-reg RE #xef)
(test-reg RPC #x0108)
(sim-step) ; LD H,10  ;  Verify LD H,10
; Verify that register H is 10 and PC is 10A
(test-reg RH #x10)
(test-reg RPC #x010a)
(sim-step) ; LD L,01  ;  Verify LD L,01
; Verify that register L is 01 and PC is 10C
(test-reg RL #x01)
(test-reg RPC #x010c)
(sim-step) ; LD (HL),12  ;  Verify LD (HL),12
; Verify that memory location 1001 is 12 and PC is 10E
(test-memb #x1001 #x12)
(test-reg RPC #x010e)
(sim-step) ; LD A,34  ;  Verify LD A,34
; Verify that register A is 34 and PC is 110
(test-reg RA #x34)
(test-reg RPC #x0110)
;-------------------------------------------------------------------------------
;  Test Miscellaneous instructions
;
; Load memory
;
; Setup low memory for RST instructions
(memw #x0000 #xc976) ; RET/HLT
(memw #x0008 #xc976) ; RET/HLT
(memw #x0010 #xc976) ; RET/HLT
(memw #x0018 #xc976) ; RET/HLT
(memw #x0020 #xc976) ; RET/HLT
(memw #x0028 #xc976) ; RET/HLT
(memw #x0030 #xc976) ; RET/HLT
(memw #x0038 #xc976) ; RET/HLT
;
(memb #x0100 #x37) ; SCF
(memb #x0101 #x3f) ; CCF
(memb #x0102 #x2f) ; CPL
(memb #x0103 #x00) ; NOP
(memw #x0104 #x3e55) ; LD A,55
(memb #x0106 #x32) ; LD (2000),A
(memw #x0107 #x0020)
(memb #x0109 #xaf) ; XOR A,A
(memb #x010a #x3a) ; LD A,(2000)
(memw #x010b #x0020)
(memb #x010d #x21) ; LD HL,AA55
(memw #x010e #x55aa)
(memb #x0110 #x22) ; LD (2002),HL
(memw #x0111 #x0220)
(memb #x0113 #x21) ; LD HL,0
(memw #x0114 #x0000)
(memb #x0116 #x2a) ; LD HL,(2002)
(memw #x0117 #x0220)
(memb #x0119 #xf3) ; DI
(memb #x011a #xfb) ; EI
(memb #x011b #x31) ; LD SP,2000 to initialize stack
(memw #x011c #x0020)
; Test RST instructions
(memb #x011e #xc7) ; RST 0
(memb #x011f #xcf) ; RST 1
(memb #x0120 #xd7) ; RST 2
(memb #x0121 #xdf) ; RST 3
(memb #x0122 #xe7) ; RST 4
(memb #x0123 #xef) ; RST 5
(memb #x0124 #xf7) ; RST 6
(memb #x0125 #xff) ; RST 7
;
(memb #x0126 #xe3) ; EX (SP),HL
(memb #x0127 #x21) ; LD HL,12C
(memw #x0128 #x2c01)
(memb #x012a #xe9) ; JP (HL)
(memb #x012b #x76) ; HLT
(memb #x012c #xf9) ; LD SP,HL
(memb #x012d #xeb) ; EX DE,HL
;
(memw #x012e #xd310) ; OUT 10
(memw #x0130 #xdb10) ; IN 10
;
(memw #x0132 #x3e99) ; LD A,99
(memb #x0134 #x87) ; ADD A,A
(memb #x0135 #x27) ; DAA
(memw #x0136 #x3e19) ; LD A,19
(memb #x0138 #x87) ; ADD A,A
(memb #x0139 #x27) ; DAA
(memw #x013a #x3e91) ; LD A,91
(memb #x013c #x87) ; ADD A,A
(memb #x013d #x27) ; DAA
;
;  Add code here to test DAA with subtraction.
;
(memw #x013e #x3e11) ; LD A,11
(memw #x0140 #xd605) ; SUB A,5
(memb #x0142 #x27) ; DAA
(memw #x0143 #x3e11) ; LD A,11
(memw #x0145 #xd699) ; SUB A,99
(memb #x0147 #x27) ; DAA
;
(memb #x0148 #x76) ; HLT
;
;  Execute test
;
(print "==> Testing Miscellaneous instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; SCF  ;  Verify SCF instruction
;  Verify carry flag is set and all registers, except PC are zero.
;  PC should be 0101.
(test-reg RBC #x0000)
(test-reg RDE #x0000)
(test-reg RHL #x0000)
(test-reg RSP #x0000)
(test-reg RPC #x0101)
(test-reg RA #x00)
(test-mask #x01 MPSW)
(sim-step) ; CCF  ;  Verify CCF instruction
;  Verify carry flag is cleared and all registers, except PC are zero.
;  PC should be 0102.
(test-reg RBC #x0000)
(test-reg RDE #x0000)
(test-reg RHL #x0000)
(test-reg RSP #x0000)
(test-reg RPC #x0102)
(test-reg RA #x00)
(test-mask #x00 MPSW)
(sim-step) ; CPL  ;  Verify CPL instruction
;  Verify register A is FF, PC is 103, and all other registers are zero.
(test-reg RBC #x0000)
(test-reg RDE #x0000)
(test-reg RHL #x0000)
(test-reg RSP #x0000)
(test-reg RPC #x0103)
(test-reg RA #xff)
(test-mask #x02 MPSW)
(sim-step) ; NOP  ; Verify NOP
; Verify that registers are unchanged except PC is 104
(test-reg RBC #x0000)
(test-reg RDE #x0000)
(test-reg RHL #x0000)
(test-reg RSP #x0000)
(test-reg RPC #x0104)
(test-reg RA #xff)
(test-mask #x02 MPSW)
(sim-step) ; LD A,55  ; Load A with 55
; Verify A is 55
(test-reg RA #x55)
(sim-step)  ; LD (2000),A  ; Verify LD (2000),A
; Verify that memory location 2000 is 55
(test-memb #x2000 #x55)
(sim-step) ; XOR A,A  ; Clear A
; Verify that A is 0
(test-reg RA #x00)
(sim-step) ; LD A,(2000)  ; Verify LD A,(2000)
; Verify that A is 55
(test-reg RA #x55)
(sim-step) ; LD HL,AA55  ; Load AA55 into HL
; Verify that HL is AA55
(test-reg RHL #xaa55)
(sim-step) ; LD (2002),HL  ; Verify LD (2002),HL
; Verify that memory location 2002 is 55 and 2003 is AA
(test-memw #x2002 #x55aa)
(sim-step) ; LD HL,0  ; Load 0 into HL
; Verify that HL is 0
(test-reg RHL #x0000)
(sim-step) ; LD HL,(2002)  ; Verify LD HL,(2002)
; Verify that HL is AA55
(test-reg RHL #xaa55)
(sim-step) ; DI  ; Verify DI
; Verify that interrupts are disabled
(test-int-dis)
(sim-step) ; EI  ; Verify EI
; Verify that interrupts are disabled
(test-int-dis)
(sim-step) ; LD SP,2000 to initialize stack
; Verify that SP is 2000
(test-reg RSP #x2000)
; Verify that interrupts are enabled
(test-int-en)
;
(sim-step) ; RST 0  ; Verify RST 0
; Verify that PC is 0 and SP is 1FFE
(test-reg RPC #x0000)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x1f01)
(sim-step) ; RET  ; Return
; Verify that PC is 11F and SP is 2000
(test-reg RPC #x011f)
(test-reg RSP #x2000)
(sim-step) ; RST 1  ; Verify RST 1
; Verify that PC is 8 and SP is 1FFE
(test-reg RPC #x0008)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2001)
(sim-step) ; RET ; Return
; Verify that PC is 120 and SP is 2000
(test-reg RPC #x0120)
(test-reg RSP #x2000)
(sim-step) ; RST 2  ; Verify RST 2
; Verify that PC is 10 and SP is 1FFE
(test-reg RPC #x0010)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2101)
(sim-step) ; RET  ; Return
; Verify that PC is 121 and SP is 2000
(test-reg RPC #x0121)
(test-reg RSP #x2000)
(sim-step) ; RST 3 ; Verify RST 3
; Verify that PC is 18 and SP is 1FFE
(test-reg RPC #x0018)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2201)
(sim-step) ; RET  ; Return
; Verify that PC is 122 and SP is 2000
(test-reg RPC #x0122)
(test-reg RSP #x2000)
(sim-step) ; RST 4  ; Verify RST 4
; Verify that PC is 20 and SP is 1FFE
(test-reg RPC #x0020)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2301)
(sim-step) ; RET  ; Return
; Verify that PC is 123 and SP is 2000
(test-reg RPC #x0123)
(test-reg RSP #x2000)
(sim-step) ; RST 5  ; Verify RST 5
; Verify that PC is 28 and SP is 1FFE
(test-reg RPC #x0028)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2401)
(sim-step) ; RET  ; Return
; Verify that PC is 124 and SP is 2000
(test-reg RPC #x0124)
(test-reg RSP #x2000)
(sim-step) ; RST 6  ; Verify RST 6
; Verify that PC is 30 and SP is 1FFE
(test-reg RPC #x0030)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2501)
(sim-step) ; RET  ; Return
; Verify that PC is 125 and SP is 2000
(test-reg RPC #x0125)
(test-reg RSP #x2000)
(sim-step) ; RST 7  ; Verify RST 7
; Verify that PC is 38 and SP is 1FFE
(test-reg RPC #x0038)
(test-reg RSP #x1ffe)
(test-memw #x1ffe #x2601)
(sim-step) ; RET  ; Return
; Verify that PC is 126 and SP is 2000
(test-reg RPC #x0126)
(test-reg RSP #x2000)
;
(memw #x2000 #x0102)  ; Setup memory for next test
(memw #x2002 #x0000)
(sim-step) ; EX (SP),HL  ; Verify EX (SP),HL
; Verify that HL is 0201 and that memory 2000 is 55 and 2001 is AA
(test-reg RHL #x0201)
(test-memw #x2000 #x55aa)
(sim-step) ; LD HL,12C  ; Load HL
; Verify that HL is 12C, DE is 00 and PC is 12A
(test-reg RHL #x012c)
(test-reg RDE #x0000)
(test-reg RPC #x012a)
(sim-step) ; JP (HL)  ; Verify JP (HL)
; Verify that PC is 12C
(test-reg RDE #x0000)
(test-reg RHL #x012c)
(test-reg RPC #x012c)
(sim-step) ; LD SP,HL  ; Verify LD SP,HL
; Verify that SP is 12C and PC is 12D
(test-reg RDE #x0000)
(test-reg RHL #x012c)
(test-reg RSP #x012c)
(test-reg RPC #x012d)
(sim-step) ; EX DE,HL  ; Verify EX DE,HL
; Verify that DE is 12C, HL is 0, and PC is 12E
(test-reg RDE #x012c)
(test-reg RHL #x0000)
(test-reg RPC #x012e)
; Test I/O
(sim-step) ; OUT 10  ; Verify OUT 10
; Verify that 55 has been sent to port 10.
(test-out #x10 #x55)
(override-in #x10 #x20)
(sim-step) ; IN 10  ; Verify IN 10
; Verify that A is 20 and data read from port 10.
(test-reg RA #x20)
; Test DAA instruction
(print "Testing DAA instruction with addition")
(terpri)
(sim-step)  ; LD A,99  ; Load 99 into accumulator
; Verify accumulator is 99
(test-reg RA #x99)
(sim-step)  ; ADD A,A  ; Add accumulator to itself
; Verify Accumulator 32 is and C,P,&A flags are set
(test-reg RA #x32)
(test-mask #x15 MPSW)
(sim-step) ; DAA  ; Verify DAA
; Verify accumulator is 98
(test-reg RA #x98)
(sim-step)  ; LD A,19  ; Load 19 into accumulator
; Verify accumulator is 19
(test-reg RA #x19)
(sim-step) ; ADD A,A  ; Add accumulator to itself
; Verify accumulator is 32 and A flag is set
(test-reg RA #x32)
(test-mask #x10 MPSW)
(sim-step) ; DAA  ; Verify DAA
; Verify accumulator is 38 and no flags are set
(test-reg RA #x38)
(test-mask #x00 MPSW)
(sim-step)  ; LD A,91  ; Load 91 into accumulator
; Verify accumulator is 91
(test-reg RA #x91)
(sim-step) ; ADD A,A  ; Add accumulator to itself
; Verify accumulator is 22 and carry flag is set
(test-reg RA #x22)
(test-mask #x05 MPSW)
(sim-step) ; DAA  ; Verify DAA
; Verify accumulator is 82
(test-reg RA #x82)
(print "Testing DAA instruction with subtraction")
(terpri)
;  Test DAA instruction with subtraction
(sim-step) ; LD A,11
(test-reg RA #x11)
(sim-step) ; SUB A,5
(test-reg RA #x0c)
(test-reg RPC #x0142)
(test-mask #x12 MPSW)
;  11-5 should be 6.
(sim-step) ; DAA
(test-reg RA #x06)
(test-reg RPC #x0143)
(sim-step) ; LD A,11
(test-reg RA #x11)
(sim-step) ; SUB A,99
(test-reg RA #x78)
(test-mask #x13 MPSW)
; 11-99 should be -88
(sim-step) ; DAA
(test-reg RA #x88)
(test-mask #x87 MPSW)
;
; Test halt instruction
(test-reg RPC #x0148)
(sim-step) ; HLT ; Verify HLT
; Verify that registers are unchanged except PC is 13F
(test-reg RBC #x0000)
(test-reg RDE #x012c)
(test-reg RHL #x0000)
(test-reg RSP #x012c)
(test-reg RPC #x0149)
(test-reg RA #x88)
(if (halted)
   (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print "Simulation halted - PASS"))
   (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print "Simulation not halted - *** FAIL ***")))
(terpri)
(sim-step) ; HLT  ; Verify that CPU is halted
(test-reg RPC #x0149)
;-------------------------------------------------------------------------------
;  Test OR instructions
;
; Load memory
;
;  First set register values
(memw #x0100 #x06de) ; LD B,DE
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3612) ; LD (HL),12
(memw #x010e #x3e34) ; LD A,34
;
;  Test ORA instructions
(memb #x0110 #xb0) ; OR A,B
(memb #x0111 #xb1) ; OR A,C
(memb #x0112 #xaf) ; XOR A,A (Clear A)
(memb #x0113 #xb2) ; OR A,D
(memb #x0114 #xb3) ; OR A,E
(memb #x0115 #xaf) ; XOR A,A (Clear A)
(memb #x0116 #xb4) ; OR A,H
(memb #x0117 #xb5) ; OR A,L
(memb #x0118 #xb6) ; OR A,(HL)
(memb #x0119 #xb7) ; OR A,A (effectively a NOP)
;
;  Execute test
;
(print "==> Testing OR instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,DE
(sim-step) ; LD C,AD
(sim-step) ; LD D,BE
(sim-step) ; LD E,EF
(sim-step) ; LD H,10
(sim-step) ; LD L,01
(sim-step) ; LD (HL),12
(sim-step) ; LD A,34
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-memb #x1001 #x12)
(test-reg RA #x34)
(sim-step) ; OR A,B  ; Verify OR A,B
; Verify that register A is FE and PC is 111
(test-reg RA #xfe)
(test-reg RPC #x0111)
(sim-step) ; OR A,C  ; Verify OR A,C
; Verify that register A is FF and PC is 112
(test-reg RA #xff)
(test-reg RPC #x0112)
(sim-step) ; XOR A,A  ; Clear A
(test-reg RA #x00)
(sim-step) ; OR A,D  ; Verify OR A,D
; Verify that register A is BE and PC is 114
(test-reg RA #xbe)
(test-reg RPC #x114)
(sim-step) ; OR A,E  ; Verify OR A,E
; Verify that register A is FF and PC is 115
(test-reg RA #xff)
(test-reg RPC #x0115)
(sim-step) ; XOR A,A  ; Clear A
(test-reg RA #x00)
(sim-step) ; OR A,H  ; Verify OR A,H
; Verify that register A is 10 and PC is 117
(test-reg RA #x10)
(test-reg RPC #x0117)
(sim-step) ; OR A,L  ; Verify OR A,L
; Verify that register A is 11 and PC is 118
(test-reg RA #x11)
(test-reg RPC #x0118)
(sim-step) ; OR A,(HL)  ; Verify OR A,(HL)
; Verify that register A is 13 and PC is 119
(test-reg RA #x13)
(test-reg RPC #x119)
(sim-step) ; OR A,A  ; Verify OR A,A
; Verify that register A is unchanged and PC is 11A
(test-reg RA #x13)
(test-reg RPC #x11a)
;-------------------------------------------------------------------------------
;  Test PUSH-POP instructions
;
; Load memory
;
(memw #x0100 #x3e34) ; LD A,34
(memb #x0102 #x01) ; LD BC,5678
(memw #x0103 #x7856)
(memb #x0105 #x11) ; LD DE,9ABC
(memw #x0106 #xbc9a)
(memb #x0108 #x21) ; LD HL,DEF0
(memw #x0109 #xf0de)
(memb #x010b #x31) ; LD SP,2000
(memw #x010c #x0020)
(memb #x010e #xc5) ; PUSH BC
(memb #x010f #xd5) ; PUSH DE
(memb #x0110 #xe5) ; PUSH HL
(memb #x0111 #xf5) ; PUSH AF
(memb #x0112 #xc1) ; POP BC
(memb #x0113 #xd1) ; POP DE
(memb #x0114 #xe1) ; POP HL
(memb #x0115 #xf1) ; POP AF
;
;  Execute test
;
(print "==> Testing PUSH-POP instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD A,34
(sim-step) ; LD BC,5678
(sim-step) ; LD DE,9ABC
(sim-step) ; LD HL,DEF0
(sim-step) ; LD SP,2000
(test-reg RA #x34)
(test-reg RBC #x5678)
(test-reg RDE #x9abc)
(test-reg RHL #xdef0)
(test-reg RSP #x2000)
(test-reg RPC #x010e)
;
;  Test PUSH x instructions
(sim-step) ; PUSH BC ; Verify PUSH BC
; Verify that memory location 1FFE is 78, 1FFF is 56, SP
; is 1FFE, PC is 10F, and other registers unchanged.
(test-reg RA #x34)
(test-reg RBC #x5678)
(test-reg RDE #x9abc)
(test-reg RHL #xdef0)
(test-reg RSP #x1ffe)
(test-reg RPC #x010f)
(test-memw #x1ffe #x7856)
(sim-step) ; PUSH DE ; Verify PUSH DE
; Verify that memory location 1FFC is BC, 1FFD is 9A, SP
; is 1FFC, PC is 110, and other registers unchanged.
(test-reg RA #x34)
(test-reg RBC #x5678)
(test-reg RDE #x9abc)
(test-reg RHL #xdef0)
(test-reg RSP #x1ffc)
(test-reg RPC #x0110)
(test-memw #x1ffc #xbc9a)
(sim-step)  ; PUSH HL ; Verify PUSH HL
; Verify that memory location 1FFA is F0, 1FFB is DE, SP
; is 1FFA, PC is 111, and other registers unchanged.
(test-reg RA #x34)
(test-reg RBC #x5678)
(test-reg RDE #x9abc)
(test-reg RHL #xdef0)
(test-reg RSP #x1ffa)
(test-reg RPC #x0111)
(test-memw #x1ffa #xf0de)
(sim-step)  ; PUSH AF ; Verify PUSH AF
; Verify that memory location 1FF8 is 2A, 1FF9 is 34, SP
; is 1FF8, PC is 112, and other registers unchanged.
(test-reg RA #x34)
(test-reg RBC #x5678)
(test-reg RDE #x9abc)
(test-reg RHL #xdef0)
(test-reg RSP #x1ff8)
(test-reg RPC #x0112)
(test-memw #x1ff8 #x2834)
;  Test POP instructions
(sim-step) ; POP BC ; Verify POP BC
; verify that the register pair BC is 342A, SP is 1FFA, PC is 113
(test-reg RA #x34)
(test-reg RBC #x3428)
(test-reg RDE #x9abc)
(test-reg RHL #xdef0)
(test-reg RSP #x1ffa)
(test-reg RPC #x0113)
(sim-step)  ; POP DE  ; Verify POP DE
; verify that the register pair DE is DEF0, SP is 1FFC, PC is 114
(test-reg RA #x34)
(test-reg RBC #x3428)
(test-reg RDE #xdef0)
(test-reg RHL #xdef0)
(test-reg RSP #x1ffc)
(test-reg RPC #x0114)
(sim-step)  ; POP HL  ; Verify POP HL
; verify that the register pair HL is 9ABC, SP is 1FFE, PC is 115
(test-reg RA #x34)
(test-reg RBC #x3428)
(test-reg RDE #xdef0)
(test-reg RHL #x9abc)
(test-reg RSP #x1ffe)
(test-reg RPC #x0115)
(sim-step)  ; POP AF  ; Verify POP AF
; verify that register A is 56, Z&A flags are set, SP is 2000, PC is 116
(test-reg RA #x56)
(test-mask #x50 MPSW)
(test-reg RBC #x3428)
(test-reg RDE #xdef0)
(test-reg RHL #x9abc)
(test-reg RSP #x2000)
(test-reg RPC #x0116)
;-------------------------------------------------------------------------------
;  Test Rotate instructions
;
; Load memory
;
(memw #x0100 #x3e55) ; LD A,55
(memb #x0102 #x07) ; RLCA
(memb #x0103 #x07) ; RLCA
(memb #x0104 #x07) ; RLCA
(memb #x0105 #x17) ; RLA
(memb #x0106 #x17) ; RLA
(memb #x0107 #x17) ; RLA
(memw #x0108 #x3e55) ; LD A,55
(memb #x010a #x37) ; SCF
(memb #x010b #x3f) ; CCF
(memb #x010c #x0f) ; RRCA
(memb #x010d #x0f) ; RRCA
(memb #x010e #x0f) ; RRCA
(memb #x010f #x1f) ; RRA
(memb #x0110 #x1f) ; RRA
(memb #x0111 #x1f) ; RRA
;
;  Execute test
;
(print "==> Testing Rotate instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step)  ; LD A,55  ; Load accumulator
; Verify that A is 55
(test-reg RA #x55)
(sim-step)  ; RLCA  ; Verify RLCA
; Verify that A is AA and Carry is not set
(test-reg RA #xaa)
(test-mask #x00 #x01)
(sim-step)  ; RLCA  ; Verify RLCA
; Verify that A is 55 and Carry is set
(test-reg RA #x55)
(test-mask #x01 #x01)
(sim-step)  ; RLCA  ; Verify RLCA
; Verify that A is AA and Carry is not set
(test-reg RA #xaa)
(test-mask #x00 #x01)
(sim-step) ; RLA  ; Verify RLA
; Verify that A is 54 and Carry is set
(test-reg RA #x54)
(test-mask #x01 #x01)
(sim-step) ; RLA  ; Verify RLA
; Verify that A is A9 and Carry is not set
(test-reg RA #xa9)
(test-mask #x00 #x01)
(sim-step) ; RLA  ; Verify RLA
; Verify that A is 52 and Carry is set
(test-reg RA #x52)
(test-mask #x01 #x01)
; Reset A and Carry flag
(sim-step) ; LD A,55
(sim-step) ; SCF
(sim-step) ; CCF
; Verify that A is 55 and Carry is not set
(test-reg RA #x55)
(test-mask #x00 #x01)
(sim-step) ; RRCA  ; Verify RRCA
; Verify that A is AA and Carry is set
(test-reg RA #xaa)
(test-mask #x01 #x01)
(sim-step) ; RRCA  ; Verify RRCA
; Verify that A is 55 and Carry is not set
(test-reg RA #x55)
(test-mask #x00 #x01)
(sim-step) ; RRCA  ; Verify RRCA
; Verify that A is AA and Carry is set
(test-reg RA #xaa)
(test-mask #x01 #x01)
(sim-step) ; RRA  ; Verify RAR
; Verify that A is D5 and Carry is clear
(test-reg RA #xd5)
(test-mask #x00 #x01)
(sim-step) ; RRA  ; Verify RAR
; Verify that A is 6A and Carry is set
(test-reg RA #x6a)
(test-mask #x01 #x01)
(sim-step) ; RRA  ; Verify RAR
; Verify that A is B5 and Carry is clear
(test-reg RA #xb5)
(test-mask #x00 #x01)
;-------------------------------------------------------------------------------
;  Test SUB/SBC instructions
;
; Load memory
;
;  Verify SUB and SBC instructions
;  Initialize registers
(memw #x0100 #x06de) ; LD B,DE
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3612) ; LD M,12
(memw #x010e #x3e34) ; LD A,34
;
;  Test SUB x instructions
(memb #x0110 #x90) ; SUB A,B
(memb #x0111 #x91) ; SUB A,C
(memb #x0112 #x92) ; SUB A,D
(memb #x0113 #x93) ; SUB A,E
(memb #x0114 #x94) ; SUB A,H
(memb #x0115 #x95) ; SUB A,L
(memb #x0116 #x96) ; SUB A,(HL) (address in HL (12A), contents 0E)
(memb #x0117 #x97) ; SUB A,A
;
;  Test SBB x instructions
(memb #x0118 #x98) ; SBC A,B
(memb #x0119 #x99) ; SBC A,C
(memb #x011a #x9a) ; SBC A,D
(memb #x011b #x9b) ; SBC A,E
(memb #x011c #x9c) ; SBC A,H
(memb #x011d #x9d) ; SBC A,L
(memb #x011e #x9e) ; SBC A,(HL) (address in HL (12A), contents 0E)
(memb #x011f #x9f) ; SBC A,A
;
;  Execute test
;
(print "==> Testing SUB/SBC instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,DE
(sim-step) ; LD C,AD
(sim-step) ; LD D,BE
(sim-step) ; LD E,EF
(sim-step) ; LD H,10
(sim-step) ; LD L,01
(sim-step) ; LD M,12
(sim-step) ; LD A,34
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-memb #x1001 #x12)
(test-reg RA #x34)
;
(sim-step) ; SUB A,B  ; Verify SUB A,B
; Verify that A is 56, PC is 111, A,P,&C flags are set
(test-reg RA #x56)
(test-reg RPC #x0111)
(test-mask #x13 MPSW)
(sim-step) ; SUB A,C  ; Verify SUB A,C
; Verify that A is A9, PC is 112, S,A,P,&C flags are set
(test-reg RA #xa9)
(test-reg RPC #x0112)
(test-mask #x97 MPSW)
(sim-step) ; SUB A,D  ; Verify SUB A,D
; Verify that A is EB, PC is 113, S,A,P,&C flags are set
(test-reg RA #xeb)
(test-reg RPC #x0113)
(test-mask #x93 MPSW)
(sim-step) ; SUB A,E  ; Verify SUB A,E
; Verify that A is FC, PC is 114, S,A,P,&C flags are set
(test-reg RA #xfc)
(test-reg RPC #x0114)
(test-mask #x93 MPSW)
(sim-step) ; SUB A,H  ; Verify SUB A,H
; Verify that A is EC, PC is 115, S flag is set
(test-reg RA #xec)
(test-reg RPC #x0115)
(test-mask #x82 MPSW)
(sim-step) ; SUB A,L  ; Verify SUB A,L
; Verify that A is EB, PC is 116, S&P flags are set
(test-reg RA #xeb)
(test-reg RPC #x0116)
(test-mask #x82 MPSW)
(sim-step) ; SUB A,(HL)  ; Verify SUB A,(HL)
; Verify that A is D9, PC is 117, S flag is set
(test-reg RA #xd9)
(test-reg RPC #x0117)
(test-mask #x82 MPSW)
(sim-step)  ; SUB A,A  ; Verify SUB A,A
; Verify that A is 00, PC is 118, Z&P flags are set
(test-reg RA #x00)
(test-reg RPC #x0118)
(test-mask #x42 MPSW)
;
(sim-step) ; SBC A,B  ; Verify SBC A,B
; Verify that A is 22, PC is 119, A,P,&C flags are set
(test-reg RA #x22)
(test-reg RPC #x0119)
(test-mask #x13 MPSW)
(sim-step) ; SBC A,C  ; Verify SBC A,C
; Verify that A is 74, PC is 11A, A,P,&C flags are set
(test-reg RA #x74)
(test-reg RPC #x011a)
(test-mask #x13 MPSW)
(sim-step) ; SBC A,D  ; Verify SBC A,D
; Verify that A is B5, PC is 11B, S,A,&C flags are set
(test-reg RA #xb5)
(test-reg RPC #x011b)
(test-mask #x97 MPSW)
(sim-step) ; SBC A,E  ; Verify SBC A,E
; Verify that A is C5, PC is 11C, S,A,P,&C flags are set
(test-reg RA #xc5)
(test-reg RPC #x011c)
(test-mask #x93 MPSW)
(sim-step) ; SBC A,H  ; Verify SBC A,H
; Verify that A is B4, PC is 11D, S&P flags are set
(test-reg RA #xb4)
(test-reg RPC #x011d)
(test-mask #x82 MPSW)
(sim-step) ; SBC A,L  ; Verify SBC A,L
; Verify that A is B3, PC is 11E, S flag is set
(test-reg RA #xb3)
(test-reg RPC #x011e)
(test-mask #x82 MPSW)
(sim-step) ; SBC A,(HL)  ; Verify SBC A,(HL)
; Verify that A is A1, PC is 11F, S flag is set
(test-reg RA #xa1)
(test-reg RPC #x011f)
(test-mask #x82 MPSW)
(sim-step) ; SBC A,A  ; Verify SBC A,A
; Verify that A is 00, PC is 120, Z&P flags are set
(test-reg RA #x00)
(test-reg RPC #x0120)
(test-mask #x42 MPSW)
;-------------------------------------------------------------------------------
;  Test XOR instructions
;
; Load memory
;
;  First set register values
;
(memw #x0100 #x06de) ; LD B,DE
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3612) ; LD (HL),12
(memw #x010e #x3e34) ; LD A,34
;
;  Test XRA instructions
(memb #x0110 #xa8) ; XOR A,B
(memb #x0111 #xa9) ; XOR A,C
(memb #x0112 #xaa) ; XOR A,D
(memb #x0113 #xab) ; XOR A,E
(memb #x0114 #xac) ; XOR A,H
(memb #x0115 #xad) ; XOR A,L
(memb #x0116 #xae) ; XOR A,(HL) (address 1001, contents 12)
(memb #x0117 #xaf) ; XOR A,A
;
;  Execute test
;
(print "==> Testing XOR instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,DE
(sim-step) ; LD C,AD
(sim-step) ; LD D,BE
(sim-step) ; LD E,EF
(sim-step) ; LD H,10
(sim-step) ; LD L,01
(sim-step) ; LD (HL),12
(sim-step) ; LD A,34
(test-reg RBC #xdead)
(test-reg RDE #xbeef)
(test-reg RHL #x1001)
(test-memb #x1001 #x12)
(test-reg RA #x34)
(sim-step) ; XOR A,B  ; Verify XOR A,B
; Verify that register A is EA, and PC is 111
(test-reg RA #xea)
(test-reg RPC #x0111)
(sim-step) ; XOR A,C  ; Verify XOR A,C
; Verify that register A is 47, and PC is 112
(test-reg RA #x47)
(test-reg RPC #x0112)
(sim-step) ; XOR A,D  ; Verify XOR A,D
; Verify that register A is F9, and PC is 113
(test-reg RA #xf9)
(test-reg RPC #x0113)
(sim-step) ; XOR A,E  ; Verify XOR A,E
; Verify that register A is 16, and PC is 114
(test-reg RA #x16)
(test-reg RPC #x0114)
(sim-step) ; XOR A,H  ; Verify XOR A,H
; Verify that register A is 06, and PC is 115
(test-reg RA #x06)
(test-reg RPC #x0115)
(sim-step) ; XOR A,L ; Verify XOR A,L
; Verify that register A is 07, and PC is 116
(test-reg RA #x07)
(test-reg RPC #x0116)
(sim-step) ; XOR A,(HL)  ; Verify XOR A,(HL)
; Verify that register A is 15, and PC is 117
(test-reg RA #x15)
(test-reg RPC #x0117)
(sim-step) ; XOR A,A  ; Verify XOR A,A
; Verify that register A is zero, and PC is 118
(test-reg RA #x00)
(test-reg RPC #x0118)
;===============================================================================
;  Test Z-80 instructions
;
; Test EX AF,AF'
(memw #x0100 #x3e34) ; LD A,34
(memb #x0102 #x08) ; EX AF,AF'
(memw #x0103 #x3e56) ; LD A,56
(memb #x0105 #x37) ; SCF
(memb #x0106 #x08) ; EX AF,AF'
; Test EXX
(memb #x0107 #x01) ; LD BC,1234
(memw #x0108 #x3412)
(memb #x010a #x11) ; LD DE,5678
(memw #x010b #x7856)
(memb #x010d #x21) ; LD HL,90ab
(memw #x010e #xab90)
(memb #x0110 #xd9) ; EXX
(memb #x0111 #x01) ; LD BC,4321
(memw #x0112 #x2143)
(memb #x0114 #x11) ; LD DE,8765
(memw #x0115 #x6587)
(memb #x0117 #x21) ; LD HL,ba09
(memw #x0118 #x09ba)
(memb #x011a #xd9) ; EXX
;
;  Execute test
;
(print "==> Testing Z-80 instructions EX AF,AF' and EXX")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD A,34
(test-reg RA #x34)
(test-mask #x00 MPSW)
(sim-step) ; EX AF,AF'
(test-reg RAP #x34)
(sim-step) ; LD A,56
(test-reg RA #x56)
(sim-step) ; SCF
(test-mask #x01 MPSW)
(sim-step) ; EX AF,AF'
(test-reg RA #x34)
(test-reg RAP #x56)
(test-mask #x00 MPSW)
(test-reg RPSWP #x29)
;
(sim-step) ; LD BC,1234
(test-reg RBC #x1234)
(sim-step) ; LD DE,5678
(test-reg RDE #x5678)
(sim-step) ; LD HL,90ab
(test-reg RHL #x90ab)
(sim-step) ; EXX
(sim-step) ; LD BC,4321
(test-reg RBC #x4321)
(sim-step) ; LD DE,8765
(test-reg RDE #x8765)
(sim-step) ; LD HL,ba09
(test-reg RHL #xba09)
(sim-step) ; EXX
(test-reg RBC #x1234)
(test-reg RDE #x5678)
(test-reg RHL #x90ab)
(test-reg RBCP #x4321)
(test-reg RDEP #x8765)
(test-reg RHLP #xba09)
;-------------------------------------------------------------------------------
;
; Test DJNZ
(memw #x0100 #x0603) ; LD B,03
(memw #x0102 #x3e00) ; LD A,00
(memb #x0104 #x3c) ; INC A
(memw #x0105 #x10fd) ; DJNZ -3
;
;  Execute test
;
(print "==> Testing Z-80 instructions DJNZ")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,03
(test-reg RB #x03)
(sim-step) ; LD A,00
(test-reg RA #x00)
(sim-step) ; INC A
(test-reg RA #x01)
(sim-step) ; DJNZ -3
(test-reg RB #x02)
(test-reg RPC #x0104)
(sim-step) ; INC A
(test-reg RA #x02)
(sim-step) ; DJNZ -3
(test-reg RB #x01)
(test-reg RPC #x0104)
(sim-step) ; INC A
(test-reg RA #x03)
(sim-step) ; DJNZ -3
(test-reg RB #x00)
(test-reg RPC #x0107)
;-------------------------------------------------------------------------------
;
; Test Relative jumps
(memw #x0100 #x1801) ; JR +1
(memb #x0102 #x76) ; HALT
(memb #x0103 #x37) ; SCF
(memw #x0104 #x3801) ; JR C,+1
(memb #x0106 #x76) ; HALT
(memb #x0107 #x3F) ; CCF
(memw #x0108 #x3001) ; JR NC,+1
(memb #x010a #x76) ; HALT
(memb #x010b #xaf) ; XOR A,A
(memw #x010c #x2801) ; JR Z,+1
(memb #x010e #x76) ; HALT
(memb #x010f #x3c) ; INC A
(memw #x0110 #x2001) ; JR NZ,+1
(memb #x0112 #x76) ; HALT
(memb #x0113 #x00) ; NOP
;
;  Execute test
;
(print "==> Testing Z-80 relative jumps")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; JR +1
(test-reg RPC #x0103)
(sim-step) ; SCF
(test-mask #x01 #x01)
(sim-step) ; JR C,+1
(test-reg RPC #x0107)
(sim-step) ; CCF
(test-mask #x00 #x01)
(sim-step) ; JR NC,+1
(test-reg RPC #x010b)
(sim-step) ; XOR A,A
(test-reg RA #x00)
(test-mask #x40 #x40)
(sim-step) ; JR Z,+1
(test-reg RPC #x010f)
(sim-step) ; INC A
(test-reg RA #x01)
(test-mask #x00 #x40)
(sim-step) ; JR NZ,+1
(test-reg RPC #x0113)
(sim-step) ; NOP
(test-reg RPC #x0114)
(if (halted)
   (progn (setq *FAIL-COUNT* (+ *FAIL-COUNT* 1)) (print "Simulation halted - *** FAIL ***"))
   (progn (setq *PASS-COUNT* (+ *PASS-COUNT* 1)) (print "Simulation not halted - PASS")))
(terpri)
;-------------------------------------------------------------------------------
;
;  Test Z-80 instructions RLC r, RLC (HL), RRC r, RRC (HL)
(memw #x0100 #x0683) ; LD B,83
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3e34) ; LD A,34
;
(memw #x010e #xcb00) ; RLC B
(memw #x0110 #xcb01) ; RLC C
(memw #x0112 #xcb02) ; RLC D
(memw #x0114 #xcb03) ; RLC E
(memw #x0116 #xcb04) ; RLC H
(memw #x0118 #xcb05) ; RLC L
(memw #x011a #xcb06) ; RLC (HL)
(memw #x011c #xcb07) ; RLC A
;
(memw #x011e #xcb08) ; RRC B
(memw #x0120 #xcb09) ; RRC C
(memw #x0122 #xcb0a) ; RRC D
(memw #x0124 #xcb0b) ; RRC E
(memw #x0126 #xcb0e) ; RRC (HL)
(memw #x0128 #xcb0c) ; RRC H
(memw #x012a #xcb0d) ; RRC L
(memw #x012c #xcb0f) ; RRC A
;
(memb #x2002 #xa5)   ; Memory value
;
;  Execute test
;
(print "==> Testing Z-80 instructions RLC r, RLC (HL), RRC r, RRC (HL)")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,83
(sim-step) ; LD C,AD
(sim-step) ; LD D,BE
(sim-step) ; LD E,EF
(sim-step) ; LD H,10
(sim-step) ; LD L,01
(sim-step) ; LD A,34
(test-reg RA #x34)
(test-reg RB #x83)
(test-reg RC #xad)
(test-reg RD #xbe)
(test-reg RE #xef)
(test-reg RH #x10)
(test-reg RL #x01)
(sim-step) ; RLC B
(test-reg RB #x07)
(test-mask #x01 MPSW)
(sim-step) ; RLC C
(test-reg RC #x5B)
(test-mask #x01 MPSW)
(sim-step) ; RLC D
(test-reg RD #x7D)
(test-mask #x05 MPSW)
(sim-step) ; RLC E
(test-reg RE #xDF)
(test-mask #x81 MPSW)
(sim-step) ; RLC H
(test-reg RH #x20)
(test-mask #x00 MPSW)
(sim-step) ; RLC L
(test-reg RL #x02)
(test-mask #x00 MPSW)
(sim-step) ; RLC (HL)
(test-memb #x2002 #x4b)
(test-mask #x05 MPSW)
(sim-step) ; RLC A
(test-reg RA #x68)
(test-mask #x00 MPSW)
;
(sim-step) ; RRC B
(test-reg RB #x83)
(test-mask #x81 MPSW)
(sim-step) ; RRC C
(test-reg RC #xAD)
(test-mask #x81 MPSW)
(sim-step) ; RRC D
(test-reg RD #xBE)
(test-mask #x85 MPSW)
(sim-step) ; RRC E
(test-reg RE #xEF)
(test-mask #x81 MPSW)
(sim-step) ; RRC (HL)
(test-memb #x2002 #xa5)
(test-mask #x85 MPSW)
(sim-step) ; RRC H
(test-reg RH #x10)
(test-mask #x00 MPSW)
(sim-step) ; RRC L
(test-reg RL #x01)
(test-mask #x00 MPSW)
(sim-step) ; RRC A
(test-reg RA #x34)
(test-mask #x00 MPSW)
;-------------------------------------------------------------------------------
;
;  Test Rotate Left and Right
(memw #x0100 #x0683) ; LD B,83
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3e34) ; LD A,34
;
(memw #x010e #xcb10) ; RL B
(memw #x0110 #xcb11) ; RL C
(memw #x0112 #xcb12) ; RL D
(memw #x0114 #xcb13) ; RL E
(memw #x0116 #xcb14) ; RL H
(memw #x0118 #xcb15) ; RL L
(memw #x011a #xcb16) ; RL (HL)
(memw #x011c #xcb17) ; RL A
;
(memw #x011e #xcb18) ; RR B
(memw #x0120 #xcb19) ; RR C
(memw #x0122 #xcb1a) ; RR D
(memw #x0124 #xcb1b) ; RR E
(memw #x0126 #xcb1e) ; RR (HL)
(memw #x0128 #xcb1c) ; RR H
(memw #x012a #xcb1d) ; RR L
(memw #x012c #xcb1f) ; RR A
;
(memb #x2102 #xa5)   ; Memory value
;
;  Execute test
;
(print "==> Testing Z-80 instructions RL r, RL (HL), RR r, RR (HL)")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,83
(sim-step) ; LD C,AD
(sim-step) ; LD D,BE
(sim-step) ; LD E,EF
(sim-step) ; LD H,10
(sim-step) ; LD L,01
(sim-step) ; LD A,34)
(test-reg RA #x34)
(test-reg RB #x83)
(test-reg RC #xad)
(test-reg RD #xbe)
(test-reg RE #xef)
(test-reg RH #x10)
(test-reg RL #x01)
(sim-step) ; RL B
(test-reg RB #x06)
(test-mask #x05 MPSW)
(sim-step) ; RL C
(test-reg RC #x5B)
(test-mask #x01 MPSW)
(sim-step) ; RL D
(test-reg RD #x7D)
(test-mask #x05 MPSW)
(sim-step) ; RL E
(test-reg RE #xDF)
(test-mask #x81 MPSW)
(sim-step) ; RL H
(test-reg RH #x21)
(test-mask #x04 MPSW)
(sim-step) ; RL L
(test-reg RL #x02)
(test-mask #x00 MPSW)
(sim-step) ; RL (HL)
(test-memb #x2102 #x4a)
(test-mask #x01 MPSW)
(sim-step) ; RL A
(test-reg RA #x69)
(test-mask #x04 MPSW)
;
(sim-step) ; RR B
(test-reg RB #x03)
(test-mask #x04 MPSW)
(sim-step) ; RR C
(test-reg RC #x2D)
(test-mask #x05 MPSW)
(sim-step) ; RR D
(test-reg RD #xBE)
(test-mask #x85 MPSW)
(sim-step) ; RR E
(test-reg RE #xEF)
(test-mask #x81 MPSW)
(sim-step) ; RR (HL)
(test-memb #x2102 #xa5)
(test-mask #x84 MPSW)
(sim-step) ; RR H
(test-reg RH #x10)
(test-mask #x01 MPSW)
(sim-step) ; RR L
(test-reg RL #x81)
(test-mask #x84 MPSW)
(sim-step) ; RR A
(test-reg RA #x34)
(test-mask #x01 MPSW)
;-------------------------------------------------------------------------------
;
;  Test Shift Left and Right arithmatic
(memw #x0100 #x0683) ; LD B,83
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3e34) ; LD A,34
;
(memw #x010e #xcb20) ; SLA B
(memw #x0110 #xcb21) ; SLA C
(memw #x0112 #xcb22) ; SLA D
(memw #x0114 #xcb23) ; SLA E
(memw #x0116 #xcb24) ; SLA H
(memw #x0118 #xcb25) ; SLA L
(memw #x011a #xcb26) ; SLA (HL)
(memw #x011c #xcb27) ; SLA A
;
(memw #x011e #xcb28) ; SRA B
(memw #x0120 #xcb29) ; SRA C
(memw #x0122 #xcb2a) ; SRA D
(memw #x0124 #xcb2b) ; SRA E
(memw #x0126 #xcb2e) ; SRA (HL)
(memw #x0128 #xcb2c) ; SRA H
(memw #x012a #xcb2d) ; SRA L
(memw #x012c #xcb2f) ; SRA A
;
(memb #x2002 #xa5)   ; Memory value
;
;  Execute test
;
(print "==> Testing Z-80 instructions SLA r, SLA (HL), SRA r, SRA (HL)")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,83
(sim-step) ; LD C,AD
(sim-step) ; LD D,BE
(sim-step) ; LD E,EF
(sim-step) ; LD H,10
(sim-step) ; LD L,01
(sim-step) ; LD A,34
(test-reg RA #x34)
(test-reg RB #x83)
(test-reg RC #xad)
(test-reg RD #xbe)
(test-reg RE #xef)
(test-reg RH #x10)
(test-reg RL #x01)
(sim-step) ; SLA B
(test-reg RB #x06)
(test-mask #x05 MPSW)
(sim-step) ; SLA C
(test-reg RC #x5A)
(test-mask #x05 MPSW)
(sim-step) ; SLA D
(test-reg RD #x7C)
(test-mask #x01 MPSW)
(sim-step) ; SLA E
(test-reg RE #xDE)
(test-mask #x85 MPSW)
(sim-step) ; SLA H
(test-reg RH #x20)
(test-mask #x00 MPSW)
(sim-step) ; SLA L
(test-reg RL #x02)
(test-mask #x00 MPSW)
(sim-step) ; SLA (HL)
(test-memb #x2002 #x4a)
(test-mask #x01 MPSW)
(sim-step) ; SLA A
(test-reg RA #x68)
(test-mask #x00 MPSW)
;
(sim-step) ; SRA B
(test-reg RB #x03)
(test-mask #x04 MPSW)
(sim-step) ; SRA C
(test-reg RC #x2D)
(test-mask #x04 MPSW)
(sim-step) ; SRA D
(test-reg RD #x3E)
(test-mask #x00 MPSW)
(sim-step) ; SRA E
(test-reg RE #xEF)
(test-mask #x80 MPSW)
(sim-step) ; SRA (HL)
(test-memb #x2002 #x25)
(test-mask #x00 MPSW)
(sim-step) ; SRA H
(test-reg RH #x10)
(test-mask #x00 MPSW)
(sim-step) ; SRA L
(test-reg RL #x01)
(test-mask #x00 MPSW)
(sim-step) ; SRA A
(test-reg RA #x34)
(test-mask #x00 MPSW)
;-------------------------------------------------------------------------------
;
;  Test Z-80 instructions SLL r (undocumented), SLL (HL) (undocumented), SRL r, SRL (HL)
(memw #x0100 #x0683) ; LD B,83
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2610) ; LD H,10
(memw #x010a #x2e01) ; LD L,01
(memw #x010c #x3e34) ; LD A,34
;
(memw #x010e #xcb30) ; SLL B
(memw #x0110 #xcb31) ; SLL C
(memw #x0112 #xcb32) ; SLL D
(memw #x0114 #xcb33) ; SLL E
(memw #x0116 #xcb34) ; SLL H
(memw #x0118 #xcb35) ; SLL L
(memw #x011a #xcb36) ; SLL (HL)
(memw #x011c #xcb37) ; SLL A
;
(memw #x011e #xcb38) ; SRL B
(memw #x0120 #xcb39) ; SRL C
(memw #x0122 #xcb3a) ; SRL D
(memw #x0124 #xcb3b) ; SRL E
(memw #x0126 #xcb3e) ; SRL (HL)
(memw #x0128 #xcb3c) ; SRL H
(memw #x012a #xcb3d) ; SRL L
(memw #x012c #xcb3f) ; SRL A
;
(memb #x2103 #xa5)   ; Memory value
;
;  Execute test
;
(print "==> Testing Z-80 instructions SLL r (undocumented), SLL (HL) (undocumented), SRL r, SRL (HL)")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,83
(sim-step) ; LD C,AD
(sim-step) ; LD D,BE
(sim-step) ; LD E,EF
(sim-step) ; LD H,10
(sim-step) ; LD L,01
(sim-step) ; LD A,34
(test-reg RA #x34)
(test-reg RB #x83)
(test-reg RC #xad)
(test-reg RD #xbe)
(test-reg RE #xef)
(test-reg RH #x10)
(test-reg RL #x01)
(sim-step) ; SLA B
(test-reg RB #x07)
(test-mask #x01 MPSW)
(sim-step) ; SLA C
(test-reg RC #x5b)
(test-mask #x01 MPSW)
(sim-step) ; SLA D
(test-reg RD #x7d)
(test-mask #x05 MPSW)
(sim-step) ; SLA E
(test-reg RE #xdf)
(test-mask #x81 MPSW)
(sim-step) ; SLA H
(test-reg RH #x21)
(test-mask #x04 MPSW)
(sim-step) ; SLA L
(test-reg RL #x03)
(test-mask #x04 MPSW)
(sim-step) ; SLA (HL)
(test-memb #x2103 #x4b)
(test-mask #x05 MPSW)
(sim-step) ; SLA A
(test-reg RA #x69)
(test-mask #x04 MPSW)
;
(sim-step) ; SRA B
(test-reg RB #x03)
(test-mask #x04 MPSW)
(sim-step) ; SRA C
(test-reg RC #x2D)
(test-mask #x04 MPSW)
(sim-step) ; SRA D
(test-reg RD #x3E)
(test-mask #x00 MPSW)
(sim-step) ; SRA E
(test-reg RE #x6F)
(test-mask #x04 MPSW)
(sim-step) ; SRA (HL)
(test-memb #x2002 #x25)
(test-mask #x00 MPSW)
(sim-step) ; SRA H
(test-reg RH #x10)
(test-mask #x00 MPSW)
(sim-step) ; SRA L
(test-reg RL #x01)
(test-mask #x00 MPSW)
(sim-step) ; SRA A
(test-reg RA #x34)
(test-mask #x00 MPSW)
;-------------------------------------------------------------------------------
;
;  Test Z-80 instructions BIT n,r, BIT n,(HL)
(memw #x0100 #x0683) ; LD B,83
(memw #x0102 #x0ead) ; LD C,AD
(memw #x0104 #x16be) ; LD D,BE
(memw #x0106 #x1eef) ; LD E,EF
(memw #x0108 #x2620) ; LD H,20
(memw #x010a #x2e02) ; LD L,02
(memw #x010c #x3e34) ; LD A,34
;
(memw #x010e #xcb40) ; BIT 0,B
(memw #x0110 #xcb48) ; BIT 1,B
(memw #x0112 #xcb50) ; BIT 2,B
(memw #x0114 #xcb58) ; BIT 3,B
(memw #x0116 #xcb60) ; BIT 4,B
(memw #x0118 #xcb68) ; BIT 5,B
(memw #x011a #xcb70) ; BIT 6,B
(memw #x011c #xcb78) ; BIT 7,B
(memw #x011e #xcb41) ; BIT 0,C
(memw #x0120 #xcb71) ; BIT 6,C
(memw #x0122 #xcb4a) ; BIT 1,D
(memw #x0124 #xcb72) ; BIT 6,D
(memw #x0126 #xcb43) ; BIT 0,E
(memw #x0128 #xcb63) ; BIT 4,E
(memw #x012a #xcb4c) ; BIT 1,H
(memw #x012c #xcb6c) ; BIT 5,H
(memw #x012e #xcb4d) ; BIT 1,L
(memw #x0130 #xcb65) ; BIT 3,L
(memw #x0132 #xcb46) ; BIT 0,(HL)
(memw #x0134 #xcb5e) ; BIT 3,(HL)
(memw #x0136 #xcb57) ; BIT 2,A
(memw #x0138 #xcb7f) ; BIT 7,A
;
(memb #x2002 #x43)
;
;  Execute test
;
(print "==> Testing Z-80 instructions BIT n,r, BIT n,(HL)")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,83
(sim-step) ; LD C,AD
(sim-step) ; LD D,BE
(sim-step) ; LD E,EF
(sim-step) ; LD H,10
(sim-step) ; LD L,01
(sim-step) ; LD A,34
(test-reg RA #x34)
(test-reg RB #x83)
(test-reg RC #xad)
(test-reg RD #xbe)
(test-reg RE #xef)
(test-reg RH #x20)
(test-reg RL #x02)
(sim-step) ; BIT 0,B
(test-mask #x10 MPSW)
(sim-step) ; BIT 1,B
(test-mask #x10 MPSW)
(sim-step) ; BIT 2,B
(test-mask #x50 MPSW)
(sim-step) ; BIT 3,B
(test-mask #x50 MPSW)
(sim-step) ; BIT 4,B
(test-mask #x50 MPSW)
(sim-step) ; BIT 5,B
(test-mask #x50 MPSW)
(sim-step) ; BIT 6,B
(test-mask #x50 MPSW)
(sim-step) ; BIT 7,B
(test-mask #x10 MPSW)
(sim-step) ; BIT 0,C
(test-mask #x10 MPSW)
(sim-step) ; BIT 6,C
(test-mask #x50 MPSW)
(sim-step) ; BIT 1,D
(test-mask #x10 MPSW)
(sim-step) ; BIT 6,D
(test-mask #x50 MPSW)
(sim-step) ; BIT 0,E
(test-mask #x10 MPSW)
(sim-step) ; BIT 4,E
(test-mask #x50 MPSW)
(sim-step) ; BIT 1,H
(test-mask #x50 MPSW)
(sim-step) ; BIT 5,H
(test-mask #x10 MPSW)
(sim-step) ; BIT 1,L
(test-mask #x10 MPSW)
(sim-step) ; BIT 4,L
(test-mask #x50 MPSW)
(sim-step) ; BIT 0,(HL)
(test-mask #x10 MPSW)
(sim-step) ; BIT 3,(HL)
(test-mask #x50 MPSW)
(sim-step) ; BIT 2,A
(test-mask #x10 MPSW)
(sim-step) ; BIT 7,A
(test-mask #x50 MPSW)
;-------------------------------------------------------------------------------
;
;  Test bit clear instructions
(memw #x0100 #x06ff) ; LD B,FF
(memw #x0102 #x0eff) ; LD C,FF
(memw #x0104 #x16ff) ; LD D,FF
(memw #x0106 #x1eff) ; LD E,FF
(memw #x0108 #x26ff) ; LD H,FF
(memw #x010a #x2eff) ; LD L,FF
(memw #x010c #x3eff) ; LD A,FF
;
(memw #x010e #xcb80) ; RES 0,B
(memw #x0110 #xcb88) ; RES 1,B
(memw #x0112 #xcb90) ; RES 2,B
(memw #x0114 #xcb98) ; RES 3,B
(memw #x0116 #xcba0) ; RES 4,B
(memw #x0118 #xcba8) ; RES 5,B
(memw #x011a #xcbb0) ; RES 6,B
(memw #x011c #xcbb8) ; RES 7,B
(memw #x011e #xcb81) ; RES 0,C
(memw #x0120 #xcbb1) ; RES 6,C
(memw #x0122 #xcb8a) ; RES 1,D
(memw #x0124 #xcbb2) ; RES 6,D
(memw #x0126 #xcb83) ; RES 0,E
(memw #x0128 #xcba3) ; RES 4,E
(memw #x012a #xcb8c) ; RES 1,H
(memw #x012c #xcbac) ; RES 5,H
(memw #x012e #xcb8d) ; RES 1,L
(memw #x0130 #xcb9d) ; RES 3,L
(memw #x0132 #xcb86) ; RES 0,(HL)
(memw #x0134 #xcb9e) ; RES 3,(HL)
(memw #x0136 #xcb97) ; RES 2,A
(memw #x0138 #xcbbf) ; RES 7,A
;
(memb #xddf5 #xff)
;
;  Execute test
;
(print "==> Testing Z-80 instructions RES n,r, RES n,(HL)")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,FF
(sim-step) ; LD C,FF
(sim-step) ; LD D,FF
(sim-step) ; LD E,FF
(sim-step) ; LD H,FF
(sim-step) ; LD L,FF
(sim-step) ; LD A,FF
(test-reg RA #xFF)
(test-reg RB #xFF)
(test-reg RC #xFF)
(test-reg RD #xFF)
(test-reg RE #xFF)
(test-reg RH #xFF)
(test-reg RL #xFF)
(sim-step) ; RES 0,B
(test-reg RB #xfe)
(sim-step) ; RES 1,B
(test-reg RB #xfc)
(sim-step) ; RES 2,B
(test-reg RB #xf8)
(sim-step) ; RES 3,B
(test-reg RB #xf0)
(sim-step) ; RES 4,B
(test-reg RB #xe0)
(sim-step) ; RES 5,B
(test-reg RB #xc0)
(sim-step) ; RES 6,B
(test-reg RB #x80)
(sim-step) ; RES 7,B
(test-reg RB #x00)
(sim-step) ; RES 0,C
(test-reg RC #xfe)
(sim-step) ; RES 6,C
(test-reg RC #xbe)
(sim-step) ; RES 1,D
(test-reg RD #xfd)
(sim-step) ; RES 6,D
(test-reg RD #xbd)
(sim-step) ; RES 0,E
(test-reg RE #xfe)
(sim-step) ; RES 4,E
(test-reg RE #xee)
(sim-step) ; RES 1,H
(test-reg RH #xfd)
(sim-step) ; RES 5,H
(test-reg RH #xdd)
(sim-step) ; RES 1,L
(test-reg RL #xfd)
(sim-step) ; RES 3,L
(test-reg RL #xf5)
(sim-step) ; RES 0,(HL)
(test-memb #xddf5 #xfe)
(sim-step) ; RES 3,(HL)
(test-memb #xddf5 #xf6)
(sim-step) ; RES 2,A
(test-reg RA #xfb)
(sim-step) ; RES 7,A
(test-reg RA #x7b)
;-------------------------------------------------------------------------------
;
;  Test bit set instructions
(memw #x0100 #x0600) ; LD B,00
(memw #x0102 #x0e00) ; LD C,00
(memw #x0104 #x1600) ; LD D,00
(memw #x0106 #x1e00) ; LD E,00
(memw #x0108 #x2600) ; LD H,00
(memw #x010a #x2e00) ; LD L,00
(memw #x010c #x3e00) ; LD A,00
;
(memw #x010e #xcbc0) ; SET 0,B
(memw #x0110 #xcbc8) ; SET 1,B
(memw #x0112 #xcbd0) ; SET 2,B
(memw #x0114 #xcbd8) ; SET 3,B
(memw #x0116 #xcbe0) ; SET 4,B
(memw #x0118 #xcbe8) ; SET 5,B
(memw #x011a #xcbf0) ; SET 6,B
(memw #x011c #xcbf8) ; SET 7,B
(memw #x011e #xcbc1) ; SET 0,C
(memw #x0120 #xcbf1) ; SET 6,C
(memw #x0122 #xcbca) ; SET 1,D
(memw #x0124 #xcbf2) ; SET 6,D
(memw #x0126 #xcbc3) ; SET 0,E
(memw #x0128 #xcbe3) ; SET 4,E
(memw #x012a #xcbcc) ; SET 1,H
(memw #x012c #xcbec) ; SET 5,H
(memw #x012e #xcbcd) ; SET 1,L
(memw #x0130 #xcbdd) ; SET 3,L
(memw #x0132 #xcbc6) ; SET 0,(HL)
(memw #x0134 #xcbde) ; SET 3,(HL)
(memw #x0136 #xcbd7) ; SET 2,A
(memw #x0138 #xcbff) ; SET 7,A
;
(memb #x220a #x00)
;
;  Execute test
;
(print "==> Testing Z-80 instructions SET n,r, SET n,(HL)")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,00
(sim-step) ; LD C,00
(sim-step) ; LD D,00
(sim-step) ; LD E,00
(sim-step) ; LD H,00
(sim-step) ; LD L,00
(sim-step) ; LD A,00
(test-reg RA #x00)
(test-reg RB #x00)
(test-reg RC #x00)
(test-reg RD #x00)
(test-reg RE #x00)
(test-reg RH #x00)
(test-reg RL #x00)
(sim-step) ; SET 0,B
(test-reg RB #x01)
(sim-step) ; SET 1,B
(test-reg RB #x03)
(sim-step) ; SET 2,B
(test-reg RB #x07)
(sim-step) ; SET 3,B
(test-reg RB #x0f)
(sim-step) ; SET 4,B
(test-reg RB #x1f)
(sim-step) ; SET 5,B
(test-reg RB #x3f)
(sim-step) ; SET 6,B
(test-reg RB #x7f)
(sim-step) ; SET 7,B
(test-reg RB #xff)
(sim-step) ; SET 0,C
(test-reg RC #x01)
(sim-step) ; SET 6,C
(test-reg RC #x41)
(sim-step) ; SET 1,D
(test-reg RD #x02)
(sim-step) ; SET 6,D
(test-reg RD #x42)
(sim-step) ; SET 0,E
(test-reg RE #x01)
(sim-step) ; SET 4,E
(test-reg RE #x11)
(sim-step) ; SET 1,H
(test-reg RH #x02)
(sim-step) ; SET 5,H
(test-reg RH #x22)
(sim-step) ; SET 1,L
(test-reg RL #x02)
(sim-step) ; SET 3,L
(test-reg RL #x0a)
(sim-step) ; SET 0,(HL)
(test-memb #x220a #x01)
(sim-step) ; SET 3,(HL)
(test-memb #x220a #x09)
(sim-step) ; RES 2,A
(test-reg RA #x04)
(sim-step) ; RES 7,A
(test-reg RA #x84)
;-------------------------------------------------------------------------------
;
;  Test index registers
(memw #x0100 #xdd21) ; LD IX,1000
(memw #x0102 #x0010)
(memw #x0104 #xdd7e) ; LD A,(IX-1)
(memb #x0106 #xff)
(memb #x0107 #x21) ; LD HL,1234
(memw #x0108 #x3412)
(memw #x010a #xfd21) ; LD IY,2000
(memw #x010c #x0020)
(memw #x010e #xfd7e) ; LD A,(IY+2)
(memb #x0110 #x02)
(memw #x0111 #xdd66) ; LD H,(IX+1)
(memb #x0113 #x01)
(memw #x0114 #xfd6e) ; LD L,(IY-2)
(memb #x0116 #xfe)
(memw #x0117 #xdd77) ; LD (IX+6),A
(memb #x0119 #x6)
(memw #x011a #xdd23) ; INC IX
(memw #x011c #xfd2b) ; DEC IY
(memw #x011e #xdd24) ; INC IXh
(memw #x0120 #xfd24) ; INC IYh
(memw #x0122 #xfd2d) ; DEC IYl
(memw #x0124 #xdd2d) ; DEC IXl
;
(memb #x0ffe #x01)
(memb #x0fff #x02)
(memb #x1000 #x03)
(memb #x1001 #x04)
(memb #x1002 #x05)
(memb #x1ffe #x1f)
(memb #x1fff #x2f)
(memb #x2000 #x3f)
(memb #x2001 #x4f)
(memb #x2002 #x5f)
;
;  Execute test
;
(print "==> Testing Z-80 index register instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; IX Prefix
(sim-step) ; LD IX,1000
(test-reg RIX #x1000)
(sim-step) ; IX Prefix
(sim-step) ; LD A,(IX-1)
(test-reg RA #x02)
(sim-step) ; LD HL,1234
(test-reg RHL #x1234)
(sim-step) ; IY Prefix
(sim-step) ; LD IY,2000
(test-reg RIY #x2000)
(sim-step) ; IY Prefix
(sim-step) ; LD A,(IY+2)
(test-reg RA #x5f)
(sim-step) ; IX Prefix
(sim-step) ; LD H,(IX+1)
(test-reg RH #x04)
(sim-step) ; IY Prefix
(sim-step) ; LD L,(IY-2)
(test-reg RL #x1f)
(sim-step) ; IX Prefix
(sim-step) ; LD (IX+6),A
(test-memb #x1006 #x5f)
(sim-step) ; IX Prefix
(sim-step) ; INC IX
(test-reg RIX #x1001)
(sim-step) ; IY Prefix
(sim-step) ; DEC IY
(test-reg RIY #x1fff)
(sim-step) ; IX Prefix
(sim-step) ; INC IXh
(test-reg RIX #x1101)
(sim-step) ; IY Prefix
(sim-step) ; INC IYh
(test-reg RIY #x20ff)
(sim-step) ; IY Prefix
(sim-step) ; DEC IYl
(test-reg RIY #x20fe)
(sim-step) ; IX Prefix
(sim-step) ; DEC IXl
(test-reg RIX #x1100)
;-------------------------------------------------------------------------------
;
;  Test input/output instructions
(memw #x0100 #x0600) ; LD B,00
(memw #x0102 #x0e10) ; LD C,10
(memw #x0104 #xed40) ; IN B,(C)
(memw #x0106 #x0e11) ; LD C,11
(memw #x0108 #xed60) ; IN H,(C)
(memw #x010a #xed70) ; IN F,(C)
(memw #x010c #x0e20) ; LD C,20
(memw #x010e #x0655) ; LD B,55
(memw #x0110 #x1eaa) ; LD E,AA
(memw #x0112 #xed41) ; OUT (C),B
(memw #x0114 #xed59) ; OUT (C),E
(memw #x0116 #xed71) ; OUT (C),0
;
;  Execute test
;
(print "==> Testing Z-80 EB Input/Output instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD B,00
(sim-step) ; LD C,10
(test-reg RB #x00)
(test-reg RC #x10)
(override-in #x10 #x20)
(sim-step) ; IN B,(C)
(test-reg RB #x20)
(test-reg RC #x10)
(test-mask #x00 MPSW)
(sim-step) ; LD C,11
(override-in #x11 #x82)
(sim-step) ; IN H,(C)
(test-reg RH #x82)
(test-mask #x84 MPSW)
(override-in #x11 #xff)
(sim-step) ; IN F,(C)
(test-mask #x84 MPSW)
;
(sim-step) ; LD C,20
(sim-step) ; LD B,55
(sim-step) ; LD E,AA
(test-reg RB #x55)
(test-reg RC #x20)
(test-reg RE #xaa)
(sim-step) ; OUT (C),B
(test-out #x20 #x55)
(sim-step) ; OUT (C),E
(test-out #x20 #xaa)
(sim-step) ; OUT (C),0
(test-out #x20 #x00)
;-------------------------------------------------------------------------------
;
;  Test SBC instructions
(memb #x0100 #x01) ; LD BC,1
(memw #x0101 #x0100)
(memb #x0103 #x11) ; LD DE,9ABC
(memw #x0104 #xbc9a)
(memb #x0106 #x21) ; LD HL,8000
(memw #x0107 #x0080)
(memb #x0109 #x31) ; LXI SP,2000
(memw #x010a #x0020)
(memb #x010c #x37) ; SCF
(memb #x010d #x3f) ; CCF
(memw #x010e #xed42) ; SBC HL,BC
(memb #x0110 #x37) ; SCF
(memw #x0111 #xed42) ; SBC HL,BC
(memw #x0113 #xed62) ; SBC HL,HL
(memw #x0115 #xed42) ; SBC HL,BC
;
;  Execute test
;
(print "==> Testing Z-80 EB 16-bit SBC instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,1
(sim-step) ; LD DE,9abc
(sim-step) ; LD HL,8000
(sim-step) ; LD SP,2000
(test-reg RBC #x0001)
(test-reg RDE #x9abc)
(test-reg RHL #x8000)
(test-reg RSP #x2000)
(sim-step) ; SCF
(sim-step) ; CCF
(test-mask #x00 MPSW)
(sim-step) ; SBC HL,BC
(test-reg RHL #x7fff)
(test-mask #x12 MPSW)
(sim-step) ; SCF
(test-mask #x11 MPSW)
(sim-step) ; SBC HL,BC
(test-reg RHL #x7ffd)
(test-mask #x02 MPSW)
(sim-step) ; SBC HL,HL
(test-reg RHL #x0000)
(test-mask #x42 MPSW)
(sim-step) ; SBC HL,BC
(test-reg RHL #xffff)
(test-mask #x93 MPSW)
;-------------------------------------------------------------------------------
;
;  Test LD (nn),dd and LD dd,(nn) instructions
(memw #x0100 #xed4b) ; LD BC,(1000)
(memw #x0102 #x0010)
(memw #x0104 #xed5b) ; LD DE,(1002)
(memw #x0106 #x0210)
(memw #x0108 #xed6b) ; LD HL,(1004)
(memw #x010a #x0410)
(memw #x010c #xed7b) ; LD SP,(1006)
(memw #x010e #x0610)
;
(memw #x0110 #xed43) ; LD (2000),BC
(memw #x0112 #x0020)
(memw #x0114 #xed53) ; LD (2002),DE
(memw #x0116 #x0220)
(memw #x0118 #xed63) ; LD (2004),HL
(memw #x011a #x0420)
(memw #x011c #xed73) ; LD (2006),SP
(memw #x011e #x0620)
;
(memw #x1000 #x3412)
(memw #x1002 #x7856)
(memw #x1004 #xbc9a)
(memw #x1006 #xadde)
;
(memw #x2000 #x0000)
(memw #x2002 #x0000)
(memw #x2004 #x0000)
(memw #x2006 #x0000)
;
;  Execute test
;
(print "==> Testing Z-80 EB LD (nn),dd and LD dd,(nn) instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,(1000)
(test-reg RBC #x1234)
(sim-step) ; LD DE,(1002)
(test-reg RDE #x5678)
(sim-step) ; LD HL,(1004)
(test-reg RHL #x9abc)
(sim-step) ; LD SP,(1006)
(test-reg RSP #xdead)
;
(sim-step) ; LD (2000),BC
(test-memw #x2000 #x3412)
(sim-step) ; LD (2002),DE
(test-memw #x2002 #x7856)
(sim-step) ; LD (2004),HL
(test-memw #x2004 #xbc9a)
(sim-step) ; LD (2006),SP
(test-memw #x2006 #xadde)
;-------------------------------------------------------------------------------
;
;  Test ADC instructions
(memb #x0100 #x01) ; LD BC,1
(memw #x0101 #x0100)
(memb #x0103 #x11) ; LD DE,9ABC
(memw #x0104 #xbc9a)
(memb #x0106 #x21) ; LD HL,8000
(memw #x0107 #x0080)
(memb #x0109 #x31) ; LD SP,2000
(memw #x010a #x0020)
(memb #x010c #x37) ; SCF
(memb #x010d #x3f) ; CCF
(memw #x010e #xed4A) ; ADC HL,BC
(memb #x0110 #x37) ; SCF
(memw #x0111 #xed4A) ; ADC HL,BC
(memw #x0113 #xed6A) ; ADC HL,HL
(memw #x0115 #xed4A) ; ADC HL,BC
(memb #x0117 #x21) ; LD HL,800
(memw #x0118 #x0008)
(memw #x011a #xed6a) ; ADC HL,HL
;
;  Execute test
;
(print "==> Testing Z-80 EB 16-bit ADC instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,1
(sim-step) ; LD DE,9abc
(sim-step) ; LD HL,8000
(sim-step) ; LD SP,2000
(test-reg RBC #x0001)
(test-reg RDE #x9abc)
(test-reg RHL #x8000)
(test-reg RSP #x2000)
(sim-step) ; SCF
(sim-step) ; CCF
(test-mask #x00 MPSW)
(sim-step) ; ADC HL,BC
(test-reg RHL #x8001)
(test-mask #x80 MPSW)
(sim-step) ; SCF
(test-mask #x81 MPSW)
(sim-step) ; ADC HL,BC
(test-reg RHL #x8003)
(test-mask #x80 MPSW)
(sim-step) ; ADC HL,HL
(test-reg RHL #x0006)
(test-mask #x01 MPSW)
(sim-step) ; ADC HL,BC
(test-reg RHL #x0008)
(test-mask #x00 MPSW)
(sim-step) ; LD HL,800
(test-reg RHL #x0800)
(sim-step) ; ADC HL,HL
(test-reg RHL #x1000)
(test-mask #x10 MPSW)
;-------------------------------------------------------------------------------
;
;  Test NEG instruction
(memw #x0100 #x3e00) ; LD A,00
(memw #x0102 #xed44) ; NEG
(memw #x0104 #x3e80) ; LD A,80
(memw #x0106 #xed44) ; NEG
(memw #x0108 #x3eff) ; LD A,FF
(memw #x010a #xed44) ; NEG
(memw #x010c #x3e7f) ; LD A,7F
(memw #x010e #xed44) ; NEG
;
;  Execute test
;
(print "==> Testing Z-80 NEG instruction")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD A,00
(test-reg RA #x00)
(sim-step) ; NEG
(test-reg RA #x00)
(test-mask #x52 MPSW)
(sim-step) ; LD A,80
(test-reg RA #x80)
(sim-step) ; NEG
(test-reg RA #x80)
(test-mask #x97 MPSW)
(sim-step) ; LD A,FF
(test-reg RA #xFF)
(sim-step) ; NEG
(test-reg RA #x01)
(test-mask #x03 MPSW)
(sim-step) ; LD A,7F
(test-reg RA #x7f)
(sim-step) ; NEG
(test-reg RA #x81)
(test-mask #x83 MPSW)
;-------------------------------------------------------------------------------
;
;  Test RETN/RETI instructions
;  Note that these can't be fully tested until interrupt processing is
;  implemented.  Right now, just test that these properly return from a
;  CALL instruction.
(memb #x0100 #x31) ; LD SP,3000
(memw #x0101 #x0030)
(memb #x0103 #xcd) ; CALL 2000
(memw #x0104 #x0020) ;
(memb #x0106 #xcd) ; CALL 2002
(memw #x0107 #x0220)
(memb #x0109 #x00) ; NOP
;
(memw #x2000 #xed45) ; RETN
(memw #x2002 #xed4d) ; RETI
;
;  Execute test
;
(print "==> Testing Z-80 RETN/RETI instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD SP,3000
(test-reg RSP #x3000)
(sim-step) ; CALL 2000
(test-reg RSP #x2ffe)
(test-reg RPC #x2000)
(test-memw #x2ffe #x0601)
(sim-step) ; RETN
(test-reg RSP #x3000)
(test-reg RPC #x0106)
(sim-step) ; CALL 2002
(test-reg RSP #x2ffe)
(test-reg RPC #x2002)
(test-memw #x2ffe #x0901)
(sim-step) ; RETI
(test-reg RSP #x3000)
(test-reg RPC #x0109)
;-------------------------------------------------------------------------------
;
; Test LD I,A; LD R,A; LD A,I; and LD A,R instructions
(memb #x0100 #xf3)   ; DI
(memw #x0101 #x3e55) ; LD A,55
(memw #x0103 #xed47) ; LD I,A
(memw #x0105 #x3e00) ; LD A,00
(memw #x0107 #xed57) ; LD A,I
;
(memw #x0109 #x3eaa) ; LD A,AA
(memw #x010b #xed4f) ; LD R,A
(memw #x010d #x3e00) ; LD A,00
(memw #x010f #xed5f) ; LD A,R
;
(memb #x0111 #xfb)   ; EI
(memw #x0112 #x3e55) ; LD A,55
(memw #x0114 #xed47) ; LD I,A
(memw #x0116 #x3e00) ; LD A,00
(memw #x0118 #xed57) ; LD A,I
;
(memw #x011a #x3eaa) ; LD A,AA
(memw #x011c #xed4f) ; LD R,A
(memw #x011e #x3e00) ; LD A,00
(memw #x0120 #xed5f) ; LD A,R
;
;  Execute test
;
(terpri)
(print "==> Testing Z-80 LD I and R instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; DI
(sim-step) ; LD A,55
(test-reg RA #x55)
(sim-step) ; LD I,A
(test-reg RI #x55)
(sim-step) ; LD A,00
(test-reg RA #x00)
(test-reg RI #x55)
(sim-step) ; LD A,I
(test-reg RA #x55)
(test-mask #x00 MPSW)
;
(sim-step) ; LD A,AA
(test-reg RA #xaa)
(sim-step) ; LD R,A
(test-reg RM #xaa)
(sim-step) ; LD A,00
(test-reg RA #x00)
(sim-step) ; LD A,R
(test-reg RA #xac)
(test-mask #x80 MPSW)
;
(sim-step) ; EI
(sim-step) ; LD A,55
(test-reg RA #x55)
(sim-step) ; LD I,A
(test-reg RI #x55)
(sim-step) ; LD A,00
(test-reg RA #x00)
(test-reg RI #x55)
(sim-step) ; LD A,I
(test-reg RA #x55)
(test-mask #x04 MPSW)
;
(sim-step) ; LD A,AA
(test-reg RA #xaa)
(sim-step) ; LD R,A
(test-reg RM #xaa)
(sim-step) ; LD A,00
(test-reg RA #x00)
(sim-step) ; LD A,R
(test-reg RA #xac)
(test-mask #x84 MPSW)
;-------------------------------------------------------------------------------
;
;  Test RLD and RRD instructions
(memw #x0100 #x3e12) ; LD A,12
(memb #x0102 #x21) ; LD HL,1000
(memw #x0103 #x0010)
(memw #x0105 #xed67) ; RRD
(memw #x0107 #xed6f) ; RLD
;
(memb #x1000 #x34)
;
;  Execute test
;
(terpri)
(print "==> Testing Z-80 RLD and RRD instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD A,12
(test-reg RA #x12)
(sim-step) ; LD HL,1000
(test-reg RHL #x1000)
(sim-step) ; RRD
(test-reg RA #x14)
(test-memb #x1000 #x23)
(test-mask #x04 MPSW)
(sim-step) ; RLD
(test-reg RA #x12)
(test-memb #x1000 #x34)
(test-mask #x04 MPSW)
;-------------------------------------------------------------------------------
;
;  Test LDI and LDIR instructions
(memb #x0100 #x01) ; LD BC,0003
(memw #x0101 #x0300)
(memb #x0103 #x11) ; LD DE,2000
(memw #x0104 #x0020)
(memb #x0106 #x21) ; LD HL,1000
(memw #x0107 #x0010)
(memw #x0109 #xeda0) ; LDI
(memw #x010b #xedb0) ; LDIR
;
(memb #x1000 #x12)
(memb #x1001 #x23)
(memb #x1002 #x34)
(memb #x1003 #x45)
(memb #x1004 #x56)
(memb #x1005 #x67)
(memb #x1006 #x78)
(memb #x1007 #x89)
(memw #x1FFE #x0000)
(memw #x2000 #x0000)
(memw #x2002 #x0000)
(memw #x2004 #x0000)
(memw #x2006 #x0000)
(memw #x2008 #x0000)
;
;  Execute test
;
(print "==> Testing Z-80 LDI and LDIR instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,0003
(test-reg RBC #x0003)
(sim-step) ; LD DE,2000
(test-reg RDE #x2000)
(sim-step) ; LD HL,1000
(test-reg RHL #x1000)
(sim-step) ; LDI
(test-memb #x2000 #x12)
(test-reg RBC #x0002)
(test-reg RDE #x2001)
(test-reg RHL #x1001)
(test-mask #x04 MPSW)
(sim-step) ; LDIR
(test-memb #x2001 #x23)
(test-reg RBC #x0001)
(test-reg RDE #x2002)
(test-reg RHL #x1002)
(test-reg RPC #x010b)
(test-mask #x04 MPSW)
(sim-step) ; LDIR
(test-memb #x2002 #x34)
(test-reg RBC #x0000)
(test-reg RDE #x2003)
(test-reg RHL #x1003)
(test-reg RPC #x010d)
(test-mask #x00 MPSW)
;-------------------------------------------------------------------------------
;
;  Test LDD and LDDR instructions
(memb #x0100 #x01) ; LD BC,0003
(memw #x0101 #x0300)
(memb #x0103 #x11) ; LD DE,2007
(memw #x0104 #x0720)
(memb #x0106 #x21) ; LD HL,1007
(memw #x0107 #x0710)
(memw #x0109 #xeda8) ; LDD
(memw #x010b #xedb8) ; LDDR
;
(memb #x1000 #x12)
(memb #x1001 #x23)
(memb #x1002 #x34)
(memb #x1003 #x45)
(memb #x1004 #x56)
(memb #x1005 #x67)
(memb #x1006 #x78)
(memb #x1007 #x89)
(memw #x1FFE #x0000)
(memw #x2000 #x0000)
(memw #x2002 #x0000)
(memw #x2004 #x0000)
(memw #x2006 #x0000)
(memw #x2008 #x0000)
;
;  Execute test
;
(print "==> Testing Z-80 LDD and LDDR instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,0003
(test-reg RBC #x0003)
(sim-step) ; LD DE,2007
(test-reg RDE #x2007)
(sim-step) ; LD HL,1007
(test-reg RHL #x1007)
(sim-step) ; LDD
(test-memb #x2007 #x89)
(test-reg RBC #x0002)
(test-reg RDE #x2006)
(test-reg RHL #x1006)
(test-mask #x04 MPSW)
(sim-step) ; LDDR
(test-memb #x2006 #x78)
(test-reg RBC #x0001)
(test-reg RDE #x2005)
(test-reg RHL #x1005)
(test-reg RPC #x010b)
(test-mask #x04 MPSW)
(sim-step) ; LDDR
(test-memb #x2005 #x67)
(test-reg RBC #x0000)
(test-reg RDE #x2004)
(test-reg RHL #x1004)
(test-reg RPC #x010d)
(test-mask #x00 MPSW)
;-------------------------------------------------------------------------------
;
;  Test CPI and CPIR instructions
(memb #x0100 #x01) ; LD BC,0005
(memw #x0101 #x0500)
(memb #x0103 #x21) ; LD HL,1000
(memw #x0104 #x0010)
(memw #x0106 #x3e45) ; LD A,45
(memw #x0108 #xeda1) ; CPI
(memw #x010a #xedb1) ; CPIR
(memw #x010c #x0000) ; NOP, NOP
;
(memb #x1000 #x12)
(memb #x1001 #x23)
(memb #x1002 #x34)
(memb #x1003 #x45)
(memb #x1004 #x56)
(memb #x1005 #x67)
(memb #x1006 #x78)
(memb #x1007 #x89)
;
;  Execute test
;
(print "==> Testing Z-80 CPI and CPIR instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,0005
(test-reg RBC #x0005)
(sim-step) ; LD HL,1000
(test-reg RHL #x1000)
(sim-step) ; LD A,45
(test-reg RA #x45)
(sim-step) ; CPI
(test-reg RBC #x0004)
(test-reg RHL #x1001)
(test-mask #x06 MPSW)
(sim-step) ; CPIR
(test-reg RBC #x0003)
(test-reg RHL #x1002)
(test-reg RPC #x010a)
(test-mask #x06 MPSW)
(sim-step) ; CPIR
(test-reg RBC #x0002)
(test-reg RHL #x1003)
(test-reg RPC #x010a)
(test-mask #x06 MPSW)
(sim-step) ; CPIR
(test-reg RBC #x0001)
(test-reg RHL #x1004)
(test-reg RPC #x010c)
(test-mask #x46 MPSW)
(sim-step) ; NOP
(test-reg RBC #x0001)
(test-reg RHL #x1004)
(test-reg RPC #x010d)
(test-mask #x46 MPSW)
;-------------------------------------------------------------------------------
;
;  Test CPD and CPDR instructions
(memb #x0100 #x01) ; LD BC,0005
(memw #x0101 #x0500)
(memb #x0103 #x21) ; LD HL,1007
(memw #x0104 #x0710)
(memw #x0106 #x3e56) ; LD A,56
(memw #x0108 #xeda9) ; CPD
(memw #x010a #xedb9) ; CPDR
(memw #x010c #x0000) ; NOP, NOP
;
(memb #x1000 #x12)
(memb #x1001 #x23)
(memb #x1002 #x34)
(memb #x1003 #x45)
(memb #x1004 #x56)
(memb #x1005 #x67)
(memb #x1006 #x78)
(memb #x1007 #x89)
;
;  Execute test
;
(print "==> Testing Z-80 CPD and CPDR instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,0005
(test-reg RBC #x0005)
(sim-step) ; LD HL,1007
(test-reg RHL #x1007)
(sim-step) ; LD A,56
(test-reg RA #x56)
(sim-step) ; CPD
(test-reg RBC #x0004)
(test-reg RHL #x1006)
(test-mask #x97 MPSW)
(sim-step) ; CPDR
(test-reg RBC #x0003)
(test-reg RHL #x1005)
(test-reg RPC #x010a)
(test-mask #x97 MPSW)
(sim-step) ; CPDR
(test-reg RBC #x0002)
(test-reg RHL #x1004)
(test-reg RPC #x010a)
(test-mask #x97 MPSW)
(sim-step) ; CPDR
(test-reg RBC #x0001)
(test-reg RHL #x1003)
(test-reg RPC #x010c)
(test-mask #x46 MPSW)
(sim-step) ; NOP
(test-reg RBC #x0001)
(test-reg RHL #x1003)
(test-reg RPC #x010d)
(test-mask #x46 MPSW)
;-------------------------------------------------------------------------------
;
;  Test INI and INIR instructions
(memb #x0100 #x01) ; LD BC,0310
(memw #x0101 #x1003)
(memb #x0103 #x21) ; LD HL,1000
(memw #x0104 #x0010)
(memw #x0106 #xeda2) ; INI
(memw #x0108 #xedb2) ; INIR
(memw #x010a #x0000) ; NOP, NOP
;
(memw #x1000 #x0000)
(memb #x1002 #x0000)
(memb #x1004 #x0000)
(memb #x1006 #x0000)
(memb #x1008 #x0000)
(memb #x100a #x0000)
;
;  Execute test
;
(print "==> Testing Z-80 INI and INIR instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,0310
(test-reg RBC #x0310)
(sim-step) ; LD HL,1000
(test-reg RHL #x1000)
(override-in #x10 #x23)
(sim-step) ; INI
(test-memb #x1000 #x23)
(test-reg RBC #x0210)
(test-reg RHL #x1001)
(test-mask #x02 MPSW)
(override-in #x10 #x34)
(sim-step) ; INIR
(test-memb #x1001 #x34)
(test-reg RBC #x0110)
(test-reg RHL #x1002)
(test-reg RPC #x0108)
(test-mask #x02 MPSW)
(override-in #x10 #x45)
(sim-step) ; INIR
(test-memb #x1002 #x45)
(test-reg RBC #x0010)
(test-reg RHL #x1003)
(test-reg RPC #x010a)
(test-mask #x46 MPSW)
;-------------------------------------------------------------------------------
;
;  Test IND and INDR instructions
(memb #x0100 #x01) ; LD BC,0310
(memw #x0101 #x1003)
(memb #x0103 #x21) ; LD HL,1008
(memw #x0104 #x0810)
(memw #x0106 #xedaa) ; IND
(memw #x0108 #xedba) ; INDR
(memw #x010a #x0000) ; NOP, NOP
;
(memw #x1000 #x0000)
(memw #x1002 #x0000)
(memw #x1004 #x0000)
(memw #x1006 #x0000)
(memw #x1008 #x0000)
(memw #x100a #x0000)
;
;  Execute test
;
(print "==> Testing Z-80 INI and INIR instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,0310
(test-reg RBC #x0310)
(sim-step) ; LD HL,1008
(test-reg RHL #x1008)
(override-in #x10 #x23)
(sim-step) ; IND
(test-memb #x1008 #x23)
(test-reg RBC #x0210)
(test-reg RHL #x1007)
(test-mask #x02 MPSW)
(override-in #x10 #x34)
(sim-step) ; INDR
(test-memb #x1007 #x34)
(test-reg RBC #x0110)
(test-reg RHL #x1006)
(test-reg RPC #x0108)
(test-mask #x02 MPSW)
(override-in #x10 #x45)
(sim-step) ; INDR
(test-memb #x1006 #x45)
(test-reg RBC #x0010)
(test-reg RHL #x1005)
(test-reg RPC #x010a)
(test-mask #x46 MPSW)
;-------------------------------------------------------------------------------
;
;  Test OUTI and OTIR instructions
(memb #x0100 #x01) ; LD BC,0310
(memw #x0101 #x1003)
(memb #x0103 #x21) ; LD HL,1000
(memw #x0104 #x0010)
(memw #x0106 #xeda3) ; OUTI
(memw #x0108 #xedb3) ; OTIR
(memw #x010a #x0000) ; NOP, NOP
;
(memw #x1000 #x1234)
(memw #x1002 #x5678)
(memw #x1004 #x0000)
(memw #x1006 #x0000)
(memw #x1008 #x0000)
(memw #x100a #x0000)
;
;  Execute test
;
(print "==> Testing Z-80 OUTI and OTIR instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,0310
(test-reg RBC #x0310)
(sim-step) ; LD HL,1000
(test-reg RHL #x1000)
(sim-step) ; OUTI
(test-out #x10 #x12)
(test-reg RBC #x0210)
(test-reg RHL #x1001)
(test-mask #x02 MPSW)
(sim-step) ; OTIR
(test-out #x10 #x34)
(test-memb #x1001 #x34)
(test-reg RBC #x0110)
(test-reg RHL #x1002)
(test-reg RPC #x0108)
(test-mask #x02 MPSW)
(sim-step) ; OTIR
(test-out #x10 #x56)
(test-reg RBC #x0010)
(test-reg RHL #x1003)
(test-reg RPC #x010a)
(test-mask #x46 MPSW)
;-------------------------------------------------------------------------------
;
;  Test OUTD and OTDR instructions
(memb #x0100 #x01) ; LD BC,0310
(memw #x0101 #x1003)
(memb #x0103 #x21) ; LD HL,1004
(memw #x0104 #x0410)
(memw #x0106 #xedab) ; OUTD
(memw #x0108 #xedbb) ; OTDR
(memw #x010a #x0000) ; NOP, NOP
;
(memw #x1000 #x1234)
(memw #x1002 #x5678)
(memw #x1004 #x9abc)
(memw #x1006 #xdef0)
(memw #x1008 #x0000)
(memw #x100a #x0000)
;
;  Execute test
;
(print "==> Testing Z-80 OUTD and OTDR instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD BC,0310
(test-reg RBC #x0310)
(sim-step) ; LD HL,1004
(test-reg RHL #x1004)
(sim-step) ; OUTD
(test-out #x10 #x9a)
(test-reg RBC #x0210)
(test-reg RHL #x1003)
(test-mask #x02 MPSW)
(sim-step) ; OTDR
(test-out #x10 #x78)
(test-memb #x1001 #x34)
(test-reg RBC #x0110)
(test-reg RHL #x1002)
(test-reg RPC #x0108)
(test-mask #x02 MPSW)
(sim-step) ; OTDR
(test-out #x10 #x56)
(test-reg RBC #x0010)
(test-reg RHL #x1001)
(test-reg RPC #x010a)
(test-mask #x46 MPSW)
;-------------------------------------------------------------------------------
;  Test Interrupts IM 0
;
; Load memory
;
(memw #x0000 #xfbc9) ; EI/RET
(memw #x0008 #xfbc9) ; EI/RET
(memw #x0010 #xfbc9) ; EI/RET
(memw #x0018 #xfbc9) ; EI/RET
(memw #x0020 #xfbc9) ; EI/RET
(memw #x0028 #xfbc9) ; EI/RET
(memw #x0030 #xfbc9) ; EI/RET
(memw #x0038 #xfbc9) ; EI/RET
;
(memb #x00ff #xfb)  ;  EI
(memb #x0100 #x00)  ;  NOP
(memb #x0101 #x00)  ;  NOP
(memb #x0102 #x00)  ;  NOP
(memb #x0103 #x00)  ;  NOP
(memb #x0104 #x00)  ;  NOP
(memb #x0105 #x00)  ;  NOP
(memb #x0106 #x00)  ;  NOP
(memb #x0107 #x00)  ;  NOP
(memb #x0108 #xf3)  ;  DI
(memb #x0109 #x00)  ;  NOP
(memb #x010a #xfb)  ;  EI
(memb #x010b #x00)  ;  NOP
(memb #x010c #x00)  ;  NOP
(memb #x010d #x00)  ;  NOP
(memb #x010e #x00)  ;  NOP
(memb #x010f #x00)  ;  NOP
;
;  Execute test
;
(terpri)
(print "==> Testing IM 0 interrupt processing")
(terpri)
(sim-init)
(go #x00ff)
(sim-step) ; EI
(sim-step) ; NOP
(test-reg RPC #x0101)
(send-int #xc7) ; RST 0
(sim-step) ; RST 0
(test-reg RPC #x0000)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x0001)
(sim-step) ; RET
(test-reg RPC #x0101)
(test-int-en)
;
(sim-step) ; NOP
(test-reg RPC #x0102)
(send-int #xcf) ; RST 1
(sim-step) ; RST 1
(test-reg RPC #x0008)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x0009)
(sim-step) ; RET
(test-reg RPC #x0102)
(test-int-en)
;
(sim-step) ; NOP
(test-reg RPC #x0103)
(send-int #xd7) ; RST 2
(sim-step) ; RST 2
(test-reg RPC #x0010)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x0011)
(sim-step) ; RET
(test-reg RPC #x0103)
(test-int-en)
;
(sim-step) ; NOP
(test-reg RPC #x0104)
(send-int #xdf) ; RST 3
(sim-step) ; RST 3
(test-reg RPC #x0018)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x0019)
(sim-step) ; RET
(test-reg RPC #x0104)
(test-int-en)
;
(sim-step) ; NOP
(test-reg RPC #x0105)
(send-int #xe7) ; RST 4
(sim-step) ; RST 4
(test-reg RPC #x0020)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x0021)
(sim-step) ; RET
(test-reg RPC #x0105)
(test-int-en)
;
(sim-step) ; NOP
(test-reg RPC #x0106)
(send-int #xef) ; RST 5
(sim-step) ; RST 5
(test-reg RPC #x0028)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x0029)
(sim-step) ; RET
(test-reg RPC #x0106)
(test-int-en)
;
(sim-step) ; NOP
(test-reg RPC #x0107)
(send-int #xf7) ; RST 6
(sim-step) ; RST 6
(test-reg RPC #x0030)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x0031)
(sim-step) ; RET
(test-reg RPC #x0107)
(test-int-en)
;
(sim-step) ; NOP
(test-reg RPC #x0108)
(send-int #xff) ; RST 7
(sim-step) ; RST 7
(test-reg RPC #x0038)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x0039)
(sim-step) ; RET
(test-reg RPC #x0108)
(test-int-en)
;
(sim-step) ; DI
(test-reg RPC #x0109)
(test-int-dis)
(send-int #xdf) ; RST 3
(sim-step) ; NOP
(test-reg RPC #x010a)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x010b)
(test-int-dis)
(sim-step) ; NOP
(test-reg RPC #x010c)
(sim-step) ; RST 3
(test-reg RPC #x0018)
(test-int-dis)
;-------------------------------------------------------------------------------
;  Test Interrupts NMI
;
; Load memory
;
(memw #x0066 #xed45)  ;  RETN
;
(memb #x0100 #xf3)  ;  DI
(memb #x0101 #x00)  ;  NOP
(memb #x0102 #xfb)  ;  EI
(memb #x0103 #x00)  ;  NOP
(memb #x0104 #x00)  ;  NOP
;
;  Execute test
;
(terpri)
(print "==> Testing NMI interrupt processing")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; DI
(test-int-dis)
(test-reg RPC #x0101)
(send-int #xffffffff) ; Code for Z80 NMI
(sim-step) ; Process NMI
(test-reg RPC #x0066)
(test-int-dis)
(sim-step) ; RETN
(test-reg RPC #x0101)
(test-int-dis)
(sim-step) ; NOP
(test-reg RPC #x0102)
(sim-step) ; EI
(test-reg RPC #x0103)
(test-int-dis)
(sim-step) ; NOP
(test-int-en)
(test-reg RPC #x0104)
(send-int #xffffffff) ; Code for Z80 NMI
(sim-step) ; Process NMI
(test-reg RPC #x0066)
(test-int-dis)
(sim-step) ; RETN
(test-reg RPC #x0104)
(test-int-en)
(sim-step) ; NOP
(test-int-en)
(test-reg RPC #x0105)
;-------------------------------------------------------------------------------
;  Test Interrupts IM 1
;
; Load memory
;
(memw #x0038 #xfbc9) ; EI/RET
;
(memw #x0100 #xed56) ;  IM 1
(memb #x0102 #xfb)   ;  EI
(memb #x0103 #x00)   ;  NOP
(memb #x0104 #x00)   ;  NOP
(memb #x0105 #xf3)   ;  DI
(memb #x0106 #x00)   ;  NOP
(memb #x0107 #x00)   ;  NOP
;
;  Execute test
;
(terpri)
(print "==> Testing IM 1 interrupt processing")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; IM 1
(test-reg RPC #x0102)
(sim-step) ; EI
(test-reg RPC #x0103)
(sim-step) ; NOP
(test-reg RPC #x0104)
(test-int-en)
(send-int #xff) ; RST 7
(sim-step) ; Process interrupt
(test-reg RPC #x0038)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x0039)
(sim-step) ; RET
(test-reg RPC #x0104)
(test-int-en)
(sim-step) ; NOP
(test-reg RPC #x0105)
(send-int #xc7) ; RST 0
(sim-step) ; Process interrupt
(test-reg RPC #x0038)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x0039)
(sim-step) ; RET
(test-reg RPC #x0105)
(test-int-en)
(sim-step) ; DI
(test-reg RPC #x0106)
(test-int-dis)
(send-int #xc7) ; RST 0
(sim-step) ; NOP
(test-reg RPC #x0107)
(test-int-dis)
;-------------------------------------------------------------------------------
;  Test Interrupts IM 2
;
; Load memory
;
(memw #x0100 #x3e10)  ;  LD A,10
(memw #x0102 #xed47)  ;  LD I,A
(memw #x0104 #xed5e)  ;  IM 2
(memb #x0106 #xfb)    ;  EI
(memb #x0107 #x00)    ;  NOP
(memb #x0108 #x00)    ;  NOP
(memb #x0109 #xf3)    ;  DI
(memb #x010a #x00)    ;  NOP
(memb #x010b #x00)    ;  NOP
(memb #x010c #x00)    ;  NOP
(memb #x010d #x00)    ;  NOP
(memb #x010e #x00)    ;  NOP
(memb #x010f #x00)    ;  NOP
;
(memw #x1000 #x0020)  ;  Vector 0
(memw #x1002 #x0220)  ;  Vector 2
(memw #x1004 #x0420)  ;  Vector 4
;
(memw #x2000 #xfbc9)  ;  EI/RET
(memw #x2002 #xfbc9)  ;  EI/RET
(memw #x2004 #xfbc9)  ;  EI/RET
;
;  Execute test
;
(terpri)
(print "==> Testing IM 2 interrupt processing")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; LD A,10
(test-reg RA #x10)
(test-reg RPC #x0102)
(sim-step) ; LD I,A
(test-reg RI #x10)
(test-reg RPC #x0104)
(sim-step) ; IM 2
(test-reg RPC #x0106)
(sim-step) ; EI
(test-reg RPC #x0107)
(sim-step) ; NOP
(test-int-en)
(test-reg RPC #x0108)
(send-int #xff01)
(sim-step) ; Process interrupt
(test-reg RPC #x2000)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x2001)
(test-int-dis)
(sim-step) ; RET
(test-reg RPC #x0108)
(test-int-en)
(sim-step) ; NOP
(test-reg RPC #x0109)
(send-int #x04)
(sim-step) ; Process interrupt
(test-reg RPC #x2004)
(test-int-dis)
(sim-step) ; EI
(test-reg RPC #x2005)
(test-int-dis)
(sim-step) ; RET
(test-reg RPC #x0109)
(test-int-en)
(sim-step) ; DI
(test-reg RPC #x010a)
(test-int-dis)
(send-int #x02)
(sim-step) ; NOP
(test-reg RPC #x010b)
;-------------------------------------------------------------------------------
;  Test Interrupts between DD and FD prefixes and following instructions
;
; Load memory
;
(memw #x0038 #xfbc9) ; EI/RET
;
(memw #x0100 #xed56) ;  IM 1
(memb #x0102 #xfb)   ;  EI
(memw #x0103 #xdd21) ;  LD IX,1000
(memw #x0105 #x0010)
(memw #x0107 #xfd21) ;  LD IY,2000
(memw #x0109 #x0020)
(memb #x010b #x00)   ;  NOP
;
;  Execute test
;
(terpri)
(print "==> Testing interrupts between prefixes and following instructions")
(terpri)
(sim-init)
(go #x0100)
(sim-step) ; IM 1
(test-reg RPC #x0102)
(sim-step) ; EI
(test-reg RPC #x0103)
(sim-step) ; DD Prefix
(test-reg RPC #x0104)
(send-int 0)
(sim-step) ; LD IX,1000
(test-reg RIX #x1000)
(test-reg RPC #x0107)
(sim-step) ; Process interrupt
(test-reg RPC #x0038)
(sim-step) ; EI
(sim-step) ; RET
(test-reg RPC #x0107)
(sim-step) ; FD Prefix
(test-reg RPC #x0108)
(send-int 1)
(sim-step) ; LD IY,2000
(test-reg RIY #x2000)
(test-reg RPC #x010b)
(sim-step) ; Process interrupt
(test-reg RPC #x0038)
(sim-step) ; EI
(sim-step) ; RET
(test-reg RPC #x010b)
;
;===============================================================================
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
