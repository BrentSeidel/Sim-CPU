3
0
lisp
;
;  Lisp test cases for 6502 simulator
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
(setq RIX 2)
(setq RIY 3)
(setq RSP 4)
(setq RPC 5)
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
;  Status register bits are S|O|-|B|D|I|Z|C
;                           7 6 5 4 3 2 1 0
;
;  PSW mask for 6502 is DF
(setq MPSW #xDF)
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
;  Test LDA instructions
;
(memb #x0000 #xff)  ;  Data FF
(memb #x0001 #x00)  ;  Data 00
(memb #x0002 #x7f)  ;  Data 7F
;
(memw #x0080 #x0010)  ;  Address 1000
(memw #x0082 #x0110)  ;  Address 1001
(memw #x0084 #x0210)  ;  Address 1002
;
(memb #x1000 #x00)  ;  Data 00
(memb #x1001 #xff)  ;  Data FF
(memb #x1002 #x7f)  ;  Data 7F
;
(memb #x1040 #x00)  ;  Data 00
(memb #x1041 #xf0)  ;  Data F0
(memb #x1042 #x0f)  ;  Data 0F
;
(memw #x0200 #xa900)  ;  LDA #0
(memw #x0202 #xa901)  ;  LDA #1
(memw #x0204 #xa980)  ;  LDA #-128
;
(memw #x0206 #xa500)  ;  LDA 00
(memw #x0208 #xa501)  ;  LDA 01
(memw #x020a #xa502)  ;  LDA 02
;
(memb #x020c #xad)  ;  LDA 1000
(memw #x020d #x0010)
(memb #x020f #xad)  ;  LDA 1001
(memw #x0210 #x0110)
(memb #x0212 #xad)  ;  LDA 1002
(memw #x0213 #x0210)
;
(memw #x0215 #xa280)  ;  LDX #80
(memw #x0217 #xb580)  ;  LDA 80,X
(memw #x0219 #xb581)  ;  LDA 81,X
(memw #x021b #xb582)  ;  LDA 82,X
;
(memb #x021d #xbd)  ;  LDA F80,X
(memw #x021e #x800f)
(memb #x0220 #xbd)  ;  LDA F81,X
(memw #x0221 #x810f)
(memb #x0223 #xbd)  ;  LDA F82,X
(memw #x0224 #x820f)
;
(memw #x0226 #xa040)  ;  LDY #40
(memb #x0228 #xb9)  ;  LDA FC0,Y
(memw #x0229 #xc00f)
(memb #x022b #xb9)  ;  LDA FC1,Y
(memw #x022c #xc10f)
(memb #x022e #xb9)  ;  LDA FC2,Y
(memw #x022f #xc20f)
;
(memw #x0231 #xa100)  ;  LDA (0,X)
(memw #x0233 #xa102)  ;  LDA (2,X)
(memw #x0235 #xa104)  ;  LDA (4,X)
;
(memw #x0237 #xb180)  ;  LDA (80),Y
(memw #x0239 #xb182)  ;  LDA (82),Y
(memw #x023b #xb184)  ;  LDA (84),Y
;
;  Execute test
;
(print "==> Testing LDA immediate instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDA #0
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDA #1
(test-reg RA #x01)
(test-mask #x00 #x82)
(sim-step)  ;  LDA #-128
(test-reg RA #x80)
(test-mask #x80 #x82)
(print "==> Testing LDA zero page instruction")
(terpri)
(sim-step)  ;  LDA 00
(test-reg RA #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDA 01
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDA 02
(test-reg RA #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDA absolute instruction")
(terpri)
(sim-step)  ;  LDA 1000
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDA 1001
(test-reg RA #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDA 1002
(test-reg RA #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDA zero page, X instruction")
(terpri)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(sim-step)  ;  LDA 80,X
(test-reg RA #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDA 81,X
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDA 82,X
(test-reg RA #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDA absolute, X instruction")
(terpri)
(sim-step)  ;  LDA F80,X
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDA F81,X
(test-reg RA #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDA F82,X
(test-reg RA #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDA absolute, Y instruction")
(terpri)
(sim-step)  ;  LDY #40
(test-reg RIY #x40)
(sim-step)  ;  LDA FC0,Y
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDA FC1,Y
(test-reg RA #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDA FC2,Y
(test-reg RA #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDA (indirect, X) instruction")
(terpri)
(sim-step)  ;  LDA (0,X)
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDA (2,X)
(test-reg RA #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDA (4,X)
(test-reg RA #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDA (indirect), Y instruction")
(terpri)
(sim-step)  ;  LDA (80),Y
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDA (82),Y
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  LDA (84),Y
(test-reg RA #x0f)
(test-mask #x00 #x82)
;
;-------------------------------------------------------------------------------
;  Test LDX instructions
;
(memw #x0200 #xa200)  ;  LDX #0
(memw #x0202 #xa201)  ;  LDX #1
(memw #x0204 #xa280)  ;  LDX #-128
;
;  Execute test
;
(print "==> Testing LDX immediate instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDX #0
(test-reg RIX #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDX #1
(test-reg RIX #x01)
(test-mask #x00 #x82)
(sim-step)  ;  LDX #-128
(test-reg RIX #x80)
(test-mask #x80 #x82)
;
;-------------------------------------------------------------------------------
;  Test LDY instructions
;
(memw #x0200 #xa000)  ;  LDY #0
(memw #x0202 #xa001)  ;  LDY #1
(memw #x0204 #xa080)  ;  LDY #-128
;
;  Execute test
;
(print "==> Testing LDY immediate instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDY #0
(test-reg RIY #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDY #1
(test-reg RIY #x01)
(test-mask #x00 #x82)
(sim-step)  ;  LDY #-128
(test-reg RIY #x80)
(test-mask #x80 #x82)
;
;-------------------------------------------------------------------------------
;  Test TAX and TAY instructions
;
(memw #x0200 #xa900)  ;  LDA #0
(memb #x0202 #xaa)    ;  TAX
(memb #x0203 #xa8)    ;  TAY
(memw #x0204 #xa901)  ;  LDA #1
(memb #x0206 #xaa)    ;  TAX
(memb #x0207 #xa8)    ;  TAY
(memw #x0208 #xa980)  ;  LDA #-128
(memb #x020a #xaa)    ;  TAX
(memb #x020b #xa8)    ;  TAY
;
;  Execute test
;
(print "==> Testing TAX and TAY instructions")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDA #0
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  TAX
(test-reg RIX #x00)
(test-mask #x02 #x82)
(sim-step)  ;  TAY
(test-reg RIY #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDA #1
(test-reg RA #x01)
(test-mask #x00 #x82)
(sim-step)  ;  TAX
(test-reg RIX #x01)
(test-mask #x00 #x82)
(sim-step)  ;  TAY
(test-reg RIY #x01)
(test-mask #x00 #x82)
(sim-step)  ;  LDA #-128
(test-reg RA #x80)
(test-mask #x80 #x82)
(sim-step)  ;  TAX
(test-reg RIX #x80)
(test-mask #x80 #x82)
(sim-step)  ;  TAY
(test-reg RIY #x80)
(test-mask #x80 #x82)
;
;-------------------------------------------------------------------------------
;  Test flag instructions
;
(memb #x0200 #x18)  ;  CLC
(memb #x0201 #x38)  ;  SEC
(memb #x0202 #x18)  ;  CLC
;
(memb #x0203 #xd8)  ;  CLD
(memb #x0204 #xf8)  ;  SED
(memb #x0205 #xd8)  ;  CLD
;
(memb #x0206 #x58)  ;  CLI
(memb #x0207 #x78)  ;  SEI
(memb #x0208 #x58)  ;  CLI
;
;(memb #x0209 #xb8)  ;  CLV
;(memb #x020a #x
;(memb #x020b #xb8)  ;  CLV
;
;  Execute test
;
(print "==> Testing CLC and SEC instructions")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  CLC
(test-mask #x00 #x01)
(sim-step)  ;  SEC
(test-mask #x01 #x01)
(sim-step)  ;  CLC
(test-mask #x00 #x01)
(print "==> Testing CLD and SED instructions")
(terpri)
(sim-step)  ;  CLD
(test-mask #x00 #x08)
(sim-step)  ;  SED
(test-mask #x08 #x08)
(sim-step)  ;  CLD
(test-mask #x00 #x08)
(print "==> Testing CLI and SEI instructions")
(terpri)
(sim-step)  ;  CLI
(test-mask #x00 #x04)
(sim-step)  ;  SEI
(test-mask #x04 #x04)
(sim-step)  ;  CLI
(test-mask #x00 #x04)
;-------------------------------------------------------------------------------
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
