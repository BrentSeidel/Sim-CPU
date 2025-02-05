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
;
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
(memw #x0200 #xa900)  ;  LDA #00
(memw #x0202 #xa901)  ;  LDA #01
(memw #x0204 #xa980)  ;  LDA #80
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
(memb #x0000 #xff)  ;  Data FF
(memb #x0001 #x00)  ;  Data 00
(memb #x0002 #x7f)  ;  Data 7F
;
(memb #x1000 #x00)  ;  Data 00
(memb #x1001 #xff)  ;  Data FF
(memb #x1002 #x7f)  ;  Data 7F
;
(memw #x0200 #xa200)  ;  LDX #00
(memw #x0202 #xa201)  ;  LDX #01
(memw #x0204 #xa280)  ;  LDX #80
;
(memw #x0206 #xa600)  ;  LDX 00
(memw #x0208 #xa601)  ;  LDX 01
(memw #x020a #xa602)  ;  LDX 02
;
(memw #x020c #xa080)  ;  LDY #80
(memw #x020e #xb680)  ;  LDX 80,Y
(memw #x0210 #xb681)  ;  LDX 81,Y
(memw #x0212 #xb682)  ;  LDX 82,Y
;
(memb #x0214 #xae)  ;  LDX 1000
(memw #x0215 #x0010)
(memb #x0217 #xae)  ;  LDX 1001
(memw #x0218 #x0110)
(memb #x021a #xae)  ;  LDX 1002
(memw #x021b #x0210)
;
(memb #x021d #xbe)  ;  LDX F80,Y
(memw #x021e #x800f)
(memb #x0220 #xbe)  ;  LDX F81,Y
(memw #x0221 #x810f)
(memb #x0223 #xbe)  ;  LDX F82,Y
(memw #x0224 #x820f)
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
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(test-mask #x80 #x82)
(print "==> Testing LDX zero page instruction")
(terpri)
(sim-step)  ;  LDX 00
(test-reg RIX #xFF)
(test-mask #x80 #x82)
(sim-step)  ;  LDX 01
(test-reg RIX #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDX 02
(test-reg RIX #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDX zero page,Y instruction")
(terpri)
(sim-step)  ;  LDY #80
(test-reg RIY #x80)
(sim-step)  ;  LDX 80,Y
(test-reg RIX #xFF)
(test-mask #x80 #x82)
(sim-step)  ;  LDX 81,Y
(test-reg RIX #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDX 82,Y
(test-reg RIX #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDX absolute instruction")
(terpri)
(sim-step)  ;  LDX 1000
(test-reg RIX #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDX 1001
(test-reg RIX #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDX 1002
(test-reg RIX #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDX absolute,Y instruction")
(terpri)
(sim-step)  ;  LDX F80,Y
(test-reg RIX #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDX F81,Y
(test-reg RIX #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDX F82,Y
(test-reg RIX #x7f)
(test-mask #x00 #x82)
;
;-------------------------------------------------------------------------------
;  Test LDY instructions
;
(memb #x0000 #xff)  ;  Data FF
(memb #x0001 #x00)  ;  Data 00
(memb #x0002 #x7f)  ;  Data 7F
;
(memb #x1000 #x00)  ;  Data 00
(memb #x1001 #xff)  ;  Data FF
(memb #x1002 #x7f)  ;  Data 7F
;
(memw #x0200 #xa000)  ;  LDY #00
(memw #x0202 #xa001)  ;  LDY #01
(memw #x0204 #xa080)  ;  LDY #80
;
(memw #x0206 #xa400)  ;  LDY 00
(memw #x0208 #xa401)  ;  LDY 01
(memw #x020a #xa402)  ;  LDY 02
;
(memw #x020c #xa280)  ;  LDX #80
(memw #x020e #xb480)  ;  LDY 80,X
(memw #x0210 #xb481)  ;  LDY 81,X
(memw #x0212 #xb482)  ;  LDY 82,X
;
(memb #x0214 #xac)  ;  LDY 1000
(memw #x0215 #x0010)
(memb #x0217 #xac)  ;  LDY 1001
(memw #x0218 #x0110)
(memb #x021a #xac)  ;  LDY 1002
(memw #x021b #x0210)
;
(memb #x021d #xbc)  ;  LDY F80,X
(memw #x021e #x800f)
(memb #x0220 #xbc)  ;  LDY F81,X
(memw #x0221 #x810f)
(memb #x0223 #xbc)  ;  LDY F82,X
(memw #x0224 #x820f)
;
;  Execute test
;
(print "==> Testing LDY immediate instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDY #00
(test-reg RIY #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDY #01
(test-reg RIY #x01)
(test-mask #x00 #x82)
(sim-step)  ;  LDY #80
(test-reg RIY #x80)
(test-mask #x80 #x82)
(print "==> Testing LDY zero page instruction")
(terpri)
(sim-step)  ;  LDY 00
(test-reg RIY #xFF)
(test-mask #x80 #x82)
(sim-step)  ;  LDY 01
(test-reg RIY #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDY 02
(test-reg RIY #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDY zero page,X instruction")
(terpri)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(sim-step)  ;  LDY 80,X
(test-reg RIY #xFF)
(test-mask #x80 #x82)
(sim-step)  ;  LDY 81,X
(test-reg RIY #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDY 82,X
(test-reg RIY #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDY absolute instruction")
(terpri)
(sim-step)  ;  LDY 1000
(test-reg RIY #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDY 1001
(test-reg RIY #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDY 1002
(test-reg RIY #x7f)
(test-mask #x00 #x82)
(print "==> Testing LDY absolute,X instruction")
(terpri)
(sim-step)  ;  LDY F80,X
(test-reg RIY #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDY F81,X
(test-reg RIY #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDY F82,X
(test-reg RIY #x7f)
(test-mask #x00 #x82)
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
;
;-------------------------------------------------------------------------------
;  Test AND instructions
;
(memb #x0000 #xff)  ;  Data FF
(memb #x0001 #x33)  ;  Data 33
(memb #x0002 #xcf)  ;  Data CF
;
(memw #x0080 #x0010)  ;  Address 1000
(memw #x0082 #x0110)  ;  Address 1001
(memw #x0084 #x0210)  ;  Address 1002
;
(memb #x1000 #xff)  ;  Data FF
(memb #x1001 #x33)  ;  Data 33
(memb #x1002 #xcf)  ;  Data CF
;
(memb #x1040 #xff)  ;  Data FF
(memb #x1041 #x33)  ;  Data 33
(memb #x1042 #xcf)  ;  Data CF
;
(memw #x0200 #xa9f0)  ;  LDA #F0
(memw #x0202 #x29ff)  ;  AND #FF
(memw #x0204 #x2933)  ;  AND #33
(memw #x0206 #x29cf)  ;  AND #CF
;
(memw #x0208 #xa9f0)  ;  LDA #F0
(memw #x020a #x2500)  ;  AND 00
(memw #x020c #x2501)  ;  AND 01
(memw #x020e #x2502)  ;  AND 02
;
(memw #x0210 #xa9f0)  ;  LDA #F0
(memw #x0212 #xa280)  ;  LDX #80
(memw #x0214 #x3580)  ;  AND 80,X
(memw #x0216 #x3581)  ;  AND 81,X
(memw #x0218 #x3582)  ;  AND 82,X
;
(memw #x021a #xa9f0)  ;  LDA #F0
(memb #x021c #x2d)  ;  AND 1000
(memw #x021d #x0010)
(memb #x021f #x2d)  ;  AND 1001
(memw #x0220 #x0110)
(memb #x0222 #x2d)  ;  AND 1002
(memw #x0223 #x0210)
;
(memw #x0225 #xa9f0)  ;  LDA #F0
(memw #x0227 #xa280)  ;  LDX #80
(memb #x0229 #x3d)  ;  AND F80,X
(memw #x022a #x800f)
(memb #x022c #x3d)  ;  AND F81,X
(memw #x022d #x810f)
(memb #x022f #x3d)  ;  AND F82,X
(memw #x0230 #x820f)
;
(memw #x0232 #xa9f0)  ;  LDA #F0
(memw #x0234 #xa040)  ;  LDY #40
(memb #x0236 #x39)  ;  AND FC0,Y
(memw #x0237 #xc00f)
(memb #x0239 #x39)  ;  AND FC1,Y
(memw #x023a #xc10f)
(memb #x023c #x39)  ;  AND FC2,Y
(memw #x023d #xc20f)
;
(memw #x023f #xa9f0)  ;  LDA #F0
(memw #x0241 #x2100)  ;  AND (0,X)
(memw #x0243 #x2102)  ;  AND (2,X)
(memw #x0245 #x2104)  ;  AND (4,X)
;
(memw #x0247 #xa9f0)  ;  LDA #F0
(memw #x0249 #x3180)  ;  AND (80),Y
(memw #x024b #x3182)  ;  AND (82),Y
(memw #x024d #x3184)  ;  AND (84),Y
;
;  Execute test
;
(print "==> Testing AND immediate instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDA #F0
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND #FF
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND #33
(test-reg RA #x30)
(test-mask #x00 #x82)
(sim-step)  ;  AND #CF
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing AND zero page instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND 00
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND 01
(test-reg RA #x30)
(test-mask #x00 #x82)
(sim-step)  ;  AND 02
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing AND zero page,X instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(test-mask #x80 #x82)
(sim-step)  ;  AND 80,X
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND 81,X
(test-reg RA #x30)
(test-mask #x00 #x82)
(sim-step)  ;  AND 82,X
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing AND absolute instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND 1000
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND 1001
(test-reg RA #x30)
(test-mask #x00 #x82)
(sim-step)  ;  AND 1002
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing AND absolute,X instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(test-mask #x80 #x82)
(sim-step)  ;  AND 80,X
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND 81,X
(test-reg RA #x30)
(test-mask #x00 #x82)
(sim-step)  ;  AND 82,X
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing AND absolute,Y instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  LDY #80
(test-reg RIY #x40)
(test-mask #x00 #x82)
(sim-step)  ;  AND 80,Y
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND 81,Y
(test-reg RA #x30)
(test-mask #x00 #x82)
(sim-step)  ;  AND 82,Y
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing AND (indirect,X) instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND (0,X)
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND (2,X)
(test-reg RA #x30)
(test-mask #x00 #x82)
(sim-step)  ;  AND (4,X)
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing AND (indirect),Y instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND (80),Y
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  AND (82),Y
(test-reg RA #x30)
(test-mask #x00 #x82)
(sim-step)  ;  AND (84),Y
(test-reg RA #x00)
(test-mask #x02 #x82)
;
;-------------------------------------------------------------------------------
;  Test decrement instructions
;
(memb #x0000 #x00)  ;  Data 00
(memb #x0001 #x80)  ;  Data 80
(memb #x0002 #x01)  ;  Data 01
;
(memb #x0010 #x80)  ;  Data 80
(memb #x0011 #x00)  ;  Data 00
(memb #x0012 #x01)  ;  Data 01
;
(memb #x1000 #x01)  ;  Data 01
(memb #x1001 #x80)  ;  Data 80
(memb #x1002 #x00)  ;  Data 00
;
(memb #x1010 #x80)  ;  Data 80
(memb #x1011 #x01)  ;  Data 01
(memb #x1012 #x00)  ;  Data 00
;
(memw #x0200 #xc600)  ;  DEC 00
(memw #x0202 #xc601)  ;  DEC 01
(memw #x0204 #xc602)  ;  DEC 02
;
(memw #x0206 #xa210)  ;  LDX #10
(memw #x0208 #xd600)  ;  DEC 00,X
(memw #x020a #xd601)  ;  DEC 01,X
(memw #x020c #xd602)  ;  DEC 02,X
;
(memb #x020e #xce)  ;  DEC 1000
(memw #x020f #x0010)
(memb #x0211 #xce)  ;  DEC 1001
(memw #x0212 #x0110)
(memb #x0214 #xce)  ;  DEC 1002
(memw #x0215 #x0210)
;
(memb #x0217 #xde)  ;  DEC 1000,X
(memw #x0218 #x0010)
(memb #x021a #xde)  ;  DEC 1001,X
(memw #x021b #x0110)
(memb #x021d #xde)  ;  DEC 1002,X
(memw #x021e #x0210)
;
(memw #x0220 #xa280)  ;  LDX #80
(memb #x0222 #xca)    ;  DEX
(memw #x0223 #xa200)  ;  LDX #00
(memb #x0225 #xca)    ;  DEX
(memw #x0226 #xa201)  ;  LDX #01
(memb #x0228 #xca)    ;  DEX
;
(memw #x0229 #xa080)  ;  LDY #80
(memb #x022b #x88)    ;  DEY
(memw #x022c #xa000)  ;  LDY #00
(memb #x022e #x88)    ;  DEY
(memw #x022f #xa001)  ;  LDY #01
(memb #x0231 #x88)    ;  DEY
;
;  Execute test
;
(print "==> Testing DEC zero page instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  DEC 00
(test-memb #x0000 #xff)
(test-mask #x80 #x82)
(sim-step)  ;  DEC 01
(test-memb #x0001 #x7f)
(test-mask #x00 #x82)
(sim-step)  ;  DEC 02
(test-memb #x0002 #x00)
(test-mask #x02 #x82)
(print "==> Testing DEC zero page,X instruction")
(terpri)
(sim-step)  ;  LDX #10
(test-reg RIX #x10)
(sim-step)  ;  DEC 00,X
(test-memb #x0010 #x7f)
(test-mask #x00 #x82)
(sim-step)  ;  DEC 01,X
(test-memb #x0011 #xff)
(test-mask #x80 #x82)
(sim-step)  ;  DEC 02,X
(test-memb #x0012 #x00)
(test-mask #x02 #x82)
(print "==> Testing DEC absolute instruction")
(terpri)
(sim-step)  ;  DEC 1000
(test-memb #x1000 #x00)
(test-mask #x02 #x82)
(sim-step)  ;  DEC 1001
(test-memb #x1001 #x7f)
(test-mask #x00 #x82)
(sim-step)  ;  DEC 1002
(test-memb #x1002 #xff)
(test-mask #x80 #x82)
(print "==> Testing DEC absolute,X instruction")
(terpri)
(sim-step)  ;  DEC 1000,X
(test-memb #x1010 #x7f)
(test-mask #x00 #x82)
(sim-step)  ;  DEC 1001,X
(test-memb #x1011 #x00)
(test-mask #x02 #x82)
(sim-step)  ;  DEC 1002,X
(test-memb #x1012 #xff)
(test-mask #x80 #x82)
(print "==> Testing DEX instruction")
(terpri)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(test-mask #x80 #x82)
(sim-step)  ;  DEX
(test-reg RIX #x7f)
(test-mask #x00 #x82)
(sim-step)  ;  LDX #00
(test-reg RIX #x00)
(test-mask #x02 #x82)
(sim-step)  ;  DEX
(test-reg RIX #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDX #01
(test-reg RIX #x01)
(test-mask #x00 #x82)
(sim-step)  ;  DEX
(test-reg RIX #x00)
(test-mask #x02 #x82)
(print "==> Testing DEY instruction")
(terpri)
(sim-step)  ;  LDY #80
(test-reg RIY #x80)
(test-mask #x80 #x82)
(sim-step)  ;  DEY
(test-reg RIY #x7f)
(test-mask #x00 #x82)
(sim-step)  ;  LDY #00
(test-reg RIY #x00)
(test-mask #x02 #x82)
(sim-step)  ;  DEY
(test-reg RIY #xff)
(test-mask #x80 #x82)
(sim-step)  ;  LDY #01
(test-reg RIY #x01)
(test-mask #x00 #x82)
(sim-step)  ;  DEY
(test-reg RIY #x00)
(test-mask #x02 #x82)
;
;-------------------------------------------------------------------------------
;  Test EOR instructions
;
(memb #x0000 #xff)  ;  Data FF
(memb #x0001 #xc3)  ;  Data C3
(memb #x0002 #x33)  ;  Data 33
;
(memw #x0080 #x0010)  ;  Address 1000
(memw #x0082 #x0110)  ;  Address 1001
(memw #x0084 #x0210)  ;  Address 1002
;
(memb #x1000 #xff)  ;  Data FF
(memb #x1001 #xc3)  ;  Data 33
(memb #x1002 #x33)  ;  Data CF
;
(memb #x1040 #xff)  ;  Data FF
(memb #x1041 #xc3)  ;  Data 33
(memb #x1042 #x33)  ;  Data CF
;
(memw #x0200 #xa90f)  ;  LDA #0F
(memw #x0202 #x49ff)  ;  EOR #FF
(memw #x0204 #x49c3)  ;  EOR #C3
(memw #x0206 #x4933)  ;  EOR #33
;
(memw #x0208 #xa90F)  ;  LDA #0F
(memw #x020a #x4500)  ;  EOR 00
(memw #x020c #x4501)  ;  EOR 01
(memw #x020e #x4502)  ;  EOR 02
;
(memw #x0210 #xa90f)  ;  LDA #0F
(memw #x0212 #xa280)  ;  LDX #80
(memw #x0214 #x5580)  ;  EOR 80,X
(memw #x0216 #x5581)  ;  EOR 81,X
(memw #x0218 #x5582)  ;  EOR 82,X
;
(memw #x021a #xa90f)  ;  LDA #0F
(memb #x021c #x4d)  ;  EOR 1000
(memw #x021d #x0010)
(memb #x021f #x4d)  ;  EOR 1001
(memw #x0220 #x0110)
(memb #x0222 #x4d)  ;  EOR 1002
(memw #x0223 #x0210)
;
(memw #x0225 #xa90f)  ;  LDA #0F
(memw #x0227 #xa280)  ;  LDX #80
(memb #x0229 #x5d)  ;  EOR F80,X
(memw #x022a #x800f)
(memb #x022c #x5d)  ;  EOR F81,X
(memw #x022d #x810f)
(memb #x022f #x5d)  ;  EOR F82,X
(memw #x0230 #x820f)
;
(memw #x0232 #xa90f)  ;  LDA #0F
(memw #x0234 #xa040)  ;  LDY #40
(memb #x0236 #x59)  ;  EOR FC0,Y
(memw #x0237 #xc00f)
(memb #x0239 #x59)  ;  EOR FC1,Y
(memw #x023a #xc10f)
(memb #x023c #x59)  ;  EOR FC2,Y
(memw #x023d #xc20f)
;
(memw #x023f #xa90f)  ;  LDA #0F
(memw #x0241 #x4100)  ;  EOR (0,X)
(memw #x0243 #x4102)  ;  EOR (2,X)
(memw #x0245 #x4104)  ;  EOR (4,X)
;
(memw #x0247 #xa90f)  ;  LDA #0F
(memw #x0249 #x5180)  ;  EOR (80),Y
(memw #x024b #x5182)  ;  EOR (82),Y
(memw #x024d #x5184)  ;  EOR (84),Y
;
;  Execute test
;
(print "==> Testing EOR immediate instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDA #0F
(test-reg RA #x0f)
(test-mask #x00 #x82)
(sim-step)  ;  EOR #FF
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  EOR #C3
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  EOR #33
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing EOR zero page instruction")
(terpri)
(sim-step)  ;  LDA #0F
(test-reg RA #x0f)
(test-mask #x00 #x82)
(sim-step)  ;  EOR 00
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  EOR 01
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  EOR 02
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing EOR zero page,X instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #x0f)
(test-mask #x00 #x82)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(test-mask #x80 #x82)
(sim-step)  ;  EOR 80,X
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  EOR 81,X
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  EOR 82,X
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing EOR absolute instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #x0f)
(test-mask #x00 #x82)
(sim-step)  ;  EOR 1000
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  EOR 1001
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  EOR 1002
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing EOR absolute,X instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #x0f)
(test-mask #x00 #x82)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(test-mask #x80 #x82)
(sim-step)  ;  EOR 80,X
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  EOR 81,X
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  EOR 82,X
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing EOR absolute,Y instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #x0f)
(test-mask #x00 #x82)
(sim-step)  ;  LDY #80
(test-reg RIY #x40)
(test-mask #x00 #x82)
(sim-step)  ;  EOR 80,Y
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  EOR 81,Y
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  EOR 82,Y
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing EOR (indirect,X) instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #x0f)
(test-mask #x00 #x82)
(sim-step)  ;  EOR (0,X)
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  EOR (2,X)
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  EOR (4,X)
(test-reg RA #x00)
(test-mask #x02 #x82)
(print "==> Testing EOR (indirect),Y instruction")
(terpri)
(sim-step)  ;  LDA #F0
(test-reg RA #x0f)
(test-mask #x00 #x82)
(sim-step)  ;  EOR (80),Y
(test-reg RA #xf0)
(test-mask #x80 #x82)
(sim-step)  ;  EOR (82),Y
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  EOR (84),Y
(test-reg RA #x00)
(test-mask #x02 #x82)
;
;-------------------------------------------------------------------------------
;  Test increment instructions
;
(memb #x0000 #xff)  ;  Data FF
(memb #x0001 #x7f)  ;  Data 7F
(memb #x0002 #x00)  ;  Data 00
;
(memb #x0010 #xff)  ;  Data FF
(memb #x0011 #x7f)  ;  Data 7F
(memb #x0012 #x00)  ;  Data 00
;
(memb #x1000 #xff)  ;  Data 01
(memb #x1001 #x7f)  ;  Data 80
(memb #x1002 #x00)  ;  Data 00
;
(memb #x1010 #xff)  ;  Data 80
(memb #x1011 #x7f)  ;  Data 01
(memb #x1012 #x00)  ;  Data 00
;
(memw #x0200 #xe600)  ;  INC 00
(memw #x0202 #xe601)  ;  INC 01
(memw #x0204 #xe602)  ;  INC 02
;
(memw #x0206 #xa210)  ;  LDX #10
(memw #x0208 #xf600)  ;  INC 00,X
(memw #x020a #xf601)  ;  INC 01,X
(memw #x020c #xf602)  ;  INC 02,X
;
(memb #x020e #xee)  ;  INC 1000
(memw #x020f #x0010)
(memb #x0211 #xee)  ;  INC 1001
(memw #x0212 #x0110)
(memb #x0214 #xee)  ;  INC 1002
(memw #x0215 #x0210)
;
(memb #x0217 #xfe)  ;  INC 1000,X
(memw #x0218 #x0010)
(memb #x021a #xfe)  ;  INC 1001,X
(memw #x021b #x0110)
(memb #x021d #xfe)  ;  INC 1002,X
(memw #x021e #x0210)
;
(memw #x0220 #xa2ff)  ;  LDX #FF
(memb #x0222 #xe8)    ;  INX
(memw #x0223 #xa27f)  ;  LDX #7F
(memb #x0225 #xe8)    ;  INX
(memw #x0226 #xa200)  ;  LDX #00
(memb #x0228 #xe8)    ;  INX
;
(memw #x0229 #xa0ff)  ;  LDY #FF
(memb #x022b #xc8)    ;  INY
(memw #x022c #xa07f)  ;  LDY #7F
(memb #x022e #xc8)    ;  INY
(memw #x022f #xa000)  ;  LDY #00
(memb #x0231 #xc8)    ;  INY
;
;  Execute test
;
(print "==> Testing INC zero page instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  INC 00
(test-memb #x0000 #x00)
(test-mask #x02 #x82)
(sim-step)  ;  INC 01
(test-memb #x0001 #x80)
(test-mask #x80 #x82)
(sim-step)  ;  INC 02
(test-memb #x0002 #x01)
(test-mask #x00 #x82)
(print "==> Testing INC zero page,X instruction")
(terpri)
(sim-step)  ;  LDX #10
(test-reg RIX #x10)
(sim-step)  ;  INC 00,X
(test-memb #x0010 #x00)
(test-mask #x02 #x82)
(sim-step)  ;  INC 01,X
(test-memb #x0011 #x80)
(test-mask #x80 #x82)
(sim-step)  ;  INC 02,X
(test-memb #x0012 #x01)
(test-mask #x00 #x82)
(print "==> Testing INC absolute instruction")
(terpri)
(sim-step)  ;  INC 1000
(test-memb #x1000 #x00)
(test-mask #x02 #x82)
(sim-step)  ;  INC 1001
(test-memb #x1001 #x80)
(test-mask #x80 #x82)
(sim-step)  ;  INC 1002
(test-memb #x1002 #x01)
(test-mask #x00 #x82)
(print "==> Testing INC absolute,X instruction")
(terpri)
(sim-step)  ;  INC 1000,X
(test-memb #x1010 #x00)
(test-mask #x02 #x82)
(sim-step)  ;  INC 1001,X
(test-memb #x1011 #x80)
(test-mask #x80 #x82)
(sim-step)  ;  INC 1002,X
(test-memb #x1012 #x01)
(test-mask #x00 #x82)
(print "==> Testing INX instruction")
(terpri)
(sim-step)  ;  LDX #FF
(test-reg RIX #xff)
(test-mask #x80 #x82)
(sim-step)  ;  INX
(test-reg RIX #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDX #7F
(test-reg RIX #x7f)
(test-mask #x00 #x82)
(sim-step)  ;  INX
(test-reg RIX #x80)
(test-mask #x80 #x82)
(sim-step)  ;  LDX #00
(test-reg RIX #x00)
(test-mask #x02 #x82)
(sim-step)  ;  INX
(test-reg RIX #x01)
(test-mask #x00 #x82)
(print "==> Testing INY instruction")
(terpri)
(sim-step)  ;  LDY #FF
(test-reg RIY #xff)
(test-mask #x80 #x82)
(sim-step)  ;  INY
(test-reg RIY #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDY #7F
(test-reg RIY #x7f)
(test-mask #x00 #x82)
(sim-step)  ;  INY
(test-reg RIY #x80)
(test-mask #x80 #x82)
(sim-step)  ;  LDY #00
(test-reg RIY #x00)
(test-mask #x02 #x82)
(sim-step)  ;  INY
(test-reg RIY #x01)
(test-mask #x00 #x82)
;
;-------------------------------------------------------------------------------
;  Test ORA instructions
;
(memb #x0000 #x00)  ;  Data 00
(memb #x0001 #x33)  ;  Data 33
(memb #x0002 #xcc)  ;  Data CC
;
(memw #x0080 #x0010)  ;  Address 1000
(memw #x0082 #x0110)  ;  Address 1001
(memw #x0084 #x0210)  ;  Address 1002
;
(memb #x1000 #x00)  ;  Data 00
(memb #x1001 #x33)  ;  Data 33
(memb #x1002 #xcc)  ;  Data CC
;
(memb #x1040 #x00)  ;  Data 00
(memb #x1041 #x33)  ;  Data 33
(memb #x1042 #xcc)  ;  Data CC
;
(memw #x0200 #xa900)  ;  LDA #00
(memw #x0202 #x0900)  ;  ORA #00
(memw #x0204 #x0933)  ;  ORA #33
(memw #x0206 #x09cc)  ;  ORA #CC
;
(memw #x0208 #xa900)  ;  LDA #00
(memw #x020a #x0500)  ;  ORA 00
(memw #x020c #x0501)  ;  ORA 01
(memw #x020e #x0502)  ;  ORA 02
;
(memw #x0210 #xa900)  ;  LDA #00
(memw #x0212 #xa280)  ;  LDX #80
(memw #x0214 #x1580)  ;  ORA 80,X
(memw #x0216 #x1581)  ;  ORA 81,X
(memw #x0218 #x1582)  ;  ORA 82,X
;
(memw #x021a #xa900)  ;  LDA #00
(memb #x021c #x0d)  ;  ORA 1000
(memw #x021d #x0010)
(memb #x021f #x0d)  ;  ORA 1001
(memw #x0220 #x0110)
(memb #x0222 #x0d)  ;  ORA 1002
(memw #x0223 #x0210)
;
(memw #x0225 #xa900)  ;  LDA #00
(memw #x0227 #xa280)  ;  LDX #80
(memb #x0229 #x1d)  ;  ORA F80,X
(memw #x022a #x800f)
(memb #x022c #x1d)  ;  ORA F81,X
(memw #x022d #x810f)
(memb #x022f #x1d)  ;  ORA F82,X
(memw #x0230 #x820f)
;
(memw #x0232 #xa900)  ;  LDA #00
(memw #x0234 #xa040)  ;  LDY #40
(memb #x0236 #x19)  ;  ORA FC0,Y
(memw #x0237 #xc00f)
(memb #x0239 #x19)  ;  ORA FC1,Y
(memw #x023a #xc10f)
(memb #x023c #x19)  ;  ORA FC2,Y
(memw #x023d #xc20f)
;
(memw #x023f #xa900)  ;  LDA #00
(memw #x0241 #x0100)  ;  ORA (0,X)
(memw #x0243 #x0102)  ;  ORA (2,X)
(memw #x0245 #x0104)  ;  ORA (4,X)
;
(memw #x0247 #xa900)  ;  LDA #00
(memw #x0249 #x1180)  ;  ORA (80),Y
(memw #x024b #x1182)  ;  ORA (82),Y
(memw #x024d #x1184)  ;  ORA (84),Y
;
;  Execute test
;
(print "==> Testing ORA immediate instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA #00
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA #33
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  ORA #CC
(test-reg RA #xff)
(test-mask #x80 #x82)
(print "==> Testing ORA zero page instruction")
(terpri)
(sim-step)  ;  LDA #0F
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA 00
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA 01
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  ORA 02
(test-reg RA #xff)
(test-mask #x80 #x82)
(print "==> Testing ORA zero page,X instruction")
(terpri)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(test-mask #x80 #x82)
(sim-step)  ;  ORA 80,X
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA 81,X
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  ORA 82,X
(test-reg RA #xff)
(test-mask #x80 #x82)
(print "==> Testing ORA absolute instruction")
(terpri)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA 1000
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA 1001
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  ORA 1002
(test-reg RA #xff)
(test-mask #x80 #x82)
(print "==> Testing ORA absolute,X instruction")
(terpri)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(test-mask #x80 #x82)
(sim-step)  ;  ORA 80,X
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA 81,X
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  ORA 82,X
(test-reg RA #xff)
(test-mask #x80 #x82)
(print "==> Testing ORA absolute,Y instruction")
(terpri)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDY #80
(test-reg RIY #x40)
(test-mask #x00 #x82)
(sim-step)  ;  ORA 80,Y
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA 81,Y
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  ORA 82,Y
(test-reg RA #xff)
(test-mask #x80 #x82)
(print "==> Testing ORA (indirect,X) instruction")
(terpri)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA (0,X)
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA (2,X)
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  ORA (4,X)
(test-reg RA #xff)
(test-mask #x80 #x82)
(print "==> Testing ORA (indirect),Y instruction")
(terpri)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA (80),Y
(test-reg RA #x00)
(test-mask #x02 #x82)
(sim-step)  ;  ORA (82),Y
(test-reg RA #x33)
(test-mask #x00 #x82)
(sim-step)  ;  ORA (84),Y
(test-reg RA #xff)
(test-mask #x80 #x82)
;
;-------------------------------------------------------------------------------
;  Test ADC instructions
;
;  Addition is peformed by a single function in the code, so all th variations
;  will be covered by the ADC immediate test.  The other tests will be just to
;  verify that addition occurs properly with the other addressing modes.
;
(memb #x0010 #x14)  ;  Data 14
(memb #x0011 #x32)  ;  Data 32
(memb #x0012 #xcc)  ;  Data CC
;
(memw #x0020 #x1100)  ;  Address 0011
(memw #x0022 #xe10f)  ;  ADDRESS 0FE1
;
(memb #x1000 #x12)  ;  Data 12
(memb #x1001 #x23)  ;  Data 23
(memb #x1002 #x34)  ;  Data 34
;
(memb #x0200 #xd8)    ;  CLD
(memb #x0201 #x18)    ;  CLC
(memw #x0202 #xa980)  ;  LDA #80
(memw #x0204 #x6980)  ;  ADC #80
(memw #x0206 #x6900)  ;  ADC #00
(memw #x0208 #x6901)  ;  ADC #01
(memw #x020a #x697f)  ;  ADC #7F
(memw #x020c #x69ff)  ;  ADC #FF
(memw #x020e #x6940)  ;  ADC #40
;
(memb #x0210 #x18)    ;  CLC
(memb #x0211 #xf8)    ;  SED
(memw #x0212 #xa980)  ;  LDA #80
(memw #x0214 #x6980)  ;  ADC #80
(memw #x0216 #x6900)  ;  ADC #00
(memw #x0218 #x6901)  ;  ADC #01
(memw #x021a #x6909)  ;  ADC #09
;
(memb #x021c #xd8)    ;  CLD
(memb #x021d #x18)    ;  CLC
(memw #x021e #xa910)  ;  LDA #10
(memw #x0220 #x6510)  ;  ADC 10
(memw #x0222 #xa210)  ;  LDX #10
(memw #x0224 #x7501)  ;  ADC 1,X
(memb #x0226 #x6d)    ;  ADC 1000
(memw #x0227 #x0010)
(memb #x0229 #x7d)    ;  ADC FF1,X
(memw #x022a #xf10f)
(memw #x022c #xa020)  ;  LDY #20
(memb #x022e #x79)    ;  ADC FE2,Y
(memw #x022f #xe20f)
(memw #x0231 #x6110)  ;  ADC (10,X)
(memw #x0233 #x7122)  ;  ADC (22),Y
;
;  Execute test
;
(print "==> Testing ADC immediate instruction (binary)")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  CLC
(sim-step)  ;  CLD
(sim-step)  ;  LDA #80
(test-reg RA #x80)
(test-mask #x80 #x82)
(sim-step)  ;  ADC #80
(test-reg RA #x00)
(test-mask #x43 #xcb)
(sim-step)  ;  ADC #00
(test-reg RA #x01)
(test-mask #x00 #xcb)
(sim-step)  ;  ADC #01
(test-reg RA #x02)
(test-mask #x00 #xcb)
(sim-step)  ;  ADC #7F
(test-reg RA #x81)
(test-mask #xc0 #xcb)
(sim-step)  ;  ADC #FF
(test-reg RA #x80)
(test-mask #x81 #xcb)
(sim-step)  ;  ADC #40
(test-reg RA #xc1)
(test-mask #x80 #xcb)
(print "==> Testing ADC immediate instruction (decimal)")
(terpri)
(sim-step)  ;  CLC
(sim-step)  ;  SED
(test-mask #x08 #x0d)
(sim-step)  ;  LDA #80
(test-reg RA #x80)
(test-mask #x88 #xcb)
(sim-step)  ;  ADC #80
(test-reg RA #x60)
(test-mask #x4b #xcb)
(sim-step)  ;  ADC #00
(test-reg RA #x61)
(test-mask #x08 #xcb)
(sim-step)  ;  ADC #01
(test-reg RA #x62)
(test-mask #x08 #xcb)
(sim-step)  ;  ADC #x09
(test-reg RA #x71)
(test-mask #x08 #xcb)
(print "==> Testing ADC other addressing modes")
(terpri)
(sim-step)  ;  CLD
(sim-step)  ;  CLC
(sim-step)  ;  LDA #10
(test-reg RA #x10)
(test-mask #x00 #xcb)
(sim-step)  ;  ADC 10
(test-reg RA #x24)
(test-mask #x00 #xcb)
(sim-step)  ;  LDX #10
(test-reg RIX #x10)
(sim-step)  ;  ADC 1,X
(test-reg RA #x56)
(test-mask #x00 #xcb)
(sim-step)  ;  ADC 1000
(test-reg RA #x68)
(test-mask #x00 #xcb)
(sim-step)  ;  ADC FF1,X
(test-reg RA #x8b)
(test-mask #xc0 #xcb)
(sim-step)  ;  LDY #20
(test-reg RIY #x20)
(sim-step)  ;  ADC FE2,Y
(test-reg RA #xBF)
(test-mask #xc0 #xcb)
(sim-step)  ;  ADC (10,X)
(test-reg RA #xf1)
(test-mask #xc0 #xcb)
(sim-step)  ;  ADC (20),Y
(test-reg RA #x14)
(test-mask #x41 #xcb)
;
;-------------------------------------------------------------------------------
;  Test ASL instructions
;
(memb #x0010 #x80)  ;  Data 80
(memb #x0011 #x7f)  ;  Data 7F
;
(memb #x0080 #x7f)  ;  Data 7F
(memb #x0081 #x80)  ;  Data 80
;
(memb #x1000 #x80)  ;  Data 80
(memb #x1001 #x7f)  ;  Data 7F
;
(memb #x1080 #x7f)  ;  Data 7F
(memb #x1081 #x80)  ;  Data 80
;
(memw #x0200 #xa980)  ;  LDA #80
(memb #x0202 #xb8)    ;  CLV
(memb #x0203 #x0a)    ;  ASL A
(memw #x0204 #xa97f)  ;  LDA #7F
(memb #x0206 #x0a)    ;  ASL A
;
(memb #x0207 #x18)    ;  CLC
(memw #x0208 #x0610)  ;  ASL 10
(memw #x020a #x0611)  ;  ASL 11
;
(memw #x020c #xa240)  ;  LDX #40
(memw #x020e #x1640)  ;  ASL 40,X
(memw #x0210 #x1641)  ;  ASL 41,X
;
(memb #x0212 #x18)    ;  CLC
(memb #x0213 #x0e)    ;  ASL 1000
(memw #x0214 #x0010)
(memb #x0216 #x0e)    ;  ASL 1001
(memw #x0217 #x0110)
;
(memb #x0219 #x18)    ;  CLC
(memb #x021a #x1e)    ;  ASL 1040,X
(memw #x021b #x4010)
(memb #x021d #x1e)    ;  ASL 1041,X
(memw #x021e #x4110)
;
;  Execute test
;
(print "==> Testing ASL accumulator instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDA #80
(test-reg RA #x80)
(test-mask #x80 #xcb)
(sim-step)
(test-mask #x80 #xcb)
(sim-step)  ;  ASL A
(test-reg RA #x00)
(test-mask #x03 #xcb)
(sim-step)  ;  LDA #7F
(test-reg RA #x7f)
(test-mask #x01 #xcb)
(sim-step)  ;  ASL A
(test-reg RA #xfe)
(test-mask #x80 #xcb)
(print "==> Testing ASL zero page instruction")
(terpri)
(sim-step)  ;  CLC
(test-mask #x80 #xcb)
(sim-step)  ;  ASL 10
(test-memb #x10 #x00)
(test-mask #x03 #xcb)
(sim-step)  ;  ASL 11
(test-memb #x11 #xfe)
(test-mask #x80 #xcb)
(print "==> Testing ASL zero page,X instruction")
(terpri)
(sim-step)  ;  LDX #40
(test-reg RIX #x40)
(sim-step)  ;  ASL 40,X
(test-memb #x0080 #xfe)
(test-mask #x80 #xcb)
(sim-step)  ;  ASL 41,X
(test-memb #x0081 #x00)
(test-mask #x03 #xcb)
(print "==> Testing ASL absolute instruction")
(terpri)
(sim-step)  ;  CLC
(sim-step)  ;  ASL 1000
(test-memb #x1000 #x00)
(test-mask #x03 #xcb)
(sim-step)  ;  ASL 1001
(test-memb #x1001 #xfe)
(test-mask #x80 #xcb)
(print "==> Testing ASL absolute,X instruction")
(terpri)
(sim-step)  ;  CLC
(sim-step)  ;  ASL 1040,X
(test-memb #x1080 #xfe)
(test-mask #x80 #xcb)
(sim-step)  ;  ASL 1041,X
(test-memb #x1081 #x00)
(test-mask #x03 #xcb)
;
;-------------------------------------------------------------------------------
;  Test conditional branch instructions.  CLV is also tested
;
(memb #x0200 #x18)    ;  CLC
(memw #x0201 #xa900)  ;  LDA #00
(memw #x0203 #xb07f)  ;  BCS 7F
(memw #x0205 #x9002)  ;  BCC 02
(memw #x0207 #xa9ff)  ;  LDA #FF
(memb #x0209 #x38)    ;  SEC
(memw #x020a #x90ff)  ;  BCC 7F
(memw #x020c #xb002)  ;  BCS 02
(memw #x020e #xa9ff)  ;  LDA #FF
;
(memw #x0210 #xa901)  ;  LDA #01
(memw #x0212 #xf07f)  ;  BEQ 7F
(memw #x0214 #xd002)  ;  BNE 02
(memw #x0216 #xa9ff)  ;  LDA #FF
(memw #x0218 #xa900)  ;  LDA #00
(memw #x021a #xd07f)  ;  BNE 7F
(memw #x021c #xf002)  ;  BEQ 02
(memw #x021e #xa9ff)  ;  LDA #FF
;
(memw #x0220 #xa97f)  ;  LDA #7F
(memw #x0222 #x307f)  ;  BMI 7F
(memw #x0224 #x1002)  ;  BPL 02
(memw #x0226 #xa9ff)  ;  LDA #FF
(memw #x0228 #xa980)  ;  LDA #80
(memw #x022a #x107f)  ;  BPL 7F
(memw #x022c #x3002)  ;  BMI 02
(memw #x022e #xa9ff)  ;  LDA #FF
;
(memb #x0230 #xb8)    ;  CLV
(memb #x0231 #x18)    ;  CLC
(memw #x0232 #x707f)  ;  BVS 7F
(memw #x0234 #x5002)  ;  BVC 02
(memw #x0236 #xa900)  ;  LDA #00
(memw #x0238 #xa97f)  ;  LDA #7F
(memw #x023a #x697f)  ;  ADC #7F
(memw #x023c #x507f)  ;  BVC 7F
(memw #x023e #x7002)  ;  BVS 02
(memw #x0240 #xa9ff)  ;  LDA #FF
(memb #x0242 #xb8)    ;  CLV
;
;  Execute test
;
(print "==> Testing BCC and BCS instructions")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  CLC
(test-reg RPC #x0201)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(test-reg RPC #x0203)
(sim-step)  ;  BCS 7F
(test-reg RPC #x0205)
(sim-step)  ;  BCC 02
(test-reg RPC #x0209)
(test-reg RA #x00)
(sim-step)  ;  SEC
(test-reg RA #x00)
(test-reg RPC #x020a)
(sim-step)  ;  BCC 7F
(test-reg RPC #x020c)
(sim-step)  ;  BCS 02
(test-reg RPC #x0210)
(test-reg RA #x00)
(print "==> Testing BEQ and BNE instructions")
(terpri)
(sim-step)  ;  LDA #01
(test-reg RA #x01)
(sim-step)  ;  BEQ 7F
(test-reg RPC #x0214)
(sim-step)  ;  BNE 02
(test-reg RPC #x0218)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(test-reg RPC #x021a)
(sim-step)  ;  BNE 7F
(test-reg RPC #x021c)
(sim-step)  ;  BEQ 02
(test-reg RPC #x0220)
(print "==> Testing BMI and BPL instructions")
(terpri)
(sim-step)  ;  LDA #7F
(test-reg RA #x7F)
(sim-step)  ;  BMI 7F
(test-reg RPC #x0224)
(sim-step)  ;  BPL 02
(test-reg RPC #x0228)
(sim-step)  ;  LDA #80
(test-reg RA #x80)
(test-reg RPC #x022a)
(sim-step)  ;  BPL 7F
(test-reg RPC #x022c)
(sim-step)  ;  BMI 02
(test-reg RPC #x0230)
(print "==> Testing BVC and BVS instructions")
(terpri)
(sim-step)  ;  CLV
(sim-step)  ;  CLC
(test-mask #x00 #x41)
(sim-step)  ;  BVS 7F
(test-reg RPC #x0234)
(sim-step)  ;  BVC 02
(test-reg RPC #x0238)
(sim-step)  ;  LDA #x7F
(test-reg RA #x7f)
(sim-step)  ;  ADC #7F
(test-reg RA #xfe)
(test-mask #x40 #x40)
(sim-step)  ;  BVC 7F
(test-reg RPC #x023e)
(sim-step)  ;  BVS 02
(test-reg RPC #x0242)
(test-mask #x40 #x40)
(sim-step)  ;  CLV
(test-mask #x00 #x40)
;
;-------------------------------------------------------------------------------
;  Test BIT instructions
;
(memb #x0000 #xff)  ;  Data FF
(memb #x0001 #x80)  ;  Data 80
(memb #x0002 #x40)  ;  Data 40
;
(memb #x1000 #xff)  ;  Data FF
(memb #x1001 #x40)  ;  Data 80
(memb #x1002 #x80)  ;  Data 40
;
(memw #x0200 #xa9ff)  ;  LDA #FF
(memw #x0202 #x2400)  ;  BIT 00
(memw #x0204 #x2401)  ;  BIT 01
(memw #x0206 #x2402)  ;  BIT 02
(memw #x0208 #xa9bf)  ;  LDA #BF
(memw #x020a #x2400)  ;  BIT 00
(memw #x020c #x2401)  ;  BIT 01
(memw #x020e #x2402)  ;  BIT 02
;
(memw #x0210 #xa9ff)  ;  LDA #FF
(memb #x0212 #x2c)    ;  BIT 1000
(memw #x0213 #x0010)
(memb #x0215 #x2c)    ;  BIT 1001
(memw #x0216 #x0110)
(memb #x0218 #x2c)    ;  BIT 1002
(memw #x0219 #x0210)
(memw #x021b #xa9bf)  ;  LDA #BF
(memb #x021d #x2c)    ;  BIT 1000
(memw #x021e #x0010)
(memb #x0220 #x2c)    ;  BIT 1001
(memw #x0221 #x0110)
(memb #x0223 #x2c)    ;  BIT 1002
(memw #x0224 #x0210)
;
;  Execute test
;
(print "==> Testing BIT zero page instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDA #FF
(test-reg RA #xff)
(sim-step)  ;  BIT 00
(test-mask #xc0 #xcb)
(sim-step)  ;  BIT 01
(test-mask #x80 #xcb)
(sim-step)  ;  BIT 02
(test-mask #x40 #xcb)
(sim-step)  ;  LDA #BF
(test-reg RA #xbf)
(sim-step)  ;  BIT 00
(test-mask #x80 #xcb)
(sim-step)  ;  BIT 01
(test-mask #x80 #xcb)
(sim-step)  ;  BIT 02
(test-mask #x02 #xcb)
(print "==> Testing BIT absolute instruction")
(terpri)
(sim-step)  ;  LDA #FF
(test-reg RA #xff)
(sim-step)  ;  BIT 1000
(test-mask #xc0 #xcb)
(sim-step)  ;  BIT 1001
(test-mask #x40 #xcb)
(sim-step)  ;  BIT 1002
(test-mask #x80 #xcb)
(sim-step)  ;  LDA #BF
(test-reg RA #xbf)
(sim-step)  ;  BIT 1000
(test-mask #x80 #xcb)
(sim-step)  ;  BIT 1001
(test-mask #x02 #xcb)
(sim-step)  ;  BIT 1002
(test-mask #x80 #xcb)
;
;-------------------------------------------------------------------------------
;  Test CMP instructions
;
(memb #x0000 #x7f)  ;  Data 7F
(memb #x0001 #x80)  ;  Data 80
(memb #x0002 #x81)  ;  Data 81
;
(memw #x0040 #x8010)  ;  Address 1080
(memw #x0042 #x8110)  ;  Address 1081
(memw #x0044 #x8210)  ;  Address 1082
;
(memw #x0080 #x4010)  ;  Address 1040)
(memw #x0082 #x4110)  ;  Address 1041)
(memw #x0084 #x4210)  ;  Address 1042)
;
(memb #x1080 #x81)  ;  Data 81
(memb #x1081 #x80)  ;  Data 80
(memb #x1082 #x7f)  ;  Data 7F
;
(memb #x0200 #xb8)    ;  CLV
(memb #x0201 #x18)    ;  CLC
(memw #x0202 #xa980)  ;  LDA #80
(memw #x0204 #xc980)  ;  CMP #80
(memw #x0206 #xc981)  ;  CMP #81
(memw #x0208 #xc97f)  ;  CMP #7F
;
(memw #x020a #xc500)  ;  CMP 00
(memw #x020c #xc501)  ;  CMP 01
(memw #x020e #xc502)  ;  CMP 02
;
(memw #x0210 #xa280)  ;  LDX #80
(memw #x0212 #xd580)  ;  CMP 80,X
(memw #x0214 #xd581)  ;  CMP 81,X
(memw #x0216 #xd582)  ;  CMP 82,X
;
(memb #x0218 #xcd)    ;  CMP 1080
(memw #x0219 #x8010)
(memb #x021b #xcd)    ;  CMP 1081
(memw #x021c #x8110)
(memb #x021e #xcd)    ;  CMP 1082
(memw #x021f #x8210)
;
(memb #x0221 #xdd)    ;  CMP 1000,X
(memw #x0222 #x0010)
(memb #x0224 #xdd)    ;  CMP 1001,X
(memw #x0225 #x0110)
(memb #x0227 #xdd)    ;  CMP 1002,X
(memw #x0228 #x0210)
;
(memw #x022a #xa040)  ;  LDY #40
(memb #x022c #xd9)    ;  CMP 1040,Y
(memw #x022d #x4010)
(memb #x022f #xd9)    ;  CMP 1041,Y
(memw #x0230 #x4110)
(memb #x0232 #xd9)    ;  CMP 1042,Y
(memw #x0233 #x4210)
;
(memw #x0235 #xc1c0)  ;  CMP (C0,X)
(memw #x0237 #xc1c2)  ;  CMP (C2,X)
(memw #x0239 #xc1c4)  ;  CMP (C4,X)
;
(memw #x0235 #xd180)  ;  CMP (80),Y
(memw #x0237 #xd182)  ;  CMP (82),Y
(memw #x0239 #xd184)  ;  CMP (84),Y
;
;  Execute test
;
(print "==> Testing CMP immediate instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  CLV
(sim-step)  ;  CLC
(sim-step)  ;  LDA #80
(test-reg RA #x80)
(sim-step)  ;  CMP #80
(test-mask #x02 #xcb)
(sim-step)  ;  CMP #81
(test-mask #x81 #xcb)
(sim-step)  ;  CMP #7F
(test-mask #x00 #xcb)
(print "==> Testing CMP zero page instruction")
(terpri)
(sim-step)  ;  CMP 00
(test-mask #x00 #xcb)
(sim-step)  ;  CMP 01
(test-mask #x02 #xcb)
(sim-step)  ;  CMP 02
(test-mask #x81 #xcb)
(print "==> Testing CMP zero page,X instruction")
(terpri)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(sim-step)  ;  CMP 80,X
(test-mask #x00 #xcb)
(sim-step)  ;  CMP 81,X
(test-mask #x02 #xcb)
(sim-step)  ;  CMP 82,X
(test-mask #x81 #xcb)
(print "==> Testing CMP absolute instruction")
(terpri)
(sim-step)  ;  CPM 1080
(test-mask #x81 #xcb)
(sim-step)  ;  CMP 1081
(test-mask #x02 #xcb)
(sim-step)  ;  CMP 1082
(test-mask #x00 #xcb)
(print "==> Testing CMP absolute,X instruction")
(terpri)
(sim-step)  ;  CMP 1000,X
(test-mask #x81 #xcb)
(sim-step)  ;  CMP 1001,X
(test-mask #x02 #xcb)
(sim-step)  ;  CMP 1002,X
(test-mask #x00 #xcb)
(print "==> Testing CMP absolute,Y instruction")
(terpri)
(sim-step)  ;  LDY #40
(test-reg RIY #x40)
(sim-step)  ;  CMP 1040,Y
(test-mask #x81 #xcb)
(sim-step)  ;  CMP 1041,Y
(test-mask #x02 #xcb)
(sim-step)  ;  CMP 1042,Y
(test-mask #x00 #xcb)
(print "==> Testing CMP (indirect,X) instruction")
(terpri)
(sim-step)  ;  CMP 1040,Y
(test-mask #x81 #xcb)
(sim-step)  ;  CMP 1041,Y
(test-mask #x02 #xcb)
(sim-step)  ;  CMP 1042,Y
(test-mask #x00 #xcb)
(print "==> Testing CMP (indirect),Y instruction")
(terpri)
;
;-------------------------------------------------------------------------------
;  Test CPX instructions
;
(memb #x0000 #x7f)  ;  Data 7F
(memb #x0001 #x80)  ;  Data 80
(memb #x0002 #x81)  ;  Data 81
;
(memb #x1080 #x81)  ;  Data 81
(memb #x1081 #x80)  ;  Data 80
(memb #x1082 #x7f)  ;  Data 7F
;
(memb #x0200 #xb8)    ;  CLV
(memb #x0201 #x18)    ;  CLC
(memw #x0202 #xa280)  ;  LDX #80
(memw #x0204 #xe07f)  ;  CPX #7F
(memw #x0206 #xe080)  ;  CPX #80
(memw #x0208 #xe081)  ;  CPX #81
;
(memw #x020a #xe400)  ;  CPX 00
(memw #x020c #xe401)  ;  CPX 01
(memw #x020e #xe402)  ;  CPX 02
;
(memb #x0210 #xec)    ;  CPX 1080
(memw #x0211 #x8010)
(memb #x0213 #xec)    ;  CPX 1081
(memw #x0214 #x8110)
(memb #x0216 #xec)    ;  CPX 1082
(memw #x0217 #x8210)
;
;  Execute test
;
(print "==> Testing CPX immediate instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  CLV
(sim-step)  ;  CLC
(sim-step)  ;  LDY #80
(test-reg RIX #x80)
(sim-step)  ;  CPX #7F
(test-mask #x00 #xcb)
(sim-step)  ;  CPX #80
(test-mask #x02 #xcb)
(sim-step)  ;  CPX #81
(test-mask #x81 #xcb)
(print "==> Testing CPX zero page instruction")
(terpri)
(sim-step)  ;  CPX 00
(test-mask #x00 #xcb)
(sim-step)  ;  CPX 01
(test-mask #x02 #xcb)
(sim-step)  ;  CPX 02
(test-mask #x81 #xcb)
(print "==> Testing CPX absolute instruction")
(terpri)
(sim-step)  ;  CPX 00
(test-mask #x81 #xcb)
(sim-step)  ;  CPX 01
(test-mask #x02 #xcb)
(sim-step)  ;  CPX 02
(test-mask #x00 #xcb)
;
;-------------------------------------------------------------------------------
;  Test CPY instructions
;
(memb #x0000 #x7f)  ;  Data 7F
(memb #x0001 #x80)  ;  Data 80
(memb #x0002 #x81)  ;  Data 81
;
(memb #x1080 #x81)  ;  Data 81
(memb #x1081 #x80)  ;  Data 80
(memb #x1082 #x7f)  ;  Data 7F
;
(memb #x0200 #xb8)    ;  CLV
(memb #x0201 #x18)    ;  CLC
(memw #x0202 #xa080)  ;  LDY #80
(memw #x0204 #xc081)  ;  CPY #81
(memw #x0206 #xc080)  ;  CPY #80
(memw #x0208 #xc07f)  ;  CPY #7F
;
(memw #x020a #xc400)  ;  CPY 00
(memw #x020c #xc401)  ;  CPY 01
(memw #x020e #xc402)  ;  CPY 02
;
(memb #x0210 #xcc)    ;  CPY 1080
(memw #x0211 #x8010)
(memb #x0213 #xcc)    ;  CPY 1081
(memw #x0214 #x8110)
(memb #x0216 #xcc)    ;  CPY 1082
(memw #x0217 #x8210)
;
;  Execute test
;
(print "==> Testing CPY immediate instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  CLV
(sim-step)  ;  CLC
(sim-step)  ;  LDY #80
(test-reg RIY #x80)
(sim-step)  ;  CPY #81
(test-mask #x81 #xcb)
(sim-step)  ;  CPY #80
(test-mask #x02 #xcb)
(sim-step)  ;  CPY #7F
(test-mask #x00 #xcb)
(print "==> Testing CPY zero page instruction")
(terpri)
(sim-step)  ;  CPY 00
(test-mask #x00 #xcb)
(sim-step)  ;  CPY 01
(test-mask #x02 #xcb)
(sim-step)  ;  CPY 02
(test-mask #x81 #xcb)
(print "==> Testing CPY absolute instruction")
(terpri)
(sim-step)  ;  CPY 00
(test-mask #x81 #xcb)
(sim-step)  ;  CPY 01
(test-mask #x02 #xcb)
(sim-step)  ;  CPY 02
(test-mask #x00 #xcb)
;
;-------------------------------------------------------------------------------
;  Test JMP instructions
;
(memw #x1000 #x0c02)  ;  Address 020C
;
(memw #x0200 #xa900)  ;  LDA #00
(memb #x0202 #x4c)    ;  JMP 0207
(memw #x0203 #x0702)
(memw #x0205 #xa980)  ;  LDA #80
(memb #x0207 #x6c)    ;  JMP (1000)
(memw #x0208 #x0010)
(memw #x020a #xa940)  ;  LDA #40
(memw #x020c #xa920)  ;  LDA #20
;
;  Execute test
;
(print "==> Testing JMP absolute and indirect instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(test-reg RPC #x0202)
(sim-step)  ;  JMP 0207
(test-reg RA #x00)
(test-reg RPC #x0207)
(sim-step)  ;  JMP (1000)
(test-reg RA #x00)
(test-reg RPC #x020c)
(sim-step)  ;  LDA #20
(test-reg RA #x20)
;
;-------------------------------------------------------------------------------
;  Test LSR instructions
;
(memb #x00 #xff)  ;  Data FF
(memb #x01 #x01)  ;  Data 01
;
(memb #x80 #xff)  ;  Data FF
(memb #x81 #x01)  ;  Data 01
;
(memb #x1040 #xff)  ;  Data FF
(memb #x1041 #x01)  ;  Data 01
;
(memb #x1080 #xff)  ;  Data FF
(memb #x1081 #x01)  ;  Data 01
;
(memw #x0200 #xa9ff)  ;  LDA #FF
(memb #x0202 #x4a)    ;  LSR A
(memw #x0203 #xa901)  ;  LDA #01
(memb #x0205 #x4a)    ;  LSR A
(memb #x0206 #x4a)    ;  LSR A
;
(memw #x0207 #x4600)  ;  LSR 00
(memw #x0209 #x4601)  ;  LSR 01
(memw #x020b #x4601)  ;  LSR 01
;
(memw #x020d #xa240)  ;  LDX #40
(memw #x020f #x5640)  ;  LSR 40,X
(memw #x0211 #x5641)  ;  LSR 41,X
(memw #x0213 #x5641)  ;  LSR 41,X
;
(memb #x0215 #x4e)    ;  LSR 1040
(memw #x0216 #x4010)
(memb #x0218 #x4e)    ;  LSR 1041
(memw #x0219 #x4110)
(memb #x021b #x4e)    ;  LSR 1041
(memw #x021c #x4110)
;
(memw #x021e #xa220)  ;  LDX #20
(memb #x0220 #x5e)    ;  LSR 1060,X
(memw #x0221 #x6010)
(memb #x0223 #x5e)    ;  LSR 1061,X
(memw #x0224 #x6110)
(memb #x0226 #x5e)    ;  LSR 1061,X
(memw #x0227 #x6110)
;
;  Execute test
;
(print "==> Testing LSR accumulator instruction")
(terpri)
(sim-init)
(go #x0200)
(sim-step)  ;  LDA #FF
(test-reg RA #xff)
(test-mask #x80 #xcb)
(sim-step)  ;  LSR A
(test-reg RA #x7f)
(test-mask #x01 #xcb)
(sim-step)  ;  LDA #01
(test-reg RA #x01)
(test-mask #x01 #xcb)
(sim-step)  ;  LSR A
(test-reg RA #x00)
(test-mask #x03 #xcb)
(sim-step)  ;  LSR A
(test-reg RA #x00)
(test-mask #x02 #xcb)
(print "==> Testing LSR zero page instruction")
(terpri)
(sim-step)  ;  LSR 00
(test-memb #x00 #x7f)
(test-mask #x01 #xcb)
(sim-step)  ;  LSR 01
(test-memb #x01 #x00)
(test-mask #x03 #xcb)
(sim-step)  ;  LSR 01
(test-memb #x01 #x00)
(test-mask #x02 #xcb)
(print "==> Testing LSR zero page,X instruction")
(terpri)
(sim-step)  ;  LDX #40
(test-reg RIX #x40)
(sim-step)  ;  LSR 40,X
(test-memb #x00 #x7f)
(test-mask #x01 #xcb)
(sim-step)  ;  LSR 41,X
(test-memb #x01 #x00)
(test-mask #x03 #xcb)
(sim-step)  ;  LSR 41,X
(test-memb #x01 #x00)
(test-mask #x02 #xcb)
(print "==> Testing LSR absolute instruction")
(terpri)
(sim-step)  ;  LSR 1040
(test-memb #x00 #x7f)
(test-mask #x01 #xcb)
(sim-step)  ;  LSR 1041
(test-memb #x01 #x00)
(test-mask #x03 #xcb)
(sim-step)  ;  LSR 1041
(test-memb #x01 #x00)
(test-mask #x02 #xcb)
(print "==> Testing LSR absolute,X instruction")
(terpri)
(sim-step)  ;  LDX #20
(test-reg RIX #x20)
(sim-step)  ;  LSR 1060,X
(test-memb #x00 #x7f)
(test-mask #x01 #xcb)
(sim-step)  ;  LSR 1061,X
(test-memb #x01 #x00)
(test-mask #x03 #xcb)
(sim-step)  ;  LSR 1061,X
(test-memb #x01 #x00)
(test-mask #x02 #xcb)
;
;-------------------------------------------------------------------------------
;  Test transfer instructions
;
(memw #x0200 #xa900)  ;  LDA #00
(memb #x0202 #xaa)    ;  TAX
(memb #x0203 #xa8)    ;  TAY
(memw #x0204 #xa901)  ;  LDA #01
(memb #x0206 #xaa)    ;  TAX
(memb #x0207 #xa8)    ;  TAY
(memw #x0208 #xa980)  ;  LDA #80
(memb #x020a #xaa)    ;  TAX
(memb #x020b #xa8)    ;  TAY
;
(memw #x020c #xa900)  ;  LDA #00
(memw #x020e #xa255)  ;  LDX #55
(memw #x0210 #xa0aa)  ;  LDY #AA
(memb #x0212 #x8a)    ;  TXA
(memb #x0213 #x98)    ;  TYA
(memw #x0214 #xa2aa)  ;  LDX #AA
(memw #x0216 #xa055)  ;  LDY #x55
(memw #x0218 #xa900)  ;  LDA #x00
(memb #x021a #x8a)    ;  TXA
(memb #x021b #x98)    ;  TYA
(memw #x021c #xa200)  ;  LDX #x00
(memw #x021e #xa000)  ;  LDY #x00
(memb #x0220 #x8a)    ;  TXA
(memb #x0221 #x98)    ;  TYA
;
(memw #x0222 #xa200)  ;  LDX #00
(memb #x0224 #x9a)    ;  TXS
(memw #x0225 #xa280)  ;  LDX #80
(memb #x0227 #xba)    ;  TSX
(memw #x0228 #xa280)  ;  LDX #80
(memb #x022a #x9a)    ;  TXS
(memw #x022b #xa27f)  ;  LDX #7F
(memb #x022d #xba)    ;  TSX
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
(test-reg RA #x00)
(test-reg RIX #x00)
(test-mask #x02 #x82)
(sim-step)  ;  TAY
(test-reg RA #x00)
(test-reg RIX #x00)
(test-reg RIY #x00)
(test-mask #x02 #x82)
(sim-step)  ;  LDA #1
(test-reg RA #x01)
(test-reg RIX #x00)
(test-reg RIY #x00)
(test-mask #x00 #x82)
(sim-step)  ;  TAX
(test-reg RA #x01)
(test-reg RIX #x01)
(test-reg RIY #x00)
(test-mask #x00 #x82)
(sim-step)  ;  TAY
(test-reg RA #x01)
(test-reg RIX #x01)
(test-reg RIY #x01)
(test-mask #x00 #x82)
(sim-step)  ;  LDA #-128
(test-reg RA #x80)
(test-reg RIX #x01)
(test-reg RIY #x01)
(test-mask #x80 #x82)
(sim-step)  ;  TAX
(test-reg RA #x80)
(test-reg RIX #x80)
(test-reg RIY #x01)
(test-mask #x80 #x82)
(sim-step)  ;  TAY
(test-reg RA #x80)
(test-reg RIX #x80)
(test-reg RIY #x80)
(test-mask #x80 #x82)
(print "==> Testing TXA and TYA instructions")
(terpri)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(sim-step)  ;  LDX #55
(test-reg RA #x00)
(test-reg RIX #x55)
(sim-step)  ;  LDY #AA
(test-reg RA #x00)
(test-reg RIX #x55)
(test-reg RIY #xaa)
(sim-step)  ;  TXA
(test-reg RA #x55)
(test-reg RIX #x55)
(test-reg RIY #xaa)
(test-mask #x00 #xcb)
(sim-step)  ;  TYA
(test-reg RA #xaa)
(test-reg RIX #x55)
(test-reg RIY #xaa)
(test-mask #x80 #xcb)
(sim-step)  ;  LDX #AA
(test-reg RA #xaa)
(test-reg RIX #xaa)
(test-reg RIY #xaa)
(sim-step)  ;  LDY #55
(test-reg RA #xaa)
(test-reg RIX #xaa)
(test-reg RIY #x55)
(sim-step)  ;  LDA #00
(test-reg RA #x00)
(test-reg RIX #xaa)
(test-reg RIY #x55)
(sim-step)  ;  TXA
(test-reg RA #xaa)
(test-reg RIX #xaa)
(test-reg RIY #x55)
(test-mask #x80 #xcb)
(sim-step)  ;  TYA
(test-reg RA #x55)
(test-reg RIX #xaa)
(test-reg RIY #x55)
(test-mask #x00 #xcb)
(sim-step)  ;  LDX #00
(test-reg RA #x55)
(test-reg RIX #x00)
(test-reg RIY #x55)
(sim-step)  ;  LDY #00
(test-reg RA #x55)
(test-reg RIX #x00)
(test-reg RIY #x00)
(sim-step)  ;  TXA
(test-reg RA #x00)
(test-reg RIX #x00)
(test-reg RIY #x00)
(test-mask #x02 #xcb)
(sim-step)  ;  TYA
(test-reg RA #x00)
(test-reg RIX #x00)
(test-reg RIY #x00)
(test-mask #x02 #xcb)
(print "==> Testing TXS and TSX instructions")
(terpri)
(sim-step)  ;  LDX #00
(test-reg RIX #x00)
(sim-step)  ;  TXS
(test-reg RIX #x00)
(test-reg RSP #x00)
(test-mask #x02 #xcb)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(test-reg RSP #x00)
(test-mask #x80 #xcb)
(sim-step)  ;  TSX
(test-reg RIX #x00)
(test-reg RSP #x00)
(test-mask #x02 #xcb)
(sim-step)  ;  LDX #80
(test-reg RIX #x80)
(test-reg RSP #x00)
(test-mask #x80 #xcb)
(sim-step)  ;  TXS
(test-reg RIX #x80)
(test-reg RSP #x80)
(test-mask #x80 #xcb)
(sim-step)  ;  LDX #7F
(test-reg RIX #x7f)
(test-reg RSP #x80)
(test-mask #x00 #xcb)
(sim-step)  ;  TXS
(test-reg RIX #x80)
(test-reg RSP #x80)
(test-mask #x80 #xcb)
;-------------------------------------------------------------------------------
;
;  End of test cases
;;  Status register bits are S|O|-|B|D|I|Z|C
;                           7 6 5 4 3 2 1 0

(print "===> Testing complete")
(terpri)
(summary)
(exit)
