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
l
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
;  Status register bits are S|O|-|B|D|I|Z|C
;                           7 6 5 4 3 2 1 0
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
(test-mask #x88 #xcd)
(sim-step)  ;  ADC #80
(test-reg RA #x60)
(test-mask #x49 #xcd)
(sim-step)  ;  ADC #00
(test-reg RA #x61)
(test-mask #x08 #xcd)
(sim-step)  ;  ADC #01
(test-reg RA #x62)
(test-mask #x08 #xcd)
(sim-step)  ;  ADC #x09
(test-reg RA #x71)
(test-mask #x08 #xcd)
(print "==> Testing ADC other addressing modes")
(terpri)
(sim-step)  ;  CLD
(sim-step)  ;  CLC
(sim-step)  ;  LDA #10
(test-reg RA #x10)
(test-mask #x00 #xcd)
(sim-step)  ;  ADC 10
(test-reg RA #x24)
(test-mask #x00 #xcd)
(sim-step)  ;  LDX #10
(test-reg RIX #x10)
(sim-step)  ;  ADC 1,X
(test-reg RA #x56)
(test-mask #x00 #xcd)
(sim-step)  ;  ADC 1000
(test-reg RA #x68)
(test-mask #x00 #xcd)
(sim-step)  ;  ADC FF1,X
(test-reg RA #x8b)
(test-mask #xc0 #xcd)
(sim-step)  ;  LDY #20
(test-reg RIY #x20)
(sim-step)  ;  ADC FE2,Y
(test-reg RA #xBF)
(test-mask #xc0 #xcd)
(sim-step)  ;  ADC (10,X)
(test-reg RA #xf1)
(test-mask #xc0 #xcd)
(sim-step)  ;  ADC (20),Y
(test-reg RA #x14)
(test-mask #x41 #xcd)
;-------------------------------------------------------------------------------
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
