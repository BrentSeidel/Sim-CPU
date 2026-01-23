;
;  Lisp test cases for PDP-11 simulator
(sim-cpu "PDP-11/TEST")
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
(setq R0 0)
(setq R1 1)
(setq R2 2)
(setq R3 3)
(setq R4 4)
(setq R5 5)
(setq USP 6)
(setq KSP 7)
(setq SSP 8)
(setq PC 9)
(setq PSW 10)
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
(setq MPSW #x0f)
;
(defun test-mask (expected mask)
  (print "PSW expected ")
  (print-hex expected)
  (print ", masked ")
  (print-hex (and expected mask))
  (print ", actual ")
  (print-hex (and (reg-val PSW) mask))
  (if (= (and expected mask) (and (reg-val PSW) mask))
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
  (print-hex (memll address))
  (if (= expected (memll address))
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
  (print-hex (memlw address))
  (if (= expected (memlw address))
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
;  Test MOV instruction
;
; Load memory
;
;  Addresses
;
(memlw #x0100 #x1000)
(memlw #x0102 #x1008)
(memlw #x0104 #x102a)
(memlw #x0106 #x1010)
;
(memlw #x0200 #x0000)
;
;  Instructions
;
(memlw #x1000 #o012700)  ;  MOV #123456, R0
(memlw #x1002 #o123456)
(memlw #x1004 #o012701)  ;  MOV #103050, R1
(memlw #x1006 #o103050)
(memlw #x1008 #o012702)  ;  MOV #000100, R2
(memlw #x100a #o000100)
(memlw #x100c #o012703)  ;  MOV #000200, R3
(memlw #x100e #o000200)
(memlw #x1010 #o012704)  ;  MOV #000300, R4
(memlw #x1012 #o000300)
(memlw #x1014 #o012705)  ;  MOV #000400, R5
(memlw #x1016 #o000400)
(memlw #x1018 #o012706)  ;  MOV #000600, SP
(memlw #x101a #o000500)
;
(memlw #x101c #o010001)  ;  MOV R0, R1
;
(memlw #x101e #o012737)  ;  MOV #010203, @#0x0100
(memlw #x1020 #o010203)
(memlw #x1022 #x0200)
;
(memlw #x1024 #o012700)  ;  MOV #0x1000, R0
(memlw #x1026 #x1000)
(memlw #x1028 #o012001)  ;  MOV (R0)+, R1
(memlw #x102a #o012002)  ;  MOV (R0)+, R2
;
(memlw #x102c #o014003)  ;  MOV -(R0), R3
(memlw #x102e #o014004)  ;  MOV -(R0), R4
;
(memlw #x1030 #o016005)  ;  MOV 16(R0), R5
(memlw #x1032 #o000016)
(memlw #x1034 #o017005)  ;  MOV @46(R0), R5
(memlw #x1036 #o000046)
;
(memlw #x1038 #o012700)  ;  MOV #0x0100, R0
(memlw #x103a #x0100)
(memlw #x103c #o011001)  ;  MOV (R0), R1
(memlw #x103e #o013002)  ;  MOV @(R0)+, R2
(memlw #x1040 #o013003)  ;  MOV @(R0)+, R3
(memlw #x1042 #o013004)  ;  MOV @(R0)+, R4
(memlw #x1044 #o013005)  ;  MOV @(R0)+, R5
(memlw #x1046 #o015002)  ;  MOV @-(R0), R2
(memlw #x1048 #o015003)  ;  MOV @-(R0), R3
(memlw #x104a #o015004)  ;  MOV @-(R0), R4
(memlw #x104c #o015005)  ;  MOV @-(R0), R5
;
(memlw #x104e #o012700)  ;  MOV #0, R0
(memlw #x1050 #o000000)
(memlw #x1052 #o012701)  ;  MOV #0x7FFF, R1
(memlw #x1054 #x7FFF)
(memlw #x1056 #o012702)  ;  MOV #0x8000, R2
(memlw #x1058 #x8000)
(memlw #x105a #o012703)  ;  MOV #0xFFFF, R3
(memlw #x105c #xFFFF)
;
(memlw #x105e #o012700)  ;  MOV #0x2000, R0
(memlw #x1060 #x2000)
(memlw #x1062 #o010020)  ;  MOV R0, (R0)+
(memlw #x1064 #o010050)  ;  MOV R0, -(R0)
;
;  Execute test
;
(terpri)
(print "==> Testing MOV instructions and addressing modes immediate source and register destination")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #123456, R0
(test-reg R0 #o123456)
(test-reg PC #x1004)
(sim-step) ; MOV #103050, R1
(test-reg R1 #o103050)
(test-reg PC #x1008)
(sim-step) ; MOV #000100, R2
(test-reg R2 #o000100)
(test-reg PC #x100c)
(sim-step) ; MOV #000200, R3
(test-reg R3 #o000200)
(test-reg PC #x1010)
(sim-step) ; MOV #000300, R4
(test-reg R4 #o000300)
(test-reg PC #x1014)
(sim-step) ; MOV #000400, R5
(test-reg R5 #o000400)
(test-reg PC #x1018)
(sim-step) ; MOV #000500, SP
(test-reg USP #o000000)
(test-reg KSP #o000500)
(test-reg SSP #o000000)
(test-reg PC #x101c)
(terpri)
(print "==> Testing MOV instructions and addressing modes register source and destination")
(terpri)
(sim-step) ; MOV R0, R1
(test-reg R0 #o123456)
(test_reg R1 #o123456)
(test-reg PC #x101e)
(terpri)
(print "==> Testing MOV instructions and addressing modes immediate source and absolute destination")
(terpri)
(test-memw #x0200 #o000000)
(sim-step) ; MOV #010203, @#0x0100
(test-reg PC #x1024)
(test-memw #x0200 #o010203)
(terpri)
(print "==> Testing MOV instructions and addressing modes register post increment source and register destination")
(terpri)
(sim-step) ; MOV #0x1000, R0
(test-reg R0 #x1000)
(test-reg PC #x1028)
(sim-step) ; MOV (R0)+, R1
(test-reg R0 #x1002)
(test-reg R1 #o012700)
(test-reg PC #x102a)
(sim-step) ; MOV (R0)+, R2
(test-reg R0 #x1004)
(test-reg R2 #o123456)
(test-reg PC #x102c)
(terpri)
(print "==> Testing MOV instructions and addressing modes register pre decrement source and register destination")
(terpri)
(sim-step) ; MOV -(R0), R3
(test-reg R0 #x1002)
(test-reg R3 #o123456)
(test-reg PC #x102e)
(sim-step) ; MOV -(R0), R4
(test-reg R0 #x1000)
(test-reg R4 #o012700)
(test-reg PC #x1030)
(terpri)
(print "==> Testing MOV instructions and addressing modes index source and register destination")
(terpri)
(sim-step) ; MOV 16(R0), R5
(test-reg R0 #x1000)
(test-reg R5 #o000200)
(test-reg PC #x1034)
(sim-step) ; MOV @46(R0), R4
(test-reg R0 #x1000)
(test-reg R5 #o012700)
(test-reg PC #x1038)
(terpri)
(print "==> Testing MOV instructions and addressing modes deferred source and register destination")
(terpri)
(sim-step) ; MOV #0x0100, R0
(test-reg R0 #x0100)
(test-reg PC #x103c)
(sim-step) ; MOV (R0), R1
(test-reg R0 #x0100)
(test-reg R1 #x1000)
(test-reg PC #x103e)
(sim-step) ; MOV @(R0)+, R2
(test-reg R0 #x0102)
(test-reg R2 #o012700)
(test-reg PC #x1040)
(sim-step) ; MOV @(R0)+, R3
(test-reg R0 #x0104)
(test-reg R3 #o012702)
(test-reg PC #x1042)
(sim-step) ; MOV @(R0)+, R4
(test-reg R0 #x0106)
(test-reg R4 #o012002)
(test-reg PC #x1044)
(sim-step) ; MOV @(R0)+, R5
(test-reg R0 #x0108)
(test-reg R5 #o012704)
(test-reg PC #x1046)
(sim-step) ; MOV @-(R0), R2
(test-reg R0 #x0106)
(test-reg R2 #o012704)
(test-reg PC #x1048)
(sim-step) ; MOV @-(R0), R3
(test-reg R0 #x0104)
(test-reg R3 #o012002)
(test-reg PC #x104a)
(sim-step) ; MOV @-(R0), R4
(test-reg R0 #x0102)
(test-reg R4 #o012702)
(test-reg PC #x104c)
(sim-step) ; MOV @-(R0), R5
(test-reg R0 #x0100)
(test-reg R5 #o012700)
(test-reg PC #x104e)
(terpri)
(print "==> Testing MOV instructions flags")
(terpri)
(sim-step) ; MOV #0, R0
(test-reg R0 0)
(test-reg PC #x1052)
(test-mask 4 MPSW)
(sim-step) ; MOV #0x7FFF, R1
(test-reg R1 #x7fff)
(test-reg PC #x1056)
(test-mask 0 MPSW)
(sim-step) ; MOV #0x8000, R2
(test-reg R2 #x8000)
(test-reg PC #x105a)
(test-mask 8 MPSW)
(sim-step) ; MOV #0xFFFF, R3
(test-reg R3 #xFFFF)
(test-reg PC #x105e)
(test-mask 8 MPSW)
(terpri)
(print "==> Testing MOV processor differences")
(terpri)
(sim-step) ; MOV #0x2000, R0
(test-reg R0 #x2000)
(test-ref PC #x1060)
(sim-step) ; MOV R0, (R0)+
(test-reg R0 #x2002)
(test-reg PC #x1064)
(test-memw #x2000 #x2000)
(sim-step) ; MOV R0, -(R0)
(test-reg R0 #x2000)
(test-reg PC #x1066)
(test-memw #x2000 #x2002)
;
;-------------------------------------------------------------------------------
;  Test MOVB instruction
;
; Load memory
;
;  Addresses
;
(memlw #x0100 #x1000)
(memlw #x0102 #x1008)
(memlw #x0104 #x102a)
(memlw #x0106 #x1010)
;
(memlw #x0200 #x0000)
;
;  Instructions
;
(memlw #x1000 #o112700)  ;  MOVB #123456, R0
(memlw #x1002 #o123456)
(memlw #x1004 #o112701)  ;  MOVB #103050, R1
(memlw #x1006 #o103050)
(memlw #x1008 #o112702)  ;  MOVB #000100, R2
(memlw #x100a #o000100)
(memlw #x100c #o112703)  ;  MOVB #000200, R3
(memlw #x100e #o000200)
(memlw #x1010 #o112704)  ;  MOVB #000300, R4
(memlw #x1012 #o000300)
(memlw #x1014 #o112705)  ;  MOVB #000400, R5
(memlw #x1016 #o000400)
(memlw #x1018 #o112706)  ;  MOVB #000600, SP
(memlw #x101a #o000500)
;
(memlw #x101c #o110001)  ;  MOVB R0, R1
;
(memlw #x101e #o112737)  ;  MOVB #110203, @#0x0100
(memlw #x1020 #o010203)
(memlw #x1022 #x0200)
;
(memlw #x1024 #o012700)  ;  MOV #0x1000, R0
(memlw #x1026 #x1000)
(memlw #x1028 #o112001)  ;  MOVB (R0)+, R1
(memlw #x102a #o112002)  ;  MOVB (R0)+, R2
;
(memlw #x102c #o114003)  ;  MOVB -(R0), R3
(memlw #x102e #o114004)  ;  MOVB -(R0), R4
;
(memlw #x1030 #o116005)  ;  MOVB 16(R0), R5
(memlw #x1032 #o000016)
(memlw #x1034 #o117005)  ;  MOVB @46(R0), R5
(memlw #x1036 #o000046)
;
(memlw #x1038 #o012700)  ;  MOV #0x0100, R0
(memlw #x103a #x0100)
(memlw #x103c #o111001)  ;  MOVB (R0), R1
(memlw #x103e #o113002)  ;  MOVB @(R0)+, R2
(memlw #x1040 #o113003)  ;  MOVB @(R0)+, R3
(memlw #x1042 #o113004)  ;  MOVB @(R0)+, R4
(memlw #x1044 #o113005)  ;  MOVB @(R0)+, R5
(memlw #x1046 #o115002)  ;  MOVB @-(R0), R2
(memlw #x1048 #o115003)  ;  MOVB @-(R0), R3
(memlw #x104a #o115004)  ;  MOVB @-(R0), R4
(memlw #x104c #o115005)  ;  MOVB @-(R0), R5
;
(memlw #x104e #o112700)  ;  MOVB #0, R0
(memlw #x1050 #o000000)
(memlw #x1052 #o112701)  ;  MOVB #0x7FFF, R1
(memlw #x1054 #x7FFF)
(memlw #x1056 #o112702)  ;  MOVB #0x8000, R2
(memlw #x1058 #x8000)
(memlw #x105a #o112703)  ;  MOVB #0xFFFF, R3
(memlw #x105c #xFFFF)
;
(memlw #x105e #o012706)  ;  MOV #0x2000, R6
(memlw #x1060 #x2000)
(memlw #x1062 #o110346)  ;  MOVB R3, -(R6)
(memlw #x1064 #o112602)  ;  MOVB (R6)+, R2
;
;  Execute test
;
(terpri)
(print "==> Testing MOVB instructions and addressing modes immediate source and register destination")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOVB #123456, R0
(test-reg R0 #o056)
(test-reg PC #x1004)
(sim-step) ; MOVB #103050, R1
(test-reg R1 #o050)
(test-reg PC #x1008)
(sim-step) ; MOVB #000100, R2
(test-reg R2 #o100)
(test-reg PC #x100c)
(sim-step) ; MOVB #000200, R3
(test-reg R3 #o177600)
(test-reg PC #x1010)
(sim-step) ; MOVB #000300, R4
(test-reg R4 #o177700)
(test-reg PC #x1014)
(sim-step) ; MOVB #000400, R5
(test-reg R5 #o000)
(test-reg PC #x1018)
(sim-step) ; MOVB #000500, SP
(test-reg USP #o000000)
(test-reg KSP #o100)
(test-reg SSP #o000000)
(test-reg PC #x101c)
(terpri)
(print "==> Testing MOVB instructions and addressing modes register source and destination")
(terpri)
(sim-step) ; MOVB R0, R1
(test-reg R0 #o000056)
(test_reg R1 #o000056)
(test-reg PC #x101e)
(terpri)
(print "==> Testing MOVB instructions and addressing modes immediate source and absolute destination")
(terpri)
(test-memw #x0200 #o000000)
(sim-step) ; MOVB #010203, @#0x0100
(test-reg PC #x1024)
(test-memw #x0200 #o000203)
(terpri)
(print "==> Testing MOVB instructions and addressing modes register post increment source and register destination")
(terpri)
(sim-step) ; MOV #0x1000, R0
(test-reg R0 #x1000)
(test-reg PC #x1028)
(sim-step) ; MOVB (R0)+, R1
(test-reg R0 #x1001)
(test-reg R1 #o177700)
(test-reg PC #x102a)
(sim-step) ; MOVB (R0)+, R2
(test-reg R0 #x1002)
(test-reg R2 #o177625)
(test-reg PC #x102c)
(terpri)
(print "==> Testing MOVB instructions and addressing modes register pre decrement source and register destination")
(terpri)
(sim-step) ; MOVB -(R0), R3
(test-reg R0 #x1001)
(test-reg R3 #o177625)
(test-reg PC #x102e)
(sim-step) ; MOVB -(R0), R4
(test-reg R0 #x1000)
(test-reg R4 #o177700)
(test-reg PC #x1030)
(terpri)
(print "==> Testing MOVB instructions and addressing modes index source and register destination")
(terpri)
(sim-step) ; MOVB 16(R0), R5
(test-reg R0 #x1000)
(test-reg R5 #o177600)
(test-reg PC #x1034)
(sim-step) ; MOVB @46(R0), R4
(test-reg R0 #x1000)
(test-reg R5 #o177700)
(test-reg PC #x1038)
(terpri)
(print "==> Testing MOVB instructions and addressing modes deferred source and register destination")
(terpri)
(sim-step) ; MOV #0x0100, R0
(test-reg R0 #x0100)
(test-reg PC #x103c)
(sim-step) ; MOVB (R0), R1
(test-reg R0 #x0100)
(test-reg R1 #x0000)
(test-reg PC #x103e)
(sim-step) ; MOVB @(R0)+, R2
(test-reg R0 #x0102)
(test-reg R2 #o177700)
(test-reg PC #x1040)
(sim-step) ; MOVB @(R0)+, R3
(test-reg R0 #x0104)
(test-reg R3 #o177702)
(test-reg PC #x1042)
(sim-step) ; MOVB @(R0)+, R4
(test-reg R0 #x0106)
(test-reg R4 #o000002)
(test-reg PC #x1044)
(sim-step) ; MOVB @(R0)+, R5
(test-reg R0 #x0108)
(test-reg R5 #o177704)
(test-reg PC #x1046)
(sim-step) ; MOVB @-(R0), R2
(test-reg R0 #x0106)
(test-reg R2 #o177704)
(test-reg PC #x1048)
(sim-step) ; MOVB @-(R0), R3
(test-reg R0 #x0104)
(test-reg R3 #o000002)
(test-reg PC #x104a)
(sim-step) ; MOVB @-(R0), R4
(test-reg R0 #x0102)
(test-reg R4 #o177702)
(test-reg PC #x104c)
(sim-step) ; MOVB @-(R0), R5
(test-reg R0 #x0100)
(test-reg R5 #o177700)
(test-reg PC #x104e)
(terpri)
(print "==> Testing MOVB instructions flags")
(terpri)
(sim-step) ; MOVB #0, R0
(test-reg R0 0)
(test-reg PC #x1052)
(test-mask 4 MPSW)
(sim-step) ; MOVB #0x7FFF, R1
(test-reg R1 #xffff)
(test-reg PC #x1056)
(test-mask 8 MPSW)
(sim-step) ; MOVB #0x8000, R2
(test-reg R2 #x0000)
(test-reg PC #x105a)
(test-mask 4 MPSW)
(sim-step) ; MOVB #0xFFFF, R3
(test-reg R3 #xFFFF)
(test-reg PC #x105e)
(test-mask 8 MPSW)
(terpri)
(print "==> Testing MOVB SP incrememnt/decrememt")
(terpri)
(sim-step) ; MOV #0x2000, R6
(test-reg KSP #x2000)
(test-reg PC #x1062)
(sim-step) ; MOVB R3, -(R6)
(test-reg KSP #x1FFE)
(test-reg PC #x1064)
(test-memw #x1FFE #x00FF)
;
;-------------------------------------------------------------------------------
;  Test CMP instruction
;
; Load memory
;
(memlw #x1000 #o022727)  ;  CMP #123456, #123456
(memlw #x1002 #o123456)
(memlw #x1004 #o123456)
(memlw #x1006 #o022727)  ;  CMP #123456, #0
(memlw #x1008 #o123456)
(memlw #x100a #o000000)
(memlw #x100c #o022727)  ;  CMP #177777, #1
(memlw #x100e #o177777)
(memlw #x1010 #o000001)
(memlw #x1012 #o022727)  ;  CMP #077777, #1
(memlw #x1014 #o077777)
(memlw #x1016 #o000001)
(memlw #x1018 #o022727)  ;  CMP #100000, #1
(memlw #x101a #o100000)
(memlw #x101c #o000001)
(memlw #x101e #o022727)  ;  CMP #0, #1
(memlw #x1020 #o000000)
(memlw #x1022 #o000001)
;
;  Execute test
;
(terpri)
(print "==> Testing CMP instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; CMP #123456, #123456
(test-reg PC #x1006)
(test-mask 4 MPSW)
(sim-step) ; CMP #123456, #0
(test-reg PC #x100c)
(test-mask 8 MPSW)
(sim-step) ; CMP #177777, #1
(test-reg PC #x1012)
(test-mask 8 MPSW)
(sim-step) ; CMP #077777, #1
(test-reg PC #x1018)
(test-mask 0 MPSW)
(sim-step) ; CMP #100000, #1
(test-reg PC #x101e)
(test-mask 2 MPSW)
(sim-step) ; CMP #0, #1
(test-reg PC #x1024)
(test-mask 9 MPSW)
;
;-------------------------------------------------------------------------------
;  Test CMPB instruction
;
; Load memory
;
(memlw #x1000 #o122727)  ;  CMPB #56, #56
(memlw #x1002 #o000056)
(memlw #x1004 #o000056)
(memlw #x1006 #o122727)  ;  CMPB #356, #0
(memlw #x1008 #o000356)
(memlw #x100a #o000000)
(memlw #x100c #o122727)  ;  CMPB #377, #1
(memlw #x100e #o000377)
(memlw #x1010 #o000001)
(memlw #x1012 #o122727)  ;  CMPB #177, #1
(memlw #x1014 #o000177)
(memlw #x1016 #o000001)
(memlw #x1018 #o122727)  ;  CMPB #200, #1
(memlw #x101a #o000200)
(memlw #x101c #o000001)
(memlw #x101e #o122727)  ;  CMPB #0, #1
(memlw #x1020 #o000000)
(memlw #x1022 #o000001)
;
;  Execute test
;
(terpri)
(print "==> Testing CMPB instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; CMPB #123456, #123456
(test-reg PC #x1006)
(test-mask 4 MPSW)
(sim-step) ; CMPB #123456, #0
(test-reg PC #x100c)
(test-mask 8 MPSW)
(sim-step) ; CMPB #177777, #1
(test-reg PC #x1012)
(test-mask 8 MPSW)
(sim-step) ; CMPB #077777, #1
(test-reg PC #x1018)
(test-mask 0 MPSW)
(sim-step) ; CMPB #100000, #1
(test-reg PC #x101e)
(test-mask 2 MPSW)
(sim-step) ; CMPB #0, #1
(test-reg PC #x1024)
(test-mask 9 MPSW)
;
;-------------------------------------------------------------------------------
;  Test ADD instruction
;
; Load memory
;
(memlw #x1000 #o012701)  ;  MOV #1, R1
(memlw #x1002 #x0001)
(memlw #x1004 #o012702)  ;  MOV #0x7FFF, R2
(memlw #x1006 #x7fff)
(memlw #x1008 #o060102)  ;  ADD R1, R2
(memlw #x100a #o060202)  ;  ADD R2, R2
(memlw #x100c #o012702)  ;  MOV #x7fff, R2
(memlw #x100e #x7fff)
(memlw #x1010 #o060202)  ;  ADD R2, R2
(memlw #x1012 #o012701)  ;  MOV #0xFFFF, R1
(memlw #x1014 #xffff)
(memlw #x1016 #o012702)  ;  MOV #0x7FFF, R2
(memlw #x1018 #x7fff)
(memlw #x101a #o060102)  ;  ADD R1, R2
;
;  Execute test
;
(terpri)
(print "==> Testing ADD instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #1, R1
(test-reg R1 1)
(test-reg PC #x1004)
(sim-step) ; MOV #0x7fff, R2
(test-reg R2 #x7fff)
(test-reg PC #x1008)
(sim-step) ; ADD R1, R2
(test-reg R1 1)
(test-reg R2 #x8000)
(test-reg PC #x100a)
(test-mask 8 MPSW)
(sim-step) ; ADD R2, R2
(test-reg R2 #x0000)
(test-reg PC #x100c)
(test-mask 1 MPSW)
(sim-step) ; MOV #0x7fff, R2
(test-reg R2 #x7fff)
(test-reg PC #x1010)
(sim-step) ; ADD R2, R2
(test-reg R2 #xfffe)
(test-reg PC #x1012)
(test-mask 8 MPSW)
(sim-step) ; MOV #0xFFFF, R1
(test-reg R1 #xFFFF)
(test-reg PC #x1016)
(sim-step) ; MOV #0x7FFF, R2
(test-reg R2 #x7FFF)
(test-reg PC #x101a)
(sim-step) ; ADD R1, R2
(test-reg R1 #xFFFF)
(test-reg R2 #x7FFE)
(test-reg PC #x101c)
(test-mask 3 MPSW)
;
;-------------------------------------------------------------------------------
;  Test SUB instruction
;
; Load memory
;
(memlw #x1000 #o012701)  ;  MOV #1, R1
(memlw #x1002 #x1)
(memlw #x1004 #o012702)  ;  MOV #0x8000, R2
(memlw #x1006 #x8000)
(memlw #x1008 #o160102)  ;  SUB R1, R2
(memlw #x100a #o160202)  ;  SUB R2, R2
(memlw #x100c #o012701)  ;  MOV #0xFFFF, R1
(memlw #x100e #xFFFF)
(memlw #x1010 #o012702)  ;  MOV #0x8000, R2
(memlw #x1012 #x8000)
(memlw #x1014 #o160201)  ;  SUB R2, R1
(memlw #x1016 #o012701)  ;  MOV #0xFFFF, R1
(memlw #x1018 #xffff)
(memlw #x101a #o012702)  ;  MOV #0x7FFF, R2
(memlw #x101c #x7fff)
(memlw #x101e #o160102)  ;  SUB R1, R2
;
;  Execute test
;
(terpri)
(print "==> Testing SUB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #1, R1
(test-reg R1 1)
(test-reg PC #x1004)
(sim-step) ; MOV #0x8000, R2
(test-reg R2 #x8000)
(test-reg PC #x1008)
(sim-step) ; SUB R1, R2
(test-reg R1 1)
(test-reg R2 #x7fff)
(test-reg PC #x100a)
(test-mask 0 MPSW)
(sim-step) ; SUB R2, R2
(test-reg R2 #x0000)
(test-reg PC #x100c)
(test-mask 4 MPSW)
(sim-step) ; MOV #0xFFFF, R1
(test-reg R1 #xFFFF)
(test-reg PC #x1010)
(sim-step) ; MOV #0x8000, R2
(test-reg R2 #x8000)
(test-reg PC #x1014)
(sim-step) ; SUB R2, R1
(test-reg R1 #x7FFF)
(test-reg PC #x1016)
(test-mask 0 MPSW)
(sim-step) ; MOV #0xFFFF, R1
(test-reg R1 #xFFFF)
(test-reg PC #x101a)
(sim-step) ; MOV #0x7FFF, R2
(test-reg R2 #x7FFF)
(test-reg PC #x101e)
(sim-step) ; SUB R1, R2
(test-reg R1 #xFFFF)
(test-reg R2 #x8000)
(test-reg PC #x1020)
(test-mask 9 MPSW)
;
;-------------------------------------------------------------------------------
;  Test BIT instruction
;
; Load memory
;
(memlw #x1000 #o032727)  ;  BIT #0xAAAA, #0x5555
(memlw #x1002 #xAAAA)
(memlw #x1004 #x5555)
(memlw #x1006 #o032727)  ;  BIT #0xFFAA, #0x5555
(memlw #x1008 #xFFAA)
(memlw #x100a #x5555)
(memlw #x100c #o032727)  ;  BIT #0xFFFF, #0x8000
(memlw #x100e #xFFFF)
(memlw #x1010 #x8000)
;
;  Execute test
;
(terpri)
(print "==> Testing BIT instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; BIT #0xAAAA, #0x5555
(test-reg PC #x1006)
(test-mask 4 MPSW)
(sim-step) ; BIT #0xFFAA, #0x5555
(test-reg PC #x100c)
(test-mask 0 MPSW)
(sim-step) ; BIT #0xFFFF, #0x8000
(test-reg PC #x1012)
(test-mask 8 MPSW)
;
;-------------------------------------------------------------------------------
;  Test BITB instruction
;
; Load memory
;
(memlw #x1000 #o132727)  ;  BITB #0xFFAA, #0xFF55
(memlw #x1002 #xFFAA)
(memlw #x1004 #xFF55)
(memlw #x1006 #o132727)  ;  BITB #0xFFFA, #0xFF55
(memlw #x1008 #xFFFA)
(memlw #x100a #xFF55)
(memlw #x100c #o132727)  ;  BITB #0xFFFF, #0x0080
(memlw #x100e #xFFFF)
(memlw #x1010 #x0080)
;
;  Execute test
;
(terpri)
(print "==> Testing BITB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; BITB #0xAA, #0x55
(test-reg PC #x1006)
(test-mask 4 MPSW)
(sim-step) ; BITB #0xFFFA, #0xFF55
(test-reg PC #x100c)
(test-mask 0 MPSW)
(sim-step) ; BITB #0xFFFF, #0x0080
(test-reg PC #x1012)
(test-mask 8 MPSW)
;
;-------------------------------------------------------------------------------
;  Test BIC instruction
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #0xFFFF, R0
(memlw #x1002 #xFFFF)
(memlw #x1004 #o042700)  ;  BIC #0x8000, R0
(memlw #x1006 #x8000)
(memlw #x1008 #o042700)  ;  BIC #0x00FF, R0
(memlw #x100a #x00FF)
(memlw #x100c #o042700)  ;  BIC #0x00FF, R0
(memlw #x100e #x00FF)
;
;  Execute test
;
(terpri)
(print "==> Testing BIC instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #0xFFFF, R0
(test-reg R0 #xFFFF)
(test-reg PC #x1004)
(sim-step) ; BIC #8000, R0
(test-reg R0 #x7FFF)
(test-reg PC #x1008)
(test-mask 0 MPSW)
(sim-step) ; BIC #00FF, R0
(test-reg R0 #x7F00)
(test-reg PC #x100c)
(test-mask 0 MPSW)
(sim-step) ; BIC #00FF, R0
(test-reg R0 #x7F00)
(test-reg PC #x1010)
(test-mask 0 MPSW)
;
;-------------------------------------------------------------------------------
;  Test BICB instruction
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #0xFFFF, R0
(memlw #x1002 #xFFFF)
(memlw #x1004 #o142700)  ;  BICB #0x0080, R0
(memlw #x1006 #x0080)
(memlw #x1008 #o142700)  ;  BICB #0x000F, R0
(memlw #x100a #x000F)
(memlw #x100c #o142700)  ;  BICB #0x000F, R0
(memlw #x100e #x000F)
;
;  Execute test
;
(terpri)
(print "==> Testing BICB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #0xFFFF, R0
(test-reg R0 #xFFFF)
(test-reg PC #x1004)
(sim-step) ; BICB #8000, R0
(test-reg R0 #xFF7F)
(test-reg PC #x1008)
(test-mask 0 MPSW)
(sim-step) ; BICB #00FF, R0
(test-reg R0 #xFF70)
(test-reg PC #x100c)
(test-mask 0 MPSW)
(sim-step) ; BICB #00FF, R0
(test-reg R0 #xFF70)
(test-reg PC #x1010)
(test-mask 0 MPSW)
;
;-------------------------------------------------------------------------------
;  Test BIS instruction
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #0x0055, R0
(memlw #x1002 #x0055)
(memlw #x1004 #o052700)  ;  BIS #0x0800, R0
(memlw #x1006 #x0800)
(memlw #x1008 #o052700)  ;  BIS #0x00FF, R0
(memlw #x100a #x00FF)
(memlw #x100c #o052700)  ;  BIS #0x00FF, R0
(memlw #x100e #x80FF)
;
;  Execute test
;
(terpri)
(print "==> Testing BIS instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #0x0055, R0
(test-reg R0 #x0055)
(test-reg PC #x1004)
(sim-step) ; BIS #8000, R0
(test-reg R0 #x0855)
(test-reg PC #x1008)
(test-mask 0 MPSW)
(sim-step) ; BIS #00FF, R0
(test-reg R0 #x08FF)
(test-reg PC #x100c)
(test-mask 0 MPSW)
(sim-step) ; BIS #00FF, R0
(test-reg R0 #x88FF)
(test-reg PC #x1010)
(test-mask 8 MPSW)
;
;-------------------------------------------------------------------------------
;  Test BISB instruction
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #0xFF55, R0
(memlw #x1002 #xFF55)
(memlw #x1004 #o152700)  ;  BISB #0x0080, R0
(memlw #x1006 #xFF80)
(memlw #x1008 #o152700)  ;  BISB #0x000F, R0
(memlw #x100a #x000F)
(memlw #x100c #o152700)  ;  BISB #0x000F, R0
(memlw #x100e #xFF0F)
;
;  Execute test
;
(terpri)
(print "==> Testing BISB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #0xFF55, R0
(test-reg R0 #xFF55)
(test-reg PC #x1004)
(sim-step) ; BISB #0080, R0
(test-reg R0 #xFFD5)
(test-reg PC #x1008)
(test-mask 8 MPSW)
(sim-step) ; BISB #000F, R0
(test-reg R0 #xFFDF)
(test-reg PC #x100c)
(test-mask 8 MPSW)
(sim-step) ; BISB #000F, R0
(test-reg R0 #xFFDF)
(test-reg PC #x1010)
(test-mask 8 MPSW)
;
;-------------------------------------------------------------------------------
;  Test SWAB instruction
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #0x1234, R0
(memlw #x1002 #x1234)
(memlw #x1004 #o012701)  ;  MOV #0x0080, R1
(memlw #x1006 #x0080)
(memlw #x1008 #o012702)  ;  MOV #0x0000, R2
(memlw #x100a #x0000)
(memlw #x100c #o000300)  ;  SWAB R0
(memlw #x100e #o000301)  ;  SWAB R1
(memlw #x1010 #o000302)  ;  SWAB R2
;
;  Execute test
;
(terpri)
(print "==> Testing BISB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #0x1234, R0
(test-reg R0 #x1234)
(test-reg PC #x1004)
(sim-step) ; MOV #0x0080, R1
(test-reg R1 #x0080)
(test-reg PC #x1008)
(sim-step) ; MOV #x0000, R2
(test-reg R2 #x0000)
(test-reg PC #x100c)
(sim-step) ; SWAB R0
(test-reg R0 #x3412)
(test-reg PC #x100e)
(test-mask 0 MPSW)
(sim-step) ; SWAB R1
(test-reg R1 #x8000)
(test-reg PC #x1010)
(test-mask 8 MPSW)
(sim-step) ; SWAB R2
(test-reg R2 #x0000)
(test-reg PC #x1012)
(test-mask 4 MPSW)
;
;-------------------------------------------------------------------------------
;  Test CLR instruction
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #0xFF55, R0
(memlw #x1002 #xFF55)
(memlw #x1004 #o005000)  ;  CLR R0
;
;  Execute test
;
(terpri)
(print "==> Testing CLR instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #0xFF55, R0
(test-reg R0 #xFF55)
(test-reg PC #x1004)
(sim-step) ; CLR R0
(test-reg R0 0)
(test-reg PC #x1006)
(test-mask 4 MPSW)
;
;-------------------------------------------------------------------------------
;  Test COM instruction
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #0xFF55, R0
(memlw #x1002 #xFF55)
(memlw #x1004 #o005100)  ;  COM R0
(memlw #x1006 #o012701)  ;  MOV #0x0001, R1
(memlw #x1008 #x0001)
(memlw #x100a #o005101)  ;  COM R1
(memlw #x100c #o005002)  ;  CLR R2
(memlw #x100e #o005102)  ;  COM R2
(memlw #x1010 #o005102)  ;  COM R2
;
;  Execute test
;
(terpri)
(print "==> Testing COM instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #0xFF55, R0
(test-reg R0 #xFF55)
(test-reg PC #x1004)
(sim-step) ; COM R0
(test-reg R0 #x00AA)
(test-reg PC #x1006)
(test-mask 1 MPSW)
(sim-step) ; MOV #0x0001, R1
(test-reg R1 #x0001)
(test-reg PC #x100a)
(sim-step) ; COM R1
(test-reg R1 #xFFFE)
(test-reg PC #x100c)
(test-mask 9 MPSW)
(sim-step) ; CLR R2
(test-reg R2 0)
(test-reg PC #x100e)
(sim-step) ; COM R2
(test-reg R2 #xFFFF)
(test-reg PC #x1010)
(test-mask 9 MPSW)
(sim-step) ; COM R2
(test-reg R2 0)
(test-reg PC #x1012)
(test-mask 5 MPSW)
;
;-------------------------------------------------------------------------------
;  Test INC/DEC instructions
;
; Load memory
;
(memlw #x1000 #o005000)  ;  CLR R0
(memlw #x1002 #o005300)  ;  DEC R0
(memlw #x1004 #o005200)  ;  INC R0
(memlw #x1006 #o012701)  ;  MOV #0x7FFF, R1
(memlw #x1008 #x7FFF)
(memlw #x100a #o005201)  ;  INC R1
(memlw #x100c #o005201)  ;  INC R1
(memlw #x100e #o005301)  ;  DEC R1
(memlw #x1010 #o005301)  ;  DEC R1
(memlw #x1012 #o005301)  ;  DEC R1
;
;  Execute test
;
(terpri)
(print "==> Testing INC/DEC instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; CLR R0
(test-reg R0 0)
(test-reg PC #x1002)
(sim-step) ; DEC R0
(test-reg R0 #xFFFF)
(test-reg PC #x1004)
(test-mask 8 MPSW)
(sim-step) ; INC R0
(test-reg R0 0)
(test-reg PC #x1006)
(test-mask 4 MPSW)
(sim-step) ; MOV #0x7FFF, R1
(test-reg R1 #x7FFF)
(test-reg PC #x100a)
(sim-step) ; INC R1
(test-reg R1 #x8000)
(test-reg PC #x100c)
(test-mask 10 MPSW)
(sim-step) ; INC R1
(test-reg R1 #x8001)
(test-reg PC #x100e)
(test-mask 8 MPSW)
(sim-step) ; DEC R1
(test-reg R1 #x8000)
(test-reg PC #x1010)
(test-mask 8 MPSW)
(sim-step) ; DEC R1
(test-reg R1 #x7FFF)
(test-reg PC #x1012)
(test-mask 2 MPSW)
(sim-step) ; DEC R1
(test-reg R1 #x7FFE)
(test-reg PC #x1014)
(test-mask 0 MPSW)
;
;-------------------------------------------------------------------------------
;  Test NEG instruction
;
; Load memory
;
(memlw #x1000 #o005000)  ;  CLR R0
(memlw #x1002 #o012701)  ;  MOV #0x8000, R1
(memlw #x1004 #x8000)
(memlw #x1006 #o012702)  ;  MOV #0x7FFF, R2
(memlw #x1008 #x7FFF)
(memlw #x100a #o012703)  ;  MOV #1, R3
(memlw #x100c #x0001)
(memlw #x100e #o005400)  ;  NEG R0
(memlw #x1010 #o005401)  ;  NEG R1
(memlw #x1012 #o005402)  ;  NEG R2
(memlw #x1014 #o005403)  ;  NEG R3
(memlw #x1016 #o005400)  ;  NEG R0
(memlw #x1018 #o005401)  ;  NEG R1
(memlw #x101a #o005402)  ;  NEG R2
(memlw #x101c #o005403)  ;  NEG R3
;
;  Execute test
;
(terpri)
(print "==> Testing NEG instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; CLR R0
(test-reg R0 0)
(test-mask 4 MPSW)
(sim-step) ; MOV #0x8000, R1
(test-reg R1 #x8000)
(sim-step) ; MOV #0x7FFF, R2
(test-reg R2 #x7FFF)
(sim-step) ; MOV #1, R3
(test-reg R3 1)
(sim-step) ; NEG R0
(test-reg R0 0)
(test-reg PC #x1010)
(test-mask 4 MPSW)
(sim-step) ; NEG R1
(test-reg R1 #x8000)
(test-reg PC #x1012)
(test-mask 10 MPSW)
(sim-step) ; NEG R2
(test-reg R2 #x8001)
(test-reg PC #x1014)
(test-mask 8 MPSW)
(sim-step) ; NEG R3
(test-reg R3 #xFFFF)
(test-reg PC #x1016)
(test-mask 8 MPSW)
(sim-step) ; NEG R0
(test-reg R0 0)
(test-reg PC #x1018)
(test-mask 4 MPSW)
(sim-step) ; NEG R1
(test-reg R1 #x8000)
(test-reg PC #x101a)
(test-mask 10 MPSW)
(sim-step) ; NEG R2
(test-reg R2 #x7FFF)
(test-reg PC #x101c)
(test-mask 0 MPSW)
(sim-step) ; NEG R3
(test-reg R3 1)
(test-reg PC #x101e)
(test-mask 0 MPSW)
;
;-------------------------------------------------------------------------------
;  Test ADC/SBC instructions
;
; Load memory
;
(memlw #x1000 #o005000)  ;  CLR R0
(memlw #x1002 #o012701)  ;  MOV #0xFFFF, R1
(memlw #x1004 #xFFFF)
(memlw #x1006 #o012702)  ;  MOV #0x8000, R2
(memlw #x1008 #x8000)
(memlw #x100a #o012703)  ;  MOV #0x7FFF, R3
(memlw #x100c #x7FFF)
(memlw #x100e #o012704)  ;  MOV #1, R4
(memlw #x1010 #x0001)
(memlw #x1012 #o005500)  ;  ADC R0
(memlw #x1014 #o005501)  ;  ADC R1
(memlw #x1016 #o005502)  ;  ADC R2
(memlw #x1018 #o005503)  ;  ADC R3
(memlw #x101a #o005104)  ;  COM R4
(memlw #x101c #o005500)  ;  ADC R0
(memlw #x101e #o005104)  ;  COM R4
(memlw #x1020 #o005501)  ;  ADC R1
(memlw #x1022 #o005104)  ;  COM R4
(memlw #x1024 #o005502)  ;  ADC R2
(memlw #x1026 #o005104)  ;  COM R4
(memlw #x1028 #o005503)  ;  ADC R3
(memlw #x102a #o005005)  ;  CLR R5
(memlw #x102c #o005600)  ;  SBC R0
(memlw #x102e #o005601)  ;  SBC R1
(memlw #x1030 #o005602)  ;  SBC R2
(memlw #x1032 #o005603)  ;  SBC R3
(memlw #x1034 #o005104)  ;  COM R4
(memlw #x1036 #o005600)  ;  SBC R0
(memlw #x1038 #o005104)  ;  COM R4
(memlw #x103a #o005601)  ;  SBC R1
(memlw #x103c #o005104)  ;  COM R4
(memlw #x103e #o005602)  ;  SBC R2
(memlw #x1040 #o005104)  ;  COM R4
(memlw #x1042 #o005603)  ;  SBC R3
;
;  Execute test
;
(terpri)
(print "==> Testing ADC/SBC instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; CLR R0
(test-reg R0 0)
(sim-step) ; MOV #0xFFFF, R1
(test-reg R1 #xFFFF)
(sim-step) ; MOV #0x8000, R2
(test-reg R2 #x8000)
(sim-step) ; MOV #0x7FFF, R3
(test-reg R3 #x7FFF)
(sim-step) ; MOV #1, R4
(test-reg R4 1)
(test-mask 0 MPSW)
(sim-step) ; ADC R0
(test-reg R0 0)
(test-reg PC #x1014)
(test-mask 4 MPSW)
(sim-step) ; ADC R1
(test-reg R1 #xFFFF)
(test-reg PC #x1016)
(test-mask 8 MPSW)
(sim-step) ; ADC R2
(test-reg R2 #x8000)
(test-reg PC #x1018)
(test-mask 8 MPSW)
(sim-step) ; ADC R3
(test-reg R3 #x7FFF)
(test-reg PC #x101a)
(test-mask 0 MPSW)
(sim-step) ; COM R4
(test-reg R4 #xFFFE)
(test-mask 9 MPSW)
(sim-step) ; ADC 0
(test-reg R0 1)
(test-reg PC #x101e)
(test-mask 0 MPSW)
(sim-step) ; COM R4
(test-reg R4 1)
(test-mask 1 MPSW)
(sim-step) ; ADC 1
(test-reg R1 0)
(test-reg PC #x1022)
(test-mask 1 MPSW)
(sim-step) ; COM R4
(test-reg R4 #xFFFE)
(test-mask 9 MPSW)
(sim-step) ; ADC 2
(test-reg R2 #x8001)
(test-reg PC #x1026)
(test-mask 8 MPSW)
(sim-step) ; COM R4
(test-reg R4 1)
(test-mask 1 MPSW)
(sim-step) ; ADC R3
(test-reg R3 #x8000)
(test-reg PC #x102a)
(test-mask 10 MPSW)
(sim-step) ; CLR R5
(test-reg R5 0)
(test-mask 4 MPSW)
(sim-step) ; SBC R0
(test-reg R0 1)
(test-reg PC #x102e)
(test-mask 0 MPSW)
(sim-step) ; SBC R1
(test-reg R1 0)
(test-reg PC #x1030)
(test-mask 4 MPSW)
(sim-step) ; SBC R2
(test-reg R2 #x8001)
(test-reg PC #x1032)
(test-mask 8 MPSW)
(sim-step) ; SBC R3
(test-reg R3 #x8000)
(test-reg PC #x1034)
(test-mask 8 MPSW)
(sim-step) ; COM R4
(test-reg R4 #xFFFE)
(test-mask 9 MPSW)
(sim-step) ; SBC R0
(test-reg R0 0)
(test-reg PC #x1038)
(test-mask 4 MPSW)
(sim-step) ; COM R4
(test-reg R4 #x1)
(test-mask 1 MPSW)
(sim-step) ; SBC R1
(test-reg R1 #xFFFF)
(test-reg PC #x103c)
(test-mask 9 MPSW)
(sim-step) ; COM R4
(test-reg R4 #xFFFE)
(test-mask 9 MPSW)
(sim-step) ; SBC R2
(test-reg R2 #x8000)
(test-reg PC #x1040)
(test-mask 8 MPSW)
(sim-step) ; COM R4
(test-reg R4 1)
(test-mask 1 MPSW)
(sim-step) ; SBC R3
(test-reg R3 #x7FFF)
(test-reg PC #x1044)
(test-mask 2 MPSW)
;
;-------------------------------------------------------------------------------
;  Test TST instruction
;
; Load memory
;
(memlw #x1000 #o005727)  ;  TST #0
(memlw #x1002 #x0000)
(memlw #x1004 #o005727)  ; TST #1
(memlw #x1006 1)
(memlw #x1008 #o005727)  ; TST #0x7FFF
(memlw #x100a #x7FFF)
(memlw #x100c #o005727)  ; TST #0x8000
(memlw #x100e #x8000)
(memlw #x1010 #o005727)  ; TST #0x8001
(memlw #x1012 #x8001)
(memlw #x1014 #o005727)  ; TST #0xFFFF
(memlw #x1016 #xFFFF)
;
;  Execute test
;
(terpri)
(print "==> Testing TST instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; TST #0
(test-reg PC #x1004)
(test-mask 4 MPSW)
(sim-step) ; TST #1
(test-reg PC #x1008)
(test-mask 0 MPSW)
(sim-step) ; TST #0x7FFF
(test-reg PC #x100c)
(test-mask 0 MPSW)
(sim-step) ; TST #0x8000
(test-reg PC #x1010)
(test-mask 8 MPSW)
(sim-step) ; TST #0x8001
(test-reg PC #x1014)
(test-mask 8 MPSW)
(sim-step) ; TST #0xFFFF
(test-reg PC #x1018)
(test-mask 8 MPSW)
;
;-------------------------------------------------------------------------------
;  Test ROR/ROL instructions
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #1, R0
(memlw #x1002 1)
(memlw #x1004 #o006000)  ;  ROR R0
(memlw #x1006 #o006100)  ;  ROL R0
(memlw #x1008 #o006000)  ;  ROR R0
(memlw #x100a #o006000)  ;  ROR R0
(memlw #x100c #o006000)  ;  ROR R0
(memlw #x100e #o006100)  ;  ROL R0
(memlw #x1010 #o006100)  ;  ROL R0
(memlw #x1012 #o006100)  ;  ROL R0
(memlw #x1014 #o006100)  ;  ROL R0
;
;  Execute test
;
(terpri)
(print "==> Testing ROR/ROL instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #1, R0
(test-reg R0 1)
(sim-step) ; ROR R0
(test-reg R0 0)
(test-reg PC #x1006)
(test-mask 7 MPSW)
(sim-step) ; ROL R0
(test-reg R0 1)
(test-reg PC #x1008)
(test-mask 0 MPSW)
(sim-step) ; ROR R0
(test-reg R0 0)
(test-reg PC #x100a)
(test-mask 7 MPSW)
(sim-step) ; ROR R0
(test-reg R0 #x8000)
(test-reg PC #x100c)
(test-mask 10 MPSW)
(sim-step) ; ROR R0
(test-reg R0 #x4000)
(test-reg PC #x100e)
(test-mask 0 MPSW)
(sim-step) ; ROL R0
(test-reg R0 #x8000)
(test-reg PC #x1010)
(test-mask 10 MPSW)
(sim-step) ; ROL R0
(test-reg R0 0)
(test-reg PC #x1012)
(test-mask 7 MPSW)
(sim-step) ; ROL R0
(test-reg R0 1)
(test-reg PC #x1014)
(test-mask 0 MPSW)
(sim-step) ; ROL R0
(test-reg R0 2)
(test-reg PC #x1016)
(test-mask 0 MPSW)
;
;-------------------------------------------------------------------------------
;  Test ASR/ASL instructions
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #1, R0
(memlw #x1002 1)
(memlw #x1004 #o006200)  ;  ASR R0
(memlw #x1006 #o006300)  ;  ASL R0
(memlw #x1008 #o012700)  ;  MOV #0x8000, R0
(memlw #x100a #x8000)
(memlw #x100c #o006200)  ;  ASR R0
(memlw #x100e #o006200)  ;  ASR R0
(memlw #x1010 #o006200)  ;  ASR R0
(memlw #x1012 #o012700)  ;  MOV #0x2000, R0
(memlw #x1014 #x2000)
(memlw #x1016 #o006300)  ;  ASL R0
(memlw #x1018 #o006300)  ;  ASL R0
(memlw #x101a #o006300)  ;  ASL R0
(memlw #x101c #o006300)  ;  ASL R0
;
;  Execute test
;
(terpri)
(print "==> Testing ASR/ASL instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #1, R0
(test-reg R0 1)
(sim-step) ; ASR R0
(test-reg R0 0)
(test-reg PC #x1006)
(test-mask 7 MPSW)
(sim-step) ; ASL R0
(test-reg R0 0)
(test-reg PC #x1008)
(test-mask 4 MPSW)
(sim-step) ; MOV #0x8000, R0
(test-reg R0 #x8000)
(sim-step) ; ASR R0
(test-reg R0 #xC000)
(test-reg PC #x100e)
(test-mask 10 MPSW)
(sim-step) ; ASR R0
(test-reg R0 #xE000)
(test-reg PC #x1010)
(test-mask 10 MPSW)
(sim-step) ; ASR R0
(test-reg R0 #xF000)
(test-reg PC #x1012)
(test-mask 10 MPSW)
(sim-step) ; MOV #0x2000, R0
(test-reg R0 #x2000)
(sim-step) ; ASL R0
(test-reg R0 #x4000)
(test-reg PC #x1018)
(test-mask 0 MPSW)
(sim-step) ; ASL R0
(test-reg R0 #x8000)
(test-reg PC #x101a)
(test-mask 10 MPSW)
(sim-step) ; ASL R0
(test-reg R0 0)
(test-reg PC #x101c)
(test-mask 7 MPSW)
(sim-step) ; ASL R0
(test-reg R0 0)
(test-reg PC #x101e)
(test-mask 4 MPSW)
;
;-------------------------------------------------------------------------------
;  Test JMP instruction
;
; Load memory
;
; Jump table
(memlw #x0100 #x2000)
(memlw #x0102 #x2010)
(memlw #x0104 #x2020)
(memlw #x0106 #x2040)
;
; Main code
(memlw #x1000 #o005000)  ;  CLR R0
(memlw #x1002 #o000170)  ;  JMP @0x100(R0)
(memlw #x1004 #x0100)
;
;  Targets
(memlw #x2000 #o005200)  ;  INC R0
(memlw #x2002 #o005200)  ;  INC R0
(memlw #x2004 #o000137)  ;  JMP @#0x1002
(memlw #x2006 #x1002)
;
(memlw #x2010 #o005200)  ;  INC R0
(memlw #x2012 #o005200)  ;  INC R0
(memlw #x2014 #o000137)  ;  JMP @#0x1002
(memlw #x2016 #x1002)
;
(memlw #x2020 #o005200)  ;  INC R0
(memlw #x2022 #o005200)  ;  INC R0
(memlw #x2024 #o000137)  ;  JMP @#0x1002
(memlw #x2026 #x1002)
;
(memlw #x2040 #o005200)  ;  INC R0
(memlw #x2042 #o005200)  ;  INC R0
(memlw #x2044 #o000137)  ;  JMP @#0x1002
(memlw #x2046 #x1002)
;
;  Execute test
;
(terpri)
(print "==> Testing JMP instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; CLR R0
(test-reg R0 0)
(test-reg PC #x1002)
(sim-step) ; JMP @#x100(R0)
(test-reg PC #x2000)
(sim-step) ; INC R0
(test-reg R0 1)
(sim-step) ; INC R0
(test-reg R0 2)
(test-reg PC #x2004)
(sim-step) ; JMP @#0x1002
(test-reg R0 2)
(test-reg PC #x1002)
(sim-step) ; JMP @0x100(R0)
(test-reg PC #x2010)
(sim-step) ; INC R0
(test-reg R0 3)
(sim-step) ; INC R0
(test-reg R0 4)
(sim-step) ; JMP @#0x1002
(test-reg PC #x1002)
(sim-step) ; JMP @0x100(R0)
(test-reg PC #x2020)
(sim-step) ; INC R0
(test-reg R0 5)
(sim-step) ; INC R0
(test-reg R0 6)
(sim-step) ; JMP @#0x1002
(test-reg PC #x1002)
(sim-step) ; JMP @0x100(R0)
(test-reg PC #x2040)
(sim-step) ; INC R0
(test-reg R0 7)
(sim-step) ; INC R0
(test-reg R0 8)
(sim-step) ; JMP @#0x1002
(test-reg PC #x1002)
;
;-------------------------------------------------------------------------------
;  Test CLRB instruction
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #0xFF55, R0
(memlw #x1002 #xFF55)
(memlw #x1004 #o105000)  ;  CLR R0
;
;  Execute test
;
(terpri)
(print "==> Testing CLRB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #0xFF55, R0
(test-reg R0 #xFF55)
(test-reg PC #x1004)
(sim-step) ; CLRB R0
(test-reg R0 #xFF00)
(test-reg PC #x1006)
(test-mask 4 MPSW)
;
;-------------------------------------------------------------------------------
;  Test COMB instruction
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #0xFF55, R0
(memlw #x1002 #xFF55)
(memlw #x1004 #o105100)  ;  COMB R0
(memlw #x1006 #o012701)  ;  MOV #0x0001, R1
(memlw #x1008 #x0001)
(memlw #x100a #o105101)  ;  COMB R1
(memlw #x100c #o005002)  ;  CLR R2
(memlw #x100e #o105102)  ;  COMB R2
(memlw #x1010 #o105102)  ;  COMB R2
;
;  Execute test
;
(terpri)
(print "==> Testing COMB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #0xFF55, R0
(test-reg R0 #xFF55)
(test-reg PC #x1004)
(sim-step) ; COMB R0
(test-reg R0 #xFFAA)
(test-reg PC #x1006)
(test-mask 9 MPSW)
(sim-step) ; MOV #0x0001, R1
(test-reg R1 #x0001)
(test-reg PC #x100a)
(sim-step) ; COMB R1
(test-reg R1 #x00FE)
(test-reg PC #x100c)
(test-mask 9 MPSW)
(sim-step) ; CLR R2
(test-reg R2 0)
(test-reg PC #x100e)
(sim-step) ; COMB R2
(test-reg R2 #x00FF)
(test-reg PC #x1010)
(test-mask 9 MPSW)
(sim-step) ; COMB R2
(test-reg R2 0)
(test-reg PC #x1012)
(test-mask 5 MPSW)
;
;-------------------------------------------------------------------------------
;  Test INCB/DECB instructions
;
; Load memory
;
(memlw #x1000 #o005000)  ;  CLR R0
(memlw #x1002 #o105300)  ;  DECB R0
(memlw #x1004 #o105200)  ;  INCB R0
(memlw #x1006 #o012701)  ;  MOV #0x7F, R1
(memlw #x1008 #x7F)
(memlw #x100a #o105201)  ;  INCB R1
(memlw #x100c #o105201)  ;  INCB R1
(memlw #x100e #o105301)  ;  DECB R1
(memlw #x1010 #o105301)  ;  DECB R1
(memlw #x1012 #o105301)  ;  DECB R1
;
;  Execute test
;
(terpri)
(print "==> Testing INCB/DECB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; CLR R0
(test-reg R0 0)
(test-reg PC #x1002)
(sim-step) ; DECB R0
(test-reg R0 #xFF)
(test-reg PC #x1004)
(test-mask 8 MPSW)
(sim-step) ; INCB R0
(test-reg R0 0)
(test-reg PC #x1006)
(test-mask 4 MPSW)
(sim-step) ; MOV #0x7F, R1
(test-reg R1 #x7F)
(test-reg PC #x100a)
(sim-step) ; INCB R1
(test-reg R1 #x80)
(test-reg PC #x100c)
(test-mask 10 MPSW)
(sim-step) ; INCB R1
(test-reg R1 #x81)
(test-reg PC #x100e)
(test-mask 8 MPSW)
(sim-step) ; DECB R1
(test-reg R1 #x80)
(test-reg PC #x1010)
(test-mask 8 MPSW)
(sim-step) ; DECB R1
(test-reg R1 #x7F)
(test-reg PC #x1012)
(test-mask 2 MPSW)
(sim-step) ; DECB R1
(test-reg R1 #x7E)
(test-reg PC #x1014)
(test-mask 0 MPSW)
;
;-------------------------------------------------------------------------------
;  Test NEGB instruction
;
; Load memory
;
(memlw #x1000 #o005000)  ;  CLR R0
(memlw #x1002 #o012701)  ;  MOV #0x80, R1
(memlw #x1004 #x80)
(memlw #x1006 #o012702)  ;  MOV #0x7F, R2
(memlw #x1008 #x7F)
(memlw #x100a #o012703)  ;  MOV #1, R3
(memlw #x100c #x0001)
(memlw #x100e #o105400)  ;  NEGB R0
(memlw #x1010 #o105401)  ;  NEGB R1
(memlw #x1012 #o105402)  ;  NEGB R2
(memlw #x1014 #o105403)  ;  NEGB R3
(memlw #x1016 #o105400)  ;  NEGB R0
(memlw #x1018 #o105401)  ;  NEGB R1
(memlw #x101a #o105402)  ;  NEGB R2
(memlw #x101c #o105403)  ;  NEGB R3
;
;  Execute test
;
(terpri)
(print "==> Testing NEGB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; CLR R0
(test-reg R0 0)
(test-mask 4 MPSW)
(sim-step) ; MOV #0x80, R1
(test-reg R1 #x80)
(sim-step) ; MOV #0x7F, R2
(test-reg R2 #x7F)
(sim-step) ; MOV #1, R3
(test-reg R3 1)
(sim-step) ; NEGB R0
(test-reg R0 0)
(test-reg PC #x1010)
(test-mask 4 MPSW)
(sim-step) ; NEGB R1
(test-reg R1 #x80)
(test-reg PC #x1012)
(test-mask 10 MPSW)
(sim-step) ; NEGB R2
(test-reg R2 #x81)
(test-reg PC #x1014)
(test-mask 8 MPSW)
(sim-step) ; NEGB R3
(test-reg R3 #xFF)
(test-reg PC #x1016)
(test-mask 8 MPSW)
(sim-step) ; NEGB R0
(test-reg R0 0)
(test-reg PC #x1018)
(test-mask 4 MPSW)
(sim-step) ; NEGB R1
(test-reg R1 #x80)
(test-reg PC #x101a)
(test-mask 10 MPSW)
(sim-step) ; NEGB R2
(test-reg R2 #x7F)
(test-reg PC #x101c)
(test-mask 0 MPSW)
(sim-step) ; NEGB R3
(test-reg R3 1)
(test-reg PC #x101e)
(test-mask 0 MPSW)
;
;-------------------------------------------------------------------------------
;  Test ADCB/SBCB instructions
;
; Load memory
;
(memlw #x1000 #o005000)  ;  CLR R0
(memlw #x1002 #o012701)  ;  MOV #0xFFFF, R1
(memlw #x1004 #xFFFF)
(memlw #x1006 #o012702)  ;  MOV #0x80, R2
(memlw #x1008 #x80)
(memlw #x100a #o012703)  ;  MOV #0x7F, R3
(memlw #x100c #x7F)
(memlw #x100e #o012704)  ;  MOV #1, R4
(memlw #x1010 #x0001)
(memlw #x1012 #o105500)  ;  ADCB R0
(memlw #x1014 #o105501)  ;  ADCB R1
(memlw #x1016 #o105502)  ;  ADCB R2
(memlw #x1018 #o105503)  ;  ADCB R3
(memlw #x101a #o005104)  ;  COM R4
(memlw #x101c #o105500)  ;  ADCB R0
(memlw #x101e #o005104)  ;  COM R4
(memlw #x1020 #o105501)  ;  ADCB R1
(memlw #x1022 #o005104)  ;  COM R4
(memlw #x1024 #o105502)  ;  ADCB R2
(memlw #x1026 #o005104)  ;  COM R4
(memlw #x1028 #o105503)  ;  ADCB R3
(memlw #x102a #o005005)  ;  CLR R5
(memlw #x102c #o105600)  ;  SBCB R0
(memlw #x102e #o105601)  ;  SBCB R1
(memlw #x1030 #o105602)  ;  SBCB R2
(memlw #x1032 #o105603)  ;  SBCB R3
(memlw #x1034 #o005104)  ;  COM R4
(memlw #x1036 #o105600)  ;  SBCB R0
(memlw #x1038 #o005104)  ;  COM R4
(memlw #x103a #o105601)  ;  SBCB R1
(memlw #x103c #o005104)  ;  COM R4
(memlw #x103e #o105602)  ;  SBCB R2
(memlw #x1040 #o005104)  ;  COM R4
(memlw #x1042 #o105603)  ;  SBCB R3
;
;  Execute test
;
(terpri)
(print "==> Testing ADCB/SBCB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; CLR R0
(test-reg R0 0)
(sim-step) ; MOV #0xFFFF, R1
(test-reg R1 #xFFFF)
(sim-step) ; MOV #0x80, R2
(test-reg R2 #x80)
(sim-step) ; MOV #0x7F, R3
(test-reg R3 #x7F)
(sim-step) ; MOV #1, R4
(test-reg R4 1)
(test-mask 0 MPSW)
(sim-step) ; ADCB R0
(test-reg R0 0)
(test-reg PC #x1014)
(test-mask 4 MPSW)
(sim-step) ; ADCB R1
(test-reg R1 #xFFFF)
(test-reg PC #x1016)
(test-mask 8 MPSW)
(sim-step) ; ADCB R2
(test-reg R2 #x80)
(test-reg PC #x1018)
(test-mask 8 MPSW)
(sim-step) ; ADCB R3
(test-reg R3 #x7F)
(test-reg PC #x101a)
(test-mask 0 MPSW)
(sim-step) ; COM R4
(test-reg R4 #xFFFE)
(test-mask 9 MPSW)
(sim-step) ; ADCB 0
(test-reg R0 1)
(test-reg PC #x101e)
(test-mask 0 MPSW)
(sim-step) ; COM R4
(test-reg R4 1)
(test-mask 1 MPSW)
(sim-step) ; ADCB 1
(test-reg R1 #xFF00)
(test-reg PC #x1022)
(test-mask 5 MPSW)
(sim-step) ; COM R4
(test-reg R4 #xFFFE)
(test-mask 9 MPSW)
(sim-step) ; ADCB 2
(test-reg R2 #x81)
(test-reg PC #x1026)
(test-mask 8 MPSW)
(sim-step) ; COM R4
(test-reg R4 1)
(test-mask 1 MPSW)
(sim-step) ; ADCB R3
(test-reg R3 #x80)
(test-reg PC #x102a)
(test-mask 10 MPSW)
(sim-step) ; CLR R5
(test-reg R5 0)
(test-mask 4 MPSW)
(sim-step) ; SBCB R0
(test-reg R0 1)
(test-reg PC #x102e)
(test-mask 0 MPSW)
(sim-step) ; SBCB R1
(test-reg R1 #xFF00)
(test-reg PC #x1030)
(test-mask 4 MPSW)
(sim-step) ; SBCB R2
(test-reg R2 #x81)
(test-reg PC #x1032)
(test-mask 8 MPSW)
(sim-step) ; SBCB R3
(test-reg R3 #x80)
(test-reg PC #x1034)
(test-mask 8 MPSW)
(sim-step) ; COM R4
(test-reg R4 #xFFFE)
(test-mask 9 MPSW)
(sim-step) ; SBCB R0
(test-reg R0 0)
(test-reg PC #x1038)
(test-mask 4 MPSW)
(sim-step) ; COM R4
(test-reg R4 #x1)
(test-mask 1 MPSW)
(sim-step) ; SBCB R1
(test-reg R1 #xFFFF)
(test-reg PC #x103c)
(test-mask 9 MPSW)
(sim-step) ; COM R4
(test-reg R4 #xFFFE)
(test-mask 9 MPSW)
(sim-step) ; SBCB R2
(test-reg R2 #x80)
(test-reg PC #x1040)
(test-mask 8 MPSW)
(sim-step) ; COM R4
(test-reg R4 1)
(test-mask 1 MPSW)
(sim-step) ; SBCB R3
(test-reg R3 #x7F)
(test-reg PC #x1044)
(test-mask 2 MPSW)
;
;-------------------------------------------------------------------------------
;  Test TSTB instruction
;
; Load memory
;
(memlw #x1000 #o105727)  ;  TSTB #0
(memlw #x1002 #x0000)
(memlw #x1004 #o105727)  ;  TSTB #1
(memlw #x1006 1)
(memlw #x1008 #o105727)  ;  TSTB #0x7F
(memlw #x100a #x7F)
(memlw #x100c #o105727)  ;  TSTB #0x80
(memlw #x100e #x80)
(memlw #x1010 #o105727)  ;  TSTB #0x81
(memlw #x1012 #x81)
(memlw #x1014 #o105727)  ;  TSTB #0xFF
(memlw #x1016 #x00FF)
;
;  Execute test
;
(terpri)
(print "==> Testing TSTB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; TSTB #0
(test-reg PC #x1004)
(test-mask 4 MPSW)
(sim-step) ; TSTB #1
(test-reg PC #x1008)
(test-mask 0 MPSW)
(sim-step) ; TSTB #0x7F
(test-reg PC #x100c)
(test-mask 0 MPSW)
(sim-step) ; TSTB #0x80
(test-reg PC #x1010)
(test-mask 8 MPSW)
(sim-step) ; TSTB #0x81
(test-reg PC #x1014)
(test-mask 8 MPSW)
(sim-step) ; TSTB #0xFF
(test-reg PC #x1018)
(test-mask 8 MPSW)
;
;-------------------------------------------------------------------------------
;  Test RORB/ROLB instructions
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #1, R0
(memlw #x1002 1)
(memlw #x1004 #o106000)  ;  RORB R0
(memlw #x1006 #o106100)  ;  ROLB R0
(memlw #x1008 #o106000)  ;  RORB R0
(memlw #x100a #o106000)  ;  RORB R0
(memlw #x100c #o106000)  ;  RORB R0
(memlw #x100e #o106100)  ;  ROLB R0
(memlw #x1010 #o106100)  ;  ROLB R0
(memlw #x1012 #o106100)  ;  ROLB R0
(memlw #x1014 #o106100)  ;  ROLB R0
;
;  Execute test
;
(terpri)
(print "==> Testing RORB/ROLB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #1, R0
(test-reg R0 1)
(sim-step) ; ROR R0
(test-reg R0 0)
(test-reg PC #x1006)
(test-mask 7 MPSW)
(sim-step) ; ROL R0
(test-reg R0 1)
(test-reg PC #x1008)
(test-mask 0 MPSW)
(sim-step) ; ROR R0
(test-reg R0 0)
(test-reg PC #x100a)
(test-mask 7 MPSW)
(sim-step) ; ROR R0
(test-reg R0 #x80)
(test-reg PC #x100c)
(test-mask 10 MPSW)
(sim-step) ; ROR R0
(test-reg R0 #x40)
(test-reg PC #x100e)
(test-mask 0 MPSW)
(sim-step) ; ROL R0
(test-reg R0 #x80)
(test-reg PC #x1010)
(test-mask 10 MPSW)
(sim-step) ; ROL R0
(test-reg R0 0)
(test-reg PC #x1012)
(test-mask 7 MPSW)
(sim-step) ; ROL R0
(test-reg R0 1)
(test-reg PC #x1014)
(test-mask 0 MPSW)
(sim-step) ; ROL R0
(test-reg R0 2)
(test-reg PC #x1016)
(test-mask 0 MPSW)
;
;-------------------------------------------------------------------------------
;  Test ASRB/ASLB instructions
;
; Load memory
;
(memlw #x1000 #o012700)  ;  MOV #1, R0
(memlw #x1002 1)
(memlw #x1004 #o106200)  ;  ASRB R0
(memlw #x1006 #o106300)  ;  ASLB R0
(memlw #x1008 #o012700)  ;  MOV #0x80, R0
(memlw #x100a #x80)
(memlw #x100c #o106200)  ;  ASRB R0
(memlw #x100e #o106200)  ;  ASRB R0
(memlw #x1010 #o106200)  ;  ASRB R0
(memlw #x1012 #o012700)  ;  MOV #0x20, R0
(memlw #x1014 #x20)
(memlw #x1016 #o106300)  ;  ASLB R0
(memlw #x1018 #o106300)  ;  ASLB R0
(memlw #x101a #o106300)  ;  ASLB R0
(memlw #x101c #o106300)  ;  ASLB R0
;
;  Execute test
;
(terpri)
(print "==> Testing ASRB/ASLB instruction")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #1, R0
(test-reg R0 1)
(sim-step) ; ASRB R0
(test-reg R0 0)
(test-reg PC #x1006)
(test-mask 7 MPSW)
(sim-step) ; ASLB R0
(test-reg R0 0)
(test-reg PC #x1008)
(test-mask 4 MPSW)
(sim-step) ; MOV #0x80, R0
(test-reg R0 #x80)
(sim-step) ; ASRB R0
(test-reg R0 #xC0)
(test-reg PC #x100e)
(test-mask 10 MPSW)
(sim-step) ; ASRB R0
(test-reg R0 #xE0)
(test-reg PC #x1010)
(test-mask 10 MPSW)
(sim-step) ; ASRB R0
(test-reg R0 #xF0)
(test-reg PC #x1012)
(test-mask 10 MPSW)
(sim-step) ; MOV #0x20, R0
(test-reg R0 #x20)
(sim-step) ; ASLB R0
(test-reg R0 #x40)
(test-reg PC #x1018)
(test-mask 0 MPSW)
(sim-step) ; ASLB R0
(test-reg R0 #x80)
(test-reg PC #x101a)
(test-mask 10 MPSW)
(sim-step) ; ASLB R0
(test-reg R0 0)
(test-reg PC #x101c)
(test-mask 7 MPSW)
(sim-step) ; ASLB R0
(test-reg R0 0)
(test-reg PC #x101e)
(test-mask 4 MPSW)
;
;-------------------------------------------------------------------------------
;  Test BR instruction
;
; Load memory
;
(memlw #x1000 #x0100)  ;  BR 0
(memlw #x1002 #x0101)  ;  BR 1
(memlw #x1004 0)
(memlw #x1006 #x01FC)  ;  BR -4
;
;  Execute test
;
(terpri)
(print "==> Testing BR instruction")
(terpri)
(sim-init)
(go #x1000)
(test-reg PC #x1000)
(sim-step) ; BR 0
(test-reg PC #x1002)
(sim-step) ; BR 1
(test-reg PC #x1006)
(sim-step) ; BR -4
(test-reg PC #x1000)
;
;-------------------------------------------------------------------------------
;  Test condition code instructions
;
; Load memory
;
(memlw #x1000 #o000277)  ;  SCC
(memlw #x1002 #o000241)  ;  CLC
(memlw #x1004 #o000242)  ;  CLV
(memlw #x1006 #o000244)  ;  CLZ
(memlw #x1008 #o000250)  ;  CLN
(memlw #x100a #o000261)  ;  SEC
(memlw #x100c #o000262)  ;  SEV
(memlw #x100e #o000264)  ;  SEZ
(memlw #x1010 #o000270)  ;  SEN
(memlw #x1012 #o000257)  ;  CCC
;
;  Execute test
;
(terpri)
(print "==> Testing condition code instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; SCC
(test-reg PC #x1002)
(test-mask 15 MPSW)
(sim-step) ; CLC
(test-reg PC #x1004)
(test-mask 14 MPSW)
(sim-step) ; CLV
(test-reg PC #x1006)
(test-mask 12 MPSW)
(sim-step) ; CLZ
(test-reg PC #x1008)
(test-mask 8 MPSW)
(sim-step) ; CLN
(test-reg PC #x100a)
(test-mask 0 MPSW)
(sim-step) ; SEC
(test-reg PC #x100c)
(test-mask 1 MPSW)
(sim-step) ; SEV
(test-reg PC #x100e)
(test-mask 3 MPSW)
(sim-step) ; SEZ
(test-reg PC #x1010)
(test-mask 7 MPSW)
(sim-step) ; SEN
(test-reg PC #x1012)
(test-mask 15 MPSW)
(sim-step) ; CCC
(test-reg PC #x1014)
(test-mask 0 MPSW)
;
;-------------------------------------------------------------------------------
;  Test branch instructions, part 1
;  This also includes some other condition code instructions.
;
; Load memory
;
(memlw #x1000 #o000257)  ;  CCC
(memlw #x1002 #x0301)  ;  BEQ 1
(memlw #x1004 #x0202)  ;  BNE 2
(memlw #x1006 #o000137)  ;  JMP @#0x1000
(memlw #x1008 #x1000)
(memlw #x100a #o000264)  ;  SEZ
(memlw #x100c #x0201)  ;  BNE 1
(memlw #x100e #x0302)  ;  BEQ 2
(memlw #x1010 #o000137)  ;  JMP @#0x1000
(memlw #x1012 #x1000)
;
(memlw #x1014 #o00257)  ;  CCC
(memlw #x1016 #x0501)  ;  BLT 1
(memlw #x1018 #x0402)  ;  BGE 2
(memlw #x101a #o000137)  ;  JMP @#0x1000
(memlw #x101c #x1000)
(memlw #x101e #o00270)  ;  SEN
(memlw #x1020 #x0401)  ;  BGT 1
(memlw #x1022 #x0502)  ;  BLE 2
(memlw #x1024 #o000137)  ;  JMP @#0x1000
(memlw #x1026 #x1000)
;
(memlw #x1028 #o000277)  ;  SCC
(memlw #x102a #x0601)  ;  BGT 1
(memlw #x102c #x0702)  ;  BLE 2
(memlw #x102e #o000137)  ;  JMP @#0x1000
(memlw #x1030 #x1000)
(memlw #x1032 #o000244)  ;  CLZ
(memlw #x1034 #x0701)  ;  BLE 1
(memlw #x1036 #x0602)  ;  BGT 2
(memlw #x1038 #o000137)  ;  JMP @#0x1000
(memlw #x103a #x1000)
;
;  Execute test
;
(terpri)
(print "==> Testing branch instructions, part 1")
(terpri)
(sim-init)
(go #x1000)
(print "--> BEQ/BNE")
(sim-step) ; CCC
(test-mask 0 MPSW)
(sim-step) ; BEQ 1
(test-reg PC #x1004)
(sim-step) ; BNE 2
(test-reg PC #x100a)
(sim-step) ; SEZ
(test-mask 4 MPSW)
(sim-step) ; BNE 1
(test-reg PC #x100e)
(sim-step) ; BEQ 2
(test-reg PC #x1014)
(print "--> BGE/BLT")
(sim-step) ; CCC
(test-mask 0 MPSW)
(sim-step) ; BLT 1
(test-reg PC #x1018)
(sim-step) ; BGE 2
(test-reg PC #x101e)
(sim-step) ; SEN
(test-mask 8 MPSW)
(sim-step) ; BGE 1
(test-reg PC #x1022)
(sim-step) ; BLT 2
(test-reg PC #x1028)
(print "--> BGT/BLE")
(sim-step) ; SCC
(test-mask 15 MPSW)
(sim-step) ; BGT 1
(test-reg PC #x102c)
(sim-step) ; BLE 2
(test-reg PC #x1032)
(sim-step) ; CLZ
(test-mask 11 MPSW)
(sim-step) ; BLE 1
(test-reg PC #x1036)
(sim-step) ; BGT 1
(test-reg PC #x103c)
;
;-------------------------------------------------------------------------------
;  Test branch instructions, part 2
;  This also includes some other condition code instructions.
;
; Load memory
;
(memlw #x1000 #o000277)  ;  SCC
(memlw #x1002 #x8001)  ;  BPL 1
(memlw #x1004 #x8102)  ;  BMI 2
(memlw #x1006 #o000137)  ;  JMP @#0x1000
(memlw #x1008 #x1000)
(memlw #x100a #o000250)  ;  CLN
(memlw #x100c #x8101)  ;  BMI 1
(memlw #x100e #x8002)  ;  BPL 2
(memlw #x1010 #o000137)  ;  JMP @#0x1000
(memlw #x1012 #x1000)
;
(memlw #x1014 #o00257)  ;  CCC
(memlw #x1016 #x8301)  ;  BLOS 1
(memlw #x1018 #x8202)  ;  BHI 2
(memlw #x101a #o000137)  ;  JMP @#0x1000
(memlw #x101c #x1000)
(memlw #x101e #o00265)  ;  SEZ/C
(memlw #x1020 #x8201)  ;  BHI 1
(memlw #x1022 #x8302)  ;  BLOS 2
(memlw #x1024 #o000137)  ;  JMP @#0x1000
(memlw #x1026 #x1000)
;
(memlw #x1028 #o000257)  ;  CCC
(memlw #x102a #x8501)  ;  BVS 1
(memlw #x102c #x8402)  ;  BVC 2
(memlw #x102e #o000137)  ;  JMP @#0x1000
(memlw #x1030 #x1000)
(memlw #x1032 #o000262)  ;  SEV
(memlw #x1034 #x8401)  ;  BVC 1
(memlw #x1036 #x8502)  ;  BVS 2
(memlw #x1038 #o000137)  ;  JMP @#0x1000
(memlw #x103a #x1000)
;
(memlw #x103c #o000257)  ;  CCC
(memlw #x103e #x8701)  ;  BCS 1
(memlw #x1040 #x8602)  ;  BCC 2
(memlw #x1042 #o000137)  ;  JMP @#0x1000
(memlw #x1044 #x1000)
(memlw #x1046 #o000261)  ;  SEC
(memlw #x1048 #x8601)  ;  BCC 1
(memlw #x104a #x8702)  ;  BCS 2
(memlw #x104c #o000137)  ;  JMP @#0x1000
(memlw #x104e #x1000)
;
;  Execute test
;
(terpri)
(print "==> Testing branch instructions, part 2")
(terpri)
(sim-init)
(go #x1000)
(print "--> BPL/BMI")
(sim-step) ; SCC
(test-mask 15 MPSW)
(sim-step) ; BPL 1
(test-reg PC #x1004)
(sim-step) ; BMI 2
(test-reg PC #x100a)
(sim-step) ; CLN
(test-mask 7 MPSW)
(sim-step) ; BMI 1
(test-reg PC #x100e)
(sim-step) ; BPL 2
(test-reg PC #x1014)
(print "--> BHI/BLOS")
(sim-step) ; CCC
(test-mask 0 MPSW)
(sim-step) ; BLOS 1
(test-reg PC #x1018)
(sim-step) ; BHI 2
(test-reg PC #x101e)
(sim-step) ; SEZ/C
(test-mask 5 MPSW)
(sim-step) ; BHI 1
(test-reg PC #x1022)
(sim-step) ; BLOS 2
(test-reg PC #x1028)
(print "--> BVC/BVS")
(sim-step) ; CCC
(test-mask 0 MPSW)
(sim-step) ; BVS 1
(test-reg PC #x102c)
(sim-step) ; BVC 2
(test-reg PC #x1032)
(sim-step) ; SEV
(test-mask 2 MPSW)
(sim-step) ; BVC 1
(test-reg PC #x1036)
(sim-step) ; BVS 2
(test-reg PC #x103c)
(print "--> BCC/BCS")
(sim-step) ; CCC
(test-mask 0 MPSW)
(sim-step) ; BCS 1
(test-reg PC #x1040)
(sim-step) ; BCC 2
(test-reg PC #x1046)
(sim-step) ; SEC
(test-mask 1 MPSW)
(sim-step) ; BCC 1
(test-reg PC #x104a)
(sim-step) ; BCS 2
(test-reg PC #x1050)
;
;-------------------------------------------------------------------------------
;  Test JSR/RTS insructions
;
; Load memory
;
(memlw #x1000 #o012706)  ;  MOV #0x3000, SP
(memlw #x1002 #x3000)
(memlw #x1004 #o004737)  ;  JSR PC, @#0x2000
(memlw #x1006 #x2000)
(memlw #x1008 #o004537)  ;  JSR R5, @#0x2010
(memlw #x100A #x2010)
;
(memlw #x2000 #o000207)  ;  RTS PC
;
(memlw #x2010 #o000205)  ;  RTS R5
;
;  Execute test
;
(terpri)
(print "==> Testing JSR/RTS instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #0x3000,SP
(test-reg KSP #x3000)
(sim-step) ; JSR PC, @#0x2000
(test-reg KSP #x2FFE)
(test-reg PC #x2000)
(sim-step) ; RTS PC
(test-reg KSP #x3000)
(test-reg PC #x1008)
(sim-step) ; JSR R5, @#0x2010
(test-reg PC #x2010)
(test-reg R5 #x100C)
(test-reg KSP #x2FFE)
(sim-step) ; RTS R5
(test-reg PC #x100C)
(tets-reg KSP #x3000)
(test-reg R5 0)
;
;-------------------------------------------------------------------------------
;  Test various exception insructions
;
; Load memory
;
;  vectors
(memlw #o014 #x2000)  ;  BPT Vector
(memlw #o016 #x000F)
(memlw #o020 #x2010)  ;  IOT Vector
(memlw #o022 #x000C)
(memlw #o030 #x2020)  ;  EMT Instructions
(memlw #o032 #x0000)
(memlw #o034 #x2030)  ;  TRAP instructions
(memlw #o036 #x0000)
;
(memlw #x1000 #o012706)  ;  MOV #0x3000, SP
(memlw #x1002 #x3000)
(memlw #x1004 #o000003)  ;  BPT
(memlw #x1006 #o000004)  ;  IOT
(memlw #x1008 #o104377)  ;  EMT 377
(memlw #x100a #o104400)  ;  TRAP 0
;
(memlw #x2000 #o000002)
(memlw #x2010 #o000002)
(memlw #x2020 #o000002)
(memlw #x2030 #o000002)
;
;  Execute test
;
(terpri)
(print "==> Testing JSR/RTS instructions")
(terpri)
(sim-init)
(go #x1000)
(sim-step) ; MOV #0x3000, SP
(test-reg KSP #x3000)
(print "-->  Testing BPT")
(sim-step) ; BPT
(test-reg PC #x2000)
(test-reg KSP #x2FFC)
(test-mask #x000F #xC00F)
(sim-step) ; RTI
(test-reg PC #x1006)
(test-mask 0 #xC00F)
(test-reg KSP #x3000)
(print "-->  Testing IOT")
(sim-step) ; IOT
(test-reg PC #x2010)
(test-reg KSP #x2FFC)
(test-mask #x000C #xC00F)
(sim-step) ; RTI
(test-reg PC #x1008)
(test-mask 0 #xC00F)
(test-reg KSP #x3000)
(print "-->  Testing EMT")
(sim-step) ; EMT 377
(test-reg PC #x2020)
(test-reg KSP #x2FFC)
(test-mask 0 #xC00F)
(sim-step) ; RTI
(test-reg PC #x100a)
(test-reg KSP #x3000)
(test-mask 0 #xC00F)
(print "-->  Testing TRAP")
(sim-step) ; TRAP 0
(test-reg PC #x2030)
(test-reg KSP #x2FFC)
(test-mask 0 #xC00F)
(sim-step) ; RTI
(test-reg PC #x100c)
(test-reg KSP #x3000)
(test-mask 0 #xC00F)
;
;===============================================================================
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
