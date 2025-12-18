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
;  Test MOV instructions
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
;
;-------------------------------------------------------------------------------
;  Test MOVB instructions
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
(print "==> Testing MOV instructions flags")
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
;===============================================================================
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
