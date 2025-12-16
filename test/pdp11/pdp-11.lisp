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
;  Execute test
;
(terpri)
(print "==> Testing MOV instructions and addressing modes")
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
;===============================================================================
;  End of test cases
;
(print "===> Testing complete")
(terpri)
(summary)
(exit)
