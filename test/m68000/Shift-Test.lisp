;
;  Load software
;
(memw #x1000 #x5555) ; DC.W $5555
(memw #x1002 #xe1f8) ; ASL DATA
(memw #x1004 #x1000)
(memw #x1006 #xe0f8) ; ASR DATA
(memw #x1008 #x1000)
(memw #x100a #x0680) ; ADD.L #$55555555,D0
(memw #x100c #x5555)
(memw #x100e #x5555)
(memw #x1010 #x5601) ; ADD.B #3,D1
(memw #x1012 #xe300) ; ASL.B #1,D0
(memw #x1014 #xe340) ; ASL.W #1,D0
(memw #x1016 #xe380) ; ASL.L #1,D0
(memw #x1018 #xe2a0) ; ASR.L D1,D0
(memw #x101a #xe260) ; ASR.W D1,D0
(memw #x101c #xe220) ; ASR.B D1,D0
(memw #x101e #x4280) ; CLR.L D0
(memw #x1020 #x0680) ; ADD.L #$55555555,D0
(memw #x1022 #x5555)
(memw #x1024 #x5555)
(memw #x1026 #xe308) ; LSL.B #1,D0
(memw #x1028 #xe348) ; LSL.W #1,D0
(memw #x102a #xe388) ; LSL.L #1,D0
(memw #x102c #xe2a8) ; LSR.L D1,D0
(memw #x102e #xe268) ; LSR.W D1,D0
(memw #x1030 #xe228) ; LSR.B D1,D0
;
;  Define functions
;
(defun test-reg (reg-num expected)
  (print "Reg " reg-num " expected " expected " actual " (reg-val reg-num))
  (if (= expected (reg-val reg-num))
    (print " Pass")
    (print " *** FAIL ***"))
  (terpri))
(defun test-memw (address expected)
  (print "Memory " address " expected " expected " actual " (memw address))
  (if (= expected (memw address))
    (print " Pass")
    (print " *** FAIL ***"))
  (terpri))
;
;  Execute test
;
(go #x1002)
(test-memw #x1000 #x5555)
(print "Testing memory ASL/R")
(terpri)
(sim-step)
(test-memw #x1000 #xaaaa)
(test-reg 18 #x200a)
(sim-step)
(test-memw #x1000 #xd555)
(test-reg 18 #x2008)
(print "Testing fixed count ASL/R")
(terpri)
(sim-step)
(test-reg 0 #x55555555)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 3)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x555555aa)
(test-reg 18 #x200a)
(sim-step)
(test-reg 0 #x5555ab54)
(test-reg 18 #x200a)
(sim-step)
(test-reg 0 #xaaab56a8)
(test-reg 18 #x200a)
(print "Testing register count ASL/R")
(terpri)
(sim-step)
(test-reg 0 #xf5556ad5)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xf5550d5a)
(test-reg 18 #x2011)
(sim-step)
(test-reg 0 #xf5550d0b)
(test-reg 18 #x2000)
(print "Testing LSL/LSR")
(terpri)
(sim-step)
(test-reg 0 0)
(sim-step)
(test-reg 0 #x55555555)
(print "Testing fixed count LSL/R")
(terpri)
(sim-step)
(test-reg 0 #x555555aa)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #x5555ab54)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xaaab56a8)
(test-reg 18 #x2008)
(print "Testing register count LSL/R")
(terpri)
(sim-step)
(test-reg 0 #x15556ad5)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x15550d5a)
(test-reg 18 #x2011)
(sim-step)
(test-reg 0 #x15550d0b)
(test-reg 18 #x2000)
