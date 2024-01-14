;
;  Load software
;
(memw #x1000 #x5555) ; DC.W $5555
;
(memw #x1002 #xe1f8) ; ASL DATA
(memw #x1004 #x1000)
(memw #x1006 #xe0f8) ; ASR DATA
(memw #x1008 #x1000)
;
(memw #x100a #x203c) ; MOVE.L #$55555555,D0
(memw #x100c #x5555)
(memw #x100e #x5555)
(memw #x1010 #x7203) ; MOVEQ #3,D1
(memw #x1012 #xe300) ; ASL.B #1,D0
(memw #x1014 #xe340) ; ASL.W #1,D0
(memw #x1016 #xe380) ; ASL.L #1,D0
(memw #x1018 #xe2a0) ; ASR.L D1,D0
(memw #x101a #xe260) ; ASR.W D1,D0
(memw #x101c #xe220) ; ASR.B D1,D0
;
(memw #x101e #x203c) ; MOVE.L #$55555555,D0
(memw #x1020 #x5555)
(memw #x1022 #x5555)
(memw #x1024 #xe308) ; LSL.B #1,D0
(memw #x1026 #xe348) ; LSL.W #1,D0
(memw #x1028 #xe388) ; LSL.L #1,D0
(memw #x102a #xe2a8) ; LSR.L D1,D0
(memw #x102c #xe268) ; LSR.W D1,D0
(memw #x102e #xe228) ; LSR.B D1,D0
;
(memw #x1030 #xe7f8) ; ROL DATA
(memw #x1032 #x1000)
(memw #x1034 #xe7f8) ; ROL DATA
(memw #x1036 #x1000)
(memw #x1038 #xe6f8) ; ROR DATA
(memw #x103a #x1000)
(memw #x103c #xe6f8) ; ROR DATA
(memw #x103e #x1000)
;
(memw #x1040 #x203c) ; MOVE.L #$55555555,D0
(memw #x1042 #x5555)
(memw #x1044 #x5555)
(memw #x1046 #xe318) ; ROL.B #1,D0
(memw #x1048 #xe358) ; ROL.W #1,D0
(memw #x104a #xe398) ; ROL.L #1,D0
(memw #x104c #xe2b8) ; ROR.L D1,D0
(memw #x104e #xe278) ; ROL.W D1,D0
(memw #x1050 #xe238) ; ROL.B D1,D0
;
(memw #x1052 #xe5f8) ; ROXL DATA
(memw #x1054 #x1000)
(memw #x1056 #xe5f8) ; ROXL DATA
(memw #x1058 #x1000)
(memw #x105a #xe5f8) ; ROXL DATA
(memw #x105c #x1000)
(memw #x105e #xe4f8) ; ROXR DATA
(memw #x1060 #x1000)
(memw #x1062 #xe4f8) ; ROXR DATA
(memw #x1064 #x1000)
(memw #x1066 #xe4f8) ; ROXR DATA
(memw #x1068 #x1000)
;
(memw #x106a #x203c) ; MOVE.L #$55555555,D0
(memw #x106c #x5555)
(memw #x106e #x5555)
(memw #x1070 #x7203) ; MOVEQ #3,D1
(memw #x1072 #x44fc) ; MOVE #$10,CCR
(memw #x1074 #x0010)
;
(memw #x1076 #xe710) ; ROXL.B #3,D0
(memw #x1078 #xe750) ; ROXL.W #3,D0
(memw #x107a #xe790) ; ROXL.L #3,D0
(memw #x107c #xe2b0) ; ROXR.L D1,D0
(memw #x107e #xe270) ; ROXR.W D1,D0
(memw #x1080 #xe230) ; ROXR.B D1,D0
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
(print "Testing memory ROL/R")
(terpri)
(sim-step)
(test-memw #x1000 #xaaab)
(test-reg 18 #x2009)
(sim-step)
(test-memw #x1000 #x5557)
(test-reg 18 #x2001)
(sim-step)
(test-memw #x1000 #xaaab)
(test-reg 18 #x2009)
(sim-step)
(test-memw #x1000 #xd555)
(test-reg 18 #x2009)
(print "Testing fixed count ROL/R")
(terpri)
(sim-step)
(test-reg 0 #x55555555)
(sim-step)
(test-reg 0 #x555555aa)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #x5555ab54)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xaaab56a8)
(test-reg 18 #x2008)
(sim-step)
(print "Testing register count ROL/R")
(terpri)
(test-reg 0 #x15556ad5)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x1555ad5a)
(test-reg 18 #x2009)
(sim-step)
(test-reg 0 #x1555ad4b)
(test-reg 18 #x2000)
(print "Testing memory ROXL/R")
(terpri)
(sim-step)
(test-memw #x1000 #xaaaa)
(test-reg 18 #x2019)
(sim-step)
(test-memw #x1000 #x5555)
(test-reg 18 #x2011)
(sim-step)
(test-memw #x1000 #xaaab)
(test-reg 18 #x2008)
(sim-step)
(test-memw #x1000 #x5555)
(test-reg 18 #x2011)
(sim-step)
(test-memw #x1000 #xaaaa)
(test-reg 18 #x2019)
(sim-step)
(test-memw #x1000 #xd555)
(test-reg 18 #x2008)
(print "Testing register ROXL/R setup")
(terpri)
(sim-step)
(test-reg 0 #x55555555)
(test-reg 18 #x2000)
(sim-step)
(test-reg 1 3)
(test-reg 18 #x2000)
(sim-step)
(test-reg 18 #x2010)
(print "Testing fixed count ROXL")
(terpri)
(sim-step)
(test-reg 0 #x555555ad)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #x5555ad69)
(test-reg 18 #x2008)
(sim-step)
(test-reg 0 #xaaad6b49)
(test-reg 18 #x2008)
(print "Testing register count ROXR")
(terpri)
(sim-step)
(test-reg 0 #x5555ad69)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x555555ad)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 #x55555555)
(test-reg 18 #x2011)
