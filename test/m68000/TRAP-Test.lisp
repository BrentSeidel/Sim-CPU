;
;  Load software
;
;  TRAP Exception vectors
;
(meml #x0080 #x00001022)  ;  TRAP 0
(meml #x0084 #x00001026)
(meml #x0088 #x0000102a)
(meml #x008c #x0000102e)
(meml #x0090 #x00001032)
(meml #x0094 #x00001036)
(meml #x0098 #x0000103a)
(meml #x009c #x0000103e)
(meml #x00a0 #x00001042)
(meml #x00a4 #x00001046)
(meml #x00a8 #x0000104a)
(meml #x00ac #x0000104e)
(meml #x00b0 #x00001052)
(meml #x00b4 #x00001056)
(meml #x00b8 #x0000105a)
(meml #x00bc #x0000105e)  ;  TRAP 15
;
;  Program code
;
(memw #x1000 #x70ff) ; MOVE.L #$ffffffff,D0
(memw #x1002 #x4e40) ; TRAP #0
(memw #x1004 #x4e41) ; TRAP #1
(memw #x1006 #x4e42) ; TRAP #2
(memw #x1008 #x4e43) ; TRAP #3
(memw #x100a #x4e44) ; TRAP #4
(memw #x100c #x4e45) ; TRAP #5
(memw #x100e #x4e46) ; TRAP #6
(memw #x1010 #x4e47) ; TRAP #7
(memw #x1012 #x4e48) ; TRAP #8
(memw #x1014 #x4e49) ; TRAP #9
(memw #x1016 #x4e4a) ; TRAP #10
(memw #x1018 #x4e4b) ; TRAP #11
(memw #x101a #x4e4c) ; TRAP #12
(memw #x101c #x4e4d) ; TRAP #13
(memw #x101e #x4e4e) ; TRAP #14
(memw #x1020 #x4e4f) ; TRAP #15
; Exception handlers
(memw #x1022 #x7000) ; MOVEQ #0,D0
(memw #x1024 #x4e73) ; RTE
(memw #x1026 #x7001) ; MOVEQ #1,D0
(memw #x1028 #x4e73) ; RTE
(memw #x102a #x7002) ; MOVEQ #2,D0
(memw #x102c #x4e73) ; RTE
(memw #x102e #x7003) ; MOVEQ #3,D0
(memw #x1030 #x4e73) ; RTE
(memw #x1032 #x7004) ; MOVEQ #4,D0
(memw #x1034 #x4e73) ; RTE
(memw #x1036 #x7005) ; MOVEQ #5,D0
(memw #x1038 #x4e73) ; RTE
(memw #x103a #x7006) ; MOVEQ #6,D0
(memw #x103c #x4e73) ; RTE
(memw #x103e #x7007) ; MOVEQ #7,D0
(memw #x1040 #x4e73) ; RTE
(memw #x1042 #x7008) ; MOVEQ #8,D0
(memw #x1044 #x4e73) ; RTE
(memw #x1046 #x7009) ; MOVEQ #9,D0
(memw #x1048 #x4e73) ; RTE
(memw #x104a #x700a) ; MOVEQ #10,D0
(memw #x104c #x4e73) ; RTE
(memw #x104e #x700b) ; MOVEQ #11,D0
(memw #x1050 #x4e73) ; RTE
(memw #x1052 #x700c) ; MOVEQ #12,D0
(memw #x1054 #x4e73) ; RTE
(memw #x1056 #x700d) ; MOVEQ #13,D0
(memw #x1058 #x4e73) ; RTE
(memw #x105a #x700e) ; MOVEQ #14,D0
(memw #x105c #x4e73) ; RTE
(memw #x105e #x700f) ; MOVEQ #15,D0
(memw #x1060 #x4e73) ; RTE
;
;  Define function
;
(defun test-reg (reg-num expected)
  (print "Reg " reg-num " expected " expected " actual " (reg-val reg-num))
  (if (= expected (reg-val reg-num))
    (print " Pass")
    (print " *** FAIL ***"))
  (terpri))
;
;  Execute test
;
(go #x1000)
(print "Testing TRAP instruction")
(terpri)
(sim-step)
(test-reg 0 #xffffffff)
;
(sim-step)
(test-reg 17 #x1022)
(sim-step)
(test-reg 0 0)
(sim-step)
(test-reg 17 #x1004)
;
(sim-step)
(test-reg 17 #x1026)
(sim-step)
(test-reg 0 1)
(sim-step)
(test-reg 17 #x1006)
;
(sim-step)
(test-reg 17 #x102a)
(sim-step)
(test-reg 0 2)
(sim-step)
(test-reg 17 #x1008)
;
(sim-step)
(test-reg 17 #x102e)
(sim-step)
(test-reg 0 3)
(sim-step)
(test-reg 17 #x100a)
;
(sim-step)
(test-reg 17 #x1032)
(sim-step)
(test-reg 0 4)
(sim-step)
(test-reg 17 #x100c)
;
(sim-step)
(test-reg 17 #x1036)
(sim-step)
(test-reg 0 5)
(sim-step)
(test-reg 17 #x100e)
;
(sim-step)
(test-reg 17 #x103a)
(sim-step)
(test-reg 0 6)
(sim-step)
(test-reg 17 #x1010)
;
(sim-step)
(test-reg 17 #x103e)
(sim-step)
(test-reg 0 7)
(sim-step)
(test-reg 17 #x1012)
;
(sim-step)
(test-reg 17 #x1042)
(sim-step)
(test-reg 0 8)
(sim-step)
(test-reg 17 #x1014)
;
(sim-step)
(test-reg 17 #x1046)
(sim-step)
(test-reg 0 8)
(sim-step)
(test-reg 17 #x1016)
;
(sim-step)
(test-reg 17 #x104a)
(sim-step)
(test-reg 0 10)
(sim-step)
(test-reg 17 #x1018)
;
(sim-step)
(test-reg 17 #x104e)
(sim-step)
(test-reg 0 11)
(sim-step)
(test-reg 17 #x101a)
;
(sim-step)
(test-reg 17 #x1052)
(sim-step)
(test-reg 0 12)
(sim-step)
(test-reg 17 #x101c)
;
(sim-step)
(test-reg 17 #x1056)
(sim-step)
(test-reg 0 13)
(sim-step)
(test-reg 17 #x101e)
;
(sim-step)
(test-reg 17 #x105a)
(sim-step)
(test-reg 0 14)
(sim-step)
(test-reg 17 #x1020)
;
(sim-step)
(test-reg 17 #x105e)
(sim-step)
(test-reg 0 15)
(sim-step)
(test-reg 17 #x1022)
