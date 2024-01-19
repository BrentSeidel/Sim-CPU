;
;  Load software
;
;  TRAPV Exception vector
;
(meml #x001c #x00001240)
;  TRAP Exception vectors
;
(meml #x0080 #x00001200)  ;  TRAP 0
(meml #x0084 #x00001204)
(meml #x0088 #x00001208)
(meml #x008c #x0000120c)
(meml #x0090 #x00001210)
(meml #x0094 #x00001214)
(meml #x0098 #x00001218)
(meml #x009c #x0000121c)
(meml #x00a0 #x00001220)
(meml #x00a4 #x00001224)
(meml #x00a8 #x00001228)
(meml #x00ac #x0000122c)
(meml #x00b0 #x00001230)
(meml #x00b4 #x00001234)
(meml #x00b8 #x00001238)
(meml #x00bc #x0000123c)  ;  TRAP 15
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
(memw #x1022 #x44fc) ; MOVE #0,CCR
(memw #x1024 #x0000)
(memw #x1026 #x4e76) ; TRAPV
(memw #x1028 #x44fc) ; MOVE #2,CCR
(memw #x102a #x0002)
(memw #x102c #x4e76) ; TRAPV
; Exception handlers
(memw #x1200 #x7000) ; MOVEQ #0,D0
(memw #x1202 #x4e73) ; RTE
(memw #x1204 #x7001) ; MOVEQ #1,D0
(memw #x1206 #x4e73) ; RTE
(memw #x1208 #x7002) ; MOVEQ #2,D0
(memw #x120a #x4e73) ; RTE
(memw #x120c #x7003) ; MOVEQ #3,D0
(memw #x120e #x4e73) ; RTE
(memw #x1210 #x7004) ; MOVEQ #4,D0
(memw #x1212 #x4e73) ; RTE
(memw #x1214 #x7005) ; MOVEQ #5,D0
(memw #x1216 #x4e73) ; RTE
(memw #x1218 #x7006) ; MOVEQ #6,D0
(memw #x121a #x4e73) ; RTE
(memw #x121c #x7007) ; MOVEQ #7,D0
(memw #x121e #x4e73) ; RTE
(memw #x1220 #x7008) ; MOVEQ #8,D0
(memw #x1222 #x4e73) ; RTE
(memw #x1224 #x7009) ; MOVEQ #9,D0
(memw #x1226 #x4e73) ; RTE
(memw #x1228 #x700a) ; MOVEQ #10,D0
(memw #x122a #x4e73) ; RTE
(memw #x122c #x700b) ; MOVEQ #11,D0
(memw #x122e #x4e73) ; RTE
(memw #x1230 #x700c) ; MOVEQ #12,D0
(memw #x1232 #x4e73) ; RTE
(memw #x1234 #x700d) ; MOVEQ #13,D0
(memw #x1236 #x4e73) ; RTE
(memw #x1238 #x700e) ; MOVEQ #14,D0
(memw #x123a #x4e73) ; RTE
(memw #x123c #x700f) ; MOVEQ #15,D0
(memw #x123e #x4e73) ; RTE
(memw #x1240 #x7010) ; MOVEQ #16,D0
(memw #x1242 #x4e73) ; RTE
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
(test-reg 17 #x1200)
(sim-step)
(test-reg 0 0)
(sim-step)
(test-reg 17 #x1004)
;
(sim-step)
(test-reg 17 #x1204)
(sim-step)
(test-reg 0 1)
(sim-step)
(test-reg 17 #x1006)
;
(sim-step)
(test-reg 17 #x1208)
(sim-step)
(test-reg 0 2)
(sim-step)
(test-reg 17 #x1008)
;
(sim-step)
(test-reg 17 #x120c)
(sim-step)
(test-reg 0 3)
(sim-step)
(test-reg 17 #x100a)
;
(sim-step)
(test-reg 17 #x1210)
(sim-step)
(test-reg 0 4)
(sim-step)
(test-reg 17 #x100c)
;
(sim-step)
(test-reg 17 #x1214)
(sim-step)
(test-reg 0 5)
(sim-step)
(test-reg 17 #x100e)
;
(sim-step)
(test-reg 17 #x1218)
(sim-step)
(test-reg 0 6)
(sim-step)
(test-reg 17 #x1010)
;
(sim-step)
(test-reg 17 #x121c)
(sim-step)
(test-reg 0 7)
(sim-step)
(test-reg 17 #x1012)
;
(sim-step)
(test-reg 17 #x1220)
(sim-step)
(test-reg 0 8)
(sim-step)
(test-reg 17 #x1014)
;
(sim-step)
(test-reg 17 #x1224)
(sim-step)
(test-reg 0 9)
(sim-step)
(test-reg 17 #x1016)
;
(sim-step)
(test-reg 17 #x1228)
(sim-step)
(test-reg 0 10)
(sim-step)
(test-reg 17 #x1018)
;
(sim-step)
(test-reg 17 #x122c)
(sim-step)
(test-reg 0 11)
(sim-step)
(test-reg 17 #x101a)
;
(sim-step)
(test-reg 17 #x1230)
(sim-step)
(test-reg 0 12)
(sim-step)
(test-reg 17 #x101c)
;
(sim-step)
(test-reg 17 #x1234)
(sim-step)
(test-reg 0 13)
(sim-step)
(test-reg 17 #x101e)
;
(sim-step)
(test-reg 17 #x1238)
(sim-step)
(test-reg 0 14)
(sim-step)
(test-reg 17 #x1020)
;
(sim-step)
(test-reg 17 #x123c)
(sim-step)
(test-reg 0 15)
(sim-step)
(test-reg 17 #x1022)
;
(sim-step)
(test-reg 18 #x2000)
(sim-step)
(test-reg 0 15)
(test-reg 17 #x1028)
(sim-step)
(test-reg 18 #x2002)
(sim-step)
(test-reg 17 #x1240)
(sim-step)
(test-reg 0 16)
(sim-step)
(test-reg 17 #x102e)
