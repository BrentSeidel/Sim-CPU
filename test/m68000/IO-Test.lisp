;
;  Simple test for telnet terminal
;
;  Load program
; START:
(memw #x1000 #x3e7c) ; MOVE #STACK,SP
(memw #x1002 #x2000)
(memw #x1004 #x307c) ; MOVE #STR1,A0
(memw #x1006 #x1500)
(memw #x1008 #x6100) ; BSR PUTS
(memw #x100a #x0012)
(memw #x100c #x307c) ; MOVE #SRT2,A0
(memw #x100e #x1510)
(memw #x1010 #x6100) ; BSR PUTS
(memw #x1012 #x000a)
(memw #x1014 #x6000) ; BRA ECHO
(memw #x1016 #x0014)
;
(memw #x1018 #xffff) ; EAsy68k Code to halt simulator
(memw #x101a #xffff)
; PUTS
(memw #x101c #x2f08) ; MOVE.L A0,-(SP)
(memw #x101e #x13d8) ; MOVE.B (A0)+,TTYDAT
(memw #x1020 #x00FF)
(memw #x1022 #xFF00)
(memw #x1024 #x66f8) ; BNE L1
(memw #x1026 #x205f) ; MOVE.L (SP)+,A0
(memw #x1028 #x4e75) ; RTS
; ECHO
; L2
(memw #x102a #x1039) ; MOVE.B TTYSTAT,D0
(memw #x102c #x00ff)
(memw #x102e #xff01)
(memw #x1030 #x67f8) ; BEQ L2
(memw #x1032 #x1039) ; MOVE.B TTYDAT,D0
(memw #x1034 #x00ff)
(memw #x1036 #xff00)
(memw #x1038 #x13c0) ; MOVE.B D0,TTYDAT
(memw #x103a #x00ff)
(memw #x103c #xff00)
(memw #x103e #x60ea) ; BRA ECHO
; STR1
(memw #x1500 #x5361)
(memw #x1502 #x6d70)
(memw #x1504 #x6c65)
(memw #x1506 #x2073)
(memw #x1508 #x7472)
(memw #x150a #x696e)
(memw #x150c #x670d)
(memw #x150e #x0a00)
; STR2
(memw #x1510 #x416e)
(memw #x1512 #x6f74)
(memw #x1514 #x6865)
(memw #x1516 #x7220)
(memw #x1518 #x7361)
(memw #x151a #x6d70)
(memw #x151c #x6c65)
(memw #x151e #x2073)
(memw #x1520 #x7472)
(memw #x1522 #x696e)
(memw #x1524 #x670d)
(memw #x1526 #x00a00)
;
(go #x1000)
