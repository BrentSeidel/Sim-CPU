;
;  Simple test for telnet terminal
;
;  Load program
; START:
(memw #x1000 #x307c) ; MOVE #STR1,A0
(memw #x1002 #x1500)
(memw #x1004 #x6000) ; BRA PUTS
(memw #x1006 #x0006)
;
(memw #x1008 #xffff) ; EAsy68k Code to halt simulator
(memw #x100a #xffff)
; PUTS
(memw #x100c #x13d8) ; MOVE.B (A0)+,TTYDAT
(memw #x100e #x00FF)
(memw #x1010 #xFF00)
(memw #x1012 #x66f8) ; BNE L1
(memw #x1014 #x6000) ; BRA ECHO
(memw #x1016 #x0002)
; DONE
; ECHO
; L2
(memw #x1018 #x1039) ; MOVE.B TTYSTAT,D0
(memw #x101a #x00ff)
(memw #x101c #xff01)
(memw #x101e #x67f8) ; BEQ L2
(memw #x1020 #x1039) ; MOVE.B TTYDAT,D0
(memw #x1022 #x00ff)
(memw #x1024 #xff00)
(memw #x1026 #x13c0) ; MOVE.B D0,TTYDAT
(memw #x1028 #x00ff)
(memw #x102a #xff00)
(memw #x102c #x60ea) ; BRA ECHO
; STR1
(memw #x1500 #x5361)
(memw #x1502 #x6d70)
(memw #x1504 #x6c65)
(memw #x1506 #x2073)
(memw #x1508 #x7472)
(memw #x150a #x696e)
(memw #x150c #x670d)
(memw #x150e #x0a00)
(memw #x1510 #x0000)
;
(go #x1000)
