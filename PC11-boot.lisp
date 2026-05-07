;
;  Tests for booting a PC11 attached to PDP-11
;
(sim-cpu "PDP-11/10")
;
;  Attach required hardware
;
;  KW11 always attaches to memory address 777546 and vector 100, BR6.
(attach "KW11" #o777546 "MEM" (+ #o100 #x060000))
;
; Rx vector is #o060, Tx vector is #o064, both at BR4.  Combined value is #o15000060
(attach "DL11" #o777560 "MEM" #o15000060 2171)
;
;  PC11 RX vector is #o070, TX vector is #o074.  Both at BR4.  Combined value is #o17000070
(attach "PC11" #o777550 "MEM" #o17000070)
(print "Enter image to boot [images/rtv4_rk.dsk]: ")
(setq image (read-line))
(if (< (length image) 1)
  (setq image "images/rtv4_rk.dsk"))
(print "Booting image <" image ">")
(tape-open "PC0" "RDR" image)
(tape-open "PC0" "PUN" "punch.txt")
;
(terpri)
(print "Loading PC11 bootstrap.")
;                        LOAD=xx7400            ;  Buffer start address
;                        .=LOAD+0344            ;  Start address of bootstrap loader (xx7744)
(memlw #o157744 #o016701)  ;  MOV DEVICE,R1     ;  Get reader CSR address
(memlw #o157746 #o000026)
(memlw #o157750 #o012702)  ;  MOV #.-LOAD+2,R2  ;  Get buffer pointer
(memlw #o157752 #o000352)                       ;   (<--- pointer to buffer)
(memlw #o157754 #o005211)  ;  INC @R1           ;  Enable the paper tape reader
(memlw #o157756 #o105711)  ;  TSTB @R1          ;  Wait until data available
(memlw #o157760 #o100376)  ;  BPL -2
(memlw #o157762 #o116162)  ;  MOVB 2(R1),LOAD(R2)  ;  Transfer byte to buffer
(memlw #o157764 #o000002)
(memlw #o157766 #o157400)
(memlw #o157770 #o005267)  ;  INC LOOP+2        ;  Increment pointer to buffer
(memlw #o157772 #o177756)
(memlw #o157774 #o000765)  ;  BR LOOP           ;  Continue reading (<--- modified branch instruction)
(memlw #o157776 #o177550)                       ;  Paper tape reader CSR address
(go #o157744)
(print "Connect to the console on port 2171")
(print "and type RUN to start.")
(exit)
