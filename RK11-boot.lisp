;
;  Tests for booting a RK11 attached to PDP-11
;
(sim-cpu "PDP-11/TEST")
;
;  Attach required hardware
;
;  KW11 always attaches to memory address 777546 and vector 100, BR6.
(attach "KW11" #o777546 "MEM" (+ #o100 #x060000))
;
; Rx vector is #o060, Tx vector is #o064, both at BR4.  Combined value is #o032060
(attach "DL11" #o777560 "MEM" 2171 #o032060)
;
;  RK11 vector is 220 at BR5
(attach "RK11" #o777400 "MEM" (+ #o220 #x050000))
(print "Enter image to boot [images/rtv4_rk.dsk]: ")
(setq image (read-line))
(if (< (length image) 1)
  (setq image "images/rtv4_rk.dsk"))
(print "Booting image <" image ">")
(disk-open "DK0" 0 image)

(terpri)
(print "Loading RK11 bootstrap.")
(memlw #o1000 #o012700)  ;  MOV #177406,R0    ; Move the address of the Word Count register into R0
(memlw #o1002 #o177406)
(memlw #o1004 #o012710)  ;  MOV #177400,(R0)  ; Move block size (negative) into Word Count register
(memlw #o1006 #o177400)                       ; meaning that octal 400 words are to be read.
(memlw #o1010 #o012740)  ;  MOV #5,-(R0)      ; Move 'Read Go' command into CSR
(memlw #o1012 #o000005)
(memlw #o1014 #o105710)  ;  TSTB (R0)         ; Test for 'Done' bit in CSR
(memlw #o1016 #o100376)  ;  BPL 1014          ; Jump backward if not set
(memlw #o1020 #o005007)  ;  CLR PC            ; Start loaded bootstrap with jump to 0
(go #o1000)
(print "Connect to the console on port 2171")
(print "and type RUN to start.")
(exit)
