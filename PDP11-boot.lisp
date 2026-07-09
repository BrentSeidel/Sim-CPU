;
;  Configuration for booting a RK11 or RK611 attached to PDP-11
;
(setq model "0")
(dowhile (= model "0")
  (print "PDP-11 model available") (terpri)
  (print "1. PDP-11/10 or PDP-11/05") (terpri)
  (print "2. PDP-11/20 or PDP-11/15") (terpri)
  (print "3. PDP-11/04") (terpri)
  (print "4. PDP-11/34") (terpri)
  (print "5. PDP-11/40 or PDP-11/35") (terpri)
  (print "Select model: ")
  (setq model (read-line))
  (if (= model "1") (sim-cpu "PDP-11/10")
  (if (= model "2") (sim-cpu "PDP-11/20")
  (if (= model "3") (sim-cpu "PDP-11/04")
  (if (= model "4") (sim-cpu "PDP-11/34")
  (if (= model "5") (sim-cpu "PDP-11/40")
      (progn (print "Unknown model: " model)
        (terpri)
        (setq model "0"))))))))
;
;  Attach required hardware
;
;  Attach BM792 boot ROM for PDP-11/04
;
(if (= model "3") (attach "BM792" #o773300 "MEM" 0))
;
;  Attach KT11 MMU for 11/34 and 11/40
;
(if (= model "4") (attach "KT11" 0 "MEM" #o250))
(if (= model "5") (attach "KT11" 0 "MEM" #o250))
;
;  KW11 always attaches to memory address 777546 and vector 100, BR6.
(attach "KW11" #o777546 "MEM" (+ #o100 #x060000))
;
; Rx vector is #o060, Tx vector is #o064, both at BR4.  Combined value is #o15000060
(attach "DL11" #o777560 "MEM" #o15000060 2171)
;
;  PC11 RX vector is #o070, TX vector is #o074.  Both at BR4.  Combined value is #o17000070
(attach "PC11" #o777550 "MEM" #o17000070)
(tape-open "PC0" "RDR" "ansi.for")
(tape-open "PC0" "PUN" "punch.txt")
;
;  Install disk controllers and attach image files.  Change as appropriate
;  for your installation.
;
;  RK11 vector is 220 at BR5
(attach "RK11" #o777400 "MEM" (+ #o220 #x050000))
(disk-open "DK0" 0 "images/rk_boot.dsk")
(disk-open "DK0" 1 "images/rk_scratch.dsk")
(disk-open "DK0" 2 "images/DECUS/advent.dsk")
(disk-open "DK0" 3 "images/DECUS/pascal.dsk")
(disk-open "DK0" 4 "images/DECUS/lisp11.dsk")
;
;  RK611 Vector is 210 at BR5
(attach "RK611" #o777440 "MEM" (+ #o210 #x050000))
(disk-open "DM1" 0 "images/rk07_boot.dsk")
(disk-open "DM1" 1 "images/rk07_working.dsk")
;
;  Install tape controller at attach some tape images.  Tape controller
;  is currently under development, so use with caution.
;
;  TM11 Vector is 224 at BR5 (under development)
(attach "TM11" #o772520 "MEM" (+ #o224 #x050000))
(disk-open "MT0" 0 "images/AP-P752D-BC_RT-11_V5.1C_BIN_8MT9_1984.tap")
(disk-protect "MT0" 0 1)
(disk-open "MT0" 1 "images/scratch.tap")
;
;  Note that the bootstrap code is based on that found in open-simh, which
;  is probably copied from the original DEC bootstraps.
;
(terpri)
(print "Loading RK11 bootstrap.")
(defun boot-rk05 (base)
  (memlw (+ base  0) #o012700)  ;  MOV #177406,R0    ; Move the address of the Word Count register into R0
  (memlw (+ base  2) #o177406)
  (memlw (+ base  4) #o012710)  ;  MOV #177400,(R0)  ; Move block size (negative) into Word Count register
  (memlw (+ base  6) #o177400)                       ; meaning that octal 400 words are to be read.
  (memlw (+ base  8) #o012740)  ;  MOV #5,-(R0)      ; Move 'Read Go' command into CSR
  (memlw (+ base 10) #o000005)
  (memlw (+ base 12) #o105710)  ;  TSTB (R0)         ; Test for 'Done' bit in CSR
  (memlw (+ base 14) #o100376)  ;  BPL 1014          ; Jump backward if not set
  (memlw (+ base 16) #o005007)  ;  CLR PC            ; Start loaded bootstrap with jump to 0
  (go base))
(print "Loading RK611 bootstrap")
(defun boot-rk07 (base)
  (memlw (+ base  0) #o012706)  ;  MOV #START,SP     ; Initialize stack pointer
  (memlw (+ base  2) #o002000)
  (memlw (+ base  4) #o012700)  ;  MOV #0,R0         ; Move unit number to R0
  (memlw (+ base  6) #o000000)                       ; Change if a different unit is desired
  (memlw (+ base  8) #o012701)  ;  MOV #RKCS1,R1     ; Point to RK611 CSR
  (memlw (+ base 10) #o177440)
  (memlw (+ base 12) #o012761)  ;  MOV #CS2_CLR,10(R1) ; Subsystem clear in CSR2
  (memlw (+ base 14) #o000040)
  (memlw (+ base 16) #o000010)
  (memlw (+ base 18) #o010061)  ;  MOV R0,10(R1)     ; Set drive number in CSR2
  (memlw (+ base 20) #o000010)
  (memlw (+ base 22) #o016102)  ;  MOV 12(R1),R2     ; Get drive type
  (memlw (+ base 24) #o000012)
  (memlw (+ base 26) #o100375)  ;  BPL .-4           ; Valid?
  (memlw (+ base 28) #o042702)  ;  BIC #177377,R2    ; Clear the rest
  (memlw (+ base 30) #o177377)
  (memlw (+ base 32) #o006302)  ;  ASL R2
  (memlw (+ base 34) #o006302)  ;  ASL R2
  (memlw (+ base 36) #o012703)  ;  MOV #PACK+GO,R3
  (memlw (+ base 38) #o000003)
  (memlw (+ base 40) #o050203)  ;  BIS R2,R3         ; Merge drive type
  (memlw (+ base 42) #o010311)  ;  MOV R3,(R1)       ; Pack Ack command
  (memlw (+ base 44) #o105711)  ;  TSTB (R1)         ; Check for complete
  (memlw (+ base 46) #o100376)  ;  BPL .-2
  (memlw (+ base 48) #o012761)  ;  MOV #-512.,2(R1)  ; Set RKWC
  (memlw (+ base 50) #o177000)
  (memlw (+ base 52) #o000002)
  (memlw (+ base 54) #o005061)  ;  CLR 4(R1)         ; Clear RKBA
  (memlw (+ base 56) #o000004)
  (memlw (+ base 58) #o005061)  ;  CLR 6(R1)         ; Clear RKDA
  (memlw (+ base 60) #o000006)
  (memlw (+ base 62) #o005061)  ;  CLR 20(R1)        ; Clear RKDC
  (memlw (+ base 64) #o000020)
  (memlw (+ base 66) #o012703)  ;  MOV #READ+GO,R3
  (memlw (+ base 68) #o000021)
  (memlw (+ base 70) #o050203)  ;  BIS R2,R3         ; Merge drive type
  (memlw (+ base 72) #o010311)  ;  MOV R3,(R1)       ; Read command
  (memlw (+ base 74) #o105711)  ;  TSTB (R1)         ; Check for complete
  (memlw (+ base 76) #o100376)  ;  BPL .-2
  (memlw (+ base 78) #o005002)  ;  CLR R2
  (memlw (+ base 80) #o005003)  ;  CLR R3
  (memlw (+ base 82) #o012704)  ;  MOV #START+020,R4
  (memlw (+ base 84) #o002020)
  (memlw (+ base 86) #o005005)  ;  CLR R5
  (memlw (+ base 88) #o005007)  ;  CLR PC
  (go base))
(setq model "0")
(dowhile (= model "0")
  (print "Boot devices available") (terpri)
  (print "1. RK11  RK0") (terpri)
  (print "2. RK611 DM0") (terpri)
  (print "Select boot device: ")
  (setq model (read-line))
  (if (= model "1") (boot-rk05 #o1000)
  (if (= model "2") (boot-rk07 #o2000)
      (progn (print "Unknown device: " model)
        (terpri)
        (setq model "0")))))
(print "Connect to the console on port 2171")
(print "and type RUN to start.")
(exit)
