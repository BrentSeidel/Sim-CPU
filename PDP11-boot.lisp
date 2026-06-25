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
;  RK11 vector is 220 at BR5
(attach "RK11" #o777400 "MEM" (+ #o220 #x050000))
(disk-open "DK0" 0 "images/rk_boot.dsk")
(disk-open "DK0" 1 "images/rk_scratch.dsk")
;
;  RK611 Vector is 210 at BR5
(attach "RK611" #o777440 "MEM" (+ #o210 #x050000))
(disk-open "DM1" 0 "images/rk07_boot.dsk")
(disk-open "DM1" 1 "images/rk07_working.dsk")
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
(memlw #o1000 #o012700)  ;  MOV #177406,R0    ; Move the address of the Word Count register into R0
(memlw #o1002 #o177406)
(memlw #o1004 #o012710)  ;  MOV #177400,(R0)  ; Move block size (negative) into Word Count register
(memlw #o1006 #o177400)                       ; meaning that octal 400 words are to be read.
(memlw #o1010 #o012740)  ;  MOV #5,-(R0)      ; Move 'Read Go' command into CSR
(memlw #o1012 #o000005)
(memlw #o1014 #o105710)  ;  TSTB (R0)         ; Test for 'Done' bit in CSR
(memlw #o1016 #o100376)  ;  BPL 1014          ; Jump backward if not set
(memlw #o1020 #o005007)  ;  CLR PC            ; Start loaded bootstrap with jump to 0
;(go #o1000)
(print "Loading RK611 bootstrap")
(memlw #o2000 #o012706)  ;  MOV #START,SP      ; Initialize stack pointer
(memlw #o2002 #o002000)
(memlw #o2004 #o012700)  ;  MOV #0,R0         ; Move unit number to R0
(memlw #o2006 #o000000)                       ; Change if a different unit is desired
(memlw #o2010 #o012701)  ;  MOV #RKCS1,R1     ; Point to RK611 CSR
(memlw #o2012 #o177440)
(memlw #o2014 #o012761)  ;  MOV #CS2_CLR,10(R1) ; Subsystem clear in CSR2
(memlw #o2016 #o000040)
(memlw #o2020 #o000010)
(memlw #o2022 #o010061)  ;  MOV R0,10(R1)     ; Set drive number in CSR2
(memlw #o2024 #o000010)
(memlw #o2026 #o016102)  ;  MOV 12(R1),R2     ; Get drive type
(memlw #o2030 #o000012)
(memlw #o2032 #o100375)  ;  BPL .-4           ; Valid?
(memlw #o2034 #o042702)  ;  BIC #177377,R2    ; Clear the rest
(memlw #o2036 #o177377)
(memlw #o2040 #o006302)  ;  ASL R2
(memlw #o2042 #o006302)  ;  ASL R2
(memlw #o2044 #o012703)  ;  MOV #PACK+GO,R3
(memlw #o2046 #o000003)
(memlw #o2050 #o050203)  ;  BIS R2,R3         ; Merge drive type
(memlw #o2052 #o010311)  ;  MOV R3,(R1)       ; Pack Ack command
(memlw #o2054 #o105711)  ;  TSTB (R1)         ; Check for complete
(memlw #o2056 #o100376)  ;  BPL .-2
(memlw #o2060 #o012761)  ;  MOV #-512.,2(R1)  ; Set RKWC
(memlw #o2062 #o177000)
(memlw #o2064 #o000002)
(memlw #o2066 #o005061)  ;  CLR 4(R1)         ; Clear RKBA
(memlw #o2070 #o000004)
(memlw #o2072 #o005061)  ;  CLR 6(R1)         ; Clear RKDA
(memlw #o2074 #o000006)
(memlw #o2076 #o005061)  ;  CLR 20(R1)        ; Clear RKDC
(memlw #o2100 #o000020)
(memlw #o2102 #o012703)  ;  MOV #READ+GO,R3
(memlw #o2104 #o000021)
(memlw #o2106 #o050203)  ;  BIS R2,R3         ; Merge drive type
(memlw #o2110 #o010311)  ;  MOV R3,(R1)       ; Read command
(memlw #o2112 #o105711)  ;  TSTB (R1)         ; Check for complete
(memlw #o2114 #o100376)  ;  BPL .-2
(memlw #o2116 #o005002)  ;  CLR R2
(memlw #o2120 #o005003)  ;  CLR R3
(memlw #o2122 #o012704)  ;  MOV #START+020,R4
(memlw #o2124 #o002020)
(memlw #o2126 #o005005)  ;  CLR R5
(memlw #o2130 #o005007)  ;  CLR PC
(setq model "0")
(dowhile (= model "0")
  (print "Boot devices available") (terpri)
  (print "1. RK11  RK0") (terpri)
  (print "2. RK611 DM0") (terpri)
  (print "Select boot device: ")
  (setq model (read-line))
  (if (= model "1") (go #o1000)
  (if (= model "2") (go #o2000)
      (progn (print "Unknown device: " model)
        (terpri)
        (setq model "0")))))
(print "Connect to the console on port 2171")
;(print "Type GO 200 to boot RK11 controller")
;(print "Type GO 400 to boot RK611 controller")
(print "and type RUN to start.")
(exit)
