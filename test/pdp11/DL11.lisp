;
;  Tests for DL11 attached to PDP-11
(sim-cpu "PDP-11/TEST")
; Rx vector is #o060, Tx vector is #o064.  Combined value is #o032060
(attach "DL11" #o777560 "MEM" 2171 #o032060)
(set-pause-count 1)
;
(terpri)
(print "Starting test.")
(print "  Loading memory.")
;
;  Load memory
;
;  Vectors
;
(memlw #o000060 #o000001)  ;                RXVECT:  .WORD 1  ;  PC for receive interrupt
(memlw #o000062 #o000002)  ;                         .WORD 2  ;  PSW
(memlw #o000064 #o000003)  ;                TXVECT:  .WORD 3  ;  PC for transmit interrupt
(memlw #o000066 #o000004)  ;                         .WORD 4  ;  PSW
;
;  Code
;
(memlw #o001000 #o012706)
(memlw #o001002 #o005000)  ;                START:   MOV #TOP,SP
(memlw #o001004 #o012737)  ;                         MOV #MSG1,@#TXTPTR
(memlw #o001006 #o002004)
(memlw #o001010 #o002000)
(memlw #o001012 #o012737)  ;                         MOV #MSG1LEN,@#TXTCNT
(memlw #o001014 #o000041)
(memlw #o001016 #o002002)
(memlw #o001020 #o004767)  ;                         JSR PC,SEND
(memlw #o001022 #o000002)
(memlw #o001024 #o000000)  ;                         HALT
;
;  Send text to the console using polling.  Entered with:
;  TXTPTR - Point to start of string
;  TXTCNT - Number of characters to send.
;
;  On exit:
;  TXTPTR - Points one past end of string
;  TXTCNT - Set to zero.
;
(memlw #o001026 #o013700)  ;                SEND:    MOV @#TXPTR,R0
(memlw #o001030 #o002000)
(memlw #o001032 #o013701)  ;                         MOV @#TXCNT,R1
(memlw #o001034 #o002002)
(memlw #o001036 #o001411)  ;                         BEQ 1$      ; Exit if nothing to print
(memlw #o001040 #o033727)  ;                2$:      BIT @#TXSTAT,READY
(memlw #o001042 #o177564)
(memlw #o001044 #o000200)
(memlw #o001046 #o001774)  ;                         BEQ 2$      ; Busy wait until TX ready
(memlw #o001050 #o112037)  ;                         MOVB (R0)+,TXDATA
(memlw #o001052 #o177566)
(memlw #o001054 #o005301)  ;                         DEC R1
(memlw #o001056 #o001401)  ;                         BEQ 1$      ; Exit when count is zero
(memlw #o001060 #o000767)  ;                         BR 2$       ; Next character
;
(memlw #o001062 #o010037)  ;                1$:      MOV R0,@#TXPTR
(memlw #o001064 #o002000)
(memlw #o001066 #o010137)  ;                         MOV R1,@#TXCNT
(memlw #o001070 #o002002)
(memlw #o001072 #o000207)  ;                         RTS PC
(memb #o002004 #o110)      ;                MSG1:    .ASCII /Hello world sent using polling./<15><12>
(memb #o002005 #o145)
(memb #o002006 #o154)
(memb #o002007 #o154)
(memb #o002010 #o157)
(memb #o002011 #o040)
(memb #o002012 #o167)
(memb #o002013 #o157)
(memb #o002014 #o162)
(memb #o002015 #o154)
(memb #o002016 #o144)
(memb #o002017 #o040)
(memb #o002020 #o163)
(memb #o002021 #o145)
(memb #o002022 #o156)
(memb #o002023 #o164)
(memb #o002024 #o040)
(memb #o002025 #o165)
(memb #o002026 #o163)
(memb #o002027 #o151)
(memb #o002030 #o156)
(memb #o002031 #o147)
(memb #o002032 #o040)
(memb #o002033 #o160)
(memb #o002034 #o157)
(memb #o002035 #o154)
(memb #o002036 #o154)
(memb #o002037 #o151)
(memb #o002040 #o156)
(memb #o002041 #o147)
(memb #o002042 #o056)
(memb #o002043 #o015)
(memb #o002044 #o012)
;
;  Run test
;
(print "  Telnet to port 2171 on localhost.")
(print "  Enter RUN command to the simulator.")
(print "  A message should appear on the telnet screen.")
(go #o001000)
