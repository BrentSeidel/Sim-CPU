;
;  Tests for KW11 attached to PDP-11
;
(sim-cpu "PDP-11/TEST")
;
;  KW11 always attaches to memory address 777546 and vector 100.
;
(attach "KW11" #o777546 "MEM" #o100)
(set-pause-count 1)
;
(terpri)
(print "Starting KW11 test.")
(print "  Loading memory.")
;
;  Load memory
;
;  Vectors
;
(memlw #o000100 #o005000)  ;                CLKVECT: .WORD CLKISR   ;  PC for clock
(memlw #o000102 #o000000)  ;                         .WORD 0        ;  PSW
;
;  Check ready bit with interrupts disabled
;
(memlw #o001000 #o005037)  ;                START:   CLR @#KW11ST         ; Disable interrupts
(memlw #o001002 #o177546)
(memlw #o001004 #o005000)  ;                         CLR R0
(memlw #o001006 #o005001)  ;                         CLR R1
(memlw #o001010 #o005002)  ;                         CLR R2
(memlw #o001012 #o005003)  ;                         CLR R3
(memlw #o001014 #o005004)  ;                         CLR R4
(memlw #o001016 #o005005)  ;                         CLR R5
(memlw #o001020 #o012706)  ;                         MOV #TOP,SP
(memlw #o001022 #o005000)
;
(memlw #o001024 #o032737)  ;                1$:      BIT #READY,@#KW11ST  ; Check ready bit
(memlw #o001026 #o000200)
(memlw #o001030 #o177546)
(memlw #o001032 #o001774)  ;                         BEQ 1$
(memlw #o001034 #o005037)  ;                         CLR @#KW11ST         ; Clear ready bit
(memlw #o001036 #o177546)
;
(memlw #o001040 #o005200)  ;                2$:      INC R0
(memlw #o001042 #o032737)  ;                         BIT #READY,@#KW11ST  ; Check ready bit
(memlw #o001044 #o000200)
(memlw #o001046 #o177546)
(memlw #o001050 #o001773)  ;                         BEQ 2$
(memlw #o001052 #o005037)  ;                         CLR @#KW11ST         ; Clear ready bit
(memlw #o001054 #o177546)
(memlw #o001056 #o005201)  ;                         INC R1
(memlw #o001060 #o022701)  ;                         CMP #6,R1
(memlw #o001062 #o000006)
(memlw #o001064 #o001365)  ;                         BNE 2$               ;  Loop 6 times, should be about 1/10th of a second
;
;  Check ready bit and interrupts with interrupts enabled
;
(memlw #o001066 #o012737)  ;                         MOV #INTRE,@#KW11ST  ; Enable interrupts
(memlw #o001070 #o000100)
(memlw #o001072 #o177546)
(memlw #o001074 #o005202)  ;                3$:      INC R2
(memlw #o001076 #o032737)  ;                         BIT #READY,@#KW11ST
(memlw #o001100 #o000200)
(memlw #o001102 #o177546)
(memlw #o001104 #o001773)  ;                         BEQ 3$
(memlw #o001106 #o012737)  ;                         MOV #INTRE,@#KW11ST  ; Clear ready bit
(memlw #o001110 #o000100)
(memlw #o001112 #o177546)
(memlw #o001114 #o005204)  ;                         INC R4
(memlw #o001116 #o022704)  ;                         CMP #6,R4
(memlw #o001120 #o000006)
(memlw #o001122 #o001364)  ;                         BNE 3$               ;  Loop 6 times, should be about 1/10th of a second
;
(memlw #o001124 #o000000)  ;                EXIT:    HALT
;
;  Subroutines go here so that they aren't relocated everytime code is
;  added to the main program.
;
;  Clock interrupt service routine
;
(memlw #o005000 #o005205)  ;                CLKISR:  INC R5
(memlw #o005002 #o000002)  ;                         RTI
;
;  Run test
;
(print "Enter RUN command to the simulator.")
(print "  Once the program halts, R1, R4, and R5 should be about 6.")
(print "  R0 and R2 should be much larger, and R3 remains at 0.")
(go #o001000)
