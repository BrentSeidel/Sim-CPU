;
;**************************************************************
;*
;*        B I O S   J U M P   T A B L E
;*
;**************************************************************
;
BOOT	JMP	0	;NOTE WE USE FAKE DESTINATIONS
WBOOT	JMP	0
CONST	JMP	TTST
CONIN	JMP	TTIN
CONOUT	JMP	TTOUT
LIST	JMP	NOTIMP
PUNCH	JMP	NOTIMP
READER	JMP	RETEOF
HOME	JMP	0
SELDSK	JMP	0
SETTRK	JMP	0
SETSEC	JMP	0
SETDMA	JMP	0
READ	JMP	0
WRITE	JMP	0
PRSTAT	JMP	0
SECTRN	JMP	0
;
;  I/O Ports
;
TTYDAT  EQU 0   ; Simple console device at ports 0 & 1
TTYST   EQU 1
;
;  Implementation of BIOS functions
;
;  Return console status (A and flags are impacted).
;  A = 00H - No data
;  A = FFH - Data ready
TTST    IN TTYST
        ANI 1       ; Test input status bit
        JNZ TTST1
        RET
TTST1   MVI A,FFH
        RET
;
;  Wait for an input character and return it in A
TTIN    IN TTYST
        ANI 1
        JZ TTIN     ; Wait for status bit to be 1
        IN TTYDAT
        RET
;
;  Write character in C to console output
TTOUT   PUSH PSW
        MOV A,C
        OUT TTYDAT
        POP PSW
        RET
;
;  Output devices that are not implemented simply return
NOTIMP  RET
;
;  Input devices that are not implemented return ^Z (EOF).
RETEOF  MVI A,1AH
        RET
;
;*
;******************   E N D   O F   C P / M   *****************
;*

