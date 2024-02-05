;-----------------------------------------------------------
; Title      : Console I/O
; Written by : Brent Seidel
; Date       : 31-Jan-2024
; Description: Console I/O for simulated 68000
;-----------------------------------------------------------
    INCLUDE Sections.X68
    INCLUDE Macros.X68
;==============================================================================
;  Main code section
;
    SECTION CSECT
START:
;
;  Print some messages
;
    MOVE.W #0,-(SP)    ;  Flags (byte)
    MOVE.L #INSTR,-(SP)
    MOVE.L #255,-(SP)  ;  Number
    JSR HEXSTR
    ADDQ.L #8,SP       ;  Clean 10 bytes off the stack.
    ADDQ.L #2,SP
    MOVE.L #CVT1,A0
    JSR PUTSTR
    MOVE.L #INSTR,A0
    JSR PUTSTR
    MOVE.L #NEWLINE,A0
    JSR PUTSTR
;
    MOVE.W #5,-(SP)    ;  Flags (signed, word)
    MOVE.L #INSTR,-(SP)
    MOVE.L #32767,-(SP) ; Number
    JSR HEXSTR
    ADDQ.L #8,SP       ;  Clean 10 bytes off the stack.
    ADDQ.L #2,SP
    MOVE.L #CVT2,A0
    JSR PUTSTR
    MOVE.L #INSTR,A0
    JSR PUTSTR
    MOVE.L #NEWLINE,A0
    JSR PUTSTR
;
    MOVE.W #10,-(SP)   ;  Flags (leading zeros, long)
    MOVE.L #INSTR,-(SP)
    MOVE.L #1000000,-(SP) ; Number
    JSR HEXSTR
    ADDQ.L #8,SP       ;  Clean 10 bytes off the stack.
    ADDQ.L #2,SP
    MOVE.L #CVT3,A0
    JSR PUTSTR
    MOVE.L #INSTR,A0
    JSR PUTSTR
    MOVE.L #NEWLINE,A0
    JSR PUTSTR
;
    MOVE.L #MSG1,A0
    JSR PUTSTR
    MOVE.L #MSG2,A0
    JSR PUTSTR
    MOVE.L #TXT1,A0
    JSR PUTSTR
;
;  Print a prompt
;
    MOVE.L #PROMPT,A0
    JSR PUTSTR
;
;  Get some text
;
    MOVE.L #INSTR,A0
    JSR GETSTR
    MOVE.L #NEWLINE,A0
    JSR PUTSTR
;
;  Echo it back out
;
    MOVE.l #INSTR,A0
    JSR PUTSTR
    MOVE.L #NEWLINE,A0
    JSR PUTSTR
    RTS

;==============================================================================
;  Data section for main code
;
    SECTION DSECT

    STRING INSTR,$100
    TEXT PROMPT,<GT,' '>
    TEXT MSG1,<'68000 Assembly language test program.',CR,LF>
    TEXT MSG2,<'Simulated 68000 written in Ada',CR,LF>
    TEXT TXT1,<'Enter some text at the prompt below:',CR,LF>
    TEXT CVT1,<'255 in hexidecimal is '>
    TEXT CVT2,<'32767 in hexidecimal is '>
    TEXT CVT3,<'1000000 in hexidimal is '>
;==============================================================================
;  Exception Vectors
;
    SECTION VSECT
    DC.L NULLVEC
    DC.L SSTACK
    DCB.L $100-2,NULLVEC
;==============================================================================
;  Operating system data.  This includes data for exception handlers and
;  hardware abstraction layer.
;
    SECTION ODSECT
CLKCOUNT:
    DC.L 0              ;  This gets incremented once per clock tick
    TEXT ILLEXP,<'Illegal instruction encountered at '>
    TEXT PRIVEXP,<'Privilege violation encountered at '>
    TEXT UNINITIALIZED,<'Uninitialized exception encountered at '>
    TEXT TIMESG,<'Elapsed time is '>
    STRING OSTXT,$100
;==============================================================================
;  Operating system, such as it is.  It just does initialization.
;
    SECTION OSECT
INIT:
;
;  Setup the stack pointers
;
    MOVE.L #USRSTK,A0   ;  Setup the user and supervisor stack
    MOVE.L A0,USP
    MOVE.L #SUPSTK,SP
;
;  Setup the exception handlers
;
    MOVE.L #4,D0        ;  Illegal Instruction exception
    MOVE.L #ILLINSTHANDLE,A0
    JSR SETVEC
    MOVE.L #8,D0        ;  Privilege violation exception
    MOVE.L #PRIVHANDLE,A0
    JSR SETVEC
    MOVE.L #64,D0
    MOVE.L #CLOCKHANDLE,A0
    JSR SETVEC
;
;  Start the clock
;
    MOVE.B #10,CLKRATE  ;  Rate is once a second
    MOVE.B #1,CLKSTAT   ;  Enable the clock
;
;  Run the user program
;
    JSR START           ;  Call the main program
;
;  Cleanup - stop the clock
;
    CLR.B CLKSTAT      ;  Stop the clock
    MOVE.W #10,-(SP)   ;  Flags (leading zeros, long)
    MOVE.L #OSTXT,-(SP)
    MOVE.L CLKCOUNT,-(SP) ;  Number
    JSR HEXSTR
    ADDQ.L #8,SP       ;  Clean 10 bytes off the stack.
    ADDQ.L #2,SP
    MOVE.L #TIMESG,A0
    JSR PUTSTR
    MOVE #OSTXT,A0
    JSR PUTSTR
    MOVE #NEWLINE,A0
    JSR PUTSTR
    STOP #$2000
    BRA *
    SIMHALT             ;  halt simulator
;
;  Set an exception vector
;  Input: A0 is the address of the handler
;         D0 is the vector number
;
SETVEC:
    MOVE.L A1,-(SP)
    MOVE.L D0,-(SP)
    ASL.L #2,D0         ;  Multiply by 4 to get address in table
    MOVE.L D0,A1
    MOVE.L A0,(A1)      ;  Set the vector
    MOVE.L (SP)+,D0
    MOVE.L (SP)+,A1
    RTS
;
;  Exception vectors
;
NULLVEC:                ;  All vectors initialized to this.
    MOVE.L 2(SP),A0
    MOVE.W #10,-(SP)   ;  Flags (leading zeros, long)
    MOVE.L #OSTXT,-(SP)
    MOVE.L A0,-(SP)    ;  Number
    JSR HEXSTR
    ADDQ.L #8,SP       ;  Clean 10 bytes off the stack.
    ADDQ.L #2,SP
    MOVE.L #UNINITIALIZED,A0
    JSR PUTSTR          ;  Print message
    MOVE.L #OSTXT,A0
    JSR PUTSTR
    MOVE.L #NEWLINE,A0
    JSR PUTSTR
    BRA *
    RTE
;
ILLINSTHANDLE:         ;  Illegal instruction handler
    MOVE.L 2(SP),A0
    MOVE.W #10,-(SP)   ;  Flags (leading zeros, long)
    MOVE.L #OSTXT,-(SP)
    MOVE.L A0,-(SP)    ;  Number
    JSR HEXSTR
    ADDQ.L #8,SP       ;  Clean 10 bytes off the stack.
    ADDQ.L #2,SP
    MOVE.L #ILLEXP,A0
    JSR PUTSTR
    MOVE.L #OSTXT,A0
    JSR PUTSTR
    MOVE.L #NEWLINE,A0
    JSR PUTSTR
    BRA *
    RTE
;
PRIVHANDLE:            ;  Privilege violation handler
    MOVE.L 2(SP),A0
    MOVE.W #10,-(SP)   ;  Flags (leading zeros, long)
    MOVE.L #OSTXT,-(SP)
    MOVE.L A0,-(SP)    ;  Number
    JSR HEXSTR
    ADDQ.L #8,SP       ;  Clean 10 bytes off the stack.
    ADDQ.L #2,SP
    MOVE.L #PRIVEXP,A0
    JSR PUTSTR
    MOVE.L #OSTXT,A0
    JSR PUTSTR
    MOVE.L #NEWLINE,A0
    JSR PUTSTR
    BRA *
    RTE
;
CLOCKHANDLE:            ;  Clock handler
    ADDQ.L #1,CLKCOUNT
    RTE
;==============================================================================
; Setup the stack sections
;
    SECTION USTACK
    DS.L $1000
USRSTK:
;==============================================================================
    SECTION SSTACK
    DS.L $1000
SUPSTK:
;==============================================================================
;  Hardware Abstraction Layer section
;
    SECTION HSECT
;
;  Prints a string to the console.
;  Input: Address of string in A0.
;  Output: A0 unchanged.
;
PUTSTR:
    MOVE.L A0,-(SP)
    MOVE.L D0,-(SP)
    ADDQ #2,A0
    CLR.L D0
    MOVE.W (A0)+,D0      ;  Get length of string
    SUBQ.W #1,D0
.L1:
    MOVE.B (A0)+,TTYDAT
    DBF D0,.L1
    MOVE.L (SP)+,D0
    MOVE.L (SP)+,A0
    RTS
;
;  Put a single character to the console.
;  Input: Character in D0.B
;  Output: None
;
PUTC:
    MOVE.B D0,TTYDAT
    RTS
;
;  Get a single character from the console.  This waits
;  until a character is ready before returning.
;  Input: None
;  Output: Character in D0.B
;
GETC:
.L1:
    BTST #0,TTYSTAT
    BEQ .L1             ;  Wait for bit 0 to be set
    MOVE.B TTYDAT,D0
    RTS
;
;  Get a string from the console.  This is the bounded string data structure.
;  The string is terminated by the structure begin full or a CR or LF entered
;  on the console.
;  Input: A0 contains the base address of the string structure.
;
GETSTR:
    MOVE.L D0,-(SP)     ;  Contains received character
    MOVE.L D1,-(SP)     ;  Max length of string (count down)
    MOVE.L D2,-(SP)     ;  Max length of string (fixed)
    MOVE.L D3,-(SP)     ;  Size of string
    MOVE.L A0,-(SP)
    CLR.L D1
    CLR.L D3
    MOVE.W (A0),D1      ;  Size of string
    MOVE.L D1,D2
    ADDQ.L #4,A0        ;  Point to buffer
.L1:
    BSR GETC
    CMP.B #CR,D0
    BEQ .DONE
    CMP.B #LF,D0
    BEQ .DONE
    CMP.B #BS,D0
    BEQ .BACK
    CMP.B #DEL,D0
    BEQ .BACK
    MOVE.B D0,(A0)+
    BSR PUTC            ;  Echo the character
    ADDQ.L #1,D3
    DBF D1,.L1
.BACK:                  ;  Handle backspace
    CMP.W D1,D2
    BEQ .EMPTY          ;  Check if string is empty
    SUBQ.L #1,A0        ;  Move pointer back
    ADDQ.L #1,D1        ;  Move counter up
    SUBQ.L #1,D3        ;  Move size back
    MOVE.B #BS,TTYDAT   ;  Update display
    MOVE.B #SP,TTYDAT
    MOVE.B #BS,TTYDAT
    BRA .L1
.EMPTY
    MOVE.B #BELL,TTYDAT ;  Just ring the bell
    BRA .L1
.DONE:
    MOVE.B #CR,TTYDAT   ;  Carriage return
    MOVE.B #LF,TTYDAT   ;  Line-feed
    MOVE.L (SP)+,A0
    MOVE.W D3,2(A0)
    MOVE.L (SP)+,D3
    MOVE.L (SP)+,D2
    MOVE.L (SP)+,D1
    MOVE.L (SP)+,D0
    RTS
;==============================================================================
;  Addresses for hardware I/O ports
;
    SECTION PSECT
TTYDAT:             ;  Console data
    DS.B 1
TTYSTAT:            ;  Console status (LSB indicates data ready to read)
    DS.B 1
CLKSTAT:            ;  Clock status and control
    DS.B 1
CLKRATE:            ;  Clock interval
    DS.B 1
;==============================================================================
    INCLUDE Library.X68
;==============================================================================

    END INIT        ;  Start at INIT entry point





*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
