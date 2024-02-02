;-----------------------------------------------------------
; Title      : Console I/O
; Written by : Brent Seidel
; Date       : 31-Jan-2024
; Description: Console I/O for simulated 68000
;-----------------------------------------------------------
;
;  Define section numbers
CSECT   EQU  0      ;  Code section
DSECT   EQU  1      ;  Data section
LSECT   EQU  7      ;  Library section
LDSECT  EQU  8      ;  Library data section
USTACK  EQU  9      ;  User stack section
SSTACK  EQU 10      ;  Supervisor stack section
OSECT   EQU 11      ;  Vector handler/Operating System section
ODSECT  EQU 12      ;  Operating system data
HSECT   EQU 13      ;  Hardware Abstraction Layer section
PSECT   EQU 14      ;  I/O Port section
VSECT   EQU 15      ;  Vector section
;
;  Constants for ASCII characters
;
BELL    EQU  7      ;  Ring the bell
BS      EQU  8      ;  Back space
TAB     EQU  9      ;  Horizontal tab
LF      EQU 10      ;  Line feed
CR      EQU 13      ;  Carriage return
SP      EQU 32      ;  Space
LT      EQU 60      ;  Less than '<'
GT      EQU 62      ;  Greater than '>'
DEL     EQU 127     ;  Delete
;
;  Strings are a data structure as follows:
;  Word: Max size of string
;  Word: Current size of string
;  Bytes: Text of the string
;
;  Macros
;
;  Define space for a string.  The first argument is
;  the lable, the second is the maximum size.
;
STRING  MACRO
\1: DC.W \2
    DC.W 0
    DCB.B \2,0
    ENDM
;
;  Define a string with text.  The first argument is
;  the label.  The second is the text.  Enclose the text
;  with <>.  Note that the text also needs to be surrounded
;  by ''.  The text can also include bytes.
;
TEXT    MACRO
\1: DC.W \@-\1-4
    DC.W \@-\1-4
    DC.B \2
\@  EQU *
    ENDM
;==============================================================================
;  Main code section
;
    SECTION CSECT
    ORG $1000
START:
;
;  Print some messages
;
    MOVE #MSG1,A0
    JSR PUTSTR
    MOVE #MSG2,A0
    JSR PUTSTR
    MOVE #TXT1,A0
    JSR PUTSTR
;
;  Print a prompt
;
    MOVE #PROMPT,A0
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
    ILLEGAL
    RTS

;==============================================================================
;  Data section for main code
;
    SECTION DSECT
    ORG $2000

    STRING INSTR,$100
    TEXT PROMPT,<GT,' '>
    TEXT MSG1,<'68000 Assembly language test program.',CR,LF>
    TEXT MSG2,<'Simulated 68000 written in Ada',CR,LF>
    TEXT TXT1,<'Enter some text at the prompt below:',CR,LF>
;==============================================================================
;  Exception Vectors
;
    SECTION VSECT
    ORG 0
    DC.L NULLVEC
    DC.L SSTACK
    DCB.L $100-2,NULLVEC
;==============================================================================
;  Library data section.  Common data and data for library routines go here.
;
    SECTION LDSECT
    ORG $00FB0000
;NEWLN:
;    DC.B CR,LF,0        ;  Carriage return, new-line combination
    TEXT NEWLINE,<CR,LF>
;==============================================================================
;  Library section.  Common routines would go here.
;
    SECTION LSECT
    ORG $00FC000
;==============================================================================
;  Operating system data.  This includes data for exception handlers and
;  hardware abstraction layer.'
;
    SECTION ODSECT
    ORG $00FD0000
    TEXT ILLEXP,<'Illegal instruction encountered.',CR,LF>
    TEXT PRIVEXP,<'Privilege violation encountered.',CR,LF>
;==============================================================================
;  Operating system, such as it is.  It just does initialization.
;
    SECTION OSECT
    ORG $00FE0000
INIT:
    MOVE.L #USRSTK,A0   ; Setup the user and supervisor stack
    MOVE.L A0,USP
    MOVE.L #SUPSTK,SP
    MOVE.L #4,D0        ; Illegal Instruction exception
    MOVE.L #ILLINSTHANDLE,A0
    JSR SETVEC
    MOVE.L #8,D0        ; Privilege violation exception
    MOVE.L #PRIVHANDLE,A0
    JSR SETVEC
    JSR START           ; Call the main program
    STOP #$2000
    BRA *
    SIMHALT             ; halt simulator
;
;  Set an exception vector
;  Input: A0 is the address of the handler
;         D0 is the vector number
;
SETVEC:
    MOVE.L A1,-(SP)
    MOVE.L D0,-(SP)
    ASL.L #2,D0         ; Multiply by 4 to get address in table
    MOVE.L D0,A1
    MOVE.L A0,(A1)      ; Set the vector
    MOVE.L (SP)+,D0
    MOVE.L (SP)+,A1
    RTS
;
;  Exception vectors
;
NULLVEC:                ; All vectors initialized to this
    RTE
;
ILLINSTHANDLE:          ;  Illegal instruction handler
    MOVE.L #ILLEXP,A0
    JSR PUTSTR
    BRA *
    RTE
;
PRIVHANDLE:             ;  Privilege violation handler
    MOVE.L #PRIVEXP,A0
    JSR PUTSTR
    BRA *
    RTE
;==============================================================================
; Setup the stack sections
;
    SECTION USTACK
    ORG $10000
    DS.L $1000
;==============================================================================
USRSTK:
    SECTION SSTACK
    ORG $20000
    DS.L $1000
SUPSTK:
;==============================================================================
;  Hardware Abstraction Layer section
;
    SECTION HSECT
    ORG $00FF0000
;
; *** Depricated ***
;  Put a zero terminated string to the console.
;  Input: Address of string in register A0
;  Output: A0 is address of end of string
;
;PUTS:
;    MOVE.L D0,-(SP)
;.L1: MOVE.B (A0)+,D0    ; Get character
;    TST.B D0
;    BEQ .L1A            ; If it's zero then exit
;    MOVE.B D0,TTYDAT    ; Print the character
;    BRA .L1
;.L1A:
;    MOVE.L (SP)+,D0
;    RTS
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
    MOVE.W (A0)+,D0      ; Get length of string
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
    BEQ .L1             ; Wait for bit 0 to be set
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
    ORG $00FFFF00
TTYDAT:             ;  Console data
    DS.B 1
TTYSTAT             ;  Console status (LSB indicates data ready to read)
    DS.B 1

    END INIT    ; last line of source


*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
