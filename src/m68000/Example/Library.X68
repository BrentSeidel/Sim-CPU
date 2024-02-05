;-----------------------------------------------------------
; Title      : Numbers Library
; Written by : Brent Seidel
; Date       : 4-Feb-2024
; Description: Conversions between numbers and ASCII (oct, dec, and hex)
;-----------------------------------------------------------
;==============================================================================
;  Library data section.  Common data and data for library routines go here.
;
    SECTION LDSECT
    TEXT NEWLINE,<CR,LF>
NUMTBL:                 ;  Table for converting between numbers and ASCII strings
    DC.B '0123456789ABCDEFGHIJKLMNOPQRSTUVWXTZ'
;==============================================================================
;  Library section.  Common routines would go here.
;
    SECTION LSECT
;
;  Convert numbers to string
;  Inputs: Number to convert (long)
;          Address of destination string (long)
;          Conversion flags (word)
;   Conversion flags:
;       0-1: Size (00-Byte, 01-Word, 10-Long)
;         2: Sign (0-unsigned, 1-signed)
;         3: Lead (0-no leading characters, 1-leading zeros)
;         4: Negative (internal use only)
;       5-7: Unused
;      8-15: Unused
;  Stack:
;    16(A6) - Flags (Word)
;    12(A6) - Address of destination (long)
;     8(A6) - Number to convert (long)
;     4(A6) - Old PC (long)
;      (A6) - Old A6
;   -18(A6) - Conversion buffer
;   Registers:
;       D0 - Number to convert
;       D1 - Conversion flags
;       D2 - Scratch
;       D3 - Max size of result
;       D4 - Fill character
;       A0 - Address of buffer
;       A1 - Index into conversion table
;
;  Convert a number to an octal string
;
OCTSTR:
    LINK A6,#-20
    MOVEM.L D0-D3/A0-A1,-(SP)
    MOVE.L 8(A6),D0    ;  Number to convert
    MOVE.W 16(A6),D1   ;  Conversion flags
    LEA -2(A6),A0      ;  Address of end of conversion buffer
    ;
    ;  Check conversion size
    ;
.SIZE
    MOVE.L D1,D2       ;  Get size field
    AND.B #3,D2
    BNE .N1
    AND.L #$FF,D0      ;  Byte size
    MOVE #3,D3         ;  Number of characters
    BTST #2,D1         ;  Check if signed
    BEQ .N3
    TST.B D0           ;  Check if positive or negative
    BPL .N3
    BSET #4,D1         ;  Set flag for negative
    NEG.B D0
    BRA .N3
.N1:
    SUBQ.B #1,D2
    BNE .N2
    AND.L #$FFFF,D0    ;  Word size
    MOVE #6,D3
    BTST #2,D1         ;  Check if signed
    BEQ .N3
    TST.W D0           ;  Check if positive or negative
    BPL .N3
    BSET #4,D1         ;  Set flag for negative
    NEG.W D0
    BRA .N3
.N2:
    MOVE #11,D3        ;  Long size
    BTST #2,D1         ;  Check if signed
    BEQ .N3
    TST.L D0           ;  Check if positive or negative
    BPL .N3
    BSET #4,D1         ;  Set flag for negative
    NEG.L D0
.N3:
    ;
    ;  Conversion loop
    ;
.L1:
    MOVE.L D0,D2
    AND.L #7,D2
    MOVE.L D2,A1
    MOVE.B NUMTBL(A1),-(A0)
    LSR.L #3,D0
    BNE .L2
    BTST #3,D1
    BEQ .L3
.L2:
    DBF D3,.L1
    BRA .L4
.L3:
    SUBQ.B #1,D3
.L4:
    ;
    ;  Check for adding negative sign
    ;
    BTST #2,D1
    BEQ .D1            ;  Not signed
    BTST #4,D1
    BEQ .D1            ;  Not negative
    MOVE.B #'-',-(A0)
    ;
    ;  Move to destination
    ;
.D1:
    MOVE.L A0,D2       ;  Pointer to start of string
    MOVE.L A0,A1
    LEA -2(A6),A0      ;  Address of end of conversion buffer
    MOVE.L A0,D3
    SUB.L D2,D3        ;  Length of converted string
    MOVE.L 12(A6),A0   ;  Address of destination
    MOVE.W (A0),D2     ;  Max size of destination
    CMP.W D3,D2
    BGE .D2
    MOVE.W D2,D3
.D2:
    MOVE.W D3, 2(A0)   ;  Set length of destination
    SUBQ.L #1,D3
    ADDQ.L #4,A0       ;  Point to start of text part of string
.D3
    MOVE.B (A1)+,(A0)+
    DBF D3,.D3
    ;
    ;  Cleanup and return
    ;
    MOVEM.L (SP)+,D0-D3/A0-A1
    UNLK A6
    RTS
;
;  Convert a number to an hexidecimal string
;
HEXSTR:
    LINK A6,#-20
    MOVEM.L D0-D3/A0-A1,-(SP)
    MOVE.L 8(A6),D0    ;  Number to convert
    MOVE.W 16(A6),D1   ;  Conversion flags
    LEA -2(A6),A0      ;  Address of end of conversion buffer
    ;
    ;  Check conversion size
    ;
.SIZE
    MOVE.L D1,D2       ;  Get size field
    AND.B #3,D2
    BNE .N1
    AND.L #$FF,D0      ;  Byte size
    MOVE #2,D3         ;  Number of characters
    BTST #2,D1         ;  Check if signed
    BEQ .N3
    TST.B D0           ;  Check if positive or negative
    BPL .N3
    BSET #4,D1         ;  Set flag for negative
    NEG.B D0
    BRA .N3
.N1:
    SUBQ.B #1,D2
    BNE .N2
    AND.L #$FFFF,D0    ;  Word size
    MOVE #4,D3
    BTST #2,D1         ;  Check if signed
    BEQ .N3
    TST.W D0           ;  Check if positive or negative
    BPL .N3
    BSET #4,D1         ;  Set flag for negative
    NEG.W D0
    BRA .N3
.N2:
    MOVE #8,D3         ;  Long size
    BTST #2,D1         ;  Check if signed
    BEQ .N3
    TST.L D0           ;  Check if positive or negative
    BPL .N3
    BSET #4,D1         ;  Set flag for negative
    NEG.L D0
.N3:
    ;
    ;  Conversion loop
    ;
.L1:
    MOVE.L D0,D2
    AND.L #15,D2
    MOVE.L D2,A1
    MOVE.B NUMTBL(A1),-(A0)
    LSR.L #4,D0
    BNE .L2
    BTST #3,D1
    BEQ .L3
.L2:
    DBF D3,.L1
    BRA .L4
.L3:
    SUBQ.B #1,D3
.L4:
    ;
    ;  Check for adding negative sign
    ;
    BTST #2,D1
    BEQ .D1            ;  Not signed
    BTST #4,D1
    BEQ .D1            ;  Not negative
    MOVE.B #'-',-(A0)
    ;
    ;  Move to destination
    ;
.D1:
    MOVE.L A0,D2       ;  Pointer to start of string
    MOVE.L A0,A1
    LEA -2(A6),A0      ;  Address of end of conversion buffer
    MOVE.L A0,D3
    SUB.L D2,D3        ;  Length of converted string
    MOVE.L 12(A6),A0   ;  Address of destination
    MOVE.W (A0),D2     ;  Max size of destination
    CMP.W D3,D2
    BGE .D2
    MOVE.W D2,D3
.D2:
    MOVE.W D3, 2(A0)   ;  Set length of destination
    SUBQ.L #1,D3
    ADDQ.L #4,A0       ;  Point to start of text part of string
.D3
    MOVE.B (A1)+,(A0)+
    DBF D3,.D3
    ;
    ;  Cleanup and return
    ;
    MOVEM.L (SP)+,D0-D3/A0-A1
    UNLK A6
    RTS

*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
