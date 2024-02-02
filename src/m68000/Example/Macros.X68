;-----------------------------------------------------------
; Title      : Macros.S68
; Written by : Brent Seidel
; Date       : 2-Feb-2024
; Description: A collection of macros and definitions
;-----------------------------------------------------------
;
;  ***  This is not a stand-alone file.  It gets included into
;  ***  other assembly language programs.
;
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

*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
