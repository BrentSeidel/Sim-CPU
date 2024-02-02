;-----------------------------------------------------------
; Title      : Sections.S68
; Written by : Brent Seidel
; Date       : 2-Feb-2024
; Description: Defines section numbers and locations
;-----------------------------------------------------------
;
;  ***  This is not a stand-alone file.  It gets included into
;  ***  other assembly language programs.
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
;  Define section starting addresses
;
    SECTION CSECT
    ORG $1000
    SECTION DSECT
    ORG $2000
    SECTION VSECT
    ORG 0
    SECTION LDSECT
    ORG $00FB0000
    SECTION LSECT
    ORG $00FC000
    SECTION ODSECT
    ORG $00FD0000
    SECTION OSECT
    ORG $00FE0000
    SECTION USTACK
    ORG $10000
    SECTION SSTACK
    ORG $20000
    SECTION HSECT
    ORG $00FF0000
    SECTION PSECT
    ORG $00FFFF00

*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
