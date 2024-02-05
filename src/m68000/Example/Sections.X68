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
;  Define section numbers and starting addresses
;
CSECT   EQU  0      ;  Code section 
    SECTION CSECT
    ORG $10000
;
DSECT   EQU  1      ;  Data section
    SECTION DSECT
    ORG $18000
;
LSECT   EQU  7      ;  Library section
    SECTION LSECT
    ORG $4000
;
LDSECT  EQU  8      ;  Library data section
    SECTION LDSECT
    ORG $5000
;
USTACK  EQU  9      ;  User stack section
    SECTION USTACK
    ORG $1C000
;
SSTACK  EQU 10      ;  Supervisor stack section
    SECTION SSTACK
    ORG $C000
;
OSECT   EQU 11      ;  Vector handler/Operating System section
    SECTION OSECT
    ORG $2000
;
ODSECT  EQU 12      ;  Operating system data
    SECTION ODSECT
    ORG $3000
;
HSECT   EQU 13      ;  Hardware Abstraction Layer section
    SECTION HSECT
    ORG $1000
;
PSECT   EQU 14      ;  I/O Port section (400-FFF)
    SECTION PSECT
    ORG $400
;
VSECT   EQU 15      ;  Vector section (0-3FF)
    SECTION VSECT
    ORG 0
;
;  Define section starting addresses
;


*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
