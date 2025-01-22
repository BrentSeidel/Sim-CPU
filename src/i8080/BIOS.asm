;**************************************************************
;*
;*        B I O S   J U M P   T A B L E
;*
;*  This BIOS is written for an 8080 simulator written in ADA.
;*  It does not necessarily represent any real hardware.
;*
;**************************************************************
;
BOOT::   JMP LOMEM
WBOOT::  JMP WARM
CONST::  JMP TTST
CONIN::  JMP TTIN
CONOUT:: JMP TTOUT
LIST::   JMP NOTIMP
PUNCH::  JMP PTOUT
READER:: JMP RETEOF
HOME::   JMP FDHOME
SELDSK:: JMP FDSEL
SETTRK:: JMP FDTRK
SETSEC:: JMP FDSEC
SETDMA:: JMP FDDMA
READ::   JMP FDRD
WRITE::  JMP FDWR
PRSTAT:: JMP NOTIMP
SECTRN:: JMP TRNSEC
;
;  I/O Ports
;
TTYST   .EQU 0          ;  Simple console device at ports 0 & 1
TTYDAT  .EQU 1
PTDAT   .EQU 2          ;  Simple paper tape device at ports 2 & 3
PTSTAT  .EQU 3
PFDCTL  .EQU 4          ;  Floppy control port
PFDSEC  .EQU PFDCTL+1   ;  Select sector number
PFDTRK  .EQU PFDCTL+2   ;  Select track number
PFDLSB  .EQU PFDCTL+3   ;  LSB of DMA address
PDFMSB  .EQU PFDCTL+4   ;  MSB of DMA address
PFDCNT  .EQU PFDCTL+5   ;  Number of sectors to transfer
;
;  Low memory addresses
;
JMP1    .EQU 0H0000     ;  Address 0 contains a JMP instruction for warm boot
JMP1AD  .EQU 0H0001     ;  Address 1 is address for JMP instruction
;IOBYTE  .EQU 0H0003     ; I/O definition byte.
;TDRIVE  .EQU 0H0004     ; current drive name and user number.
;ENTRY   .EQU 0H0005     ;  Address 5 contains a JMP instruction for BDOS entry
JMP2AD  .EQU 0H0006     ;  Address 6 contains address for JMP instruction
FBUFF   .EQU 0H0080     ;  Default file buffer is at 0H0080
;
JMPINST .EQU 0HC3       ;  Code for a JMP instruction
;
;  Code to read CCP from disk and jump to it for a warm boot
;
WARM:   MVI C,0
        CALL FDSEL      ;  Select drive 0
        XRA A           ;  Zero accumulator
        OUT PFDTRK      ;  Select track 0

        MVI A,1
        OUT PFDSEC      ;  Select sector 1 (sector numbers start at 1)

        LXI B,CBASE     ;  Start address of CP/M
        CALL FDDMA      ;  Set the DMA address
        MVI A,17

        OUT PFDCNT      ;  Load 17 sectors to load CCP
        MVI A,0H40
        OUT PFDCTL      ;  Read sectors

        MVI A,1
        OUT PFDCNT      ;  Set sector count back to 1
        JMP CBASE       ;  Transfer control to CCP
;
;  Setup low memory and jump to CCP for a cold boot.  The full CP/M image
;  should already be loaded into memory at this point, so no need to load
;  any part of it.
;
LOMEM:  MVI A,JMPINST   ;  C3 is a JMP instruction
        STA JMP1        ;  for JMP to WBOOT
        LXI H,WBOOT     ;  WBOOT entry point
        SHLD JMP1AD     ;  set address field for JMP at 0
;
        STA ENTRY       ;  for JMP to BDOS
        LXI H,FBASE     ;  BDOS entry point
        SHLD JMP2AD     ;  address field of Jump at 5 to BDOS
;
        LXI H,TDRIVE
        XRA A
        MOV M,A         ;  Set current disk and user to 0.
        LXI H,IOBYTE
;  Setup IOBYTE
; 7 6 5 4 3 2 1 0
; LST PUN RDR CON
; 00 TTY  00 TTY
; 01 CRT  01 PTR
; 10 LPT  10 UR1
; 11 UL1  11 UR2
;     00 TTY  00 TTY
;     01 PTP  01 CRT
;     10 UP1  10 BAT
;     11 UP2  11 UC1
; We want LST=LPT, PUN=PTP, RDR=PTR, and CON=CRT
;  10 01 01 01 = 95H
        MVI A,0H95
        MOV M,A         ;  Clear IOBYTE
;
        MVI A,1
        OUT PFDSEC      ;  Set sector to valid number
        OUT PFDTRK      ;  Set track to valid number
;
        LXI SP,CCPSTACK ; setup stack area
;
        MVI A,1
        OUT PFDCNT      ;  Set sector count back to 1
        LXI B,FBUFF     ;  default DMA address is 80h (default file buffer)
        CALL FDDMA
;
        LXI H,BOOTMSG   ;  Print boot message to console
1$:     MOV A,M
        ORA A           ;  Set flags
        JZ 2$
        OUT TTYDAT
        INX H
        JMP 1$
2$:
;
        EI              ;  enable the interrupt system
        LDA TDRIVE      ;  get current disk number
        MOV C,A         ;  send to the CCP
;
        JMP CBASE       ;  go to CCP for further processing
;
;  Implementation of BIOS functions
;  -------------------------------------
;  Console functions
;  -------------------------------------
;
;  Return console status (A and flags are impacted).
;  A = 0H00 - No data
;  A = 0HFF - Data ready
TTST:   IN TTYST
        ANI 1       ; Test input status bit
        JNZ 1$
        RET
1$:     MVI A,0HFF
        RET
;
;  Wait for an input character and return it in A
TTIN:   IN TTYST
        ANI 1
        JZ TTIN     ; Wait for status bit to be 1
        IN TTYDAT
        RET
;
;  Write character in C to console output
TTOUT:  PUSH PSW
        MOV A,C
        OUT TTYDAT
        POP PSW
        RET
;
;  Move selected disk to the home track (0)
FDHOME: PUSH PSW
        XRA A
        OUT PFDTRK
        POP PSW
        RET
;  -------------------------------------
;  Paper tape (reader and punch) functions
;  -------------------------------------
;
;  Read a character from the reader.  Return ^Z if EOF or no source.
PTIN:   IN PTSTAT
        ANI 5       ;  Check for input status bits
        XRI 1       ;  Invert input present bit
        JNZ 1$      ;  If no data...
        IN PTDAT    ;  Read data
        RET
1$:     MVI A,0H1A  ;  Return ^Z (EOF) marker
        RET
;
;  Send a character to the punch.  No error checking.
PTOUT:  PUSH PSW
        MOV A,C
        OUT PTDAT
        POP PSW
        RET
;  -------------------------------------
;  Disk functions
;  -------------------------------------
;
;  Select FD disk.  Drive number is in C, return address of DPH in HL.
;  Register E bit 0 = 1 if the disk has been logged in before
FDSEL:  PUSH PSW
        XRA A       ; Clear A
        MOV E,A     ; Set E to 0
        MOV A,C
        ORA A       ; Check for disk 0
        JNZ 1$
        LXI H,DPH0  ; Get appropriate disk parameter header
        JMP 99$
1$:     CPI 1       ; Check for disk 1
        JNZ 2$
        LXI H,DPH1  ; Get appropriate disk parameter header
        JMP 99$
2$:     CPI 2       ; Check for disk 2
        JNZ 3$
        LXI H,DPH2  ; Get appropriate disk parameter header
        JMP 99$
3$:     CPI 3       ; Check for disk 3
        JNZ 4$
        LXI H,DPH3  ; Get appropriate disk parameter header
        JMP 99$
4$:     CPI 4       ; Check for disk 4
        JNZ 5$
        LXI H,DPH4  ; Get appropriate disk parameter header
        JMP 99$
5$:     CPI 5       ; Check for disk 5
        JNZ 6$
        LXI H,DPH5  ; Get appropriate disk parameter header
        JMP 99$
6$:     CPI 6       ; Check for disk 6
        JNZ 7$
        LXI H,DPH6  ; Get appropriate disk parameter header
        JMP 99$
7$:     CPI 7       ; Check for disk 7
        JNZ 8$
        LXI H,DPH7
        JMP 99$
8$:
        LXI H,0     ; Unknown disk
        XRA A       ; Select disk 0 if unknown
        LXI H,DPH0  ; Get appropriate disk parameter header
99$:    ORI 0HC0    ; Select disk command to controller
        OUT PFDCTL  ; Send command
        POP PSW     ; Restore PSW and return
        RET
;
;  Select FD track (range 0-255 in BC - only C used)
FDTRK:  PUSH PSW
        MOV A,C
        OUT PFDTRK
        POP PSW
        RET
;
;  Select FD sector (range 1-255 in BC - only C used)
;  Since BDOS calls for the sector translation, it probably shouldn't be
;  done here.  The requested sector here should already be translated.
FDSEC:  PUSH PSW
        MOV A,C
        OUT PFDSEC
        POP PSW
        RET
;
;  Set the DMA address for the next disk read/write
FDDMA:  PUSH PSW
        MOV A,C
        OUT PFDLSB
        MOV A,B
        OUT PDFMSB
        POP PSW
        RET
;
;  Read from the floppy disk.  Return value in A (It turns out that the code at IORET just checks for non-zero):
;   0 - Success
;   1 - Error
;  FF - Media changed (currently ignored)
FDRD:   IN PFDCTL       ; Get controller status
        ANI 0HE0        ; Get status bits for changed, offline, or error
        JZ 2$           ; No bits set?  Everything's good
        ANI 0H20        ; Check for disk changed
        JZ 1$
        MVI A,0HFF      ; Disk changed
        JMP 2$          ; Ignore disk change because CP/M doesn't handle it properly
;        RET
1$:     MVI A,1         ; Some other error
        RET
2$:     XRA A           ; Zero A to return success
        PUSH PSW
        MVI A,1
        OUT PFDCNT      ; Read one sector
        MVI A,0H40
        OUT PFDCTL
        POP PSW
        RET
;
;  Write to the floppy disk.
;  On entry C is set to (not yet implemented):
;   0 - Write can be deferred
;   1 - Write must be immediate
;   2 - Write can be deferred, no pre-read is necessary.
;  Return value in A (It turns out that the code at IORET just checks for non-zero):
;   0 - Success
;   1 - Error
;   2 - Disk is read-only
;  FF - Media changed (currently ignored)
FDWR:   IN PFDCTL       ; Get controller status
        ANI 0HF0        ; Get status bits for changed, offline, readonly or error
        JZ 3$           ; No bits set?  Everything's good
        PUSH PSW
        ANI 0H20        ; Check for disk changed
        JZ 1$
        POP PSW
        MVI A,0HFF      ; Disk changed
        JMP 3$          ; Ignore disk change because CP/M doesn't handle it properly
;        RET
1$:     POP PSW
        ANI 0H10        ; Check for read-only
        JZ 2$
        MVI A,2
        RET
2$:     MVI A,1         ; Some other error
        RET
3$:     XRA A           ; Zero A to return success
        PUSH PSW
        MVI A,1
        OUT PFDCNT      ; Write one sector
        MVI A,0H80
        OUT PFDCTL
        POP PSW
        RET
;
; Translate the sector given by BC (zero based) using the translate table
; given by DE.  The physical sector number is returned in HL.
TRNSEC: XCHG        ; hl=.trans
        DAD B       ; hl=.trans (sector)
        MOV L,M     ; L=trans (sector)
        MVI H,0     ; HL=trans (sector)
        RET         ; with value in HL
;  -------------------------------------
;  Unimplemented device functions
;  -------------------------------------
;
;  Output devices that are not implemented simply return
NOTIMP: RET
;
;  Input devices that are not implemented return ^Z (EOF).
RETEOF: MVI A,0H1A
        RET
;
    .list (me)
;  -------------------------------------
;  Start of BIOS Data.
;  -------------------------------------
;  Message to print on boot
;
BOOTMSG: .ASCII 'CP/M 2.2 with BIOS for 8080/8085/Z80 Simulator'
        .DB 13,10,0
;
;  Data tables for disks
;
;  sector translate vector (same for all 8-inch disks)
TRANS:  .DB  1,  7, 13, 19  ; sectors  1,  2,  3,  4
        .DB 25,  5, 11, 17  ; sectors  5,  6,  7,  6
        .DB 23,  3,  9, 15  ; sectors  9, 10, 11, 12
        .DB 21,  2,  8, 14  ; sectors 13, 14, 15, 16
        .DB 20, 26,  6, 12  ; sectors 17, 18, 19, 20
        .DB 18, 24,  4, 10  ; sectors 21, 22, 23, 24
        .DB 16, 22          ; sectors 25, 26
;
; Disk parameter block (same for all 8-inch disks)
DPB0:   .DW  26     ; Number of sectors per track
        .DB  3      ; Block shift (1K)?
        .DB  7      ; Block mask (1K)?
        .DB  0      ; Extent mask?
        .DW  242    ; Number of blocks on disk - 1
        .DW  63     ; Number of directory entries - 1
        .DB  0HF0   ; First byte of directory allocation bitmap
        .DB  00     ; Second byte of directory allocation bitmap
        .DW  16     ; Checksum vector size
        .DW  2      ; Number of reserved tracks
;
; Disk parameter header macro.  "tbl" is the address translation table
; and "dpb" is the disk parameter block.
    .macro dph tbl,dpb,num
DPH'num:   .DW tbl     ; Address translation table
        .DW 0,0,0   ; Workspace for CP/M
        .DW DSKBUF  ; Address of sector buffer
        .DW dpb    ; Address of DPB
        .DW CKV'num    ; Address of checksum vector
        .DW ALV'num    ; Address of allocation vector
    .endm
;
;  Checksum and allocation vectors macro.  Each dph need to have an associated
;  vect.  They are split so that the vects can be placed at the end with
;  other uninitialized data.
    .macro vect num
CKV'num:  .DS 16  ;  Checksum vector
ALV'num:  .DS 32  ;  Allocation vector
    .endm
;
; Disk parameter headers
;
    dph TRANS,DPB0,0
    dph TRANS,DPB0,1
    dph TRANS,DPB0,2
    dph TRANS,DPB0,3
    dph TRANS,DPB0,4
    dph TRANS,DPB0,5
    dph TRANS,DPB0,6
    dph TRANS,DPB0,7
;
;  Checksum and allocation vectors and 128 Byte buffer for all disks.
;  Note that buffer does not need to be included in CP/M image written
;  to disk.
;
;  All data beyond this point is initialized by the software, so it doesn't
;  actually need to be saved and loaded.  CPMEND is the end address of what
;  needs to be written to the boot tracks.
;
CPMEND::
    vect 0
    vect 1
    vect 2
    vect 3
    vect 4
    vect 5
    vect 6
    vect 7
DSKBUF: .DS 128
;*
;******************   E N D   O F   C P / M   *****************
;*
    .END BOOT
