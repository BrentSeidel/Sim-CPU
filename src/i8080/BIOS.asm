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
LIST::   JMP PRNOUT
PUNCH::  JMP NOTIMP
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
TTYST   .EQU 0
TTYDAT  .EQU 1          ; Simple console device at ports 0 & 1
PRNDAT  .EQU 2          ; Simple printer at port 2
PFDCTL  .EQU 3          ; Floppy control port
PFDSEC  .EQU PFDCTL+1   ; Select sector number
PFDTRK  .EQU PFDCTL+2   ; Select track number
PFDLSB  .EQU PFDCTL+3   ; LSB of DMA address
PDFMSB  .EQU PFDCTL+4   ; MSB of DMA address
PFDCNT  .EQU PFDCTL+5   ; Number of sectors to transfer
;
;  Low memory addresses
;
JMP1    .EQU 0H0000     ;  Address 0 contains a JMP instruction for warm boot
JMP1AD  .EQU 0H0001     ;  Address 1 is address for JMP instruction
CDISK   .EQU 0H0004     ;  current disk number 0=a,... l5=p
JMP2    .EQU 0H0005     ;  Address 5 contains a JMP instruction for BDOS entry
JMP2AD  .EQU 0H0006     ;  Address 6 contains address for JMP instruction
FBUFF   .EQU 0H0080     ;  Default file buffer is at 0H0080
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
;  should already be loaded into memory at this point, no no need to load
;  any part of it.
;
LOMEM:  MVI A, 0HC3     ;  C3 is a JMP instruction
        STA JMP1        ;  for JMP to wboot
        LXI H,WBOOT     ;  WBOOT entry point
        SHLD JMP1AD     ;  set address field for JMP at 0
;
        STA JMP2        ;  for JMP to bdos
        LXI H,FBASE     ;  BDOS entry point
        SHLD JMP2AD     ;  address field of Jump at 5 to BDOS
;
        LXI SP,CCPSTACK ; setup stack area
;
        MVI A,1
        OUT PFDCNT      ;  Set sector count back to 1
        LXI B,FBUFF     ;  default dma address is 80h (default file buffer)
        CALL SETDMA
;
        LXI D,0         ;  Disk number 0
        MVI C,0H0D      ;  BDOS Reset Disks Function
        CALL FBASE1     ;  Call BDOS
;
        EI              ;  enable the interrupt system
        LDA CDISK       ;  get current disk number
        MOV C,A         ;  send to the ccp
        JMP CBASE       ;  go to CCP for further processing
;
;  Implementation of BIOS functions
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
;  Write character in C to printer port
PRNOUT: PUSH PSW
        MOV A,C
        OUT PRNDAT
        POP PSW
        RET
;
;  Move selected disk to the home track (0)
FDHOME: PUSH PSW
        XRA A
        OUT PFDTRK
        POP PSW
        RET
;
;  Select FD disk.  Drive number is in C, return address of DPH in HL.
;  Register E bit 0 = 1 if the disk has been logged in before
FDSEL:  PUSH PSW
        MOV A,C
        CPI 0       ; Check for disk 0
        JNZ 1$
        ORI 0HC0
        OUT PFDCTL
        LXI H,DPH0
        JMP 5$
1$:     CPI 1       ; Check for disk 1
        JNZ 2$
        ORI 0HC0
        OUT PFDCTL
        LXI H,DPH1
        JMP 5$
2$:     CPI 2       ; Check for disk 2
        JNZ 3$
        ORI 0HC0
        OUT PFDCTL
        LXI H,DPH2
        JMP 5$
3$:     CPI 3       ; Check for disk 3
        JNZ 4$
        ORI 0HC0
        OUT PFDCTL
        LXI H,DPH3
        JMP 5$
4$:     LXI H,0     ; Unknown disk
5$:     POP PSW
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
;  Read from the floppy disk.  Return value in A (not yet implemented):
;   0 - Success
;   1 - Error
;  FF - Media changed
FDRD:   XRA A           ; Zero A to return success
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
;  Return value in A (not yet implemented):
;   0 - Success
;   1 - Error
;   2 - Disk is read-only
;  FF - Media changed
FDWR:   XRA A           ; Zero A to return success
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
        MOV L,M     ; l=trans (sector)
        MVI H,0     ; hl=trans (sector)
        RET         ; with value in hl
;
;  Output devices that are not implemented simply return
NOTIMP: RET
;
;  Input devices that are not implemented return ^Z (EOF).
RETEOF: MVI A,0H1A
        RET
;
;  Data tables for disks
;
;  sector translate vector (same for all disks)
trans:  .DB  1,  7, 13, 19  ; sectors  1,  2,  3,  4
        .DB 25,  5, 11, 17  ; sectors  5,  6,  7,  6
        .DB 23,  3,  9, 15  ; sectors  9, 10, 11, 12
        .DB 21,  2,  8, 14  ; sectors 13, 14, 15, 16
        .DB 20, 26,  6, 12  ; sectors 17, 18, 19, 20
        .DB 18, 24,  4, 10  ; sectors 21, 22, 23, 24
        .DB 16, 22          ; sectors 25, 26
;
; Disk parameter block (same for all disks)
;
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
; Disk parameter headers
;
DPH0:   .DW trans   ; Address translation table
        .DW 0,0,0   ; Workspace for CP/M
        .DW DSKBUF  ; Address of sector buffer
        .DW DPB0    ; Address of DPB
        .DW CHSMV0  ; Address of checksum vector
        .DW ALOCV0  ; Address of allocation vector
;
;  Checksum and allocation vectors
;

CHSMV0: .DS 16      ; Checksum vector
ALOCV0: .DS 32      ; Allocation vector
;
DPH1:   .DW trans   ; Address translation table
        .DW 0,0,0   ; Workspace for CP/M
        .DW DSKBUF  ; Address of sector buffer
        .DW DPB0    ; Address of DPB
        .DW CHSMV1  ; Address of checksum vector
        .DW ALOCV1  ; Address of allocation vector
;
;  Checksum and allocation vectors
;

CHSMV1: .DS 16      ; Checksum vector
ALOCV1: .DS 32      ; Allocation vector
;
DPH2:   .DW trans   ; Address translation table
        .DW 0,0,0   ; Workspace for CP/M
        .DW DSKBUF  ; Address of sector buffer
        .DW DPB0    ; Address of DPB
        .DW CHSMV2  ; Address of checksum vector
        .DW ALOCV2  ; Address of allocation vector
;
;  Checksum and allocation vectors
;
CHSMV2: .DS 16      ; Checksum vector
ALOCV2: .DS 32      ; Allocation vector
;
DPH3:   .DW trans   ; Address translation table
        .DW 0,0,0   ; Workspace for CP/M
        .DW DSKBUF  ; Address of sector buffer
        .DW DPB0    ; Address of DPB
        .DW CHSMV3  ; Address of checksum vector
        .DW ALOCV3  ; Address of allocation vector
;
;  Checksum and allocation vectors
;
CHSMV3: .DS 16      ; Checksum vector
ALOCV3: .DS 32      ; Allocation vector
;
;  128 Byte buffer for all disks
;
DSKBUF: .DS 128
CPMEND::
;
;*
;******************   E N D   O F   C P / M   *****************
;*
