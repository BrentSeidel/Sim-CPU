;
;  This is a bootstrap routine for a simulated 8080.  The simulated disk
;  controller has a count field so many sectors can be loaded at once.
;  This greatly simplifies the loader so effectively the initial loader
;  would be the same as the boot sector loader, rendering it unneeded.
;
FDCTL   .EQU 3          ; Floppy control port
FDSEC   .EQU FDCTL+1    ; Select sector number
FDTRK   .EQU FDCTL+2    ; Select track number
FDLSB   .EQU FDCTL+3    ; LSB of DMA address
FDMSB   .EQU FDCTL+4    ; MSB of DMA address
FDCNT   .EQU FDCTL+5    ; Number of sectors to transfer
RD      .EQU 0H40       ; Read command
SEL0    .EQU 0HC0       ; Select drive 0 command
;
LOAD    .EQU 0HE400     ; Load start address
ENTRY   .EQU 0HF9FD     ; CP/M entry point on boot
;
;  For convenience, start at address 0.  Code can be relocated anywhere
;
    .AREA BOOT (ABS)
    .ORG 0H
START: MVI A,SEL0
    OUT FDCTL       ; Select drive 0
    XRA A
    OUT FDTRK       ; Select track 0
    MVI A,1
    OUT FDSEC       ; Select sector 1 (sector numbers start at 1)
    MVI A,(LOAD >> 8)
    OUT FDMSB       ; DMA MSB
    MVI A,(LOAD & 0HFF)
    OUT FDLSB       ; DMA LSB
    MVI A,52
;
;  26 sectors per track and two tracks.  Minus one sector for the boot
;  sector.
;
    OUT FDCNT       ; Load 52 sectors (17 sectors to load CCP)
    MVI A,RD
    OUT FDCTL       ; Read sector
    JMP ENTRY       ; Transfer control to loaded code
;
;  End of boot loader
;
    .END START
