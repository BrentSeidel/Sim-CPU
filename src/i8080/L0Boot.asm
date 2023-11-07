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
;  For convenience, start at address 0.  Code can be relocated anywhere
;
    .AREA BOOT (ABS)
    .ORG 0H
    MVI A,SEL0
    OUT FDCTL       ; Select drive 0
    XRA A
    OUT FDTRK       ; Select track 0
    MVI A,1
    OUT FDSEC       ; Select sector 1 (sector numbers start at 1)
    MVI A,0HE4
    OUT FDMSB       ; DMA MSB E4
    XRA A
    OUT FDLSB       ; DMA LSB 0
    MVI A,51
;
;  26 sectors per track and two tracks.  Minus one sector for the boot
;  sector.
;
    OUT FDCNT       ; Load 51 sectors (17 sectors to load CCP)
    MVI A,RD
    OUT FDCTL       ; Read sector
    JMP 0HF9FD      ; Transfer control to loaded code
;
;  End of boot loader
;
