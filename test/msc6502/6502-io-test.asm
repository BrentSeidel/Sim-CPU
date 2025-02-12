const   .equ 0hF000     ;  Console status register
condt   .equ 0hF001     ;  Console data register

   .area test(ABS)
;
;  zero page area
;
   .org 0
msgptr:
    .dw msg
;
;  program area
;
    .org 0h400

start::
;
;  Wait for a character on the consol
;
pause:
    lda 0hF000
    and #1
    beq pause

;
;  Print the message
;
    ldy #0
print:
    lda (msg),y
    beq exit
    sta condt
    iny
    jmp print

exit:
    jmp exit
msg:
    .ascii 'Hello world!'
    .db 13,10,0
   .end start
