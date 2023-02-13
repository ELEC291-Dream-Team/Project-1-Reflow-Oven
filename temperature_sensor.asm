$MODLP51RC2
org 0000H
	ljmp MainProgram


CLK  EQU 22118400
BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))

$NOLIST	
$include(math32.inc)
$include(LCD_4bit.inc)
$LIST


BSEG
mf: dbit 1
CE_ADC    EQU  P2.0 
MY_MOSI   EQU  P2.1  
MY_MISO   EQU  P2.2 
MY_SCLK   EQU  P2.3 


DSEG at 40H
x: ds 4
y: ds 4
bcd: ds 5
 
 
CSEG 

; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

Left_blank mac
	mov a, %0
	anl a, #0xf0
	swap a
	jz Left_blank_%M_a
	ljmp %1
	Left_blank_%M_a:
	Display_char(#' ')
	mov a, %0
	anl a, #0x0f
	jz Left_blank_%M_b
	ljmp %1
	Left_blank_%M_b:
	Display_char(#' ')
endmac

BOOT_BUTTON   equ P4.5

temperature: db 'TempLCM:', 0

INIT_SPI: 
    setb MY_MISO    ; Make MISO an input pin 
    clr MY_SCLK     ; For mode (0,0) SCLK is zero 
    ret 
  
DO_SPI_G: 
    push acc 
    mov R1, #0      ; Received byte stored in R1 
    mov R2, #8      ; Loop counter (8-bits) 

DO_SPI_G_LOOP: 
    mov a, R0       ; Byte to write is in R0 
    rlc a           ; Carry flag has bit to write 
    mov R0, a 
    mov MY_MOSI, c 
    setb MY_SCLK    ; Transmit 
    mov c, MY_MISO  ; Read received bit 
    mov a, R1       ; Save received bit in R1 
    rlc a 
    mov R1, a 
    clr MY_SCLK 
    djnz R2, DO_SPI_G_LOOP 
    pop acc 
    ret 
    
; Configure the serial port and baud rate
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	orl	PCON,#0x80
	mov	SCON,#0x52
	mov	BDRCON,#0x00
	mov	BRL,#BRG_VAL
	mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
    ret
    
; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret

Read_ADC_Channel MAC
	mov b, #%0
	lcall _Read_ADC_Channel
ENDMAC

_Read_ADC_Channel:
	clr CE_ADC
	mov R0, #00000001B
	lcall DO_SPI_G
	mov a, b
	swap a
	anl a, #0F0H
	setb acc.7
	mov R0, a
	lcall DO_SPI_G
	mov a, R1
	anl a, #00000011B
	mov R7, a
	mov R0, #55H
	lcall DO_SPI_G
	mov a, R1
	mov R6, a
	setb CE_ADC
	clr a
	
Convert:
	; Copy the 10-bits of the ADC conversion into the 32-bits of 'x'
	mov x+0, r6
	mov x+1, r7 
	mov x+2, #0
	mov x+3, #0
	; Multiply by 410
	load_Y(410)
	lcall mul32
	; Divide result by 1023
	load_Y(1023)
	lcall div32
	; Subtract 273 from result
	load_Y(273)
	lcall sub32
	; The 4-bytes of x have the temperature in binary
	lcall hex2bcd ; converts binary in x to BCD in BCD
	lcall Display_10_digit_BCD
	
; Sends 10-digit BCD number in bcd to the LCD
Display_10_digit_BCD:
	Set_Cursor(1, 13)
	;Display_BCD(bcd+4)
	;Display_BCD(bcd+3)
	;Display_BCD(bcd+2)
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
	; Replace all the zeros to the left with blanks
	Set_Cursor(1, 13)
	;Left_blank(bcd+4, skip_blank)
	;Left_blank(bcd+3, skip_blank)
	;Left_blank(bcd+2, skip_blank)
	;Left_blank(bcd+1, skip_blank)
	mov a, bcd+0
	anl a, #0f0h
	swap a
	jnz skip_blank
	Display_char(#' ')

skip_blank:
	ret
	
;---------------------------------------;
; Send a BCD number to PuTTY in ASCIII  ;
;---------------------------------------;
Send_BCD mac
	push ar0
	mov r0, %0
	lcall ?Send_BCD
	pop ar0
endmac

?Send_BCD:
	push acc
	; Send most significant digit
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall putchar
	; Send least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall putchar
	pop acc
	ret
	
MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    lcall InitSerialPort
	Read_ADC_Channel(1)
	Send_BCD(bcd)
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar
	mov r7, #0
	lcall LCD_4BIT
	Set_Cursor(1,1)
    Send_Constant_String(#temperature)
    
loop:
	jb BOOT_BUTTON, loop_temp_button  
	Wait_Milli_Seconds(#50)	
	jb BOOT_BUTTON, loop_temp_button  
	jnb BOOT_BUTTON, $		

loop_temp_button:
	lcall display_10_digit_bcd
	
delay:
	Wait_Milli_Seconds(#250)
	inc r7
	cjne r7, #4, delay
	ret
END