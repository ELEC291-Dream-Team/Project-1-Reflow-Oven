org 0x0000
    ljmp reset ; Reset vector
org 0x0003 ; External interrupt 0 vector (not used in this code)
	reti
org 0x000B ; Timer/Counter 0 overflow interrupt vector
	reti
org 0x0013 ; External interrupt 1 vector (not used in this code)
	reti
org 0x001B ; Timer/Counter 1 overflow interrupt vector (not used in this code)
	reti
org 0x0023 ; Serial port receive/transmit interrupt vector (not used in this code)
	reti
org 0x002B ; Timer/Counter 2 overflow interrupt vector
	ljmp Timer2_ISR

$NOLIST
$MODLP51RC2
$include(LCD_4bit.inc)
$include(math32.inc)
$LIST

CLK           equ 22122000 ; Microcontroller system crystal frequency in Hz
TIMER2_RATE   equ 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD equ ((65536-(CLK/TIMER2_RATE)))
; TIMER2_RELOAD equ 43414
BAUD          equ 115200
BRG_VAL       equ (0x100-(CLK/(16*BAUD)))

; io pins
LEFT      equ P1.0
RIGHT     equ P1.3
UP        equ P3.3
DOWN      equ P4.5
STARTSTOP equ P2.4
SPEAKER   equ P2.3

; LCD pin mapping
LCD_RS equ P1.5
; LCD_RW equ Px.x ; Always grounded
LCD_E  equ P1.4
LCD_D4 equ P1.3
LCD_D5 equ P1.2
LCD_D6 equ P1.1
LCD_D7 equ P1.0

; SPI pin mapping
CS_ADC   equ P1.1
CS_FLASH equ P1.2
SPI_MOSI equ P1.7
SPI_MISO equ P1.5
SPI_SCLK equ P1.6

dseg at 0x30
; math32 variables
x:   ds 4
y:   ds 4
bcd: ds 5
; timer variables
Counter0:    ds 2
Counter1:    ds 2
; parameter variables
TempBCDSS: ds 2
TimeBCDSS: ds 2
TempBCDSE: ds 2
TimeBCDSE: ds 2
TempBCDRS: ds 2
TimeBCDRS: ds 2
TempBCDRE: ds 2
TimeBCDRE: ds 2
TempHexSS: ds 2
TimeHexSS: ds 2
TempHexSE: ds 2
TimeHexSE: ds 2
TempHexRS: ds 2
TimeHexRS: ds 2
TempHexRE: ds 2
TimeHexRE: ds 2

bseg
; math32 bit variable
mf: dbit 1

cseg
;                      1234567890123456
Start_display_1:   db '   RIZZOVEN69   ', 0
Start_display_2:   db 'Press Start     ', 0
SoakS_display_1:   db 'Soak Start   1/4', 0
SoakS_display_2:   db 'Tmp:xxx Time:xxx', 0
SoakE_display_1:   db 'Soak End     2/4', 0
SoakE_display_2:   db 'Tmp:xxx Time:xxx', 0
ReflowS_display_1: db 'Reflow Start 3/4', 0
ReflowS_display_2: db 'Tmp:xxx Time:xxx', 0
ReflowE_display_1: db 'Reflow End   4/4', 0
ReflowE_display_2: db 'Tmp:xxx Time:xxx', 0

Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Counter0+0, a
	mov Counter0+1, a
    mov Counter1+0, a
	mov Counter1+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    ; setb TR2  ; Enable timer 2
ret

Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	; cpl P1.1 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.

	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	inc Counter0+0    ; Increment the low 8-bits first
	mov a, Counter0+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz incCounter0Done
	inc Counter0+1
    incCounter0Done:

    mov a, Counter0+0
	cjne a, #low(1000), counter0notOverflow 
	mov a, Counter0+1
	cjne a, #high(1000), counter0notOverflow
	clr a ; reset Counter0
	mov Counter0+0, a
	mov Counter0+1, a
    counter0notOverflow:
    
    pop psw
	pop acc
reti

ReadVoltage:
    Load_x(0)
    clr CS_ADC ; Enable device (active low)
    mov r0, #0x01 ; start bit
    lcall DO_SPI
    mov r0, #0x80 ; channel 0, single
    lcall DO_SPI
    mov a, r1
    anl a, #0x03
    mov x+1, a
    lcall DO_SPI
    mov x+0, r1
    setb CS_ADC
    Load_y(38)
    lcall mul32
    Load_y(250)
    lcall add32
    Load_y(10)
    lcall div32
ret

ReadTemp:
    lcall ReadVoltage
    Load_y(297)
    lcall mul32
    Load_y(2947)
    lcall div32
    Load_y(273)
    lcall sub32
    lcall hex2bcd
ret

DO_SPI:
    clr SPI_SCLK           ; Mode 0,0 default
    mov R1, #0 ; Received byte stored in R1
    mov R2, #8            ; Loop counter (8-bits)
    DO_SPI_LOOP:
        mov a, R0             ; Byte to write is in R0
        rlc a                 ; Carry flag has bit to write
        mov R0, a
        mov SPI_MOSI, c
        setb SPI_SCLK          ; Transmit
        mov c, SPI_MISO        ; Read received bit
        mov a, R1             ; Save received bit in R1
        rlc a
        mov R1, a
        clr SPI_SCLK
    djnz R2, DO_SPI_LOOP
ret

InitSPI:
    setb CS_ADC
    setb CS_FLASH
    setb SPI_MISO
    clr SPI_SCLK
ret

InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	orl	PCON, #0x80
	mov	SCON, #0x52
	mov	BDRCON, #0x00
	mov	BRL, #BRG_VAL
	mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
ret

SerialPutChar:
    jnb TI, SerialPutChar
    clr TI
    mov SBUF, a
ret

SerialSend3BCD:
    mov a, bcd+1
    anl a, #0x0f
    orl a, #0x30
    lcall SerialPutChar
    mov a, bcd+0
    swap a
    anl a, #0x0f
    orl a, #0x30
    lcall SerialPutChar
    mov a, bcd+0
    anl a, #0x0f
    orl a, #0x30
    lcall SerialPutChar
    mov a, #'\r'
    lcall SerialPutChar
    mov a, #'\n'
    lcall SerialPutChar
ret

SerialSendString:
    clr A
    movc A, @A+DPTR
    jz SerialSendStringDone
    lcall SerialPutChar
    inc DPTR
    sjmp SerialSendString
    SerialSendStringDone:
ret

LCDSendString:
    clr A
    movc A, @A+DPTR
    jz LCDSendStringDone
    lcall ?WriteData
    inc DPTR
    sjmp LCDSendString
    LCDSendStringDone:
ret

ifPressedJumpTo mac
    jb %0, %0%1%2noPress
    Wait_Milli_Seconds(#5)
    jb %0, %0%1%2noPress
    jnb %0, $
    ljmp %1
    %0%1%2noPress:
endmac

ifNotPressedJumpTo mac
    jb %0, %1
    Wait_Milli_Seconds(#5)
    jb %0, %1
    jnb %0, $
endmac

ifPressedCall mac
    jb %0, %1%0%2noPress
    Wait_Milli_Seconds(#5)
    jb %0, %1%0%2noPress
    jnb %0, $
    lcall %1
    %1%0%2noPress:
endmac

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

Display_10_digit_BCD:
	Set_Cursor(2, 7)
	Display_BCD(bcd+4)
	Display_BCD(bcd+3)
	Display_BCD(bcd+2)
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
	; Replace all the zeros to the left with blanks
	Set_Cursor(2, 7)
	Left_blank(bcd+4, skip_blank)
	Left_blank(bcd+3, skip_blank)
	Left_blank(bcd+2, skip_blank)
	Left_blank(bcd+1, skip_blank)
	mov a, bcd+0
	anl a, #0f0h
	swap a
	jnz skip_blank
	Display_char(#' ')
    skip_blank:
ret

; We can display a number any way we want.  In this case with
; four decimal places.
Display_formated_BCD:
	Set_Cursor(2, 7)
	Display_char(#' ')
	Display_BCD(bcd+3)
	Display_BCD(bcd+2)
	Display_char(#'.')
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
ret

reset:
    mov SP, #0x7F
    lcall Timer2_Init

    mov P0M0, #0
    mov P0M1, #0
    mov P1M0, #0
    mov P1M1, #0
    mov P2M0, #0
    mov P2M1, #0
    mov P3M0, #0
    mov P3M1, #0
    mov P4M0, #0
    mov P4M1, #0
    
    setb EA   ; Enable Global interrupts

    ; initialize values
    mov TempBCDSS+0, #0x00
    mov TempBCDSS+1, #0x00
    mov TimeBCDSS+0, #0x00
    mov TimeBCDSS+1, #0x00
    mov TempBCDSE+0, #0x00
    mov TempBCDSE+1, #0x00
    mov TimeBCDSE+0, #0x00
    mov TimeBCDSE+1, #0x00
    mov TempBCDRS+0, #0x00
    mov TempBCDRS+1, #0x00
    mov TimeBCDRS+0, #0x00
    mov TimeBCDRS+1, #0x00
    mov TempBCDRE+0, #0x00
    mov TempBCDRE+1, #0x00
    mov TimeBCDRE+0, #0x00
    mov TimeBCDRE+1, #0x00
    mov TempHexSS+0, #0x00
    mov TempHexSS+1, #0x00
    mov TimeHexSS+0, #0x00
    mov TimeHexSS+1, #0x00
    mov TempHexSE+0, #0x00
    mov TempHexSE+1, #0x00
    mov TimeHexSE+0, #0x00
    mov TimeHexSE+1, #0x00
    mov TempHexRS+0, #0x00
    mov TempHexRS+1, #0x00
    mov TimeHexRS+0, #0x00
    mov TimeHexRS+1, #0x00
    mov TempHexRE+0, #0x00
    mov TempHexRE+1, #0x00
    mov TimeHexRE+0, #0x00
    mov TimeHexRE+1, #0x00

    mov bcd+0, #0x00
    mov bcd+1, #0x00
    mov bcd+2, #0x00
    mov bcd+3, #0x00

    lcall InitSPI
    lcall LCD_4BIT
    lcall InitSerialPort

start:
    ; display start message
    Set_Cursor(1, 1)
    Send_Constant_String(#Start_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Start_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink

    startLoop:
        ifPressedJumpTo(STARTSTOP, adjSSParameter, 1)
        ifPressedJumpTo(LEFT, adjSSParameter, 1)
        ifPressedJumpTo(RIGHT, adjSSParameter, 1)
        ifPressedJumpTo(UP, adjSSParameter, 1)
        ifPressedJumpTo(DOWN, adjSSParameter, 1)
    ljmp startLoop

adjSSParameter:
    ; update display
    ; show cursor
    Set_Cursor(1, 1)
    Send_Constant_String(#SoakS_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#SoakS_display_2)
    WriteCommand(#0x0e) ; show cursor, no blink

    adjSSParameterLoop100000:
        Set_Cursor(2,5)
        ifPressedJumpTo(LEFT, start, 1)
        ifPressedJumpTo(RIGHT, adjSSParameterLoop010000, 1)
        ifNotPressedJumpTo(UP, _adjSSParameterLoop100000a)
            ; increment 100 of TempBCDSS
            mov a, TempBCDSS+1
            anl a, #0x0f
            add a, #0x01
            da a
            anl a, #0x0f
            mov TempBCDSS+1, a

            ; Update Hex value ; mov this part to after confirming
            ; mov bcd+1, a
            ; lcall bcd2hex
            ; mov TempHexSS+0, x+0

        _adjSSParameterLoop100000a:     
        ifNotPressedJumpTo(DOWN, _adjSSParameterLoop100000b)
            ; decrement 100 of SS Temp
            mov a, TempBCDSS+1
            anl a, #0x0f
            add a, #0x09
            da a
            anl a, #0x0f
            mov TempBCDSS+1, a

        _adjSSParameterLoop100000b:
            ; update param display
        
    ljmp adjSSParameterLoop100000

    adjSSParameterLoop010000:
        Set_Cursor(2,6)
        ifPressedJumpTo(LEFT, adjSSParameterLoop100000, 1)
        ifPressedJumpTo(RIGHT, adjSSParameterLoop001000, 1)
        ifNotPressedJumpTo(UP, _adjSSParameterLoop010000a)
            ; increment 10 of TempBCDSS
            mov a, TempBCDSS+1
            anl a, #0x0f
            add a, #0x01
            da a
            anl a, #0x0f
            mov TempBCDSS+1, a

            ; Update Hex value ; mov this part to after confirming
            ; mov bcd+1, a
            ; lcall bcd2hex
            ; mov TempHexSS+0, x+0

        _adjSSParameterLoop010000a:     
        ifNotPressedJumpTo(DOWN, _adjSSParameterLoop100000b)
            ; decrement 10 of SS Temp
            mov a, TempBCDSS+1
            anl a, #0x0f
            add a, #0x09
            da a
            anl a, #0x0f
            mov TempBCDSS+1, a

        _adjSSParameterLoop010000b:
            ; update param display

    ljmp adjSSParameterLoop010000
    adjSSParameterLoop001000:

    ljmp adjSSParameterLoop001000
    adjSSParameterLoop000100:

    ljmp adjSSParameterLoop000100
    adjSSParameterLoop000010:

    ljmp adjSSParameterLoop000010
    adjSSParameterLoop000001:

    ljmp adjSSParameterLoop000001

adjSEParameter:
    ; update display
    ; show cursor
    Set_Cursor(1, 1)
    Send_Constant_String(#SoakE_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#SoakE_display_2)
    WriteCommand(#0x0e) ; show cursor, no blink

    adjSEParameterLoop100000:
        Set_Cursor(2,5)

        ifPressedJumpTo(LEFT, adjSSParameter, 2)
        ifPressedJumpTo(RIGHT, adjSEParameterLoop010000, 1)
        ifNotPressedJumpTo(UP, _adjSEParameterLoop100000a)
            ;
        _adjSEParameterLoop100000a:
        ifNotPressedJumpTo(DOWN, _adjSEParameterLoop100000b)
            ; 
        _adjSEParameterLoop100000b:
        ; update param display
    ljmp adjSEParameterLoop100000
    adjSEParameterLoop010000:
        
    ljmp adjSEParameterLoop010000
    adjSEParameterLoop001000:

    ljmp adjSEParameterLoop001000
    adjSEParameterLoop000100:

    ljmp adjSEParameterLoop000100
    adjSEParameterLoop000010:

    ljmp adjSEParameterLoop000010
    adjSEParameterLoop000001:

    ljmp adjSEParameterLoop000001

END