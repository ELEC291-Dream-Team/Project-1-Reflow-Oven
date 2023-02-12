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
BAUD          equ 115200
BRG_VAL       equ (0x100-(CLK/(16*BAUD)))

; io pins
LEFT      equ P2.0
RIGHT     equ P2.3
UP        equ P2.3
DOWN      equ P2.5
STARTSTOP equ P2.4
SPEAKER_E equ P2.3

; LCD pin mapping
LCD_RS equ P1.5
; LCD_RW equ Px.x ; Always grounded
LCD_E  equ P1.4
LCD_D4 equ P1.3
LCD_D5 equ P1.2
LCD_D6 equ P1.1
LCD_D7 equ P1.0

; SPI pin mapping
SPI_SCLK equ P3.2
SPI_MOSI equ P3.3
SPI_MISO equ P3.4
CS_FLASH equ P3.5
CS_ADC   equ P3.6

; Commands supported by the SPI flash memory according to the datasheet
WRITE_ENABLE     EQU 0x06  ; Address:0 Dummy:0 Num:0
WRITE_DISABLE    EQU 0x04  ; Address:0 Dummy:0 Num:0
READ_STATUS      EQU 0x05  ; Address:0 Dummy:0 Num:1 to infinite
READ_BYTES       EQU 0x03  ; Address:3 Dummy:0 Num:1 to infinite
READ_SILICON_ID  EQU 0xab  ; Address:0 Dummy:3 Num:1 to infinite
FAST_READ        EQU 0x0b  ; Address:3 Dummy:1 Num:1 to infinite
WRITE_STATUS     EQU 0x01  ; Address:0 Dummy:0 Num:1
WRITE_BYTES      EQU 0x02  ; Address:3 Dummy:0 Num:1 to 256
ERASE_ALL        EQU 0xc7  ; Address:0 Dummy:0 Num:0
ERASE_BLOCK      EQU 0xd8  ; Address:3 Dummy:0 Num:0
READ_DEVICE_ID   EQU 0x9f  ; Address:0 Dummy:2 Num:1 to infinite

dseg at 0x30
; math32 variables
x:   ds 4
y:   ds 4
bcd: ds 5
; timer variables
Counter0:    ds 2
Counter1:    ds 2
; parameter variables
SoakTempBCD: ds 2
SoakTimeBCD: ds 2
ReflowTempBCD: ds 2
ReflowTimeBCD: ds 2
SoakTempHex: ds 2
SoakTimeHex: ds 2
ReflowTempHex: ds 2
ReflowTimeHex: ds 2
; flash variables
w: ds 3
FlashReadAddr: ds 3

temp_soak: ds 1
time_soak: ds 1
temp_refl: ds 1
time_refl: ds 1


bseg
mf: dbit 1 ; math32 bit variable
Updated: dbit 1 ; updated display flag

cseg
;                      		1234567890123456
Start_display_1:   		db '   RIZZOVEN69   ', 0
Start_display_2:   		db '  Press Start   ', 0
Parameter_display_1:   	db 'ST  St  RT  Rt  ', 0
Parameter_display_2:   	db 'xxx xxx xxx xxx ', 0
Ready_display_1:        db '     Ready      ', 0
Ready_display_2:        db '  Press Start   ', 0
R2Soak_display_1:       db '  Ramp to Soak  ', 0
R2Soak_display_2:       db 'Tmp:xxx Time:xxx', 0
Soak_display_1:         db '  Preheat/Soak  ', 0
Soak_display_2:         db 'Tmp:xxx Time:xxx', 0
R2Reflow_display_1:     db ' Ramp to Reflow ', 0
R2Reflow_display_2:     db 'Tmp:xxx Time:xxx', 0
Reflow_display_1:       db '     Reflow     ', 0
Reflow_display_2:       db 'Tmp:xxx Time:xxx', 0
Cooling_display_1:      db '  Cooling Down  ', 0
Cooling_display_2:      db 'Tmp:xxx Time:xxx', 0
Safe_display_1:         db ' Safe to Handle ', 0
Safe_display_2:         db 'Tmp:xxx Time:xxx', 0
Cancelled_display_1:    db '   Cancelled    ', 0
Cancelled_display_2:    db '  Press Start   ', 0

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

readVoltageChannel mac ; stores the voltage in x in mV
    Load_x(0)

    clr EA
    clr CS_ADC ; Enable device (active low)

    mov a, #0x01
    lcall Send_SPI
    mov a, #%0
    swap a
    orl a, #0x80
    lcall Send_SPI
    anl a, #0x03
    mov x+1, a
    lcall Send_SPI
    mov x+0, a

    setb CS_ADC
    setb EA
    
    Load_y(38)
    lcall mul32
    Load_y(35)
    lcall add32
    Load_y(10)
    lcall div32
endmac

ReadTemp:
    readVoltageChannel(1)
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

Send_SPI:
	SPIBIT MAC
	    ; Send/Receive bit %0
		rlc a
		mov SPI_MOSI, c
		setb SPI_SCLK
		mov c, SPI_MISO
		clr SPI_SCLK
		mov acc.0, c
	ENDMAC
	
	SPIBIT(7)
	SPIBIT(6)
	SPIBIT(5)
	SPIBIT(4)
	SPIBIT(3)
	SPIBIT(2)
	SPIBIT(1)
	SPIBIT(0)

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

; Eight bit number to display passed in ’a’.
; Sends result to LCD
SendToLCD:
	mov b, #100
	div ab
	orl a, #0x30 ; Convert hundreds to ASCII
	lcall ?WriteData ; Send to LCD
	mov a, b    ; Remainder is in register b
	mov b, #10
	div ab
	orl a, #0x30 ; Convert tens to ASCII
	lcall ?WriteData; Send to LCD
	mov a, b
	orl a, #0x30 ; Convert units to ASCII
	lcall ?WriteData; Send to LCD
ret

loadbyte mac
	mov a, %0
	movx @dptr, a
	inc dptr
endmac

Save_Configuration:
	mov FCON, #0x08 ; Page Buffer Mapping Enabled (FPS = 1)
	mov dptr, #0x7f80 ; Last page of flash memory
	; Save variables
	loadbyte(SoakTempBCD) ; @0x7f80
	loadbyte(SoakTimeBCD) ; @0x7f81
	loadbyte(ReflowTempBCD) ; @0x7f82
	loadbyte(ReflowTimeBCD) ; @0x7f83
	loadbyte(#0x55) ; First key value @0x7f84
	loadbyte(#0xAA) ; Second key value @0x7f85
	mov FCON, #0x00 ; Page Buffer Mapping Disabled (FPS = 0)
	orl EECON, #0b01000000 ; Enable auto-erase on next write sequence
	mov FCON, #0x50 ; Write trigger first byte
	mov FCON, #0xA0 ; Write trigger second byte
	; CPU idles until writing of flash completes.
	mov FCON, #0x00 ; Page Buffer Mapping Disabled (FPS = 0)
	anl EECON, #0b10111111 ; Disable auto-erase
ret

getbyte mac
	clr a
	movc a, @a+dptr
	mov %0, a
	inc dptr
Endmac

Load_Configuration:
	mov dptr, #0x7f84 ; First key value location.
	getbyte(R0) ; 0x7f84 should contain 0x55
	cjne R0, #0x55, Load_Defaults
	getbyte(R0) ; 0x7f85 should contain 0xAA
	cjne R0, #0xAA, Load_Defaults
	; Keys are good.  Get stored values.
	mov dptr, #0x7f80
	getbyte(SoakTempBCD) ; 0x7f80
	getbyte(SoakTimeBCD) ; 0x7f81
	getbyte(ReflowTempBCD) ; 0x7f82
	getbyte(ReflowTimeBCD) ; 0x7f83
ret

; Load defaults if 'keys' are incorrect
Load_Defaults:
	mov SoakTempBCD, #150
	mov SoakTimeBCD, #45
	mov ReflowTempBCD, #225
	mov ReflowTimeBCD, #30
ret

updateDisplay:
    jb Updated, _clearUpdated ; if !Updated skip print
    ljmp _updateDisplayDone
    _clearUpdated:
        clr Updated
        ; Display Soak Temp
        Set_Cursor(2,1)
        mov a, SoakTempBCD
        lcall SendToLCD
        
        ; Display Soak Time
        Set_Cursor(2,5)
        mov a, SoakTimeBCD
        lcall SendToLCD
        
        ; Display Reflow Temp
        Set_Cursor(2,9)
        mov a, ReflowTempBCD
        lcall SendToLCD
        
        ; Display Reflow Time
        Set_Cursor(2,13)
        mov a, ReflowTimeBCD
        lcall SendToLCD

        ; Update the flash memory
        lcall Save_Configuration

    _updateDisplayDone:
ret

reset:
    mov SP, #0x7F

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
    setb Updated ; update the display on reset

    ; initialize values
    mov SoakTempBCD+0, #0x00
    mov SoakTempBCD+1, #0x00
    mov SoakTimeBCD+0, #0x00
    mov SoakTimeBCD+1, #0x00
    mov ReflowTempBCD+0, #0x00
    mov ReflowTempBCD+1, #0x00
    mov ReflowTimeBCD+0, #0x00
    mov ReflowTimeBCD+1, #0x00
	mov SoakTempHex+0, #0x00
    mov SoakTempHex+1, #0x00
    mov SoakTimeHex+0, #0x00
    mov SoakTimeHex+1, #0x00
    mov ReflowTempHex+0, #0x00
    mov ReflowTempHex+1, #0x00
    mov ReflowTimeHex+0, #0x00
    mov ReflowTimeHex+1, #0x00

    mov bcd+0, #0x00
    mov bcd+1, #0x00
    mov bcd+2, #0x00
    mov bcd+3, #0x00

    lcall Timer2_Init
    lcall InitSPI
    lcall LCD_4BIT
    lcall InitSerialPort
    lcall Load_Configuration ; Update parameters with flash memory

start:
    ; display start message
    Set_Cursor(1, 1)
    Send_Constant_String(#Start_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Start_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink

    startLoop:
        ifPressedJumpTo(STARTSTOP, adjParameters, 1)
    ljmp startLoop

adjParameters:
    ; update display
    ; show cursor
    Set_Cursor(1, 1)
    Send_Constant_String(#Parameter_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Parameter_display_2)
    WriteCommand(#0x0e) ; show cursor, no blink

	; ----------------------------------------------;
	; ------------- Soak Temperature ---------------;
	; ----------------------------------------------;
    adjSTemp100:
        Set_Cursor(2,1)
        ifPressedJumpTo(LEFT, start, 1)
        ifPressedJumpTo(RIGHT, adjSTemp010, 1)
        ifNotPressedJumpTo(UP, _adjSTemp100a)
            ; increment 100's of Soak Temp
            mov a, SoakTempBCD+1
            add a, #0x01
            da a
            anl a, #0x0f
            mov SoakTempBCD+1, a
            setb Updated

        _adjSTemp100a:     
        ifNotPressedJumpTo(DOWN, _adjSTemp100b)
            ; decrement 100's of Soak Temp
            mov a, SoakTempBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov SoakTempBCD+1, a
            setb Updated

        _adjSTemp100b:
        lcall updateDisplay ; update param display
        
    ljmp adjSTemp100 
    
    adjSTemp010:
        Set_Cursor(2,2)
        ifPressedJumpTo(LEFT, adjSTemp100, 1)
        ifPressedJumpTo(RIGHT, adjSTemp001, 1)
        ifNotPressedJumpTo(UP, _adjSTemp010a)
            ; increment 10's of Soak Temp
            mov a, SoakTempBCD+0
            add a, #0x10
            da a
            mov SoakTempBCD+0, a
            setb Updated

        _adjSTemp010a:     
        ifNotPressedJumpTo(DOWN, _adjSTemp010b)
            ; decrement 10's of Soak Temp
            mov a, SoakTempBCD+0
            add a, #0x90
            da a
            mov SoakTempBCD+0, a
            setb Updated

        _adjSTemp010b:
        lcall updateDisplay ; update param display
        
    ljmp adjSTemp010 
    
    adjSTemp001:
        Set_Cursor(2,3)
        ifPressedJumpTo(LEFT, adjSTemp010, 1)
        ifPressedJumpTo(RIGHT, adjSTime100, 1)
        ifNotPressedJumpTo(UP, _adjSTemp001a)
            ; increment 1's of Soak Temp
            mov a, SoakTempBCD+0
            mov b, a
            add a, #0x01
            da a
            anl a, #0x0f
            anl b, #0xf0
            orl a, b
            mov SoakTempBCD+0, a
            setb Updated

        _adjSTemp001a:     
        ifNotPressedJumpTo(DOWN, _adjSTemp001b)
            ; decrement 1's of Soak Temp
            mov a, SoakTempBCD+0
            mov b, a
            add a, #0x09
            da a
            anl a, #0x0f
            anl b, #0xf0
            orl a, b
            mov SoakTempBCD+0, a
            setb Updated

        _adjSTemp001b:
        lcall updateDisplay ; update param display
        
    ljmp adjSTemp001
    
    ; ----------------------------------------------;
	; ---------------- Soak Time -------------------;
	; ----------------------------------------------;
    adjSTime100:
        Set_Cursor(2,5)
        ifPressedJumpTo(LEFT, adjSTemp001, 1)
        ifPressedJumpTo(RIGHT, adjSTime010, 1)
        ifNotPressedJumpTo(UP, _adjSTime100a)
            ; increment 100's of Soak Time
            mov a, SoakTimeBCD+1
            add a, #0x01
            da a
            anl a, #0x0f
            mov SoakTimeBCD+1, a
            setb Updated

        _adjSTime100a:     
        ifNotPressedJumpTo(DOWN, _adjSTime100b)
            ; decrement 100's of Soak Time
            mov a, SoakTimeBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov SoakTimeBCD+1, a
            setb Updated

        _adjSTime100b:
        lcall updateDisplay ; update param display
        
    ljmp adjSTime100 
    
    adjSTime010:
        Set_Cursor(2,6)
        ifPressedJumpTo(LEFT, adjSTime100, 1)
        ifPressedJumpTo(RIGHT, adjSTime001, 1)
        ifNotPressedJumpTo(UP, _adjSTime010a)
            ; increment 10's of Soak Time
            mov a, SoakTimeBCD+0
            add a, #0x10
            da a
            mov SoakTimeBCD+0, a
            setb Updated

        _adjSTime010a:     
        ifNotPressedJumpTo(DOWN, _adjSTime010b)
            ; decrement 10's of Soak Time
            mov a, SoakTimeBCD+0
            add a, #0x90
            da a
            mov SoakTimeBCD+0, a
            setb Updated

        _adjSTime010b:
        lcall updateDisplay ; update param display
        
    ljmp adjSTime010 
    
    adjSTime001:
        Set_Cursor(2,7)
        ifPressedJumpTo(LEFT, adjSTime010, 1)
        ifPressedJumpTo(RIGHT, adjRTemp100, 1)
        ifNotPressedJumpTo(UP, _adjSTime001a)
            ; increment 1's of Soak Time
            mov a, SoakTimeBCD+0
            mov b, a
            add a, #0x01
            da a
            anl a, #0x0f
            anl b, #0xf0
            orl a, b
            mov SoakTimeBCD+0, a
            setb Updated

        _adjSTime001a:     
        ifNotPressedJumpTo(DOWN, _adjSTime001b)
            ; decrement 1's of Soak Time
            mov a, SoakTimeBCD+0
            mov b, a
            add a, #0x09
            da a
            anl a, #0x0f
            anl b, #0xf0
            orl a, b
            mov SoakTimeBCD+0, a
            setb Updated

        _adjSTime001b:
        lcall updateDisplay ; update param display
        
    ljmp adjSTime001
    
    ; ----------------------------------------------;
	; ------------ Reflow Temperature --------------;
	; ----------------------------------------------;
    adjRTemp100:
        Set_Cursor(2,9)
        ifPressedJumpTo(LEFT, adjSTime001, 1)
        ifPressedJumpTo(RIGHT, adjRTemp010, 1)
        ifNotPressedJumpTo(UP, _adjRTemp100a)
            ; increment 100's of Reflow Temp
            mov a, ReflowTempBCD+1
            add a, #0x01
            da a
            anl a, #0x0f
            mov ReflowTempBCD+1, a
            setb Updated

        _adjRTemp100a:     
        ifNotPressedJumpTo(DOWN, _adjRTemp100b)
            ; decrement 100's of Reflow Temp
            mov a, ReflowTempBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov ReflowTempBCD+1, a
            setb Updated

        _adjRTemp100b:
        lcall updateDisplay ; update param display
        
    ljmp adjRTemp100 
    
    adjRTemp010:
        Set_Cursor(2,10)
        ifPressedJumpTo(LEFT, adjRTemp100, 1)
        ifPressedJumpTo(RIGHT, adjRTemp001, 1)
        ifNotPressedJumpTo(UP, _adjRTemp010a)
            ; increment 10's of Reflow Temp
            mov a, ReflowTempBCD+0
            add a, #0x10
            da a
            mov ReflowTempBCD+0, a
            setb Updated

        _adjRTemp010a:     
        ifNotPressedJumpTo(DOWN, _adjRTemp010b)
            ; decrement 10's of Reflow Temp
            mov a, ReflowTempBCD+0
            add a, #0x90
            da a
            mov ReflowTempBCD+0, a
            setb Updated

        _adjRTemp010b:
        lcall updateDisplay ; update param display
        
    ljmp adjRTemp010 
    
    adjRTemp001:
        Set_Cursor(2,11)
        ifPressedJumpTo(LEFT, adjRTemp010, 1)
        ifPressedJumpTo(RIGHT, adjRTime100, 1)
        ifNotPressedJumpTo(UP, _adjRTemp001a)
            ; increment 1's of Reflow Temp
            mov a, ReflowTempBCD+0
            mov b, a
            add a, #0x01
            da a
            anl a, #0x0f
            anl b, #0xf0
            orl a, b
            mov ReflowTempBCD+0, a
            setb Updated

        _adjRTemp001a:     
        ifNotPressedJumpTo(DOWN, _adjRTemp001b)
            ; decrement 1's of Reflow Temp
            mov a, ReflowTempBCD+0
            mov b, a
            add a, #0x09
            da a
            anl a, #0x0f
            anl b, #0xf0
            orl a, b
            mov ReflowTempBCD+0, a
            setb Updated

        _adjRTemp001b:
        lcall updateDisplay ; update param display
        
    ljmp adjSTemp001
    
    ; ----------------------------------------------;
	; --------------- Reflow Time ------------------;
	; ----------------------------------------------;
    adjRTime100:
        Set_Cursor(2,13)
        ifPressedJumpTo(LEFT, adjRTemp001, 1)
        ifPressedJumpTo(RIGHT, adjRTime010, 1)
        ifNotPressedJumpTo(UP, _adjRTime100a)
            ; increment 100's of Reflow Time
            mov a, ReflowTimeBCD+1
            add a, #0x01
            da a
            anl a, #0x0f
            mov ReflowTimeBCD+1, a
            setb Updated

        _adjRTime100a:     
        ifNotPressedJumpTo(DOWN, _adjRTime100b)
            ; decrement 100's of Reflow Time
            mov a, ReflowTimeBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov ReflowTimeBCD+1, a
            setb Updated

        _adjRTime100b:
        lcall updateDisplay ; update param display
        
    ljmp adjRTime100 
    
    adjRTime010:
        Set_Cursor(2,14)
        ifPressedJumpTo(LEFT, adjRTime100, 1)
        ifPressedJumpTo(RIGHT, adjRTime001, 1)
        ifNotPressedJumpTo(UP, _adjRTime010a)
            ; increment 10's of Reflow Time
            mov a, ReflowTimeBCD+0
            add a, #0x10
            da a
            mov ReflowTimeBCD+0, a
            setb Updated

        _adjRTime010a:     
        ifNotPressedJumpTo(DOWN, _adjRTime010b)
            ; decrement 10's of Reflow Time
            mov a, ReflowTimeBCD+0
            add a, #0x90
            da a
            mov ReflowTimeBCD+0, a
            setb Updated

        _adjRTime010b:
        lcall updateDisplay ; update param display
        
    ljmp adjRTime010 
    
    adjRTime001:
        Set_Cursor(2,15)
        ifPressedJumpTo(LEFT, adjRTime010, 1)
        ifPressedJumpTo(RIGHT, ready, 1)
        ifNotPressedJumpTo(UP, _adjRTime001a)
            ; increment 1's of Reflow Time
            mov a, ReflowTimeBCD+0
            mov b, a
            add a, #0x01
            da a
            anl a, #0x0f
            anl b, #0xf0
            orl a, b
            mov ReflowTimeBCD+0, a
            setb Updated

        _adjRTime001a:     
        ifNotPressedJumpTo(DOWN, _adjRTime001b)
            ; decrement 1's of Reflow Time
            mov a, ReflowTimeBCD+0
            mov b, a
            add a, #0x09
            da a
            anl a, #0x0f
            anl b, #0xf0
            orl a, b
            mov ReflowTimeBCD+0, a
            setb Updated

        _adjRTime001b:
        lcall updateDisplay ; update param display
        
    ljmp adjRTime001

ready:
    ; display ready message
    Set_Cursor(1, 1)
    Send_Constant_String(#Ready_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Ready_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink

    ;-------------------------------------------------;
    ;--------- Update Parameter Hex values -----------;
    ;-------------------------------------------------;
    
    Load_x(0) ; clear the bcd
    lcall hex2bcd
    
    ; Update Soak Temp Hex
    mov bcd+0, SoakTempBCD+0
    mov bcd+1, SoakTempBCD+1
    lcall bcd2hex
    mov SoakTempHex+0, x+0
    mov SoakTempHex+1, x+1

    ; Update Soak Time Hex
    mov bcd+0, SoakTimeBCD+0
    mov bcd+1, SoakTimeBCD+1
    lcall bcd2hex
    mov SoakTimeHex+0, x+0
    mov SoakTimeHex+1, x+1

    ; Update Reflow Temp Hex
    mov bcd+0, ReflowTempBCD+0
    mov bcd+1, ReflowTempBCD+1
    lcall bcd2hex
    mov ReflowTempHex+0, x+0
    mov ReflowTempHex+1, x+1

    ; Update Reflow Time Hex
    mov bcd+0, ReflowTimeBCD+0
    mov bcd+1, ReflowTimeBCD+1
    lcall bcd2hex
    mov ReflowTimeHex+0, x+0
    mov ReflowTimeHex+1, x+1

    readyLoop:
        ifPressedJumpTo(STARTSTOP, RampToSoak, 1)
    ljmp readyLoop

;-------------------------------------------;
;---------------- Oven FSM -----------------;
;-------------------------------------------;
RampToSoak:
    ; display Ramp to Soak message
    Set_Cursor(1, 1)
    Send_Constant_String(#R2Soak_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#R2Soak_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink
    ; 100% power
    RampToSoakLoop:
        ifPressedJumpTo(STARTSTOP, Cancelled, 1) ; Cancel if stop button pressed
        ; jmp to Soak if temp >= soak temp
        ; update LCD
    ljmp RampToSoakLoop

Soak:
    ; display Soak message
    Set_Cursor(1, 1)
    Send_Constant_String(#Soak_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Soak_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink
    SoakLoop:
        ifPressedJumpTo(STARTSTOP, Cancelled, 2) ; Cancel if stop button pressed
        ; PID Implementation to maintain currentTemp = SoakTempHex for SoakTimeHex
        ; update LCD
    ljmp SoakLoop

RampToReflow:
    ; display Ramp to Reflow message
    Set_Cursor(1, 1)
    Send_Constant_String(#R2Reflow_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#R2Reflow_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink
    ; 100% power till currentTemp = ReflowTempHex
    RampToReflowLoop:
        ifPressedJumpTo(STARTSTOP, Cancelled, 3) ; Cancel if stop button pressed
        ; jmp to reflow if temp >= reflow temp
        ; update LCD
    ljmp RampToReflowLoop

Reflow:
    ; display Reflow message
    Set_Cursor(1, 1)
    Send_Constant_String(#Reflow_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Reflow_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink
    ReflowLoop:
        ifPressedJumpTo(STARTSTOP, Cancelled, 4) ; Cancel if stop button pressed
        ; PID Implementation to maintain currentTemp = ReflowTempHex for ReflowTimeHex
        ; update LCD
    ljmp ReflowLoop

Cooling:
    Set_Cursor(1, 1) ; display Cooling message
    Send_Constant_String(#Cooling_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Cooling_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink
    ; 0% power
    CoolingLoop:
        ; jmp to Safe/home if temp <= 60
        ; update LCD
    ljmp CoolingLoop

Safe:
    Set_Cursor(1, 1) ; display Safe message
    Send_Constant_String(#Safe_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Safe_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink

    ; safe to touch if currentTemp < 60

ljmp Safe

Cancelled:
    Set_Cursor(1, 1) ; display Cancelled message
    Send_Constant_String(#Cancelled_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Cancelled_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink
    CancelledLoop:
        ifPressedJumpTo(STARTSTOP, start, 1) ; Return to the menu if start button pressed
    ljmp CancelledLoop

END