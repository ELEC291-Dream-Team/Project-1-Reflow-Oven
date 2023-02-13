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

PWM_PERIOD equ 200

; io pins
LEFT      equ P2.0
RIGHT     equ P2.3
UP        equ P2.3
DOWN      equ P2.5
STARTSTOP equ P2.4
SPEAKER_E equ P2.3
OVEN      equ P2.2

; LCD pin mapping
LCD_D7 equ P1.0
LCD_D6 equ P1.1
LCD_D5 equ P1.2
LCD_D4 equ P1.3
LCD_E  equ P1.4
; LCD_RW equ Px.x ; Always grounded
LCD_RS equ P1.5

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
    Counter1000ms: ds 2
    Counter100ms:  ds 1
    CounterPWM:    ds 1
; parameter variables
    SoakTempBCD:   ds 2
    SoakTimeBCD:   ds 2
    ReflowTempBCD: ds 2
    ReflowTimeBCD: ds 2
    SoakTempHex:   ds 2
    SoakTimeHex:   ds 2
    ReflowTempHex: ds 2
    ReflowTimeHex: ds 2
; flash variables
    w:             ds 3
    FlashReadAddr: ds 3
; temp variables
    ColdTemp: ds 4
    HotTemp:  ds 4
; PWM variables
    PWMDutyCycle: ds 1

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

zero2Bytes mac
    mov %0+0, #0x00
    mov %0+1, #0x00
endmac

Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
    zero2Bytes(Counter1000ms)
    mov Counter100ms, #0x00
    mov CounterPWM, #0x00
    mov PWMDutyCycle, #0x00
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
ret

Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	; cpl P1.1 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.

	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	inc Counter1000ms+0    ; Increment the low 8-bits first
	mov a, Counter1000ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz incCounter1000msDone
	inc Counter1000ms+1
    incCounter1000msDone:

    ; if statement to inc or not
        inc Counter100ms

    ; if statement to inc or not
        inc CounterPWM

    ; if Counter1000ms overflows
    mov a, Counter1000ms+0
	cjne a, #low(1000), Counter1000msnotOverflow 
	mov a, Counter1000ms+1
	cjne a, #high(1000), Counter1000msnotOverflow
	    clr a
	    mov Counter1000ms+0, a
	    mov Counter1000ms+1, a
    Counter1000msnotOverflow:

    ; if Counter100ms overflows
    mov a, Counter100ms
	cjne a, #100, Counter100msnotOverflow 
	    clr a
	    mov Counter100ms, a
    Counter100msnotOverflow:

    mov a, CounterPWM
	cjne a, #PWM_PERIOD, CounterPWMnotOverflow 
	    clr a
	    mov CounterPWM, a
        setb OVEN
    CounterPWMnotOverflow:
    mov a, CounterPWM
    cjne a, PWMDutyCycle, PWMNotSame
        clr OVEN
    PWMNotSame:
    
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

ReadTemp: ; stores temp in x in hex
    readVoltageChannel(1)
    Load_y(297)
    lcall mul32
    Load_y(2950)
    lcall div32
    Load_y(273)
    lcall sub32
    mov ColdTemp+3, x+3
    mov ColdTemp+2, x+2
    mov ColdTemp+1, x+1
    mov ColdTemp+0, x+0
    
    readVoltageChannel(6)
    Load_y(1000)
    lcall mul32
    Load_y(12341)
    lcall div32
    mov HotTemp+3, x+3
    mov HotTemp+2, x+2
    mov HotTemp+1, x+1
    mov HotTemp+0, x+0
    
    mov y+3, ColdTemp+3
    mov y+2, ColdTemp+2
    mov y+1, ColdTemp+1
    mov y+0, ColdTemp+0
    lcall add32

    lcall hex2bcd
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

writeByte mac
	mov a, %0
	movx @dptr, a
	inc dptr
endmac

write2Bytes mac
	mov a, %0+1
	movx @dptr, a
	inc dptr
    mov a, %0+0
	movx @dptr, a
	inc dptr
endmac

Save_Configuration:
    push IE ; Save the current state of bit EA in the stack
    clr EA ; Disable interrupts
	mov FCON, #0x08 ; Page Buffer Mapping Enabled (FPS = 1)
	mov dptr, #0x7f80 ; Last page of flash memory
	; Save variables
	write2Bytes(SoakTempBCD) ; @0x7f80
	write2Bytes(SoakTimeBCD) ; @0x7f82
	write2Bytes(ReflowTempBCD) ; @0x7f84
	write2Bytes(ReflowTimeBCD) ; @0x7f86
	writeByte(#0x55) ; First key value @0x7f87
	writeByte(#0xAA) ; Second key value @0x7f88
	mov FCON, #0x00 ; Page Buffer Mapping Disabled (FPS = 0)
	orl EECON, #0b01000000 ; Enable auto-erase on next write sequence
	mov FCON, #0x50 ; Write trigger first byte
	mov FCON, #0xA0 ; Write trigger second byte
	; CPU idles until writing of flash completes.
	mov FCON, #0x00 ; Page Buffer Mapping Disabled (FPS = 0)
	anl EECON, #0b10111111 ; Disable auto-erase
    pop IE
ret

readByte mac
	clr a
	movc a, @a+dptr
	mov %0, a
	inc dptr
Endmac

read2Bytes mac
	clr a
	movc a, @a+dptr
	mov %0+1, a
	inc dptr
    clr a
	movc a, @a+dptr
	mov %0+0, a
	inc dptr
Endmac

Load_Configuration:
	mov dptr, #0x7f87 ; First key value location.
	readByte(R0) ; 0x7f84 should contain 0x55
	cjne R0, #0x55, Load_Defaults
	readByte(R0) ; 0x7f85 should contain 0xAA
	cjne R0, #0xAA, Load_Defaults
	; Keys are good.  Get stored values.
	mov dptr, #0x7f80
	read2Bytes(SoakTempBCD) ; 0x7f80
	read2Bytes(SoakTimeBCD) ; 0x7f82
	read2Bytes(ReflowTempBCD) ; 0x7f84
	read2Bytes(ReflowTimeBCD) ; 0x7f86
ret
    Load_Defaults: ; Load defaults if 'keys' are incorrect
	    mov SoakTempBCD+1, #0x01
	    mov SoakTempBCD+0, #0x50
	    mov SoakTimeBCD+1, #0x00
	    mov SoakTimeBCD+0, #0x45
	    mov ReflowTempBCD+1, #0x02
	    mov ReflowTempBCD+0, #0x25
	    mov ReflowTimeBCD+1, #0x00
	    mov ReflowTimeBCD+0, #0x30
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

    Load_x(0)
    lcall hex2bcd

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
        ifPressedJumpTo(STARTSTOP, adjustParameters, 1)
    ljmp startLoop
; end of start state

adjustParameters:
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
    adjustSoakTemp100:
        Set_Cursor(2,1)
        ifPressedJumpTo(LEFT, start, 1)
        ifPressedJumpTo(RIGHT, adjustSoakTemp010, 1)
        ifNotPressedJumpTo(UP, _adjustSoakTemp100a)
            ; increment 100's of Soak Temp
            mov a, SoakTempBCD+1
            add a, #0x01
            da a
            anl a, #0x0f
            mov SoakTempBCD+1, a
            setb Updated

        _adjustSoakTemp100a:     
        ifNotPressedJumpTo(DOWN, _adjustSoakTemp100b)
            ; decrement 100's of Soak Temp
            mov a, SoakTempBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov SoakTempBCD+1, a
            setb Updated

        _adjustSoakTemp100b:
        lcall updateDisplay ; update param display
        
    ljmp adjustSoakTemp100 
    
    adjustSoakTemp010:
        Set_Cursor(2,2)
        ifPressedJumpTo(LEFT, adjustSoakTemp100, 1)
        ifPressedJumpTo(RIGHT, adjustSoakTemp001, 1)
        ifNotPressedJumpTo(UP, _adjustSoakTemp010a)
            ; increment 10's of Soak Temp
            mov a, SoakTempBCD+0
            add a, #0x10
            da a
            mov SoakTempBCD+0, a
            setb Updated

        _adjustSoakTemp010a:     
        ifNotPressedJumpTo(DOWN, _adjustSoakTemp010b)
            ; decrement 10's of Soak Temp
            mov a, SoakTempBCD+0
            add a, #0x90
            da a
            mov SoakTempBCD+0, a
            setb Updated

        _adjustSoakTemp010b:
        lcall updateDisplay ; update param display
        
    ljmp adjustSoakTemp010 
    
    adjustSoakTemp001:
        Set_Cursor(2,3)
        ifPressedJumpTo(LEFT, adjustSoakTemp010, 1)
        ifPressedJumpTo(RIGHT, adjustSoakTime100, 1)
        ifNotPressedJumpTo(UP, _adjustSoakTemp001a)
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

        _adjustSoakTemp001a:     
        ifNotPressedJumpTo(DOWN, _adjustSoakTemp001b)
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

        _adjustSoakTemp001b:
        lcall updateDisplay ; update param display
        
    ljmp adjustSoakTemp001
    
    ; ----------------------------------------------;
	; ---------------- Soak Time -------------------;
	; ----------------------------------------------;
    adjustSoakTime100:
        Set_Cursor(2,5)
        ifPressedJumpTo(LEFT, adjustSoakTemp001, 1)
        ifPressedJumpTo(RIGHT, adjustSoakTime010, 1)
        ifNotPressedJumpTo(UP, _adjustSoakTime100a)
            ; increment 100's of Soak Time
            mov a, SoakTimeBCD+1
            add a, #0x01
            da a
            anl a, #0x0f
            mov SoakTimeBCD+1, a
            setb Updated

        _adjustSoakTime100a:     
        ifNotPressedJumpTo(DOWN, _adjustSoakTime100b)
            ; decrement 100's of Soak Time
            mov a, SoakTimeBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov SoakTimeBCD+1, a
            setb Updated

        _adjustSoakTime100b:
        lcall updateDisplay ; update param display
        
    ljmp adjustSoakTime100 
    
    adjustSoakTime010:
        Set_Cursor(2,6)
        ifPressedJumpTo(LEFT, adjustSoakTime100, 1)
        ifPressedJumpTo(RIGHT, adjustSoakTime001, 1)
        ifNotPressedJumpTo(UP, _adjustSoakTime010a)
            ; increment 10's of Soak Time
            mov a, SoakTimeBCD+0
            add a, #0x10
            da a
            mov SoakTimeBCD+0, a
            setb Updated

        _adjustSoakTime010a:     
        ifNotPressedJumpTo(DOWN, _adjustSoakTime010b)
            ; decrement 10's of Soak Time
            mov a, SoakTimeBCD+0
            add a, #0x90
            da a
            mov SoakTimeBCD+0, a
            setb Updated

        _adjustSoakTime010b:
        lcall updateDisplay ; update param display
        
    ljmp adjustSoakTime010 
    
    adjustSoakTime001:
        Set_Cursor(2,7)
        ifPressedJumpTo(LEFT, adjustSoakTime010, 1)
        ifPressedJumpTo(RIGHT, adjustReflowTemp100, 1)
        ifNotPressedJumpTo(UP, _adjustSoakTime001a)
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

        _adjustSoakTime001a:     
        ifNotPressedJumpTo(DOWN, _adjustSoakTime001b)
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

        _adjustSoakTime001b:
        lcall updateDisplay ; update param display
        
    ljmp adjustSoakTime001
    
    ; ----------------------------------------------;
	; ------------ Reflow Temperature --------------;
	; ----------------------------------------------;
    adjustReflowTemp100:
        Set_Cursor(2,9)
        ifPressedJumpTo(LEFT, adjustSoakTime001, 1)
        ifPressedJumpTo(RIGHT, adjustReflowTemp010, 1)
        ifNotPressedJumpTo(UP, _adjustReflowTemp100a)
            ; increment 100's of Reflow Temp
            mov a, ReflowTempBCD+1
            add a, #0x01
            da a
            anl a, #0x0f
            mov ReflowTempBCD+1, a
            setb Updated

        _adjustReflowTemp100a:     
        ifNotPressedJumpTo(DOWN, _adjustReflowTemp100b)
            ; decrement 100's of Reflow Temp
            mov a, ReflowTempBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov ReflowTempBCD+1, a
            setb Updated

        _adjustReflowTemp100b:
        lcall updateDisplay ; update param display
        
    ljmp adjustReflowTemp100 
    
    adjustReflowTemp010:
        Set_Cursor(2,10)
        ifPressedJumpTo(LEFT, adjustReflowTemp100, 1)
        ifPressedJumpTo(RIGHT, adjustReflowTemp001, 1)
        ifNotPressedJumpTo(UP, _adjustReflowTemp010a)
            ; increment 10's of Reflow Temp
            mov a, ReflowTempBCD+0
            add a, #0x10
            da a
            mov ReflowTempBCD+0, a
            setb Updated

        _adjustReflowTemp010a:     
        ifNotPressedJumpTo(DOWN, _adjustReflowTemp010b)
            ; decrement 10's of Reflow Temp
            mov a, ReflowTempBCD+0
            add a, #0x90
            da a
            mov ReflowTempBCD+0, a
            setb Updated

        _adjustReflowTemp010b:
        lcall updateDisplay ; update param display
        
    ljmp adjustReflowTemp010 
    
    adjustReflowTemp001:
        Set_Cursor(2,11)
        ifPressedJumpTo(LEFT, adjustReflowTemp010, 1)
        ifPressedJumpTo(RIGHT, adjustReflowTime100, 1)
        ifNotPressedJumpTo(UP, _adjustReflowTemp001a)
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

        _adjustReflowTemp001a:     
        ifNotPressedJumpTo(DOWN, _adjustReflowTemp001b)
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

        _adjustReflowTemp001b:
        lcall updateDisplay ; update param display
        
    ljmp adjustSoakTemp001
    
    ; ----------------------------------------------;
	; --------------- Reflow Time ------------------;
	; ----------------------------------------------;
    adjustReflowTime100:
        Set_Cursor(2,13)
        ifPressedJumpTo(LEFT, adjustReflowTemp001, 1)
        ifPressedJumpTo(RIGHT, adjustReflowTime010, 1)
        ifNotPressedJumpTo(UP, _adjustReflowTime100a)
            ; increment 100's of Reflow Time
            mov a, ReflowTimeBCD+1
            add a, #0x01
            da a
            anl a, #0x0f
            mov ReflowTimeBCD+1, a
            setb Updated

        _adjustReflowTime100a:     
        ifNotPressedJumpTo(DOWN, _adjustReflowTime100b)
            ; decrement 100's of Reflow Time
            mov a, ReflowTimeBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov ReflowTimeBCD+1, a
            setb Updated

        _adjustReflowTime100b:
        lcall updateDisplay ; update param display
        
    ljmp adjustReflowTime100 
    
    adjustReflowTime010:
        Set_Cursor(2,14)
        ifPressedJumpTo(LEFT, adjustReflowTime100, 1)
        ifPressedJumpTo(RIGHT, adjustReflowTime001, 1)
        ifNotPressedJumpTo(UP, _adjustReflowTime010a)
            ; increment 10's of Reflow Time
            mov a, ReflowTimeBCD+0
            add a, #0x10
            da a
            mov ReflowTimeBCD+0, a
            setb Updated

        _adjustReflowTime010a:     
        ifNotPressedJumpTo(DOWN, _adjustReflowTime010b)
            ; decrement 10's of Reflow Time
            mov a, ReflowTimeBCD+0
            add a, #0x90
            da a
            mov ReflowTimeBCD+0, a
            setb Updated

        _adjustReflowTime010b:
        lcall updateDisplay ; update param display
        
    ljmp adjustReflowTime010 
    
    adjustReflowTime001:
        Set_Cursor(2,15)
        ifPressedJumpTo(LEFT, adjustReflowTime010, 1)
        ifPressedJumpTo(RIGHT, ready, 1)
        ifNotPressedJumpTo(UP, _adjustReflowTime001a)
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

        _adjustReflowTime001a:     
        ifNotPressedJumpTo(DOWN, _adjustReflowTime001b)
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

        _adjustReflowTime001b:
        lcall updateDisplay ; update param display
        
    ljmp adjustReflowTime001
; end of adjustParameters state

ready:
    ; display ready message
    Set_Cursor(1, 1)
    Send_Constant_String(#Ready_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Ready_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink

    ; Update the flash memory
    lcall Save_Configuration

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