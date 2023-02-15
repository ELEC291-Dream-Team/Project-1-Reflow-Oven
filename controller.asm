org 0x0000
    ljmp reset ; Reset vector
    ; ljmp debug
org 0x0003 ; External interrupt 0 vector (not used in this code)
	reti
org 0x000B ; Timer/Counter 0 overflow interrupt vector
	reti
org 0x0013 ; External interrupt 1 vector (not used in this code)
	reti
org 0x001B ; Timer/Counter 1 overflow interrupt vector (not used in this code)
	ljmp Timer1_ISR
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
TIMER1_RATE   equ 22050     ; 22050Hz is the sampling rate of the wav file we are playing
TIMER1_RELOAD equ 0x10000-(CLK/TIMER1_RATE)
TIMER2_RATE   equ 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD equ ((65536-(CLK/TIMER2_RATE)))
BAUDRATE      equ 115200
BRG_VAL       equ (0x100-(CLK/(16*BAUDRATE)))

PWM_PERIOD equ 200 ; is 1 byte
PID_PERIOD equ 20 ; 1 byte
DEGREEC    equ 0xaa

; presets
SOAKTEMP_A   equ 0x0069
SOAKTIME_A   equ 0x0420
REFLOWTEMP_A equ 0x0069
REFLOWTIME_A equ 0x0420
SOAKTEMP_B   equ 0x0069
SOAKTIME_B   equ 0x0420
REFLOWTEMP_B equ 0x0069
REFLOWTIME_B equ 0x0420
SOAKTEMP_C   equ 0x0069
SOAKTIME_C   equ 0x0420
REFLOWTEMP_C equ 0x0069
REFLOWTIME_C equ 0x0420
SOAKTEMP_D   equ 0x0069
SOAKTIME_D   equ 0x0420
REFLOWTEMP_D equ 0x0069
REFLOWTIME_D equ 0x0420

; io pins
LEFT      equ P2.7
RIGHT     equ P2.6
UP        equ P2.1
DOWN      equ P2.5
STARTSTOP equ P4.5
SPEAKER_E equ P2.4
OVEN      equ P3.7

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
    x:             ds 4
    y:             ds 4
    bcd:           ds 5
; timer variables
    Counter1000ms: ds 2
    Counter100ms:  ds 1
    Counter5s:     ds 1
    CounterPWM:    ds 1
    CounterPID:    ds 1
    WaitCountBCD:  ds 2
    RunTimeBCD:    ds 2
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
    ColdTemp:          ds 2
    HotTemp:           ds 2
    OvenTemp:          ds 2
    OvenTempBCD:       ds 2
    TargetTemp:        ds 2
    TempToBePlayedBCD: ds 2
; PWM variables
    PWMDutyCycle:  ds 1
    PWMCompare:    ds 1

bseg
; math32
    mf: dbit 1 ; math32 bit variable
; LCD
    ParametersUpdated: dbit 1 ; ParametersUpdated display flag
; timer
    WaitFlag:           dbit 1 ; wait 30 sec flag
    RunTimeFlag:        dbit 1 ; start runtimer flag
    MaintainTargetTemp: dbit 1
    FiveSecondsFlag:    dbit 1
    Flag100ms: dbit 1
; sound
    SoundIsPlaying:    dbit 1
    SpeakerIsBusy:     dbit 1
    ToBePlayed100s:    dbit 1
    ToBePlayed10s:     dbit 1
    ToBePlayed1s:      dbit 1
    ToBePlayedDegreeC: dbit 1

cseg
;                      		1234567890123456
Start_display_1:   		db '   RIZZOVEN69   ', 0
Start_display_2:   		db '  Press  Start  ', 0
Select_display_1:       db ' Select Profile ', 0
Select_display_2:   	db ' A B C D CUSTOM ', 0
Parameter_display_1:   	db 'Soak   xxxC xxxs', 0
Parameter_display_2:   	db 'Reflow xxxC xxxs', 0
Ready_display_1:        db '     Ready      ', 0
Ready_display_2:        db '  Press Start   ', 0
R2Soak_display_1:       db '  Ramp to Soak  ', 0
R2Soak_display_2:       db '  xxxC    xxxs  ', 0
Soak_display_1:         db '      Soak      ', 0
Soak_display_2:         db '  xxxC    xxxs  ', 0
R2Reflow_display_1:     db ' Ramp to Reflow ', 0
R2Reflow_display_2:     db '  xxxC    xxxs  ', 0
Reflow_display_1:       db '     Reflow     ', 0
Reflow_display_2:       db '  xxxC    xxxs  ', 0
Cooling_display_1:      db '  Cooling Down  ', 0
Cooling_display_2:      db '  xxxC    xxxs  ', 0
Done_display_1:         db ' Safe to Handle ', 0
Done_display_2:         db '  Home in xxs   ', 0
Cancelled_display_1:    db '   Cancelled    ', 0
Cancelled_display_2:    db '  Home in xxs   ', 0

SerialSend3BCD mac
    mov a, %0+1
    anl a, #0x0f
    orl a, #0x30
    lcall SerialPutChar
    mov a, %0+0
    swap a
    anl a, #0x0f
    orl a, #0x30
    lcall SerialPutChar
    mov a, %0+0
    anl a, #0x0f
    orl a, #0x30
    lcall SerialPutChar
    mov a, #'\r'
    lcall SerialPutChar
    mov a, #'\n'
    lcall SerialPutChar
endmac

LCDSend3BCD mac
    mov a, %0+1
    anl a, #0x0f
    orl a, #0x30
    lcall ?WriteData
    mov a, %0+0
    swap a
    anl a, #0x0f
    orl a, #0x30
    lcall ?WriteData
    mov a, %0+0
    anl a, #0x0f
    orl a, #0x30
    lcall ?WriteData
endmac

mov2Bytes mac
    mov %0+1, %1+1
    mov %0+0, %1+0
endmac

zero2Bytes mac
    mov %0+0, #0x00
    mov %0+1, #0x00
endmac

DAC_Init:
    ; Configure the DAC.  The DAC output we are using is P2.3, but P2.2 is also reserved.
	mov DADI, #0b_1010_0000 ; ACON=1
	mov DADC, #0b_0011_1010 ; Enabled, DAC mode, Left adjusted, CLK/4
	mov DADH, #0x80 ; Middle of scale
	mov DADL, #0
	orl DADC, #0b_0100_0000 ; Start DAC by GO/BSY=1
    check_DAC_init:
	mov a, DADC
	jb acc.6, check_DAC_init ; Wait for DAC to finish
ret

Timer1_Init:
    ; Configure timer 1
	anl	TMOD, #0x0F ; Clear the bits of timer 1 in TMOD
	orl	TMOD, #0x10 ; Set timer 1 in 16-bit timer mode.  Don't change the bits of timer 0
	mov TH1, #high(TIMER1_RELOAD)
	mov TL1, #low(TIMER1_RELOAD)
	; Set autoreload value
	mov RH1, #high(TIMER1_RELOAD)
	mov RL1, #low(TIMER1_RELOAD)

	; Enable the timer and interrupts
    setb ET1  ; Enable timer 1 interrupt
	; setb TR1 ; Timer 1 is only enabled to play stored sound
    clr TR1
ret

Timer1_ISR:
    clr EA
	; The registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Check if the play counter is zero.  If so, stop playing sound.
	mov a, w+0
	orl a, w+1
	orl a, w+2
	jz stop_playing
	
	; Decrement play counter 'w'.  In this implementation 'w' is a 24-bit counter.
	mov a, #0xff
	dec w+0
	cjne a, w+0, keep_playing
	dec w+1
	cjne a, w+1, keep_playing
	dec w+2
	
    keep_playing:
	; setb SPEAKER
    clr CS_FLASH ; Enable SPI Flash

    ; refer to datasheet page 27 
    ; https://www.winbond.com/resource-files/w25q32jv%20spi%20revc%2008302016.pdf
	mov a, #READ_BYTES
	lcall Send_SPI
	mov a, FlashReadAddr+2
	lcall Send_SPI
	mov a, FlashReadAddr+1
	lcall Send_SPI
	mov a, FlashReadAddr+0
	lcall Send_SPI
    mov a, #0x69
	lcall Send_SPI

    setb CS_FLASH
	; mov P0, a ; WARNING: Remove this if not using an external DAC to use the pins of P0 as GPIO
	add a, #0x80
	mov DADH, a ; Output to DAC. DAC output is pin P2.3
	orl DADC, #0b_0100_0000 ; Start DAC by setting GO/BSY=1

    ; increment address by 1
    inc FlashReadAddr+0
    mov a, FlashReadAddr+0
    jnz incrementAddressDone
    inc FlashReadAddr+1
    mov a, FlashReadAddr+1
    jnz incrementAddressDone
    inc FlashReadAddr+2
    incrementAddressDone:
	sjmp Timer1_ISR_Done

    stop_playing:
        clr SpeakerIsBusy
	    clr TR1 ; Stop timer 1
	    setb CS_FLASH  ; Disable SPI Flash
	    clr SPEAKER_E ; Turn off speaker.  Removes hissing noise when not playing sound.
	    mov DADH, #0x80 ; middle of range
	    orl DADC, #0b_0100_0000 ; Start DAC by setting GO/BSY=1

    Timer1_ISR_Done:	
	pop psw
	pop acc
    setb EA
reti

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
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
ret

Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
    clr EA
	; cpl P1.1 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.

	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	inc Counter1000ms+0    ; Increment the low 8-bits first
	mov a, Counter1000ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz incCounter1000msDone
	inc Counter1000ms+1
    incCounter1000msDone:

    inc Counter100ms

    inc CounterPWM

    inc CounterPID

    ; if Counter1000ms overflows
    mov a, Counter1000ms+0
	cjne a, #low(1000), Counter1000msnotOverflow 
	mov a, Counter1000ms+1
	cjne a, #high(1000), Counter1000msnotOverflow
        zero2Bytes(Counter1000ms)

        inc Counter5s

        jnb RunTimeFlag, IncrementRunTimeDone
            mov a, RunTimeBCD+0
            add a, #0x01
            da a
            mov RunTimeBCD+0, a
            jnz IncrementRunTimeDone
            mov a, RunTimeBCD+1
            add a, #0x01
            da a
            mov RunTimeBCD+1, a
        IncrementRunTimeDone:
        jnb WaitFlag, Counter1000msnotOverflow
            mov a, WaitCountBCD+0
            add a, #0x99
            da a
            mov WaitCountBCD+0, a
            cjne a, #0x99, Counter1000msnotOverflow
            mov a, WaitCountBCD+1
            add a, #0x99
            da a
            mov WaitCountBCD+1, a
    Counter1000msnotOverflow:

    mov a, CounterPID
	cjne a, #PID_PERIOD, CounterPIDnotOverflow 
        mov CounterPID, #0x00
        jnb MaintainTargetTemp, CounterPIDnotOverflow
            ; do calculation: OvenTemp -> PWMDutyCycle

            Load_x(0)
            mov2Bytes(x, TargetTemp)
            Load_y(0)
            mov2Bytes(y, OvenTemp)

            lcall x_gt_y

            jnb mf, OvenIsHigherThanSet
                mov a, PWMDutyCycle
                cjne a, #PWM_PERIOD, doInc
                    sjmp skipInc
                    doInc:
                    inc a
                    mov PWMDutyCycle, a
                skipInc:
                ljmp CounterPIDnotOverflow
            OvenIsHigherThanSet:
                mov a, PWMDutyCycle
                jz skipDec
                    dec a
                    mov PWMDutyCycle, a
                skipDec:
    CounterPIDnotOverflow:

    ; if Counter100ms overflows
    mov a, Counter100ms
	cjne a, #100, Counter100msnotOverflow 
	    mov Counter100ms, #0x00
        setb Flag100ms
        SerialSend3BCD(OvenTempBCD)
        jb RunTimeFlag, Counter1000msnotOverflow
            lcall ReadTemp
    Counter100msnotOverflow:

    ; 5 seconds timer
    mov a, Counter5s
	cjne a, #5, Counter5snotOverflow 
	    mov Counter5s, #0x00
        jnb RunTimeFlag, Counter5snotOverflow
            setb FiveSecondsFlag
    Counter5snotOverflow:

    ; PWM timer
    mov a, CounterPWM
	cjne a, #PWM_PERIOD, CounterPWMnotOverflow 
	    mov CounterPWM, #0x00
        setb OVEN
        mov PWMCompare, PWMDutyCycle
    CounterPWMnotOverflow:
    mov a, CounterPWM
    cjne a, PWMCompare, PWMNotSame
        clr OVEN
    PWMNotSame:

    pop psw
	pop acc
    setb EA
reti

readADCChannel mac ; stores ADC in x
    Load_x(0)

    push IE
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
    pop IE
endmac

readVoltageChannel mac ; stores the voltage in x in mV
    readADCChannel(%0)
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
    Load_y(2950)
    lcall div32
    Load_y(273)
    lcall sub32
    mov2Bytes(ColdTemp, x)
    
    readVoltageChannel(6)
    Load_y(1000)
    lcall mul32
    Load_y(12341)
    lcall div32
    mov2Bytes(HotTemp, x)
    
    mov y+3, #0x00
    mov y+2, #0x00
    mov2Bytes(y, ColdTemp)
    lcall add32

    mov2Bytes(OvenTemp, x)

    lcall hex2bcd
    mov2Bytes(OvenTempBCD, bcd)
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
    clr SPEAKER_E
ret

InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    ; mov R1, #222
    ; mov R0, #166
    ; djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    ; djnz R1, $-4 ; 22.51519us*222=4.998ms
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
	mov dptr, #0x7f88 ; First key value location.
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

updateDisplayParameters:
    jb ParametersUpdated, _clearUpdated ; if !ParametersUpdated skip print
    ljmp updateDisplayParametersDone
    _clearUpdated:
        clr ParametersUpdated
        ; Display Soak Temp
        Set_Cursor(1,8)
        LCDSend3BCD(SoakTempBCD)
        
        ; Display Soak Time
        Set_Cursor(1,13)
        LCDSend3BCD(SoakTimeBCD)
        
        ; Display Reflow Temp
        Set_Cursor(2,8)
        LCDSend3BCD(ReflowTempBCD)
        
        ; Display Reflow Time
        Set_Cursor(2,13)
        LCDSend3BCD(ReflowTimeBCD)

    updateDisplayParametersDone:
ret

updateDisplayReflow:
    jnb Flag100ms, updateDisplayReflowDone
        clr Flag100ms
        Set_Cursor(2,3)
        LCDSend3BCD(OvenTempBCD)
        Set_Cursor(2,11)
        LCDSend3BCD(RunTimeBCD)

        ; Set_Cursor(1,1)
        ; LCDSend3BCD(PWMDutyCycle)
        ; LCDSend3BCD(TargetTemp)
        ; LCDSend3BCD(OvenTemp)
    updateDisplayReflowDone:
ret

setup:
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

    Wait_Milli_Seconds(#10)
    lcall LCD_4BIT
    Wait_Milli_Seconds(#10)
    lcall DAC_Init
    Wait_Milli_Seconds(#10)
    lcall Timer1_Init
    Wait_Milli_Seconds(#10)
    lcall Timer2_Init
    Wait_Milli_Seconds(#10)
    lcall InitSPI
    Wait_Milli_Seconds(#10)
    lcall Load_Configuration ; Update parameters with flash memory
    Wait_Milli_Seconds(#10)
    lcall InitSerialPort

    setb LEFT
    setb RIGHT
    setb UP
    setb DOWN
    setb STARTSTOP
    setb SPEAKER_E
    setb OVEN

    mov PWMDutyCycle, #0x00
    
    setb EA   ; Enable Global interrupts
    setb ParametersUpdated ; Update the display on reset
    clr WaitFlag ; WaitFlag off initially
    clr RunTimeFlag ; RunTimeFlag off initially
    clr MaintainTargetTemp 
    clr FiveSecondsFlag    
    clr SoundIsPlaying     
    clr SpeakerIsBusy      
    clr ToBePlayed100s     
    clr ToBePlayed10s      
    clr ToBePlayed1s       
    clr ToBePlayedDegreeC

    Load_x(0)
    lcall hex2bcd

    lcall ReadTemp

    ; custom degree C character
    ; WriteCommand(#0x40)
    ; WriteData(#0x18)
    ; WriteData(#0x18)
    ; WriteData(#0x03)
    ; WriteData(#0x04)
    ; WriteData(#0x04)
    ; WriteData(#0x04)
    ; WriteData(#0x03)
    ; WriteData(#0x00)
ret

playSound:
    setb SoundIsPlaying
    clr TR1
    clr SPEAKER_E
    cjne a, #0x00, notZero
        mov FlashReadAddr+2, #0x00
	    mov FlashReadAddr+1, #0x00
	    mov FlashReadAddr+0, #0x00
	    mov w+2, #0x00
	    mov w+1, #0x2b
	    mov w+0, #0x11
        ljmp swFinish
    notZero:
    cjne a, #0x01, notOne
        mov FlashReadAddr+2, #0x00
	    mov FlashReadAddr+1, #0x2B
	    mov FlashReadAddr+0, #0x11
	    mov w+2, #0x00
	    mov w+1, #0x2b
	    mov w+0, #0x11
        ljmp swFinish
    notOne:
    cjne a, #0x02, notTwo
        mov FlashReadAddr+2, #0x00
	    mov FlashReadAddr+1, #0x56
	    mov FlashReadAddr+0, #0x22
	    mov w+2, #0x00
	    mov w+1, #0x2b
	    mov w+0, #0x11
        ljmp swFinish
    notTwo:
    cjne a, #0x03, notThree
        mov FlashReadAddr+2, #0x00
	    mov FlashReadAddr+1, #0x81
	    mov FlashReadAddr+0, #0x33
	    mov w+2, #0x00
	    mov w+1, #0x2b
	    mov w+0, #0x11
        ljmp swFinish
    notThree:
    cjne a, #0x04, notFour
        mov FlashReadAddr+2, #0x00
	    mov FlashReadAddr+1, #0xAC
	    mov FlashReadAddr+0, #0x44
	    mov w+2, #0x00
	    mov w+1, #0x2b
	    mov w+0, #0x11
        ljmp swFinish
    notFour:
    cjne a, #0x05, notFive
        mov FlashReadAddr+2, #0x00
	    mov FlashReadAddr+1, #0xD7
	    mov FlashReadAddr+0, #0x55
	    mov w+2, #0x00
	    mov w+1, #0x2b
	    mov w+0, #0x11
        ljmp swFinish
    notFive:
    cjne a, #0x06, notSix
        mov FlashReadAddr+2, #0x01
	    mov FlashReadAddr+1, #0x02
	    mov FlashReadAddr+0, #0x66
	    mov w+2, #0x00
	    mov w+1, #0x2b
	    mov w+0, #0x11
        ljmp swFinish
    notSix:
    cjne a, #0x07, notSeven
        mov FlashReadAddr+2, #0x01
	    mov FlashReadAddr+1, #0x2D
	    mov FlashReadAddr+0, #0x77
	    mov w+2, #0x00
	    mov w+1, #0x2b
	    mov w+0, #0x11
        ljmp swFinish
    notSeven:
    cjne a, #0x08, notEight
        mov FlashReadAddr+2, #0x01
	    mov FlashReadAddr+1, #0x58
	    mov FlashReadAddr+0, #0x88
	    mov w+2, #0x00
	    mov w+1, #0x2b
	    mov w+0, #0x11
        ljmp swFinish
    notEight:
    cjne a, #0x09, notNine
        mov FlashReadAddr+2, #0x01
	    mov FlashReadAddr+1, #0x83
	    mov FlashReadAddr+0, #0x99
	    mov w+2, #0x00
	    mov w+1, #0x2b
	    mov w+0, #0x11
        ljmp swFinish
    notNine:
    cjne a, #DEGREEC, swFinish
        mov FlashReadAddr+2, #0x01
	    mov FlashReadAddr+1, #0xAE
	    mov FlashReadAddr+0, #0xAA
	    mov w+2, #0x00
	    mov w+1, #0x81
	    mov w+0, #0x33
        ljmp swFinish
    swFinish:
    setb SpeakerIsBusy
    setb SPEAKER_E
    setb TR1
ret

soundHandler:
    jnb FiveSecondsFlag, noFiveSecond
        clr FiveSecondsFlag
        setb ToBePlayed100s
        setb ToBePlayed10s
        setb ToBePlayed1s
        setb ToBePlayedDegreeC
        setb SoundIsPlaying
        mov2Bytes(TempToBePlayedBCD, OvenTempBCD)
    noFiveSecond:
    jb SoundIsPlaying, psL1 ; if SoundIsPlaying
        ret
    psL1:
        jnb SpeakerIsBusy, psL2
            ret
        psL2:
        jnb ToBePlayed100s, L100sPlayed
            clr ToBePlayed100s
            mov a, TempToBePlayedBCD+1
            anl a, #0x0f
            jz N100sIsZero
                lcall playSound
            N100sIsZero:
            ret
        L100sPlayed:
        jnb ToBePlayed10s, L10sPlayed
            clr ToBePlayed10s
            mov a, TempToBePlayedBCD+0
            swap a
            anl a, #0x0f
            lcall playSound
            ret
        L10sPlayed:
        jnb ToBePlayed1s, L1sPlayed
            clr ToBePlayed1s
            mov a, TempToBePlayedBCD+0
            anl a, #0x0f
            lcall playSound
            ret
        L1sPlayed:
        jnb ToBePlayedDegreeC, dCPlayed
            clr ToBePlayedDegreeC
            mov a, #DEGREEC
            lcall playSound
            ret
        dCPlayed:
        clr SoundIsPlaying
ret

reset:
    Wait_Milli_Seconds(#0xff)
    lcall setup
    Wait_Milli_Seconds(#0xff)

start:
    ; display start message
    WriteCommand(#0x0c) ; hide cursor, no blink
    Set_Cursor(1, 1)
    Send_Constant_String(#Start_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Start_display_2)

    startLoop:
        ifPressedJumpTo(STARTSTOP, selectProfile, 1)
        ifPressedJumpTo(RIGHT, selectProfile, 1)
    ljmp startLoop
; end of start state

mov2BytesConstant mac
    mov %0+1, #high(%1)
    mov %0+0, #low(%1)
endmac

selectProfile:
    ; display select message
    WriteCommand(#0x0e) ; show cursor, no blink
    Set_Cursor(1, 1)
    Send_Constant_String(#Select_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Select_display_2)

    select_A:
        Set_Cursor(2, 2)
        ; loadPreset(Preset_A)
        mov2BytesConstant(SoakTempBCD, SOAKTEMP_A)
        mov2BytesConstant(SoakTimeBCD, SOAKTIME_A)
        mov2BytesConstant(ReflowTempBCD, REFLOWTEMP_A)
        mov2BytesConstant(ReflowTimeBCD, REFLOWTIME_A)

        ifPressedJumpTo(LEFT, start, 1)
        ifPressedJumpTo(RIGHT, select_B, 1)
        ifPressedJumpTo(STARTSTOP, displayPreset, 1)
    ljmp select_A

    select_B:
        Set_Cursor(2, 4)
        ; loadPreset(Preset_B)
        ifPressedJumpTo(LEFT, select_A, 1)
        ifPressedJumpTo(RIGHT, select_C, 1)
        ifPressedJumpTo(STARTSTOP, displayPreset, 2)
    ljmp select_B

    select_C:
        Set_Cursor(2, 6)
        ; loadPreset(Preset_C)
        ifPressedJumpTo(LEFT, select_B, 1)
        ifPressedJumpTo(RIGHT, select_D, 1)
        ifPressedJumpTo(STARTSTOP, displayPreset, 3)
    ljmp select_C
    
    select_D:
        Set_Cursor(2, 8)
        ; loadPreset(Preset_D)
        ifPressedJumpTo(LEFT, select_C, 1)
        ifPressedJumpTo(RIGHT, select_CUSTOM, 1)
        ifPressedJumpTo(STARTSTOP, displayPreset, 4)
    ljmp select_D
    
    select_CUSTOM:
        Set_Cursor(2, 10)
        ifPressedJumpTo(LEFT, select_D, 1)
        ; ifPressedJumpTo(RIGHT, adjustParameters, 1)
        ifPressedJumpTo(STARTSTOP, adjustParameters, 1)
    ljmp select_CUSTOM

    displayPreset:
        WriteCommand(#0x0c) ; hide cursor, no blink
        lcall updateDisplayParameters
        displayPresetLoop:
        ifPressedJumpTo(STARTSTOP, ready, 1)
        ifPressedJumpTo(LEFT, ready, 1)
        ifPressedJumpTo(RIGHT, ready, 1)
        ifPressedJumpTo(UP, ready, 1)
        ifPressedJumpTo(DOWN, ready, 1)
    ljmp displayPresetLoop

adjustParameters:
    ; update display
    ; show cursor
    Set_Cursor(1, 1)
    Send_Constant_String(#Parameter_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Parameter_display_2)

    WriteCommand(#0x0e) ; show cursor, no blink

    setb ParametersUpdated

	; ----------------------------------------------;
	; ------------- Soak Temperature ---------------;
	; ----------------------------------------------;
    adjustSoakTemp100:
        Set_Cursor(1,8)
        ifPressedJumpTo(LEFT, select_CUSTOM, 1)
        ifPressedJumpTo(RIGHT, adjustSoakTemp010, 1)
        ifNotPressedJumpTo(UP, _adjustSoakTemp100a)
            ; increment 100's of Soak Temp
            mov a, SoakTempBCD+1
            add a, #0x01
            da a
            anl a, #0x0f
            mov SoakTempBCD+1, a
            setb ParametersUpdated

        _adjustSoakTemp100a:     
        ifNotPressedJumpTo(DOWN, _adjustSoakTemp100b)
            ; decrement 100's of Soak Temp
            mov a, SoakTempBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov SoakTempBCD+1, a
            setb ParametersUpdated

        _adjustSoakTemp100b:
        lcall updateDisplayParameters ; update param display
        
    ljmp adjustSoakTemp100 
    
    adjustSoakTemp010:
        Set_Cursor(1,9)
        ifPressedJumpTo(LEFT, adjustSoakTemp100, 1)
        ifPressedJumpTo(RIGHT, adjustSoakTemp001, 1)
        ifNotPressedJumpTo(UP, _adjustSoakTemp010a)
            ; increment 10's of Soak Temp
            mov a, SoakTempBCD+0
            add a, #0x10
            da a
            mov SoakTempBCD+0, a
            setb ParametersUpdated

        _adjustSoakTemp010a:     
        ifNotPressedJumpTo(DOWN, _adjustSoakTemp010b)
            ; decrement 10's of Soak Temp
            mov a, SoakTempBCD+0
            add a, #0x90
            da a
            mov SoakTempBCD+0, a
            setb ParametersUpdated

        _adjustSoakTemp010b:
        lcall updateDisplayParameters ; update param display
        
    ljmp adjustSoakTemp010 
    
    adjustSoakTemp001:
        Set_Cursor(1,10)
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
            setb ParametersUpdated

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
            setb ParametersUpdated

        _adjustSoakTemp001b:
        lcall updateDisplayParameters ; update param display
        
    ljmp adjustSoakTemp001
    
    ; ----------------------------------------------;
	; ---------------- Soak Time -------------------;
	; ----------------------------------------------;
    adjustSoakTime100:
        Set_Cursor(1,13)
        ifPressedJumpTo(LEFT, adjustSoakTemp001, 1)
        ifPressedJumpTo(RIGHT, adjustSoakTime010, 1)
        ifNotPressedJumpTo(UP, _adjustSoakTime100a)
            ; increment 100's of Soak Time
            mov a, SoakTimeBCD+1
            add a, #0x01
            da a
            anl a, #0x0f
            mov SoakTimeBCD+1, a
            setb ParametersUpdated

        _adjustSoakTime100a:     
        ifNotPressedJumpTo(DOWN, _adjustSoakTime100b)
            ; decrement 100's of Soak Time
            mov a, SoakTimeBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov SoakTimeBCD+1, a
            setb ParametersUpdated

        _adjustSoakTime100b:
        lcall updateDisplayParameters ; update param display
        
    ljmp adjustSoakTime100 
    
    adjustSoakTime010:
        Set_Cursor(1,14)
        ifPressedJumpTo(LEFT, adjustSoakTime100, 1)
        ifPressedJumpTo(RIGHT, adjustSoakTime001, 1)
        ifNotPressedJumpTo(UP, _adjustSoakTime010a)
            ; increment 10's of Soak Time
            mov a, SoakTimeBCD+0
            add a, #0x10
            da a
            mov SoakTimeBCD+0, a
            setb ParametersUpdated

        _adjustSoakTime010a:     
        ifNotPressedJumpTo(DOWN, _adjustSoakTime010b)
            ; decrement 10's of Soak Time
            mov a, SoakTimeBCD+0
            add a, #0x90
            da a
            mov SoakTimeBCD+0, a
            setb ParametersUpdated

        _adjustSoakTime010b:
        lcall updateDisplayParameters ; update param display
        
    ljmp adjustSoakTime010 
    
    adjustSoakTime001:
        Set_Cursor(1,15)
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
            setb ParametersUpdated

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
            setb ParametersUpdated

        _adjustSoakTime001b:
        lcall updateDisplayParameters ; update param display
        
    ljmp adjustSoakTime001
    
    ; ----------------------------------------------;
	; ------------ Reflow Temperature --------------;
	; ----------------------------------------------;
    adjustReflowTemp100:
        Set_Cursor(2,8)
        ifPressedJumpTo(LEFT, adjustSoakTime001, 1)
        ifPressedJumpTo(RIGHT, adjustReflowTemp010, 1)
        ifNotPressedJumpTo(UP, _adjustReflowTemp100a)
            ; increment 100's of Reflow Temp
            mov a, ReflowTempBCD+1
            add a, #0x01
            da a
            anl a, #0x0f
            mov ReflowTempBCD+1, a
            setb ParametersUpdated

        _adjustReflowTemp100a:     
        ifNotPressedJumpTo(DOWN, _adjustReflowTemp100b)
            ; decrement 100's of Reflow Temp
            mov a, ReflowTempBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov ReflowTempBCD+1, a
            setb ParametersUpdated

        _adjustReflowTemp100b:
        lcall updateDisplayParameters ; update param display
        
    ljmp adjustReflowTemp100 
    
    adjustReflowTemp010:
        Set_Cursor(2,9)
        ifPressedJumpTo(LEFT, adjustReflowTemp100, 1)
        ifPressedJumpTo(RIGHT, adjustReflowTemp001, 1)
        ifNotPressedJumpTo(UP, _adjustReflowTemp010a)
            ; increment 10's of Reflow Temp
            mov a, ReflowTempBCD+0
            add a, #0x10
            da a
            mov ReflowTempBCD+0, a
            setb ParametersUpdated

        _adjustReflowTemp010a:     
        ifNotPressedJumpTo(DOWN, _adjustReflowTemp010b)
            ; decrement 10's of Reflow Temp
            mov a, ReflowTempBCD+0
            add a, #0x90
            da a
            mov ReflowTempBCD+0, a
            setb ParametersUpdated

        _adjustReflowTemp010b:
        lcall updateDisplayParameters ; update param display
        
    ljmp adjustReflowTemp010 
    
    adjustReflowTemp001:
        Set_Cursor(2,10)
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
            setb ParametersUpdated

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
            setb ParametersUpdated

        _adjustReflowTemp001b:
        lcall updateDisplayParameters ; update param display
        
    ljmp adjustReflowTemp001
    
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
            setb ParametersUpdated

        _adjustReflowTime100a:     
        ifNotPressedJumpTo(DOWN, _adjustReflowTime100b)
            ; decrement 100's of Reflow Time
            mov a, ReflowTimeBCD+1
            add a, #0x09
            da a
            anl a, #0x0f
            mov ReflowTimeBCD+1, a
            setb ParametersUpdated

        _adjustReflowTime100b:
        lcall updateDisplayParameters ; update param display
        
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
            setb ParametersUpdated

        _adjustReflowTime010a:     
        ifNotPressedJumpTo(DOWN, _adjustReflowTime010b)
            ; decrement 10's of Reflow Time
            mov a, ReflowTimeBCD+0
            add a, #0x90
            da a
            mov ReflowTimeBCD+0, a
            setb ParametersUpdated

        _adjustReflowTime010b:
        lcall updateDisplayParameters ; update param display
        
    ljmp adjustReflowTime010 
    
adjustParametersEnd:
    ; update display
    ; show cursor
    Set_Cursor(1, 1)
    Send_Constant_String(#Parameter_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Parameter_display_2)

    WriteCommand(#0x0e) ; show cursor, no blink

    setb ParametersUpdated

    adjustReflowTime001:
        Set_Cursor(2,15)
        ifPressedJumpTo(LEFT, adjustReflowTime010, 1)
        ifPressedJumpTo(RIGHT, ready, 2)
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
            setb ParametersUpdated

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
            setb ParametersUpdated

        _adjustReflowTime001b:
        lcall updateDisplayParameters ; update param display
        
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
    mov2Bytes(bcd, SoakTempBCD)
    lcall bcd2hex
    mov2Bytes(SoakTempHex, x)
    ; Update Soak Time Hex
    mov2Bytes(bcd, SoakTimeBCD)
    lcall bcd2hex
    mov2Bytes(SoakTimeHex, x)
    ; Update Reflow Temp Hex
    mov2Bytes(bcd, ReflowTempBCD)
    lcall bcd2hex
    mov2Bytes(ReflowTempHex, x)
    ; Update Reflow Time Hex
    mov2Bytes(bcd, ReflowTimeBCD)
    lcall bcd2hex
    mov2Bytes(ReflowTimeHex, x)

    readyLoop:
        ifPressedJumpTo(LEFT, adjustParametersEnd, 1)
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

    mov WaitCountBCD+0, #0x60 
    mov Counter5s, #0x00
    zero2Bytes(RunTimeBCD)
    zero2Bytes(Counter1000ms)
    setb RunTimeFlag

    ; 100% power
    clr MaintainTargetTemp
    mov PWMDutyCycle, #PWM_PERIOD
    setb WaitFlag
    setb Flag100ms

    RampToSoakLoop:
        ifPressedJumpTo(STARTSTOP, Cancelled, 1) ; Cancel if stop button pressed
        
        lcall ReadTemp
        lcall updateDisplayReflow

        clr EA
        Load_x(0)
        mov2Bytes(x, OvenTemp)
        Load_y(0)
        mov2Bytes(y, SoakTempHex)
        lcall x_gt_y
        jnb mf, soakTempNotReached ; Loop intil OvenTemp = SoakTemp
            setb EA
            ljmp Soak
        soakTempNotReached:
        setb EA

        clr EA
        mov a, WaitCountBCD+0
        jnz noThermalRunaway
            Load_x(0)
            mov2Bytes(x, OvenTemp)
            Load_y(50)
            lcall x_lt_y

            jnb mf, noThermalRunaway
                setb EA
                ljmp Cancelled
        noThermalRunaway:
        setb EA

        lcall soundHandler

    ljmp RampToSoakLoop

Soak:
    ; display Soak message
    Set_Cursor(1, 1)
    Send_Constant_String(#Soak_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Soak_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink

    ; activate PID
    mov2Bytes(TargetTemp, SoakTempHex)
    setb MaintainTargetTemp
    mov2Bytes(WaitCountBCD, SoakTimeBCD)
    zero2Bytes(Counter1000ms)
    setb WaitFlag
    mov PWMDutyCycle, #0x00
    setb Flag100ms

    SoakLoop:
        ifPressedJumpTo(STARTSTOP, Cancelled, 2) ; Cancel if stop button pressed

        lcall ReadTemp
        lcall updateDisplayReflow

        mov a, WaitCountBCD+1
        jnz stillSoaking
        mov a, WaitCountBCD+0
        jnz stillSoaking
            ljmp RampToReflow
        stillSoaking:
        
        lcall soundHandler

    ljmp SoakLoop

RampToReflow:
    ; display Ramp to Reflow message
    Set_Cursor(1, 1)
    Send_Constant_String(#R2Reflow_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#R2Reflow_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink

    ; 100% power till currentTemp = ReflowTempHex
    clr MaintainTargetTemp
    clr WaitFlag
    mov PWMDutyCycle, #PWM_PERIOD
    setb Flag100ms

    RampToReflowLoop:
        ifPressedJumpTo(STARTSTOP, Cancelled, 3) ; Cancel if stop button pressed

        lcall ReadTemp
        lcall updateDisplayReflow

        clr EA
        Load_x(0)
        mov2Bytes(x, OvenTemp)
        Load_y(0)
        mov2Bytes(y, ReflowTempHex)
        lcall x_gt_y
        jnb mf, ReflowTempNotReached ; Loop intil OvenTemp = SoakTemp
            setb EA
            ljmp Reflow
        ReflowTempNotReached:
        setb EA
        
        lcall soundHandler

    ljmp RampToReflowLoop

Reflow:
    ; display Reflow message
    Set_Cursor(1, 1)
    Send_Constant_String(#Reflow_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Reflow_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink

    ; activate PID to maintain reflow temp, handled in the timer2 ISR
    mov2Bytes(TargetTemp, ReflowTempHex)
    setb MaintainTargetTemp
    mov2Bytes(WaitCountBCD, ReflowTimeBCD)
    zero2Bytes(Counter1000ms)
    setb WaitFlag
    mov PWMDutyCycle, #0x00
    setb Flag100ms

    ReflowLoop:
        ifPressedJumpTo(STARTSTOP, Cancelled, 4) ; Cancel if stop button pressed

        lcall ReadTemp
        lcall updateDisplayReflow

        mov a, WaitCountBCD+1
        jnz stillReflowing
        mov a, WaitCountBCD+0
        jnz stillReflowing
            ljmp Cooling
        stillReflowing:

        lcall soundHandler

    ljmp ReflowLoop

Cooling:
    Set_Cursor(1, 1) ; display Cooling message
    Send_Constant_String(#Cooling_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Cooling_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink

    ; turn off oven
    clr MaintainTargetTemp
    mov PWMDutyCycle, #0x00
    setb Flag100ms

    CoolingLoop:
        lcall ReadTemp
        lcall updateDisplayReflow

        clr EA
        Load_y(60)
        Load_x(0)
        mov2Bytes(x, OvenTemp)
        lcall x_lt_y
        jnb mf, ovenStillHot
            setb EA
            ljmp Done
        ovenStillHot:
        setb EA

        lcall soundHandler

    ljmp CoolingLoop

Done:
    Set_Cursor(1, 1) ; display safe to handle message
    Send_Constant_String(#Done_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Done_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink

    ; safe to touch if currentTemp < 60
    clr RunTimeFlag
    clr MaintainTargetTemp
    zero2Bytes(Counter1000ms)
    mov PWMDutyCycle, #0x00
    mov WaitCountBCD+0, #0x15
    setb WaitFlag

    DoneLoop:
        ifPressedJumpTo(STARTSTOP, start, 1) ; Return to the menu if start button pressed
        Set_Cursor(2,11)
        Display_BCD(WaitCountBCD+0) ; display WaitCountBCD
        mov a, WaitCountBCD+0
        cjne a, #0x99, DoneLoop ; wait 15 sec
        clr WaitFlag
        ljmp start
    ljmp DoneLoop

Cancelled:
    Set_Cursor(1, 1) ; display Cancelled message
    Send_Constant_String(#Cancelled_display_1)
    Set_cursor(2, 1)
    Send_Constant_String(#Cancelled_display_2)
    WriteCommand(#0x0c) ; hide cursor, no blink

    clr RunTimeFlag
    clr MaintainTargetTemp
    zero2Bytes(Counter1000ms)
    mov PWMDutyCycle, #0x00
    mov WaitCountBCD+0, #0x15
    setb WaitFlag

    CancelledLoop:
        ifPressedJumpTo(STARTSTOP, start, 2) ; Return to the menu if start button pressed
        Set_Cursor(2,11)
        Display_BCD(WaitCountBCD) ; display WaitCountBCD
        mov a, WaitCountBCD
        cjne a, #0x99, CancelledLoop ; wait 15 sec
            clr WaitFlag
            ljmp start
    ljmp CancelledLoop

debug:
    lcall setup
    setb MaintainTargetTemp
    setb RunTimeFlag
    debugLoop:

        lcall ReadTemp

        jb Flag100ms, L100ms
            ljmp no100ms
        L100ms:
            clr Flag100ms
            clr EA
            readADCChannel(7)
            Load_y(200)
            lcall mul32
            Load_y(1023)
            lcall div32
            lcall hex2bcd
            mov2Bytes(TargetTemp, x)
            Set_Cursor(1,1)
            LCDSend3BCD(bcd)
            Set_Cursor(2,1)
            LCDSend3BCD(OvenTempBCD)
            setb EA
        no100ms:

        lcall soundHandler

    ljmp debugLoop

END