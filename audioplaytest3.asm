; AT89LP51RC2_Receiver.asm:  This program implements a simple serial port
; communication protocol to program, verify, and read an SPI flash memory.  Since
; the program was developed to store wav audio files, it also allows 
; for the playback of said audio.  It is assumed that the wav sampling rate is
; 22050Hz, 8-bit, mono.
;
; Connections:
; 
; AT89LP51RD2   SPI_FLASH
; (20) P2.0     Pin 6 (SPI_CLK)
; (21) P2.1     Pin 2 (MISO)
; (24) P2.4     Pin 5 (MOSI)
; (25) P2.5     Pin 1 (CS/)
; GND           Pin 4
; 3.3V          Pins 3, 7, 8
;
; The DAC output (P2.3, pin 23) should be connected to the
; input of power amplifier (LM386 or similar)
;
; WARNING: Pins P2.2 and P2.3 are the DAC outputs and can not be used for anything else

org 0000H
   ljmp reset
; org 0x0000 ; Reset vector
;     ljmp MainProgram
org 0x0003 ; External interrupt 0 vector (not used in this code)
	reti
org 0x000B ; Timer/Counter 0 overflow interrupt vector (not used in this code)
	reti
org 0x0013 ; External interrupt 1 vector. (not used in this code)
	reti
org 0x001B ; Timer/Counter 1 overflow interrupt vector. Used in this code to replay the wave file.
	ljmp Timer1_ISR
org 0x0023 ; Serial port receive/transmit interrupt vector (not used in this code)
	reti
org 0x005b ; Timer 2 interrupt vector. (not used in this code)
	reti
org 0x0063 ; ADC interrupt (vector must be present if debugger is used)
	reti

$NOLIST
$MODLP51RC2
$include(LCD_4bit.inc)
$include(math32.inc)
$LIST

SYSCLK         EQU 22118400  ; Microcontroller system clock frequency in Hz
TIMER1_RATE    EQU 22050     ; 22050Hz is the sampling rate of the wav file we are playing
TIMER1_RELOAD  EQU 0x10000-(SYSCLK/TIMER1_RATE)
BAUDRATE       EQU 115200
BRG_VAL        EQU (0x100-(SYSCLK/(16*BAUDRATE)))

; LCD pin mapping
LCD_RS equ P1.5
; LCD_RW equ Px.x ; Always grounded
LCD_E  equ P1.4
LCD_D4 equ P1.3
LCD_D5 equ P1.2
LCD_D6 equ P1.1
LCD_D7 equ P1.0

; SPI pin mapping
ADC_CS   equ P3.6
; SPI_MOSI equ P2.5
; SPI_MISO equ P2.6
; SPI_SCLK equ P2.7
SPI_MOSI equ P3.3
SPI_MISO equ P3.4
SPI_SCLK equ P3.2

SPEAKER  EQU P2.4 ; Used with a MOSFET to turn off speaker when not in usebcd

5s_BTN EQU P ;Used for simulating 5s timer

; The pins used for SPI
FLASH_CE  EQU  P3.5
MY_MOSI   EQU  P3.3 
MY_MISO   EQU  P3.4
MY_SCLK   EQU  P3.2 

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

; Variables used in the program:
dseg at 30H
    x:   ds 4
    y:   ds 4
    bcd: ds 5
	w:   ds 3 ; 24-bit play counter.  Decremented in Timer 1 ISR.
    FlashReadAddr: ds 3
    ; temp variables
ColdTemp: ds 4
HotTemp: ds 4

BSEG
    mf: dbit 1
    play: dbit 1
    5s_flag: dbit 1
    100s_flag: dbit 1
    10s_flag: dbit 1
    1s_flag: dbit 1
    degrees: dbit 1
    is_playing: dbit 1

; Interrupt vectors:
cseg

;-------------------------------------;
; SPEAK Macro for playing a certain   ;
; number's audio                      ;
;-------------------------------------;
SPEAK MAC
	clr a
	Load_x(%0)
	Load_y(0x2b11)
	lcall mul32

	clr TR1 ; Stop Timer 1 ISR from playing previous request
	clr SPEAKER ; Turn off speaker.
    ; set starting address
	mov FlashReadAddr+2, x+2
	mov FlashReadAddr+1, x+1
	mov FlashReadAddr+0, x+0
	; How many bytes to play? All of them!  Asume 4Mbytes memory: 0x3fffff
	mov w+2, #0x00
    mov w+1, #0x2b
    mov w+0, #0x11

    setb SPEAKER ; Turn on speaker.
   	setb TR1 ; Start playback by enabling Timer 1
	ENDMAC

;-------------------------------------;
; ISR for Timer 1.  Used to playback  ;
; the WAV file stored in the SPI      ;
; flash memory.                       ;
;-------------------------------------;
Timer1_ISR:
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
    clr FLASH_CE ; Enable SPI Flash

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
    mov a, #0x00
	lcall Send_SPI

    setb FLASH_CE
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
    mov a, FlashReadAddr+2
    incrementAddressDone:
	sjmp Timer1_ISR_Done

    stop_playing:
	    clr TR1 ; Stop timer 1
	    setb FLASH_CE  ; Disable SPI Flash
	    clr SPEAKER ; Turn off speaker.  Removes hissing noise when not playing sound.
	    mov DADH, #0x80 ; middle of range
	    orl DADC, #0b_0100_0000 ; Start DAC by setting GO/BSY=1
        clr is_playing

    Timer1_ISR_Done:	
	pop psw
	pop acc
reti

;---------------------------------;
; Sends AND receives a byte via   ;
; SPI.                            ;
;---------------------------------;
Send_SPI:
	SPIBIT MAC
	    ; Send/Receive bit %0
		rlc a
		mov MY_MOSI, c
		setb MY_SCLK
		mov c, MY_MISO
		clr MY_SCLK
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

;---------------------------------;
; SPI flash 'write enable'        ;
; instruction.                    ;
;---------------------------------;
Enable_Write:
	clr FLASH_CE
	mov a, #WRITE_ENABLE
	lcall Send_SPI
	setb FLASH_CE
ret
	
Init_all:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	
	; Enable serial communication and set up baud rate
	orl	PCON,#0x80
	mov	SCON,#0x52
	mov	BDRCON,#0x00
	mov	BRL,#BRG_VAL
	mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
	
	; Configure SPI pins and turn off speaker
	anl P2M0, #0b_1100_1110
	orl P2M1, #0b_0011_0001
	setb MY_MISO  ; Configured as input
	setb FLASH_CE ; CS=1 for SPI flash memory
	clr MY_SCLK   ; Rest state of SCLK=0
	clr SPEAKER   ; Turn off speaker.
	
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

	; Configure the DAC.  The DAC output we are using is P2.3, but P2.2 is also reserved.
	mov DADI, #0b_1010_0000 ; ACON=1
	mov DADC, #0b_0011_1010 ; Enabled, DAC mode, Left adjusted, CLK/4
	mov DADH, #0x80 ; Middle of scale
	mov DADL, #0
	orl DADC, #0b_0100_0000 ; Start DAC by GO/BSY=1
    check_DAC_init:
	mov a, DADC
	jb acc.6, check_DAC_init ; Wait for DAC to finish
	
	setb EA ; Enable interrupts

	; Not necesary if using internal DAC.
	; If using an R-2R DAC connected to P0, configure the pins of P0
	; (An external R-2R produces much better quality sound)
	mov P0M0, #0b_0000_0000
	mov P0M1, #0b_1111_1111
	
ret

InitSPI:
    setb ADC_CS
    setb SPI_MISO
    clr SPI_SCLK
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

;                     1234567890123456
Initial_Message1: db 'Ch1:    mV     C', 0
Initial_Message2: db 'Ch6:    mV      ', 0

reset:
    mov SP, #7FH ; Set the stack pointer to the begining of idata

    mov P0M0, #0x00
    mov P0M1, #0x00
    mov P1M0, #0x00
    mov P1M1, #0x00
    mov P2M0, #0x00
    mov P2M1, #0x00
    mov P3M0, #0x00
    mov P3M1, #0x00
    mov P4M0, #0x00
    mov P4M1, #0x00

    lcall Init_all ; Initialize the hardware  
    lcall InitSPI
    lcall LCD_4BIT

    Set_Cursor(1, 1)
    mov DPTR, #Initial_Message1
    lcall LCDSendString
    Set_Cursor(2, 1)
    mov DPTR, #Initial_Message2
    lcall LCDSendString
    
    loop:
        
        5s_button_sim:
        jb 5s_BTN, audio_func
        Wait_Milli_Seconds(#5)
        jb 5s_BTN, audio_func
        jnb 5s_BTN, $
        setb 5s_flag

    audio_func:

        ;check whether 5s flag has been set
        jnb 5s_flag, check_play_status
        setb play
        setb 100s_flag
        setb 10s_flag
        setb 1s_flag
        setb degrees
        clr 5s_flag

    check_play_status:

        jnb play, end_of_audio_func

        check_is_playing:
            jb is_playing, end_of_audio_func

            check_100s_flag:
                jnb 100s_flag, check_10s_flag
                clr 100s_flag
                SPEAK(5) ;for testing, replace with temperature value later
            
            check_10s_flag:
                jnb 10s_flag, check_1s_flag
                clr 10s_flag
                SPEAK(4) ;for testing, replace with temperature value later
            
            check_1s_flag:
                jnb 1s_flag, check_degrees_flag
                clr 1s_flag
                SPEAK(1) ;for testing, replace with temperature value later
            
            check_degrees_flag:
                jnb degrees, clear_play_status
                clr degrees
                SPEAK(degrees_value) ;replace with degrees value in memory
                ljmp end_of_audio_func
            
            clear_play_status:
                clr play

    end_of_audio_func:
    ;will have return statement here in actual implementation

    ljmp loop 

END