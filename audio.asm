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

$NOLIST
$MODLP51RC2
$LIST
org 0x0000 ; Reset vector
    ljmp MainProgram
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
	
SYSCLK         EQU 22118400  ; Microcontroller system clock frequency in Hz
TIMER1_RATE    EQU 22050     ; 22050Hz is the sampling rate of the wav file we are playing
TIMER1_RELOAD  EQU 0x10000-(SYSCLK/TIMER1_RATE)
BAUDRATE       EQU 115200
BRG_VAL        EQU (0x100-(SYSCLK/(16*BAUDRATE)))

SPEAKER  EQU P2.4 ; Used with a MOSFET to turn off speaker when not in use

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
	x: ds 4
	y: ds 4
	temperature: ds 2
    nextPlay: ds 1
    BCD: ds 5

	w:   ds 3 ; 24-bit play counter.  Decremented in Timer 1 ISR.

bseg
	
	
	mf: dbit 1 ; Message flag.  Set to 1 when a message is received.
    CANPLAY: dbit 1 ; flag to indicate audio is playable
	$NOLIST
	$include(math32.inc)
	$LIST
	

	
cseg

SPEAK MAC
	clr a
	Load_x(%0)
	Load_y(0x2b11)
	lcall mul32
	clr TR1 ; Stop Timer 1 ISR from playing previous request
	setb FLASH_CE
	    clr SPEAKER ; Turn off speaker.
    
	    clr FLASH_CE ; Enable SPI Flash
	    mov a, #READ_BYTES
	    lcall Send_SPI
	    ; Set the initial position in memory where to start playing
	    mov a, x+2
	    lcall Send_SPI
	    mov a, x+1
	    lcall Send_SPI
	    mov a, x+0
	    ; mov a, #0x2b
	    lcall Send_SPI
	    ; mov a, x+0 ; Request first byte to send to DAC
	    mov a, #0x00 ; Request first byte to send to DAC
	    lcall Send_SPI
    
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
	setb SPEAKER
	lcall Send_SPI ; Read the next byte from the SPI Flash...
	mov P0, a ; WARNING: Remove this if not using an external DAC to use the pins of P0 as GPIO
	add a, #0x80
	mov DADH, a ; Output to DAC. DAC output is pin P2.3
	orl DADC, #0b_0100_0000 ; Start DAC by setting GO/BSY=1
	sjmp Timer1_ISR_Done

    stop_playing:
        setb CANPLAY
	    clr TR1 ; Stop timer 1
	    setb FLASH_CE  ; Disable SPI Flash
	    clr SPEAKER ; Turn off speaker.  Removes hissing noise when not playing sound.
	    mov DADH, #0x80 ; middle of range
	    orl DADC, #0b_0100_0000 ; Start DAC by setting GO/BSY=1

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

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
MainProgram:
    setb CANPLAY
    clr a
    mov nextPlay, a
    mov SP, #0x7f ; Setup stack pointer to the start of indirectly accessable data memory minus one
    lcall Init_all ; Initialize the hardware 
    mov temperature+1, #9
    mov temperature+0, #0b_0110_0011
    sjmp forever_loop
    
    ;intermediate jump ignore
    end_audio_short:
    ljmp end_audio
    ;
    
    forever_loop:
    mov a, nextPlay
    cjne a, #3, end_audio_short
    jnb CANPLAY, end_audio_short
    
    
    clr CANPLAY
    mov a, nextPlay
    cjne a, #0, NOTHUND
    	SPEAK(low(temperature+1))
    	mov nextPlay, #1
    	ljmp end_audio
    NOTHUND:
    	cjne a, #1, ONES
    	mov nextPlay, #2
    	SPEAK(high(temperature+0))
    	sjmp end_audio
    ONES:
    	mov nextPlay, #3
    	SPEAK(low(temperature+0))
    end_audio:
	    ; jb RI, serial_get
	    jb P4.5, end_all ; Check if push-button pressed
	    jnb P4.5, $ ; Wait for push-button release
        clr a
        mov nextPlay, a
	end_all:
	ljmp forever_loop

END