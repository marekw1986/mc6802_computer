                .CR         6800            ;Select the 6800 Cross-Overlay
                .TF         test.hex,INT    ;Send code in Intel Hex format to             

CTRL            .EQ         $BC00          ;address of 373 output port
ACIA_RXD        .EQ         $A000          ;ACIA receive data port
ACIA_TXD        .EQ         $A000          ;ACIA transmit data port
ACIA_STS        .EQ         $A001          ;ACIA status port
ACIA_RES        .EQ         $A001          ;ACIA reset port
ACIA_CMD        .EQ         $A002          ;ACIA command port
ACIA_CTL        .EQ         $A003          ;ACIA control port
KBD_DATA        .EQ         $A100
KBD_STATUS      .EQ         $A101
KBD_CMD         .EQ         $A101

; VDP REGS
VDP_DATA 		.EQ			$A400
VDP_MODE 		.EQ			VDP_DATA + 1

;ZERO PAGE
KBDDATA         .EQ         $00
KBDOLD          .EQ         $01
KBDNEW          .EQ         $02
KBDKRFL         .EQ         $03
KBDSFFL         .EQ         $04
BLKLEN			.EQ			$05				;AND $06!!!! THIS IS WORD

    .OR $C000                    ;EPROM starts here
INIT:    
	LDS #$7FFF                   ;set stack pointer
    JSR ACIAINIT
    JSR KBDINIT                  ;Initialize keyboard controller

LOOP:
    LDAA #$FF                    ;set 373 port
    STAA CTRL
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    LDAA #$00                    ;set 373 port
    STAA CTRL
    
    JSR KBDRCV                   ;Try to load new scancode, it calls KBD2ASCII internally
    BEQ LOOP                     ;Go to the beginning of the loop
    JSR ACIAout                  ;Otherwise sent received character via UART
    
	JMP LOOP     
        
ACIAINIT:
    STAA ACIA_RES                ;initialise 6551 ACIA  
    LDAA #$0B
    STAA ACIA_CMD 
    LDAA #$1E        	         ;8-N-1, 9600 baud
    STAA ACIA_CTL       	     ;set control register
    RTS
    
ACIAout:
	PSHA					     ;save A
ACIAout_wait:
	LDAA	ACIA_STS		     ;get status byte
	ANDA #$10				     ;mask transmit buffer status flag
	BEQ ACIAout_wait		     ;loop if tx buffer full
	PULA         			     ;restore A
	STAA ACIA_TXD       	     ;save byte to ACIA data port
	RTS        
        
KBDINIT:                         
	JSR KBDWAITINBUF       		 ;1. Disable devices
	LDAA #$AD                    ;Send 0xAD command to the PS/2 controller
	STAA KBD_CMD                 ;2. Flush The Output Buffer
	LDAA KBD_STATUS
	ANDA #$01                    ;Check if there is data to flush
	BEQ KBDCRTLSET               ;No? Next step then
	LDAA KBD_DATA                ;Yes? Get the data byte        
KBDCRTLSET:
	JSR KBDWAITINBUF	         ;3. Set the Controller Configuration Byte		 
	LDAA #$60                    ;Send 0x60 command to the PS/2 controller
	STAA KBD_CMD
	JSR KBDWAITINBUF			 ;Send actual configuration byte
	LDAA #$08					 ;Interrupts disabled, system flag set, first port clock enabled
	STAA KBD_DATA				 ;second port clock disabled, first port translation disabled
	JSR KBDWAITINBUF             ;4. Controller self test
	LDAA #$AA                	 ;Send 0xAA command to the PS/2 controller
	STAA KBD_CMD
	JSR KBDWAITOUTBUF            ;Wait for response
	LDAA KBD_DATA                ;Get byte
	CMPA #$55                    ;Is it 0x55?
	BEQ KBDIFTEST
	LDAB #$01					 ;Return result code if not
	RTS                          ;No? Return then
KBDIFTEST:                       
	JSR KBDWAITINBUF             
	LDAA #$AB                  	 ;5. Interface test
	STAA KBD_CMD                 ;Send 0xAB command
	JSR KBDWAITOUTBUF            ;Wait for response
	LDAA KBD_DATA                ;Get byte
	CMPA #$01					 ;Check if it is CLK stuck low error
	BNE KBDIFTEST_CLKSH            
	LDAB #$02                    ;Return result code if it is
	RTS
KBDIFTEST_CLKSH:	                         
	CMPA #$02					 ;Check if it is CLK stuck high error
	BNE KBDIFTEST_DATSL         
	LDAB #$03                    ;Return result code if it is
	RTS
KBDIFTEST_DATSL:	                         
	CMPA #$03					 ;Check if it is KBD DATA stuck low error
	BNE KBDIFTEST_DATSH                     
	LDAB #$04                    ;Return result code if it is
	RTS
KBDIFTEST_DATSH:	
	CMPA #$04                    ;Check if it is KBD DATA stuck high error
	BNE KBDIFTEST_OK
	LDAB #$05                    ;Return result code if it is
	RTS
KBDIFTEST_OK:	                         
	CMPA #$00                    ;Is it 0x00? Did it pass the test? (this is needed!)
	BEQ KBDENDEVS
	LDAB #$06					 ;Return result code if not
	RTS                          ;No? Return then
KBDENDEVS:	
	JSR KBDWAITINBUF             ;6. Enable Devices
	LDAA #$AE                 	 ;Send 0xAE command
	STAA KBD_CMD                 ;7. Reset Device
	JSR KBDWAITINBUF           	 ;Wait untill ready to send
	LDAA #$FF                 	 ;Send 0xFF to device
	STAA KBD_DATA                ;Send it to device, not the controller
	LDAB 130                  	 ;Setup DELAY routine
	JSR DELAY                    ;This is required to avoid freeze
	JSR KBDWAITOUTBUF            ;Wait for response
	LDAA KBD_DATA                ;Get byte
	CMPA #$FA                    ;Is it 0xFA? 0xFC means failure. No response means no device present.
	BEQ KBD_OK
	LDAB #$07					 ;Return result code if not
	RTS                          ;No? Return then
	JSR KBDWAITINBUF	         ;3. Set the Controller Configuration Byte		 
	LDAA #$60                    ;Send 0x60 command to the PS/2 controller
	STAA KBD_CMD
	JSR KBDWAITINBUF			 ;Send actual configuration byte
	LDAA #$68					 ;Interrupts disabled, system flag reset, first port clock enabled
	STAA KBD_DATA				 ;second port clock disabled, first port translation enabled    
KBD_OK:
	LDAB #$00					 ;Return result code
	RTS
        
KBDWAITINBUF:
	LDAA KBD_STATUS
	ANDA #$02
	BNE KBDWAITINBUF
	RTS
		
KBDWAITOUTBUF:
	LDAA KBD_STATUS
	ANDA #$01
	BEQ KBDWAITOUTBUF
	RTS
    
KBDRCV:
    LDAA KBD_STATUS             ;Load status register of keyboard controller
    ANDA #$01                   ;Check if there is any data to read
    BNE KBDRCV_LD               ;If there is, read actua; data
    RTS
KBDRCV_LD:
    LDAA KBD_DATA
    STAA KBDDATA
    JSR KBD2ASCII
    RTS

KBD2ASCII:
    LDAA KBDDATA				;Load latest received PS/2 scancode
    BEQ KBD2A_CLRDATA_RETURN    ;Return if code = 0;
    TAB							;Transfer A to B to perform AND on it
    ANDB #$80					;Is MSB set?
    BEQ KBD2A_CHKSFT			;If not, go to the next stage
    ANDA #$7F					;Mask MSB off
    TAB							;Save currnt code in B
    LDAA #$01					;Set key release flag
    STAA KBDKRFL
    TBA							;Restre current code form X to A
KBD2A_CHKSFT:
    CMPA #$2A					;Check if it is (left) shift code
    BEQ KBD2A_CHKKRSETSF		;If not, go to the next stage
    CMPA #$36					;Check if it is (right) shift code
    BEQ KBD2A_CHKKRSETSF		;If not, go to the next stage
KBD2A_SVNEWDATA:
    TAB					        ;Save current code in B
    LDAA KBDNEW
    STAA KBDOLD					;Old data = new data
    TBA
    STAA KBDNEW					;New data = received code
    LDAA KBDKRFL
    CMPA #$01					;Check if key release flag is set
    BNE KBD2A_CHKSHFFLSET		;If not, go to the next stage
    LDAA KBDOLD					;Load old data to acumulator
    CMPA KBDNEW                 ;Compare it with new data
    BEQ KBD2A_CLRKRFL			;If yes, clear release flag and return
    NOP							;If not, handle error here.
    NOP							;These are just a placeholders
KBD2A_CLRKRFL:
    LDAA #$00
    STAA KBDKRFL
    JMP KBD2A_CLRDATA_RETURN
KBD2A_CHKSHFFLSET:
    LDAB #$01					;Just assume we are looking LC table
    LDAA KBDSFFL				;Check shift flag
    BEQ KBD2A_LOOKUP			;Just search in LC table
    LDAB #$02					;We are looking in UC table if shift flag is set
KBD2A_LOOKUP:		
    JSR KBDSCANTABLE			;Call scantable searching subroutine
    BEQ KBD2A_CLRDATA_RETURN	;If yes, clear data and return
    TAB					        ;Else clear KBDDATA and return
    LDAA #$00					;Passing ASCII character in A
    STAA KBDDATA
    TBA
    RTS
KBD2A_CHKKRSETSF:
    LDAA KBDKRFL
    CMPA #$01					;Check if key release flag is set
    BEQ KBD2A_CLRFLDATA_RETURN	;If yes clear flags (and data?) and return
    LDAA #$01					;If not, set shift flag
    STAA KBDSFFL
    JMP KBD2A_CLRDATA_RETURN    ;Clear KBDDATA and return    
KBD2A_CLRFLDATA_RETURN:
    LDAA #$00
    STAA KBDSFFL
    STAA KBDKRFL
KBD2A_CLRDATA_RETURN:
    LDAA #$00
    STAA KBDDATA		
    RTS

; Shift in B	
KBDSCANTABLE:
	LDX #PS2_SCANCODES  		;Table address
KBDSCANTABLE_LOOP:
	LDAA $00,X			        ;Load next scancode from table to A
    BEQ KBDSCANTABLE_END        ;If it is zero, end
	CMPA KBDNEW			        ;Compare A with current receivedscancode
    BEQ KBDSCANTABLE_FOUND1     ;If they are equal, return ASCII code
    INX                         ;Increment index pointer three time
    INX                         ;To go to the next scancode
    INX
    JMP KBDSCANTABLE_LOOP       ;Continue until you reach end of the table
KBDSCANTABLE_FOUND1:
    CMPB #$01                   
    BNE KBDSCANTABLE_FOUND2
    LDAA $01, X
    RTS
KBDSCANTABLE_FOUND2:
    CMPB #$02
    BNE KBDSCANTABLE_END
    LDAA $02, X
KBDSCANTABLE_END:    
    RTS
        
DELAY:
	LDAA #$FF
PETLA_DEL_WEWN:
	NOP
	NOP
	DECA
	BNE PETLA_DEL_WEWN                          
	DECB
	BNE DELAY
	RTS
	
;VDP routines
VDPINIT
    LDAA #$00                      ;REG0 (TEXT MODE, NO EXTERNAL VIDEO)
    STAA VDP_MODE
    LDAA #$80                      ;SELECT REG0
    STAA VDP_MODE
    LDAA #$D0                     ;REG1 (16K, ENABLE DISP, DISABLE INT)
    STAA VDP_MODE
    LDAA #$81                      ;SELECT REG1
    STAA VDP_MODE
    LDAA #$02                      ;REG2 (ADDRESS OF NAME TABLE IN VRAM = 0x0800)
    STAA VDP_MODE
    LDAA #$82                      ;SELECT REG2
    STAA VDP_MODE
    LDAA #$00                      ;REG4 (ADDRESS OF PATTERN TABLE IN VRAM = 0x0000)
    STAA VDP_MODE
    LDAA #$84                      ;SELECT REG4
    STAA VDP_MODE
    LDAA #$20                      ;REG5 (ADDRESS OF SPRITE ATTRIBUTE TABLE IN VRAM = 0x1000)
    STAA VDP_MODE
    LDAA #$85                      ;SELECT REG5
    STAA VDP_MODE
    LDAA #$00                      ;REG6 (ADDRESS OF SPRITE PATTERN TABLE IN VRAM = 0x0000)
    STAA VDP_MODE
    LDAA #$86                      ;SELECT REG6
    STAA VDP_MODE
    LDAA #$F5                     ;REG7 (WHITE TEXT ON LIGHT BLUE BACKGROUND)
    STAA VDP_MODE
    LDAA #$87                      ;SELECT REG7
    STAA VDP_MODE
    ;INITiALIZE VRAM
;   0000H + (32*8)
    LDAA #$00						;<(32*8)
    PSHA
    LDAA #$01						;>(32*8)
    PSHA
    LDX #CHARLEN
    STX BLKLEN
    LDX #CHARS
    JSR VDPWVRAM
    RTS

;RAM ADDRESS IN X, VRAM PASSED BY STACK (LSB FIRST), DATA LENGTH BLKLEN               
VDPWVRAM
    PULA
    STAA VDP_MODE
    PULA
    ORAA #$80
    STAA VDP_MODE
VDPWVRAML
    LDAA $00, X
    STAA VDP_DATA
    INX
	DEC BLKLEN+1
	BNE VDPWVRAML
	DEC BLKLEN
	BNE VDPWVRAML
    RTS
        
;RAM ADDRES IN BLKIND, VRAM PASSED BY STACK (LSB FIRST), DATA LENGTH BLKLEN
VDPRVRAM
    PULA
    STAA VDP_MODE
    PULA
    STAA VDP_MODE
VDPRVRAML
    LDAA $00, X
    STAA VDP_DATA
    INX   
	DEC BLKLEN+1
	BNE VDPRVRAML
	DEC BLKLEN
	BNE VDPRVRAML
    RTS
    

ZEROVRAM
	LDAA #$00
	STAA VDP_MODE
	NOP
	NOP
	LDAA #$40
	STAA VDP_MODE
	NOP
	NOP
	LDX #$4000
ZEROVRAML
	LDAA #$00
	STAA VDP_DATA
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	DEX
	BNE ZEROVRAML
	RTS
	
CHARS:
	;This is the IBM-PC Character ROM +$20
	.DB $00,$00,$00,$00,$00,$00,$00,$00 ;SP
	.DB $30,$78,$78,$30,$30,$00,$30,$00 ;!
	.DB $6C,$6C,$6C,$00,$00,$00,$00,$00 ;"
	.DB $6C,$6C,$FE,$6C,$FE,$6C,$6C,$00 ;#
	.DB $30,$7C,$C0,$78,$0C,$F8,$30,$00 ;$
	.DB $00,$C6,$CC,$18,$30,$66,$C6,$00 ;%
	.DB $38,$6C,$38,$76,$DC,$CC,$76,$00 ;&
	.DB $60,$60,$C0,$00,$00,$00,$00,$00 ;'
	.DB $18,$30,$60,$60,$60,$30,$18,$00 ;(
	.DB $60,$30,$18,$18,$18,$30,$60,$00 ;)
	.DB $00,$66,$3C,$FF,$3C,$66,$00,$00 ;*
	.DB $00,$30,$30,$FC,$30,$30,$00,$00 ;+
	.DB $00,$00,$00,$00,$00,$30,$30,$60 ;'
	.DB $00,$00,$00,$FC,$00,$00,$00,$00 ;-
	.DB $00,$00,$00,$00,$00,$30,$30,$00 ;.
	.DB $06,$0C,$18,$30,$60,$C0,$80,$00 ;/
	.DB $7C,$C6,$CE,$DE,$F6,$E6,$7C,$00 ;30, 0
	.DB $30,$70,$30,$30,$30,$30,$FC,$00 ;31, 1
	.DB $78,$CC,$0C,$38,$60,$CC,$FC,$00 ;32, 2
	.DB $78,$CC,$0C,$38,$0C,$CC,$78,$00 ;33, 3
	.DB $1C,$3C,$6C,$CC,$FE,$0C,$1E,$00 ;34, 4
	.DB $FC,$C0,$F8,$0C,$0C,$CC,$78,$00 ;35, 5
	.DB $38,$60,$C0,$F8,$CC,$CC,$78,$00 ;36, 6
	.DB $FC,$CC,$0C,$18,$30,$30,$30,$00 ;37, 7
	.DB $78,$CC,$CC,$78,$CC,$CC,$78,$00 ;38, 8
	.DB $78,$CC,$CC,$7C,$0C,$18,$70,$00 ;39, 9
	.DB $00,$30,$30,$00,$00,$30,$30,$00 ;:
	.DB $00,$30,$30,$00,$00,$30,$30,$60 ;;
	.DB $18,$30,$60,$C0,$60,$30,$18,$00 ;<
	.DB $00,$00,$FC,$00,$00,$FC,$00,$00 ;=
	.DB $60,$30,$18,$0C,$18,$30,$60,$00 ;>
	.DB $78,$CC,$0C,$18,$30,$00,$30,$00 ;?
	.DB $7C,$C6,$DE,$DE,$DE,$C0,$78,$00 ;@
	.DB $30,$78,$CC,$CC,$FC,$CC,$CC,$00 ;A
	.DB $FC,$66,$66,$7C,$66,$66,$FC,$00 ;B
	.DB $3C,$66,$C0,$C0,$C0,$66,$3C,$00 ;C
	.DB $F8,$6C,$66,$66,$66,$6C,$F8,$00 ;D
	.DB $FE,$62,$68,$78,$68,$62,$FE,$00 ;E
	.DB $FE,$62,$68,$78,$68,$60,$F0,$00 ;F
	.DB $3C,$66,$C0,$C0,$CE,$66,$3E,$00 ;G
	.DB $CC,$CC,$CC,$FC,$CC,$CC,$CC,$00 ;H
	.DB $78,$30,$30,$30,$30,$30,$78,$00 ;I
	.DB $1E,$0C,$0C,$0C,$CC,$CC,$78,$00 ;J
	.DB $E6,$66,$6C,$78,$6C,$66,$E6,$00 ;K
									  
	.DB $F0,$60,$60,$60,$62,$66,$FE,$00 ;L
	.DB $C6,$EE,$FE,$FE,$D6,$C6,$C6,$00 ;M
	.DB $C6,$E6,$F6,$DE,$CE,$C6,$C6,$00 ;N
	.DB $38,$6C,$C6,$C6,$C6,$6C,$38,$00 ;O
	.DB $FC,$66,$66,$7C,$60,$60,$F0,$00 ;P
	.DB $78,$CC,$CC,$CC,$DC,$78,$1C,$00 ;Q
	.DB $FC,$66,$66,$7C,$6C,$66,$E6,$00 ;R
	.DB $78,$CC,$E0,$70,$1C,$CC,$78,$00 ;S
	.DB $FC,$B4,$30,$30,$30,$30,$78,$00 ;T
	.DB $CC,$CC,$CC,$CC,$CC,$CC,$FC,$00 ;U
	.DB $CC,$CC,$CC,$CC,$CC,$78,$30,$00 ;V
	.DB $C6,$C6,$C6,$D6,$FE,$EE,$C6,$00 ;W
	.DB $C6,$C6,$6C,$38,$38,$6C,$C6,$00 ;X
	.DB $CC,$CC,$CC,$78,$30,$30,$78,$00 ;Y
	.DB $FE,$C6,$8C,$18,$32,$66,$FE,$00 ;Z
	.DB $78,$60,$60,$60,$60,$60,$78,$00 ;[
	.DB $C0,$60,$30,$18,$0C,$06,$02,$00
	;                                 
	.DB $78,$18,$18,$18,$18,$18,$78,$00 ;]
	.DB $10,$38,$6C,$C6,$00,$00,$00,$00 ;^
	.DB $00,$00,$00,$00,$00,$00,$00,$FF ;_
	.DB $30,$30,$18,$00,$00,$00,$00,$00 ;'
	.DB $00,$00,$78,$0C,$7C,$CC,$76,$00 ;a
	.DB $E0,$60,$60,$7C,$66,$66,$DC,$00 ;b
	.DB $00,$00,$78,$CC,$C0,$CC,$78,$00 ;c
	.DB $1C,$0C,$0C,$7C,$CC,$CC,$76,$00 ;d
	.DB $00,$00,$78,$CC,$FC,$C0,$78,$00 ;e
	.DB $38,$6C,$60,$F0,$60,$60,$F0,$00 ;f
	.DB $00,$00,$76,$CC,$CC,$7C,$0C,$F8 ;g
	.DB $E0,$60,$6C,$76,$66,$66,$E6,$00 ;h
	.DB $30,$00,$70,$30,$30,$30,$78,$00 ;i
	.DB $0C,$00,$0C,$0C,$0C,$CC,$CC,$78 ;j
	.DB $E0,$60,$66,$6C,$78,$6C,$E6,$00 ;k
	.DB $70,$30,$30,$30,$30,$30,$78,$00 ;l
	.DB $00,$00,$CC,$FE,$FE,$D6,$C6,$00 ;m
	.DB $00,$00,$F8,$CC,$CC,$CC,$CC,$00 ;n
	.DB $00,$00,$78,$CC,$CC,$CC,$78,$00 ;o
	.DB $00,$00,$DC,$66,$66,$7C,$60,$F0 ;p
	.DB $00,$00,$76,$CC,$CC,$7C,$0C,$1E ;q
	.DB $00,$00,$DC,$76,$66,$60,$F0,$00 ;r
	.DB $00,$00,$7C,$C0,$78,$0C,$F8,$00 ;s
	.DB $10,$30,$7C,$30,$30,$34,$18,$00 ;t
	.DB $00,$00,$CC,$CC,$CC,$CC,$76,$00 ;u
	.DB $00,$00,$CC,$CC,$CC,$78,$30,$00 ;v
	.DB $00,$00,$C6,$D6,$FE,$FE,$6C,$00 ;w
	.DB $00,$00,$C6,$6C,$38,$6C,$C6,$00 ;x
	.DB $00,$00,$CC,$CC,$CC,$7C,$0C,$F8 ;y
	.DB $00,$00,$FC,$98,$30,$64,$FC,$00 ;z
	.DB $1C,$30,$30,$E0,$30,$30,$1C,$00 ;{
	.DB $18,$18,$18,$00,$18,$18,$18,$00 ;|
	.DB $E0,$30,$30,$1C,$30,$30,$E0,$00 ;}
	.DB $76,$DC,$00,$00,$00,$00,$00,$00 ;~
	.DB $00,$10,$38,$6C,$C6,$C6,$FE,$00 ;DEL
	;
CHARS_END:

CHARLEN			.EQ			CHARS_END-CHARS    			
	
;Set 1
PS2_SCANCODES:
	.DB $29, '`', '~'
	.DB $01, $03, $03				;Esc = Ctrl+C	
	.DB $02, '1', '!'
	.DB $03, '2', '@'
	.DB $04, '3', '#'
	.DB $05, '4', '$'
	.DB $06, '5', '%'
	.DB $07, '6', '^'
	.DB $08, '7', '&'
	.DB $09, '8', '*'
	.DB $0A, '9', '('
	.DB $0B, '0', ')'
	.DB $0C, '-', '_'
	.DB $0D, '=', '+'
	.DB $0E, $08, $08				;Bacspace here!!!!
	.DB $0F, $09, $09				;TAB here!!!!!
	.DB $10, 'q', 'Q'
	.DB $11, 'w', 'W'
	.DB $12, 'e', 'E'
	.DB $13, 'r', 'R'
	.DB $14, 't', 'T'
	.DB $15, 'y', 'Y'
	.DB $16, 'u', 'U'
	.DB $17, 'i', 'I'
	.DB $18, 'o', 'O'
	.DB $19, 'p', 'P'
	.DB $1A, '[', '{'
	.DB $1B, ']', '}'
	.DB $3A, $00, $00				;CAPSLOCK here!!!!
	.DB $1E, 'a', 'A'
	.DB $1F, 's', 'S'
	.DB $20, 'd', 'D'
	.DB $21, 'f', 'F'
	.DB $22, 'g', 'G'
	.DB $23, 'h', 'H'
	.DB $24, 'j', 'J'
	.DB $25, 'k', 'K'
	.DB $26, 'l', 'L'
	.DB $27, ';', ':'
	.DB $28, $27, $22				; ' and "
	.DB $1C, $0D, $0D				;ENTER here!!!!!
	.DB $2C, 'z', 'Z'
	.DB $2D, 'x', 'X'
	.DB $2E, 'c', 'C'
	.DB $2F, 'v', 'V'
	.DB $30, 'b', 'B'
	.DB $31, 'n', 'N'
	.DB $32, 'm', 'M'
	.DB $33, ',', '<'
	.DB $34, '.', '>'
	.DB $35, '/', '?'
	.DB $39, ' ', ' '
	.DB $00, $00, $00				;Table ends here
PS2_SCANCODES_END:
    
    .OR $FFFE       ;reset vector
    .DW INIT
