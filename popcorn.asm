  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

  .rsset $0000  ;;start variables at ram location 0
  
backgroundAddress .rs 0 ;
backgroundLow     .rs 1  ; .rs 1 means reserve one byte of space
backgroundHigh    .rs 1  ; .rs 1 means reserve one byte of space
    
  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down



LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$FF              ; Compare X to hex $FF, decimal 255
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to 255
                        ; if compare was equal to 255, keep going down
              

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA #$00
  STA backgroundLow     ; put the low byte of the address of background into pointer
  LDA #HIGH(background)
  STA backgroundHigh    ; put the high byte of the address into pointer
  
  LDX #$04              ; Copy 256 bytes 4 times to copy the entire 960 (32x30 nametable) + 64 (attributes) bytes
  LDY #$00              ; start at pointer + 0
 
LoadBackgroundLoop:
  LDA [backgroundAddress], y; copy one background byte from address in pointer plus Y
  STA $2007             ; this runs 256 * 4 times
  
  INY                   ; inside loop counter
  BNE LoadBackgroundLoop; run the inside loop 256 times before continuing down
  
  INC backgroundHigh    ; low byte went 0 to 256, so high byte needs to be changed now
  
  DEX
  BNE LoadBackgroundLoop; Copy 256 bytes 4 times to copy the entire 1024 bytes


  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever      ; jump back to Forever, infinite loop
  
 

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer


LatchController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons

  LDA $4016       ; Skip A
  LDA $4016       ; Skip B
  LDA $4016       ; Skip SELECT
  LDA $4016       ; Skip START

ReadUp:
  LDA $4016       ; player 1 - Up
  AND #%00000001  ; only look at bit 0
  BEQ ReadUpDone  ; branch to ReadUpDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA $0200       ; load sprite Y position
  SEC             ; make sure the carry flag is set before subtraction
  SBC #$01        ; A = A - 1
  STA $0200       ; save sprite Y position
  STA $0204
  CLC
  ADC #8
  STA $0208
  STA $020C
ReadUpDone:       ; handling this button is done
  

ReadDown:
  LDA $4016       ; player 1 - Down
  AND #%00000001  ; only look at bit 0
  BEQ ReadDownDone; branch to ReadDownDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA $0200       ; load sprite Y position
  CLC             ; make sure the carry flag is clear before addition
  ADC #$01        ; A = A + 1
  STA $0200       ; save sprite Y position
  STA $0204
  CLC
  ADC #8
  STA $0208
  STA $020C
ReadDownDone:     ; handling this button is done

ReadLeft:
  LDA $4016       ; player 1 - Left
  AND #%00000001  ; only look at bit 0
  BEQ ReadLeftDone  ; branch to ReadLeftDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA $0203       ; load sprite Y position
  SEC             ; make sure the carry flag is set before subtraction
  SBC #$01        ; A = A - 1
  STA $0203       ; save sprite X position
  STA $020B
  CLC
  ADC #8
  STA $0207
  STA $020F
ReadLeftDone:     ; handling this button is done
  

ReadRight:
  LDA $4016         ; player 1 - Right
  AND #%00000001    ; only look at bit 0
  BEQ ReadRightDone ; branch to ReadLeftDone if button is NOT pressed (0)
                    ; add instructions here to do something when button IS pressed (1)
  LDA $0203         ; load sprite Y position
  CLC               ; make sure the carry flag is clear before addition
  ADC #$01          ; A = A + 1
  STA $0203         ; save sprite X position
  STA $020B
  CLC
  ADC #8
  STA $0207
  STA $020F
ReadRightDone:      ; handling this button is done


  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00         ; tell the ppu there is no background scrolling
  STA $2005
  STA $2005

  RTI             ; return from interrupt
 
;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $E000

background:
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 0
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00  ;;row 1
  .db $01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$70  ;;

  .db $70,$02,$03,$02,$03,$02,$03,$02,$03,$02,$03,$02,$03,$02,$03,$02  ;;row 2
  .db $03,$02,$03,$02,$03,$02,$03,$02,$03,$02,$03,$02,$03,$02,$03,$70  ;;

  .db $70,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04  ;;row 3
  .db $05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$70  ;;

  .db $70,$06,$07,$06,$07,$06,$07,$06,$07,$06,$07,$06,$07,$06,$07,$06  ;;row 4
  .db $07,$06,$07,$06,$07,$06,$07,$06,$07,$06,$07,$06,$07,$06,$07,$70  ;;

  .db $70,$08,$09,$08,$09,$08,$09,$08,$09,$08,$09,$08,$09,$08,$09,$08  ;;row 5
  .db $09,$08,$09,$08,$09,$08,$09,$08,$09,$08,$09,$08,$09,$08,$09,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 6
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 7
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 8
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 9
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 10
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 11
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 12
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 13
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 14
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 15
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 16
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 17
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 18
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 19
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 20
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 21
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 22
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 23
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 24
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 25
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B  ;;row 26
  .db $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B  ;;

  .db $70,$3C,$1C,$38,$3B,$1E,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 27
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$4C,$2C,$48,$4B,$2E,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 28
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;

  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;row 29
  .db $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70  ;;


attribute:
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000


palette:
  .db $0F,$12,$16,$30,  $0F,$32,$34,$3A,  $0F,$1C,$15,$14,  $0F,$02,$38,$3C ; background palette
  .db $0F,$12,$16,$30,  $0F,$32,$34,$3A,  $0F,$1C,$15,$14,  $0F,$02,$38,$3C ; sprite pallete

sprites:
     ;vert tile attr horiz
  ;.db $07, $00, $00, $08
  ;.db $07, $01, $00, $10
  ;.db $07, $00, $00, $18
  ;.db $07, $01, $00, $20
  ;.db $0F, $02, $00, $08
  ;.db $0F, $03, $00, $10
  ;.db $0F, $02, $00, $18
  ;.db $0F, $03, $00, $20
  ;.db $17, $04, $00, $08
  ;.db $17, $05, $00, $10
  ;.db $17, $04, $00, $18
  ;.db $17, $05, $00, $20
  ;.db $1F, $06, $00, $08
  ;.db $1F, $07, $00, $10
  ;.db $1F, $06, $00, $18
  ;.db $1F, $07, $00, $20
  ;.db $27, $08, $00, $08
  ;.db $27, $09, $00, $10
  ;.db $27, $08, $00, $18
  ;.db $27, $09, $00, $20

  ;.db $D7, $3C, $00, $08   ;sprite S
  ;.db $DF, $4C, $00, $08   ;sprite S

  ;.db $D7, $1C, $00, $10   ;sprite C
  ;.db $DF, $2C, $00, $10   ;sprite C

  ;.db $D7, $38, $00, $18   ;sprite O
  ;.db $DF, $48, $00, $18   ;sprite O

  ;.db $D7, $3B, $00, $20   ;sprite R
  ;.db $DF, $4B, $00, $20   ;sprite R

  ;.db $D7, $1E, $00, $28   ;sprite E
  ;.db $DF, $2E, $00, $28   ;sprite E


  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "popcorn.chr"   ;includes 8KB graphics file
