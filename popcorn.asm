  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

  .rsset $0000
BackgroundAddress         .rs 0
BackgroundLow             .rs 1
BackgroundHigh            .rs 1
GameSpritePointer         .rs 0
GameSpritePointerLow      .rs 1
GameSpritePointerHigh     .rs 1
Player1Buttons            .rs 1
Player2Buttons            .rs 1
PaddleSpeed               .rs 1
PaddleCount               .rs 1
PopcornSpeed              .rs 2
RandomNumber              .rs 1
GameState                 .rs 1
BackGroundRowsLeftToCopy  .rs 1

ButtonA       = %10000000
ButtonB       = %01000000
ButtonSelect  = %00100000
ButtonStart   = %00010000
ButtonUp      = %00001000
ButtonDown    = %00000100
ButtonLeft    = %00000010
ButtonRight   = %00000001
PaddleGameSpritesStart = $0200
PaddleGameSpritesCount = 20
GameState_Initializing              = 1
GameState_LoadingStartBackground    = 2
GameState_Start                     = 3
GameState_LoadingPlayBackground     = 4
GameState_Play                      = 5
GameState_LoadingGameOverBackground = 6
GameState_GameOver                  = 7


  .bank 0
  .org $C000 


ResetInterruptHandler:
  LDX #$FF
  TXS
  JSR SwitchToGameStateInitializing
  JSR InitializeCPU
  JSR WaitForVBlank
  JSR ClearMemory   
  JSR InitializeVariables
  JSR WaitForVBlank
  JSR LoadPalettes
  JSR LoadSprites
  JSR LoadStartBackground
  JSR InitializePPU
  JSR SwitchToGameStateStart
  JMP LoopForever


NMIInterruptHandler:
  JSR GenerateRandomNumber
  LDA GameState
  CMP #GameState_Initializing
  BEQ HandleInitializing
  CMP #GameState_LoadingStartBackground
  BEQ HandleLoadingStartBackground
  CMP #GameState_Start
  BEQ HandleStart
  CMP #GameState_LoadingPlayBackground
  BEQ HandleLoadingPlayBackground
  CMP #GameState_Play
  BEQ HandlePlay
  CMP #GameState_LoadingGameOverBackground
  BEQ HandleLoadingGameOverBackground
  CMP #GameState_GameOver
  BEQ HandleGameOver
HandleInitializing:
HandleLoadingPlayBackground:
HandleLoadingGameOverBackground:
HandleLoadingGameOver:
HandleGameOver:
  RTI
HandleLoadingStartBackground:
  ;JSR LoadNextChunckOfStartBackground
  RTI
HandlePlay:
  JSR TransferGameSpritesToPPU
  JSR ReadControllers
  JSR UpdatePaddles
  JSR InitializePPU
  RTI
HandleStart:
  JSR TransferGameSpritesToPPU
  JSR ReadControllers
  JSR UpdatePaddles
  ;JSR SwitchToPlayStateWhenStartIsPressed
  JSR InitializePPU
  RTI


LoopForever:
  JMP LoopForever


WaitForVBlank:
  BIT $2002
  BPL WaitForVBlank
  RTS


InitializeVariables:
  LDX #$08
  STX PaddleSpeed
  LDX #$05
  STX PaddleCount
  LDX #$00
  STX PopcornSpeed
  INX
  STX PopcornSpeed + 1
  RTS


InitializeCPU:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$00
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs
  RTS


ClearMemory:
  LDX #$00
ClearMemoryLoop:
  LDA #$00
  STA $0000, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE ClearMemoryLoop
  RTS


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA Palette, x        ; load data from address (Palette + the value in x)
                          ; 1st time through loop it will load Palette+0
                          ; 2nd time through loop it will load Palette+1
                          ; 3rd time through loop it will load Palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  RTS


LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA Sprites, x        ; load data from address (Sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$FF              ; Compare X to hex $FF, decimal 255
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to 255
                        ; if compare was equal to 255, keep going down
  RTS


SwitchToGameStateInitializing:
  LDA #GameState_Initializing
  STA GameState
  RTS

SwitchToGameStateLoadingStartBackground:
  LDA #GameState_LoadingStartBackground
  STA GameState
  LDA #$00
  STA BackgroundLow
  LDA #HIGH(StartBackground)
  STA BackgroundHigh
  LDA #30
  STA BackGroundRowsLeftToCopy
  RTS

SwitchToGameStateStart
  LDA #GameState_Start
  STA GameState
  RTS

SwitchToGameStateLoadingPlayBackground:
  LDA #GameState_LoadingPlayBackground
  STA GameState
  RTS

SwitchToGameStatePlay
  LDA #GameState_Play
  STA GameState
  RTS

SwitchToGameStateLoadingGameOverBackground:
  LDA #GameState_LoadingGameOverBackground
  STA GameState
  RTS

SwitchToGameStateGameOver
  LDA #GameState_GameOver
  STA GameState
  RTS


LoadStartBackground:
  LDA #$00
  STA BackgroundLow
  LDA #HIGH(StartBackground)
  STA BackgroundHigh
  JSR LoadBackground
  RTS

LoadPlayBackground:
  LDA #$00
  STA BackgroundLow     ; put the low byte of the address of background into pointer
  LDA #HIGH(PlayBackground)
  STA BackgroundHigh    ; put the high byte of the address into pointer
  JSR LoadBackground
  RTS

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDX #$04
  LDY #$00              ; 

LoadBackgroundLoop:
  LDA [BackgroundAddress], y  ; copy one background byte from address in pointer plus Y
  STA $2007                   ; this runs 256 * 4 times
  
  INY                         ; inside loop counter
  BNE LoadBackgroundLoop      ; run the inside loop 256 times before continuing down
  
  INC BackgroundHigh          ; low byte went 0 to 256, so high byte needs to be changed now
  
  DEX
  BNE LoadBackgroundLoop      ; Until X drops to #$00, we keep looping back for another 256 bytes.

  RTS


InitializePPU:
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00         ; No scrolling so always set background display position to 0,0
  STA $2005
  STA $2005
  RTS


ReadControllers:
  LDA #$01
  STA $4016 ; Start controller button states being continuously written to $4016 and $4017
  LDA #$00
  STA $4016 ; Stop $4016 and $4017 being written to so we can read them.

  ; Read $4016 (Player 1 controller) and $4017 (Player 2 controller) once for each button
  ; (A, B, Select, Start, Up, Down, Left, Right).  For each read bit 0 will have the state
  ; of the button.  Shift bit 0 out (LSR - Logical Shift Right) into the Carry Flag and
  ; then into our PlayerXButtons variables (ROL - Rotate Left). The end result will be bits
  ; 7 - 0 having the states of buttons A, B, Select, Start, Up, Down, Left and Right.
  LDX #$08
ReadControllersLoop:
  LDA $4016
  LSR A               ; bit 0 -> Carry
  ROL Player1Buttons  ; bit 0 <- Carry
  LDA $4017
  LSR A               ; bit 0 -> Carry
  ROL Player2Buttons  ; bit 0 <- Carry
  DEX
  BNE ReadControllersLoop
  RTS


TransferGameSpritesToPPU:
  ; Transfer game sprites to PPU
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer
  RTS


UpdatePaddles:
  LDA Player1Buttons
  AND #ButtonLeft
  BNE MovePaddlesLeft
  LDA Player1Buttons
  AND #ButtonRight
  BNE MovePaddlesRight
  RTS

MovePaddlesLeft:
  JSR InitializeMovingPaddles
MovePaddlesLeftLoop:
  LDA [GameSpritePointer],Y
  SEC
  SBC PaddleSpeed
  STA [GameSpritePointer],Y
  JSR IncrementGameSpritePointerAndDecrementX
  BNE MovePaddlesLeftLoop
  BNE MovePaddlesLeftLoop
  RTS

MovePaddlesRight:
  JSR InitializeMovingPaddles
MovePaddlesRightLoop:
  LDA [GameSpritePointer],Y
  CLC
  ADC PaddleSpeed
  STA [GameSpritePointer],Y
  JSR IncrementGameSpritePointerAndDecrementX
  BNE MovePaddlesRightLoop
  RTS

InitializeMovingPaddles:
  LDX #PaddleGameSpritesCount
  LDA #HIGH(PaddleGameSpritesStart)
  STA GameSpritePointerHigh
  LDA #LOW(PaddleGameSpritesStart)
  CLC
  ADC #$03
  STA GameSpritePointerLow
  LDY #$00
  RTS
  
IncrementGameSpritePointerAndDecrementX:
  CLC
  LDA GameSpritePointerLow
  ADC #$04
  STA GameSpritePointerLow
  LDA GameSpritePointerHigh
  ADC #$00
  STA GameSpritePointerHigh
  DEX
  RTS


SwitchToPlayStateWhenStartIsPressed:
  LDA Player1Buttons
  AND #ButtonStart
  BNE SwitchToPlayState
  RTS
SwitchToPlayState:
  LDA #GameState_Play
  STA GameState
  JSR LoadPlayBackground
  RTS


GenerateRandomNumber:
  LDA RandomNumber
  BEQ DoEor
  ASL A
  BEQ SetRandomNumber
  BCC SetRandomNumber
DoEor:
  EOR #$1d
SetRandomNumber:
  STA RandomNumber
  RTS


;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $E000

StartBackground:
  .incbin "popcorn_start.nam";
PlayBackground:
  .incbin "popcorn_play.nam";


Palette:
  .incbin "popcorn.pal" ; Sprite palette
  .incbin "popcorn.pal" ; Background palette

Sprites:
     ;vert tile attr horiz
  .db $67, $0A, $00, $70
  .db $67, $0A, $00, $78
  .db $67, $0A, $00, $80
  .db $67, $0A, $00, $88

  .db $7F, $0A, $00, $70
  .db $7F, $0A, $00, $78
  .db $7F, $0A, $00, $80
  .db $7F, $0A, $00, $88

  .db $97, $0A, $00, $70
  .db $97, $0A, $00, $78
  .db $97, $0A, $00, $80
  .db $97, $0A, $00, $88

  .db $AF, $0A, $00, $70
  .db $AF, $0A, $00, $78
  .db $AF, $0A, $00, $80
  .db $AF, $0A, $00, $88

  .db $C7, $0A, $00, $70
  .db $C7, $0A, $00, $78
  .db $C7, $0A, $00, $80
  .db $C7, $0A, $00, $88


  .org $FFFA
  .dw NMIInterruptHandler   ; When VBlank begins
  .dw ResetInterruptHandler ; When the processor first turns on or is reset
  .dw 0                     ; External interrupt IRQ is not used
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "popcorn.chr"   ;includes 8KB graphics file
