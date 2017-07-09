  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

ButtonA                                 = %10000000
ButtonB                                 = %01000000
ButtonSelect                            = %00100000
ButtonStart                             = %00010000
ButtonUp                                = %00001000
ButtonDown                              = %00000100
ButtonLeft                              = %00000010
ButtonRight                             = %00000001
PaddleSpritesCPUAddress                 = $0200
GameState_InitializeGame                = 1
GameState_InitializeGameStartScreen     = 2
GameState_Start                         = 3
GameState_IntializePlayScreen           = 4
GameState_Play                          = 5
GameState_InitializeGameGameOverScreen  = 6
GameState_GameOver                      = 7
PopcornState_Background                 = 1
PopcornState_Falling                    = 2
PopcornState_Conveyor                   = 3
PopcornState_ExParrot                   = 4
ConveyorFrameSpeed                      = 2 ; # frames before conveyor (and popcorn on conveyor) advances
ConveyorFirstSprite                     = $0B
ConveyorLastSprite                      = $0E
ConveyorBackgroundPPUAddress            = $2340


  .rsset $0000
BackgroundPointer       .rs 2
GameSpritePointer       .rs 2
Player1Buttons          .rs 1
Player2Buttons          .rs 1
NormalPaddleSpeed       .rs 1 ; Not to exceed $1F (31) or calculations for right wall limit fail. :)
CurrentPaddleSpeed      .rs 1
PaddleCount             .rs 1
PopcornFrameSpeed       .rs 1 ; # frames before popcorn advances when falling
PopcornPixelSpeed       .rs 1 ; # pixels popcorn advances when falling
RandomNumber            .rs 1
TempPointer             .rs 2
GameState               .rs 1
ConveyorSprite          .rs 1
ConveyorFrameCount      .rs 1


  .rsset $0300
PopcornState:       .rs 75
PopcornPosition:    .rs 75


  .bank 0
  .org $C000 


ResetInterruptHandler:
  LDX #$FF
  TXS
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$00
  STX $2000    ; disable NMI
  JSR DisableRendering
  JSR WaitForVBlank
  JSR ClearMemory
  JSR WaitForVBlank
  JSR InitializeVariables
  JSR LoadPalettes
  JSR InitializePPU
  JSR SwitchToGameStateInitializeStartScreen
  JMP LoopForever


NMIInterruptHandler:
  JSR GenerateRandomNumber
  LDA <GameState
  CMP #GameState_InitializeGame
  BEQ HandleInitializeGame
  CMP #GameState_InitializeGameStartScreen
  BEQ HandleInitializeStartScreen
  CMP #GameState_Start
  BEQ HandleStart
  CMP #GameState_IntializePlayScreen
  BEQ HandleIntializePlayScreen
  CMP #GameState_Play
  BEQ HandlePlay
  CMP #GameState_InitializeGameGameOverScreen
  BEQ HandleInitializeGameOverScreen
  CMP #GameState_GameOver
  BEQ HandleGameOver
  RTI
HandleInitializeGame:
  RTI
HandleInitializeStartScreen:
  JSR DisableRendering
  JSR LoadStartBackground
  JSR ClearSprites
  JSR SwitchToGameStateStart
  RTI
HandleStart:
  JSR UpdateConveyor
  JSR TransferConveyorToPPU
  JSR TransferGameSpritesToPPU
  JSR ReadControllers
  JSR SwitchToPlayStateWhenStartIsPressed
  JSR EnableRendering
  RTI
HandleIntializePlayScreen:
  JSR DisableRendering
  JSR LoadPlayBackground
  JSR LoadInitialPaddleSprites
  JSR SwitchToGameStatePlay
  RTI
HandlePlay:
  JSR UpdateConveyor
  JSR TransferConveyorToPPU
  JSR TransferGameSpritesToPPU
  JSR ReadControllers
  JSR UpdatePaddles
  JSR EnableRendering
  RTI
HandleInitializeGameOverScreen:
HandleGameOver:
  JSR UpdateConveyor
  JSR TransferConveyorToPPU
  RTI


LoopForever:
  JMP LoopForever


DisableRendering:
  LDA #%00000110   ; disable sprites & background, don't hide sprites or background in left 8 pixels, color mode
  STA $2001
  RTS

EnableRendering:
  LDA #%00011110   ; enable sprites & background, don't hide sprites or background in left 8 pixels, color mode
  STA $2001
  RTS


WaitForVBlank:
  BIT $2002
  BPL WaitForVBlank
  RTS


InitializeVariables:
  LDX #$08  ; Not to exceed $1F (31)
  STX <NormalPaddleSpeed
  LDX #$05
  STX <PaddleCount
  LDX #$18
  STX <PopcornFrameSpeed
  LDX #$01
  STX <PopcornPixelSpeed
  LDX #ConveyorFirstSprite
  STX <ConveyorSprite
  LDX #$00
  STX <ConveyorFrameCount
  JSR InitializePopcorn
  RTS


InitializePopcorn:
  LDX #75
InitializePopcornLoop:
  LDA #PopcornState_Background
  STA PopcornState, X
  LDA PopcornStartY, X
  STA PopcornPosition, X
  DEX
  BNE InitializePopcornLoop
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
  INX
  BNE ClearMemoryLoop
  RTS


ClearSprites:
  LDX #$00
  LDA #$FE
ClearSpritesLoop:
  STA $0200, x
  INX
  BNE ClearSpritesLoop
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
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  RTS


LoadInitialPaddleSprites:
  LDX #$00
LoadInitialPaddleSpritesLoop:
  LDA PaddleSprites, x
  STA $0200, x
  INX
  CPX #80
  BNE LoadInitialPaddleSpritesLoop
  RTS


SwitchToGameStateInitialize:
  LDA #GameState_InitializeGame
  STA <GameState
  RTS

SwitchToGameStateInitializeStartScreen:
  LDA #GameState_InitializeGameStartScreen
  STA <GameState
  RTS

SwitchToGameStateStart:
  LDA #GameState_Start
  STA <GameState
  RTS

SwitchToGameStateIntializePlayScreen:
  LDA #GameState_IntializePlayScreen
  STA <GameState
  RTS

SwitchToGameStatePlay:
  LDA #GameState_Play
  STA <GameState
  RTS

SwitchToGameStateInitializeGameOverScreen:
  LDA #GameState_InitializeGameGameOverScreen
  STA <GameState
  RTS

SwitchToGameStateGameOver:
  LDA #GameState_GameOver
  STA <GameState
  RTS


LoadStartBackground:
  LDA #LOW(StartBackground)
  STA <BackgroundPointer
  LDA #HIGH(StartBackground)
  STA <BackgroundPointer + 1
  JSR LoadBackground
  RTS

LoadPlayBackground:
  LDA #LOW(PlayBackground)
  STA <BackgroundPointer
  LDA #HIGH(PlayBackground)
  STA <BackgroundPointer + 1
  JSR LoadBackground
  RTS

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of PPU address
  LDA #$00
  STA $2006             ; write the low byte of PPU address

  LDX #$04
  LDY #$00

LoadBackgroundLoop:
  LDA [BackgroundPointer], y  ; copy one background byte from address in pointer plus Y
  STA $2007                   ; this runs 256 * 4 times
  
  INY                         ; inside loop counter
  BNE LoadBackgroundLoop      ; run the inside loop 256 times before continuing down
  
  INC <BackgroundPointer + 1   ; low byte went 0 to 256, so high byte needs to be changed now
  
  DEX
  BNE LoadBackgroundLoop      ; Until X drops to #$00, we keep looping back for another 256 bytes.
  RTS

TransferConveyorToPPU:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #HIGH(ConveyorBackgroundPPUAddress)
  STA $2006             ; write the high byte of PPU address
  LDA #LOW(ConveyorBackgroundPPUAddress)
  STA $2006             ; write the low byte of PPU address

  LDX #$20
  LDA <ConveyorSprite

TransferConveyorToPPULoop:
  STA $2007
  DEX
  BNE TransferConveyorToPPULoop
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of PPU address
  LDA #$00
  STA $2006             ; write the low byte of PPU address
  RTS


InitializePPU:
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
  STA $2000
  JSR EnableRendering
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
  ROL <Player1Buttons ; bit 0 <- Carry
  LDA $4017
  LSR A               ; bit 0 -> Carry
  ROL <Player2Buttons ; bit 0 <- Carry
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
  LDA <Player1Buttons
  AND #ButtonLeft
  BNE MovePaddlesLeft
  LDA <Player1Buttons
  AND #ButtonRight
  BNE MovePaddlesRight
  RTS

MovePaddlesLeft:
  JSR InitializeMovingPaddles
  LDA [GameSpritePointer],Y
  SEC
  SBC <NormalPaddleSpeed
  BCS UseNormalLeftPaddleSpeed
UseAdjustedLeftPaddleSpeed:
  LDA [GameSpritePointer],Y
  STA <CurrentPaddleSpeed
  JMP MovePaddlesLeftLoop
UseNormalLeftPaddleSpeed:
  LDA <NormalPaddleSpeed
  STA <CurrentPaddleSpeed
MovePaddlesLeftLoop:
  LDA [GameSpritePointer],Y
  SEC
  SBC <CurrentPaddleSpeed
  STA [GameSpritePointer],Y
  JSR IncrementGameSpritePointerAndDecrementX
  BNE MovePaddlesLeftLoop
  RTS


MovePaddlesRight:
  JSR InitializeMovingPaddles
  LDA [GameSpritePointer],Y
  CLC
  ADC <NormalPaddleSpeed
  CMP #$E0
  BCC UseNormalRightPaddleSpeed
UseAdjustedRightPaddleSpeed:
  ; A is over #$E0 by somewhere between #$00 and #$1F (31).
  ; We need to subtract that amount from NormalPaddleSpeed in
  ; order to get the adjusted CurrentPaddleSpeed.
  ; 1 - subtract #$E00 from A and store that temporarily.
  ; 2 - Load NormalPaddleSpeed into A.
  ; 3 - subtract the temporarily stored value from A.
  ; A now has the adjusted Paddle Speed.  Store that in
  ; CurrentPaddleSpeed.
  SEC
  SBC #$E0
  STA <CurrentPaddleSpeed
  LDA <NormalPaddleSpeed
  SEC
  SBC <CurrentPaddleSpeed
  STA <CurrentPaddleSpeed
  JMP MovePaddlesRightLoop
UseNormalRightPaddleSpeed:
  LDA <NormalPaddleSpeed
  STA <CurrentPaddleSpeed
MovePaddlesRightLoop:
  LDA [GameSpritePointer],Y
  CLC
  ADC <CurrentPaddleSpeed
  STA [GameSpritePointer],Y
  JSR IncrementGameSpritePointerAndDecrementX
  BNE MovePaddlesRightLoop
  RTS

InitializeMovingPaddles:
  LDA #HIGH(PaddleSpritesCPUAddress)
  STA <GameSpritePointer+1
  LDA #LOW(PaddleSpritesCPUAddress)
  CLC
  ADC #$03
  STA <GameSpritePointer
  LDY #$00
  LDX #20
  RTS
  
IncrementGameSpritePointerAndDecrementX:
  LDA <GameSpritePointer
  CLC
  ADC #$04
  STA <GameSpritePointer
  LDA <GameSpritePointer+1
  ADC #$00
  STA <GameSpritePointer+1
  DEX
  RTS


SwitchToPlayStateWhenStartIsPressed:
  LDA <Player1Buttons
  AND #ButtonStart
  BNE SwitchToPlayState
  RTS
SwitchToPlayState:
  JSR SwitchToGameStateIntializePlayScreen
  RTS


UpdateConveyor:
  LDX <ConveyorFrameCount
  INX
  CPX #ConveyorFrameSpeed
  BCS AdvanceConveyor
  STX <ConveyorFrameCount
  RTS
AdvanceConveyor:
  LDX #$00
  STX <ConveyorFrameCount
  LDX <ConveyorSprite
  CPX #ConveyorLastSprite
  BNE IncrementConveyorSprite
  LDX #ConveyorFirstSprite - 1
IncrementConveyorSprite:
  INX
  STX <ConveyorSprite
  RTS


GenerateRandomNumber:
  LDA <RandomNumber
  BEQ DoEor
  ASL A
  BEQ SetRandomNumber
  BCC SetRandomNumber
DoEor:
  EOR #$1d
SetRandomNumber:
  STA <RandomNumber
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

PaddleSprites:
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

PopcornSprite:
  .db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .db $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
  .db $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
  .db $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06
  .db $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08
PopcornStartX:
  .db $08, $18, $28, $38, $48, $58, $68, $78, $88, $98, $A8, $B8, $C8, $D8, $E8
  .db $08, $18, $28, $38, $48, $58, $68, $78, $88, $98, $A8, $B8, $C8, $D8, $E8
  .db $08, $18, $28, $38, $48, $58, $68, $78, $88, $98, $A8, $B8, $C8, $D8, $E8
  .db $08, $18, $28, $38, $48, $58, $68, $78, $88, $98, $A8, $B8, $C8, $D8, $E8
  .db $08, $18, $28, $38, $48, $58, $68, $78, $88, $98, $A8, $B8, $C8, $D8, $E8
PopcornStartY:
  .db $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05
  .db $0D, $0D, $0D, $0D, $0D, $0D, $0D, $0D, $0D, $0D, $0D, $0D, $0D, $0D, $0D
  .db $15, $15, $15, $15, $15, $15, $15, $15, $15, $15, $15, $15, $15, $15, $15
  .db $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D
  .db $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25


  .org $FFFA
  .dw NMIInterruptHandler   ; When VBlank begins
  .dw ResetInterruptHandler ; When the processor first turns on or is reset
  .dw 0                     ; External interrupt IRQ is not used
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "popcorn.chr"   ;includes 8KB graphics file
