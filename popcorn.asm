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
PpuSpriteBuffer                         = $0200
PpuDrawingBuffer                        = $0300
GameState_InitializeStartScreen         = 1
GameState_Start                         = 2
GameState_IntializePlayScreen           = 3
GameState_Play                          = 4
GameState_InitializeGameOverScreen      = 5
GameState_GameOver                      = 6
PopcornState_Background                 = 1
PopcornState_Falling                    = 2
PopcornState_Conveyor                   = 3
ConveyorFrameSpeed                      = 2     ; # of frames before Conveyor (and Popcorn on Conveyor) advances
ConveyorFirstTile                       = $0B   ; First tile # in Conveyor animation
ConveyorLastTile                        = $0E   ; Last tile # in Conveyor animation
ConveyorBackgroundPPUAddress            = $2340 ; Start address in PPU of Conveyor tiles to update when animating the Conveyor


  .rsset $0000
NmiNeedDma                    .rs 1
NmiNeedDraw                   .rs 1
NmiNeedPpuRegistersUpdated    .rs 1
IsMainWaitingForNmiToFinish   .rs 1
Ppu2000Buffer                 .rs 1
Ppu2001Buffer                 .rs 1
PpuScrollXBuffer              .rs 1
PpuScrollYBuffer              .rs 1
BackgroundPointer             .rs 2
GameSpritePointer             .rs 2
Player1Buttons                .rs 1
Player2Buttons                .rs 1
NormalPaddleSpeed             .rs 1   ; Not to exceed $1F (31) or calculations for right wall limit fail. :)
CurrentPaddleSpeed            .rs 1
PaddleCount                   .rs 1
ActivePopcornFrameSpeed       .rs 1   ; # frames before popcorn advances when falling
ActivePopcornPixelSpeed       .rs 1   ; # pixels popcorn advances when falling
ActivePopcornFrameCount       .rs 1   ; # of frames since last Popcorn advance
RandomNumber                  .rs 1
TempPointer                   .rs 2
Temp                          .rs 1
GameState                     .rs 1
ConveyorTile                  .rs 1   ; Current Conveyor tile in conveyor animation
ConveyorFrameCount            .rs 1   ; # of frames since last Conveyor advance
ActivePopcornIndex            .rs 1   ; Index into ActivePopcornStates where falling popcorn start (0-14)
ActivePopcornCount            .rs 1   ; # of popcorns from ActivePopcornIndex that are falling or on conveyor
ActivePopcornRow              .rs 1   ; Active row popcorn is falling from (0-4).  Tile is Row*2 (0, 2, 4, 6 or 8)
ActivePopcornColumns          .rs 15  ; Column # (0-14).  Filled with shuffled 0-14.  Indicates the order popcorn falls in row.
ActivePopcornPositions        .rs 15  ; Y when State is Background or Falling, X when State is Conveyor
ActivePopcornStates           .rs 15  ; Background, Falling, or Conveyor

  .bank 0
  .org $C000


ResetInterruptHandler:
  LDX #$FF
  TXS
  SEI                 ; disable IRQs
  CLD                 ; disable decimal mode
  LDX #$00
  STX $2000           ; disable NMI
  JSR DisableRendering
  JSR WaitForVBlank
  JSR ClearMemory
  JSR WaitForVBlank
  JSR InitializeVariables

  LDA #%10000000      ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
  STA $2000
  STA <Ppu2000Buffer
  LDA #$00            ; no scrolling so always set background display position to 0,0
  STA <PpuScrollXBuffer
  STA <PpuScrollYBuffer
  INC NmiNeedPpuRegistersUpdated
  JSR WaitForNextNmiToFinish
  JSR LoadPalettes
  JSR SwitchToGameStateInitializeStartScreen
  JMP Main


NmiInterruptHandler:
  PHA                               ; back up registers (important)
  TXA
  PHA
  TYA
  PHA

  LDA <NmiNeedDma
  BEQ DoneNeedDma
  LDA #0                            ; do sprite DMA
  STA $2003                         ; conditional via the 'needdma' flag
  LDA #HIGH(PpuSpriteBuffer)
  STA $4014
  DEC <NmiNeedDma
DoneNeedDma:

  LDA <NmiNeedDraw                  ; do other PPU drawing (NT/Palette/whathaveyou)
  BEQ DoneNeedDraw                  ; conditional via the 'needdraw' flag
  BIT $2002                         ; clear VBl flag, reset $2005/$2006 toggle
  JSR XferDrawingsToPpu             ; draw the stuff from the drawing buffer
  DEC <NmiNeedDraw
DoneNeedDraw:

  LDA <Ppu2001Buffer                ; copy buffered $2000/$2001 (conditional via needppureg)
  STA $2001
  LDA <Ppu2000Buffer
  STA $2000

  BIT $2002
  LDA <PpuScrollXBuffer             ; set X/Y scroll (conditional via needppureg)
  STA $2005
  LDA <PpuScrollYBuffer
  STA $2005
  DEC <NmiNeedPpuRegistersUpdated
DoneNeedRegisters:

  JSR GenerateRandomNumber

  LDA #0                            ; clear the Main Waiting flag so that Main will continue
  STA <IsMainWaitingForNmiToFinish  ; note that you should not 'dec' here, as sleeping might
                                    ; already be zero (will be the case during slowdown)

  PLA                               ; restore regs and exit
  TAY
  PLA
  TAX
  PLA
  RTI


; Input data has the following format:           
;   Byte 0  = length                             
;   Byte 1  = high byte of the PPU address       
;   Byte 2  = low byte of the PPU address        
;   Byte 3  = reserved for now
;   Byte 4+ = {length} bytes                     
;                                                
; Repeat until length == 0 is found.             
;
; Buffer starts at $0100, drawBuffer is declared as
;
;  .rsset $0100
; drawBuffer .rs 160

XferDrawingsToPpu:
  LDX #$00 
  LDA $2002                 ; read PPU status to reset the high/low latch        
  
XferDrawingsToPpuLoop:
    
  LDY PpuDrawingBuffer, X   ; load the length of the data to the Y register
  BEQ DoneXferDrawingsToPpu ; length equal 0 means that the drawing is done  
  
  INX                       ; X = 1
  LDA PpuDrawingBuffer, X   ; load the high byte of the target address
  STA $2006                 ; write the high byte to PPU
  
  INX                       ; X = 2
  LDA PpuDrawingBuffer, X   ; load the low byte of the target address
  STA $2006                 ; write the low byte to PPU
  
  INX                       ; X = 3 (reserved for now)
      
XferDrawingToPpuLoop:
  INX                       ; increment X so it points to the next byte
  LDA PpuDrawingBuffer, X   ; load a byte of the data
  STA $2007                 ; write it to PPU
  DEY                       ; decrement Y
  BNE XferDrawingToPpuLoop  ; if Y != 0 jump to .setLoop
    
  INX                       ; increment X so it points to the next segment      
  JMP XferDrawingsToPpuLoop ; jump back to .drawLoop
 
DoneXferDrawingsToPpu:
  RTS


Main:
  LDA <GameState
  CMP #GameState_InitializeStartScreen
  BEQ HandleInitializeStartScreen
  CMP #GameState_Start
  BEQ HandleStart
  CMP #GameState_IntializePlayScreen
  BEQ HandleIntializePlayScreen
  CMP #GameState_Play
  BEQ HandlePlay
  CMP #GameState_InitializeGameOverScreen
  BEQ HandleInitializeGameOverScreen
  CMP #GameState_GameOver
  BEQ HandleGameOver
  JMP Main
HandleInitializeStartScreen:
  JSR DisableRendering
  JSR LoadStartBackground
  JSR ClearSprites
  JSR SwitchToGameStateStart
  JSR WaitForNextNmiToFinish
  JSR EnableRendering
  JMP Main
HandleStart:
  JSR UpdateConveyor
  JSR ReadControllers
  JSR SwitchToPlayStateWhenStartIsPressed
  JSR WaitForNextNmiToFinish
  JMP Main
HandleIntializePlayScreen:
  JSR DisableRendering
  JSR LoadPlayBackground
  JSR LoadInitialPaddleSprites
  JSR SwitchToGameStatePlay
  JSR InitializePopcorn
  JSR WaitForNextNmiToFinish
  JSR EnableRendering
  JMP Main
HandlePlay:
  JSR UpdateConveyor
  JSR UpdatePaddles
  JSR ReadControllers
  JSR WaitForNextNmiToFinish
  JMP Main
HandleInitializeGameOverScreen:
HandleGameOver:
  JSR UpdateConveyor
  JSR ReadControllers
  JSR WaitForNextNmiToFinish
  JMP Main


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
  STX <ActivePopcornFrameSpeed
  LDX #$01
  STX <ActivePopcornPixelSpeed
  LDX #ConveyorFirstTile
  STX <ConveyorTile
  LDX #$00
  STX <ConveyorFrameCount
  STX <NmiNeedDma
  STX <NmiNeedDraw
  STX <NmiNeedPpuRegistersUpdated
  STX <IsMainWaitingForNmiToFinish
  STX <PpuScrollXBuffer
  STX <PpuScrollYBuffer
  LDX #%10000000
  STX <Ppu2000Buffer
  LDX #%00011110
  STX <Ppu2001Buffer

  RTS


InitializePopcorn:
  LDX #05
  STX <ActivePopcornRow
  JSR AdvanceToNextPopcornRow
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
  INC <NmiNeedDma
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
  LDA PaddleSprites, X
  STA PpuSpriteBuffer, X
  INX
  CPX #80
  BNE LoadInitialPaddleSpritesLoop
  INC NmiNeedDma
  RTS


SwitchToGameStateInitializeStartScreen:
  LDA #GameState_InitializeStartScreen
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
  LDA #GameState_InitializeGameOverScreen
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
  INC NmiNeedDma
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
  INC NmiNeedDma
  RTS

InitializeMovingPaddles:
  LDA #HIGH(PpuSpriteBuffer)
  STA <GameSpritePointer+1
  LDA #LOW(PpuSpriteBuffer)
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
  LDX <ConveyorTile
  INX
  CPX #ConveyorLastTile + 1
  BNE UpdateConveyorContinue
  LDX #ConveyorFirstTile
UpdateConveyorContinue:
  STX <ConveyorTile
  JSR BufferConveyor
  RTS

BufferConveyor:
  ;   Byte 0  = length                             
  ;   Byte 1  = high byte of the PPU address       
  ;   Byte 2  = low byte of the PPU address        
  ;   Byte 3  = reserved for now
  ;   Byte 4+ = {length} bytes
  LDA #$20
  LDX #$00
  STA PpuDrawingBuffer, X
  INX
  LDA #HIGH(ConveyorBackgroundPPUAddress)
  STA PpuDrawingBuffer, X
  INX
  LDA #LOW(ConveyorBackgroundPPUAddress)
  STA PpuDrawingBuffer, X
  INX
  INX
  LDY #$20
  LDA ConveyorTile
BufferConveyorLoop:
  STA PpuDrawingBuffer, X
  INX
  DEY
  BNE BufferConveyorLoop
  INC NmiNeedDraw
  RTS


AdvanceToNextPopcornRow:
  DEC <ActivePopcornRow
  LDX #15
  STX <ActivePopcornIndex
  LDX #$00
  STX <ActivePopcornCount
  JSR InitializeActivePopcornRow
  ;JSR ShuffleActivePopcornPosits
  JMP ShuffleActivePopcornPosits


InitializeActivePopcornRow:
  LDX #00
InitializeActivePopcornRowLoop:
  LDA #PopcornState_Background
  STA <ActivePopcornStates, X
  LDY <ActivePopcornRow
  LDA PopcornStartY, Y
  STA <ActivePopcornPositions, X
  TXA
  STA <ActivePopcornColumns, X
  INX
  CPX #15
  BNE InitializeActivePopcornRowLoop
  RTS


ShuffleActivePopcornPosits:
  LDA #00
ShuffleActivePopcornPositsLoop:
  PHA
  JSR GenerateRandomNumber
  LDA <RandomNumber
GetValidRandomNumber:
  CMP #15
  BCC GotValidRandomNumber
  SEC
  SBC #14
  JMP GetValidRandomNumber
GotValidRandomNumber:
  TAY
  PLA
  TAX ; X and Y hold the indexes of the two Position elements to swap
  LDA <ActivePopcornColumns, X
  STA <Temp
  LDA ActivePopcornColumns, Y
  STA <ActivePopcornColumns, X
  LDA <Temp
  STA ActivePopcornColumns, Y

  TXA
  CLC
  ADC #01
  CMP #15
  BNE ShuffleActivePopcornPositsLoop
  RTS


GenerateRandomNumber:
  PHA
  LDA <RandomNumber
  BEQ DoEor
  ASL A
  BEQ SetRandomNumber
  BCC SetRandomNumber
DoEor:
  EOR #$1d
SetRandomNumber:
  STA <RandomNumber
  PLA
  RTS

WaitForNextNmiToFinish:
  INC IsMainWaitingForNmiToFinish
WaitForNextNmiToFinishLoop:
  LDA IsMainWaitingForNmiToFinish
  BNE WaitForNextNmiToFinishLoop
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

PopcornStartX:  ; Indexed by column (0-14)
  .db $08, $18, $28, $38, $48, $58, $68, $78, $88, $98, $A8, $B8, $C8, $D8, $E8

PopcornStartY:  ; Indexed by Row (0-4)
  .db $05, $0D, $15,$1D, $25


  .org $FFFA
  .dw NmiInterruptHandler   ; When VBlank begins
  .dw ResetInterruptHandler ; When the processor first turns on or is reset
  .dw 0                     ; External interrupt IRQ is not used
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "popcorn.chr"   ;includes 8KB graphics file
