.macro  BGE     args
        BCS     args
.endmacro

.macro  BLT     args
        BCC     args
.endmacro

;----------

.segment "HEADER"
  .byte "NES", $1A  ; iNES header identifier
  .byte 1           ; 1x 16KB PRG code
  .byte 1           ; 1x  8KB CHR data
  .byte $01, $00    ; mapper 0, vertical mirroring

;----------

ButtonA = %10000000
ButtonB = %01000000
ButtonSelect = %00100000
ButtonStart = %00010000
ButtonUp = %00001000
ButtonDown = %00000100
ButtonLeft = %00000010
ButtonRight = %00000001
PpuSpriteBuffer = $0200 ; Must be $XX00.  When high byte written to OAMDMA ($4014), $FF bytes will be copied from $XX00 to OAM.
PpuDrawingBuffer = $0300
BlankTile = $FF
GameState_InitializeStart = 1
GameState_Start = 2
GameState_InitializePlay = 3
GameState_Play = 4
GameState_InitializeGameOver = 5
GameState_GameOver = 6
ConveyorFrameSpeed = 2
ConveyorFirstTile = $0B
ConveyorLastTile = $0E
ConveyorBackgroundPpuAddress = $2340

;----------

.segment "ZEROPAGE"  
NmiNeedDma:                       .res 1   ;
NmiNeedDraw:                      .res 1   ;
NmiNeedPpuRegistersUpdated:       .res 1   ;
IsMainWaitingForNmiToFinish:      .res 1   ;
Ppu2000Buffer:                    .res 1   ;
Ppu2001Buffer:                    .res 1   ;
PpuScrollXBuffer:                 .res 1   ;
PpuScrollYBuffer:                 .res 1   ;
PpuSpriteBufferIndex:             .res 1   ;
BackgroundPointer:                .res 2   ;
GameSpritePointer:                .res 2   ;
Player1PreviousButtons:           .res 1   ;
Player2PreviousButtons:           .res 1   ;
Player1Buttons:                   .res 1   ;
Player2Buttons:                   .res 1   ;
NormalPaddleSpeed:                .res 1   ; Not to exceed $1F (31) or calculations for right wall limit fail. :)
CurrentPaddleSpeed:               .res 1   ;
PaddleCount:                      .res 1   ;
RandomNumber:                     .res 1   ;
TempPointer:                      .res 2   ;
Temp:                             .res 1   ;
Temp2:                            .res 1   ;
GameState:                        .res 1   ;
ConveyorTile:                     .res 1   ; Current Conveyor tile in conveyor animation
ConveyorFrameCount:               .res 1   ; # of frames since last Conveyor advance

ShuffledPopcornRowIndex:          .res 1   ; Index of popcorn row currently queued for falling (0-4).  Tile is Row*2 (0, 2, 4, 6 or 8)
ShuffledPopcornIndexes:           .res 15  ; Column index (0-14).  Filled with shuffled 0-14.  Indicates the order popcorn falls in row.
ShuffledPopcornNextQueuedIndex:   .res 1   ; Index into ShuffledPopcornIndexes for next popcorn to fall (0-14)
SpritedPopcornX:                  .res 15  ; X of sprited popcorn
SpritedPopcornY:                  .res 15  ; Y of sprited popcorn
SpritedPopcornTile:               .res 15  ; Index of first tile of sprited popcorn
SpritedPopcornSpeed:              .res 15  ; Speed (0-255) of sprited popcorn
SpritedPopcornNextAvailableIndex: .res 1   ; Index into sprited popcorn lists of next available spot to store a sprited popcorn

SpeedOfPopcornOnLowestRow:        .res 1   ; The speed (0-255) of popcorn on lowest row of current level
CurrentFrameModulus8:             .res 1   ; Current frame # (0-7).

PixelsToDropPopcornAtFrame:       .res 8   ; # pixels popcorn falls at frame #

;TempTestTable:                    .res 8*17;

;----------

.segment "STARTUP"
.segment "CODE"

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
;  JSR LoadTempTestTable
  LDA #%10000000      ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
  STA $2000           ; need NMI enabled now so buffers will get handled
  STA Ppu2000Buffer   ; need to also buffer so when buffers are handled, the value we just set isn't changed
  LDA #$00            ; no scrolling so always set background display position to 0,0
  STA PpuScrollXBuffer
  STA PpuScrollYBuffer
  INC NmiNeedPpuRegistersUpdated
  JSR WaitForNextNmiToFinish
  JSR LoadPalettes
  JSR SwitchToGameStateInitializeStartScreen
  JMP Main

;----------

NmiInterruptHandler:
  PHA                               ; back up registers (important)
  TXA
  PHA
  TYA
  PHA
  LDA NmiNeedDma
  BEQ DoneNeedDma
  LDA #0                            ; do sprite DMA
  STA $2003                         ; conditional via the 'needdma' flag
  LDA #>PpuSpriteBuffer
  STA $4014
  DEC NmiNeedDma
DoneNeedDma:
  LDA NmiNeedDraw                   ; do other PPU drawing (NT/Palette/whathaveyou)
  BEQ DoneNeedDraw                  ; conditional via the 'needdraw' flag
  BIT $2002                         ; clear VBl flag, reset $2005/$2006 toggle
  JSR XferDrawingsToPpu             ; draw the stuff from the drawing buffer
  DEC NmiNeedDraw
DoneNeedDraw:
  ;LDA NmiNeedPpuRegistersUpdated
  ;BEQ DoneNeedRegisters
  LDA Ppu2001Buffer                 ; copy buffered $2000/$2001 (conditional via needppureg)
  STA $2001
  LDA Ppu2000Buffer
  STA $2000
  BIT $2002
  LDA PpuScrollXBuffer              ; set X/Y scroll (conditional via needppureg)
  STA $2005
  LDA PpuScrollYBuffer
  STA $2005
  ;DEC NmiNeedPpuRegistersUpdated
DoneNeedRegisters:
  JSR GenerateRandomNumber
  JSR IncrementCurrentFrameModulus8
  LDA #0                            ; clear the Main Waiting flag so that Main will continue
  STA IsMainWaitingForNmiToFinish   ; do not DEC here, as it might already be zero (will be the case during slowdown)
  PLA                               ; restore regs and exit
  TAY
  PLA
  TAX
  PLA
  RTI

;----------

IncrementCurrentFrameModulus8:
  INC CurrentFrameModulus8
  LDA CurrentFrameModulus8
  CMP #08
  BLT IncrementCurrentFrameModulus8Done
  LDA #00
  STA CurrentFrameModulus8
IncrementCurrentFrameModulus8Done:
  RTS

;----------

XferDrawingsToPpu:
; Input data has the following format:           
;   Byte 0  = length                             
;   Byte 1  = high byte of the PPU address       
;   Byte 2  = low byte of the PPU address        
;   Byte 3  = reserved for now
;   Byte 4+ = {length} bytes                     
; Repeat until length == 0 is found.             
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
  LDA #$00
  STA PpuDrawingBuffer ; Reset buffer
  RTS

;----------

Main:
  LDA GameState
  CMP #GameState_InitializeStart
  BEQ HandleInitializeStart
  CMP #GameState_Start
  BEQ HandleStart
  CMP #GameState_InitializePlay
  BEQ HandleInitializePlay
  CMP #GameState_Play
  BEQ HandlePlay
  CMP #GameState_InitializeGameOver
  BEQ HandleInitializeGameOver
  CMP #GameState_GameOver
  BEQ HandleGameOver
  JMP Main
HandleInitializeStart:
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
HandleInitializePlay:
  JSR DisableRendering
  JSR LoadPlayBackground
  JSR LoadInitialPaddleSprites
  JSR LoadTestPopcornSprite
  JSR InitializePopcorn
  JSR WaitForNextNmiToFinish
  JSR EnableRendering
  JSR SwitchToGameStatePlay
  JMP Main
HandlePlay:
  JSR UpdateConveyor
  JSR UpdatePaddles
  JSR ReadControllers
  JSR WaitForNextNmiToFinish
  JMP Main
HandleInitializeGameOver:
HandleGameOver:
  JSR UpdateConveyor
  JSR ReadControllers
  JSR WaitForNextNmiToFinish
  JMP Main

;----------

DisableRendering:
  LDA #%00000110   ; disable sprites & background, don't hide sprites or background in left 8 pixels, color mode
  STA $2001
  RTS

;----------

EnableRendering:
  LDA #%00011110   ; enable sprites & background, don't hide sprites or background in left 8 pixels, color mode
  STA $2001
  RTS

;----------

WaitForVBlank:
  BIT $2002
  BPL WaitForVBlank
  RTS

;----------

InitializeVariables:
  LDX #$08  ; Not to exceed $1F (31)
  STX NormalPaddleSpeed
  LDX #$05
  STX PaddleCount
  LDX #ConveyorFirstTile
  STX ConveyorTile
  LDX #$00
  STX Player1PreviousButtons
  STX Player2PreviousButtons
  STX SpeedOfPopcornOnLowestRow
  STX CurrentFrameModulus8
  STX ConveyorFrameCount
  STX NmiNeedDma
  STX NmiNeedDraw
  STX NmiNeedPpuRegistersUpdated
  STX IsMainWaitingForNmiToFinish
  STX PpuScrollXBuffer
  STX PpuScrollYBuffer
  STX PpuSpriteBufferIndex
  LDX #%10000000
  STX Ppu2000Buffer
  LDX #%00011110
  STX Ppu2001Buffer
  RTS

;----------

InitializePopcorn:
  LDX #04
  STX ShuffledPopcornRowIndex
  JSR InitializeCurrentShuffledPopcornRow
  RTS

;----------

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

;----------

ClearSprites:
  LDX #$00
  LDA #$FE
ClearSpritesLoop:
  STA $0200, x
  INX
  BNE ClearSpritesLoop
  INC NmiNeedDma
  RTS

;----------

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

;----------

LoadInitialPaddleSprites:
  LDX PpuSpriteBufferIndex
  LDY #0
LoadInitialPaddleSpritesLoop:
  LDA PaddleSprites, X
  STA PpuSpriteBuffer, Y
  INX
  INY
  CPY #80
  BNE LoadInitialPaddleSpritesLoop
  STX PpuSpriteBufferIndex
  LDX #$01
  STX NmiNeedDma
  RTS

;----------

LoadTestPopcornSprite:
  LDX PpuSpriteBufferIndex
  ;vert tile attr horiz
  LDA PopcornSpriteStartY + 4
  STA PpuSpriteBuffer, X
  INX
  LDA #$08
  STA PpuSpriteBuffer, X
  INX
  LDA #$00
  STA PpuSpriteBuffer, X
  INX
  LDA PopcornSpriteStartX + 3
  STA PpuSpriteBuffer, X
  INX
  LDA PopcornSpriteStartY + 4
  STA PpuSpriteBuffer, X
  INX
  LDA #$09
  STA PpuSpriteBuffer, X
  INX
  LDA #$00
  STA PpuSpriteBuffer, X
  INX
  LDA PopcornSpriteStartX + 3
  CLC
  ADC #$08
  STA PpuSpriteBuffer, X
  INX
  STX PpuSpriteBufferIndex
  LDX #$01
  STX NmiNeedDma
  RTS

;----------

SwitchToGameStateInitializeStartScreen:
  LDA #GameState_InitializeStart
  STA GameState
  RTS

;----------

SwitchToGameStateStart:
  LDA #GameState_Start
  STA GameState
  RTS

;----------

SwitchToGameStateIntializePlayScreen:
  LDA #GameState_InitializePlay
  STA GameState
  RTS

;----------

SwitchToGameStatePlay:
  LDA #GameState_Play
  STA GameState
  RTS

;----------

SwitchToGameStateInitializeGameOverScreen:
  LDA #GameState_InitializeGameOver
  STA GameState
  RTS

;----------

SwitchToGameStateGameOver:
  LDA #GameState_GameOver
  STA GameState
  RTS

;----------

SwitchToPlayStateWhenStartIsPressed:
  LDA Player1Buttons
  AND #ButtonStart
  BNE SwitchToGameStateIntializePlayScreen
  RTS

;----------

LoadStartBackground:
  LDA #<StartBackground
  STA BackgroundPointer
  LDA #>StartBackground
  STA BackgroundPointer + 1
  JSR LoadBackground
  RTS

;----------

LoadPlayBackground:
  LDA #<PlayBackground
  STA BackgroundPointer
  LDA #>PlayBackground
  STA BackgroundPointer + 1
  JSR LoadBackground
  RTS

;----------

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of PPU address
  LDA #$00
  STA $2006             ; write the low byte of PPU address
  LDX #$04
  LDY #$00
LoadBackgroundLoop:
  LDA (BackgroundPointer), y    ; copy one background byte from address in pointer plus Y
  STA $2007                   ; this runs 256 * 4 times
  INY                         ; inside loop counter
  BNE LoadBackgroundLoop      ; run the inside loop 256 times before continuing down
  INC BackgroundPointer + 1   ; low byte went 0 to 256, so high byte needs to be changed now
  DEX
  BNE LoadBackgroundLoop      ; Until X drops to #$00, we keep looping back for another 256 bytes.
  RTS

;----------

ReadControllers:
  LDA #$01
  STA $4016 ; Start controller button states being continuously written to $4016 and $4017
  LDA #$00
  STA $4016 ; Stop $4016 and $4017 being written to so we can read them.

  LDA Player1Buttons
  STA Player1PreviousButtons
  LDA Player2Buttons
  STA Player2PreviousButtons

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

;----------

UpdatePaddles:
  LDA Player1Buttons
  AND #ButtonLeft
  BNE MovePaddlesLeft
  LDA Player1Buttons
  AND #ButtonRight
  BNE MovePaddlesRight
  RTS

;----------

MovePaddlesLeft:
  JSR InitializeMovingPaddles
  LDA (GameSpritePointer),Y
  SEC
  SBC NormalPaddleSpeed
  BGE UseNormalLeftPaddleSpeed
UseAdjustedLeftPaddleSpeed:
  LDA (GameSpritePointer),Y
  STA CurrentPaddleSpeed
  JMP MovePaddlesLeftLoop
UseNormalLeftPaddleSpeed:
  LDA NormalPaddleSpeed
  STA CurrentPaddleSpeed
MovePaddlesLeftLoop:
  LDA (GameSpritePointer),Y
  SEC
  SBC CurrentPaddleSpeed
  STA (GameSpritePointer),Y
  JSR IncrementGameSpritePointerAndDecrementX
  BNE MovePaddlesLeftLoop
  INC NmiNeedDma
  RTS

;----------

MovePaddlesRight:
  JSR InitializeMovingPaddles
  LDA (GameSpritePointer),Y
  CLC
  ADC NormalPaddleSpeed
  CMP #$E0
  BLT UseNormalRightPaddleSpeed
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
  STA CurrentPaddleSpeed
  LDA NormalPaddleSpeed
  SEC
  SBC CurrentPaddleSpeed
  STA CurrentPaddleSpeed
  JMP MovePaddlesRightLoop
UseNormalRightPaddleSpeed:
  LDA NormalPaddleSpeed
  STA CurrentPaddleSpeed
MovePaddlesRightLoop:
  LDA (GameSpritePointer),Y
  CLC
  ADC CurrentPaddleSpeed
  STA (GameSpritePointer),Y
  JSR IncrementGameSpritePointerAndDecrementX
  BNE MovePaddlesRightLoop
  INC NmiNeedDma
  RTS

;----------

InitializeMovingPaddles:
  LDA #>PpuSpriteBuffer
  STA GameSpritePointer+1
  LDA #<PpuSpriteBuffer
  CLC
  ADC #$03
  STA GameSpritePointer
  LDY #$00
  LDX #20
  RTS

;----------

IncrementGameSpritePointerAndDecrementX:
  LDA GameSpritePointer
  CLC
  ADC #$04
  STA GameSpritePointer
  LDA GameSpritePointer+1
  ADC #$00
  STA GameSpritePointer+1
  DEX
  RTS

;----------

;LoadTempTestTable:
;  LDX #00 ; Offset into TempTestTable
;  STX Temp
;  LDX #00 ; Speed (0-16)
;  LDY #00 ; Frame (0-7)
;LoadTempTestTableLoop:
;  JSR LoadAWithPixelsToDropPopcornAtSpeedAndFrame
;  STX Temp2
;  LDX Temp
;  STA TempTestTable, X
;  INX
;  STX Temp
;  LDX Temp2
;  INY
;  CPY #8
;  BLT LoadTempTestTableLoop
;  LDY #00
;  INX
;  CPX #17
;  BLT LoadTempTestTableLoop
;  RTS

;----------

LoadAWithPixelsToDropPopcornAtSpeedAndFrame:
  ; in:  X = Speed (0-255)
  ; in:  Y = Frame (0-7)
  ; out: A = # pixels to drop popcorn
  ;      (X + 15 - 3 * (Y mod 2) - (Y + 1) / 2) / 8
  ;      Precalculated lookup for (15 - 3 * (Y mod 2) - (Y + 1) / 2) for Y = 0 to 7
  ;      (X + lookup[Y]) / 8

  TXA
  CLC
  ADC PixelsPerFrameIntermediateCalculationLookup, Y
  ROR A
  LSR A
  LSR A
  RTS

;----------

UpdateConveyor:
  LDX ConveyorFrameCount
  INX
  CPX #ConveyorFrameSpeed
  BGE AdvanceConveyor
  STX ConveyorFrameCount
  RTS
AdvanceConveyor:
  LDX #$00
  STX ConveyorFrameCount
  LDX ConveyorTile
  INX
  CPX #ConveyorLastTile + 1
  BNE UpdateConveyorContinue
  LDX #ConveyorFirstTile
UpdateConveyorContinue:
  STX ConveyorTile
  JSR BufferConveyor
  RTS

;----------

BufferConveyor:
  ;   Byte 0  = length                             
  ;   Byte 1  = high byte of the PPU address       
  ;   Byte 2  = low byte of the PPU address        
  ;   Byte 3  = reserved for now
  ;   Byte 4+ = {length} bytes
  JSR SetXToNextDrawingBuffer
  LDA #$20
  STA PpuDrawingBuffer, X
  INX
  LDA #>ConveyorBackgroundPpuAddress
  STA PpuDrawingBuffer, X
  INX
  LDA #<ConveyorBackgroundPpuAddress
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
  LDA #$00
  STA PpuDrawingBuffer, X;  Flag end of buffers
  LDA #$01
  STA NmiNeedDraw
  RTS

;----------

InitializeCurrentShuffledPopcornRow:
  JSR ResetCurrentShuffledPopcornRow
  JMP RandomizeShuffledPopcorn
  LDX #14
  STX ShuffledPopcornNextQueuedIndex
  RTS

;----------

ResetCurrentShuffledPopcornRow:
  LDX #00
ResetCurrentShuffledPopcornRowLoop:
  TXA
  STA ShuffledPopcornIndexes, X
  INX
  CPX #15
  BNE ResetCurrentShuffledPopcornRowLoop
  RTS

;----------

RandomizeShuffledPopcorn:
  LDX #00
RandomizeShuffledPopcornLoop:
  JSR LoadYWithRandom0To14
  JSR SwapShuffledPopcornIndexesXAndY
  INX
  CPX #15
  BNE RandomizeShuffledPopcornLoop
  RTS

;----------

LoadYWithRandom0To14:
  PHA
  JSR GenerateRandomNumber
  LDA RandomNumber
  JSR LoadAWithAModulus15
  TAY
  PLA
  RTS

;----------

LoadAWithAModulus15:
  CMP #15
  BLT GotAModulus15
  SEC
  SBC #14
  JMP LoadAWithAModulus15
GotAModulus15:
  RTS

;----------

SwapShuffledPopcornIndexesXAndY:
  LDA ShuffledPopcornIndexes, X
  PHA
  LDA ShuffledPopcornIndexes, Y
  STA ShuffledPopcornIndexes, X
  PLA
  STA ShuffledPopcornIndexes, Y
  RTS

;----------

StartNextActivePopcornDropping:
  ; Remove the next popcorn from the Shuffled list and add it to the Sprited list.
  JSR LoadYWithIndexOfNextQueuedShuffledPopcorn
  LDX SpritedPopcornNextAvailableIndex
  JSR AdjustShuffledAndSpritedIndexes
  JSR LoadPopcornAtIndexYIntoSpritedListAtIndexX
  JMP ClearPopcornAtIndexYFromBackground

;----------

LoadPopcornAtIndexYIntoSpritedListAtIndexX:
  ; in:  X = index into Sprited Popcorn lists where popcorn is to be moved to
  ; in:  Y = index into Shuffled Popcorn lists where popcorn is to be moved from
  TYA
  PHA
  LDA PopcornSpriteStartX, Y
  STA SpritedPopcornX, X
  JSR LoadAWithCurrentRowsPopcornTile
  STA SpritedPopcornTile, X
  LDY ShuffledPopcornRowIndex
  LDA PopcornSpriteStartY, Y
  STA SpritedPopcornY, X
  PLA
  TAY
  RTS

;----------

LoadYWithIndexOfNextQueuedShuffledPopcorn:
  LDY ShuffledPopcornNextQueuedIndex
  LDA ShuffledPopcornIndexes, Y
  TAY
  RTS

;----------

LoadYAndXWithShuffledAndSpritedIndexes:
  LDY ShuffledPopcornNextQueuedIndex
  LDA ShuffledPopcornIndexes, Y
  TAY
  LDX SpritedPopcornNextAvailableIndex
  RTS

;----------

AdjustShuffledAndSpritedIndexes:
  DEC ShuffledPopcornNextQueuedIndex
  INC SpritedPopcornNextAvailableIndex
  RTS

LoadAWithCurrentRowsPopcornTile:
  LDA ShuffledPopcornRowIndex
  ASL A                         ; Popcorn row index (0-4) * 2 == popcorn's tile index
  RTS

;----------

ClearPopcornAtIndexYFromBackground:
  TXA
  PHA
  TYA
  PHA
  ASL A
  LDY ShuffledPopcornRowIndex
  CLC
  ADC PopcornBackgroundRowOffsets, Y
  STA TempPointer+1
  LDA #$20
  STA TempPointer
  JSR BufferPopcornBlanking
  PLA
  TAY
  PLA
  TAX
  RTS

;----------

BufferPopcornBlanking:
  ;   Byte 0  = length                             
  ;   Byte 1  = high byte of the PPU address       
  ;   Byte 2  = low byte of the PPU address        
  ;   Byte 3  = reserved for now
  ;   Byte 4+ = {length} bytes
  JSR SetXToNextDrawingBuffer
  LDA #$02
  STA PpuDrawingBuffer, X
  INX
  LDA TempPointer
  STA PpuDrawingBuffer, X
  INX
  LDA TempPointer+1
  STA PpuDrawingBuffer, X
  INX
  INX
  LDA #BlankTile
  STA PpuDrawingBuffer, X
  INX
  STA PpuDrawingBuffer, X
  INX
  LDA #$00
  STA PpuDrawingBuffer, X;  Flag end of buffers
  LDA #$01
  STA NmiNeedDraw
  RTS

;----------

SetXToNextDrawingBuffer:
  LDX #$00
  TXA
SetXToNextDrawingBufferLoop:
  LDY PpuDrawingBuffer, X ; Length of this buffer's data
  BEQ SetXToNextDrawingBufferDone
  CLC
  ADC 4
  CLC
  ADC PpuDrawingBuffer, X
  TAX
  JMP SetXToNextDrawingBufferLoop
SetXToNextDrawingBufferDone:
  RTS

;----------

GenerateRandomNumber:
  PHA
  LDA RandomNumber
  BEQ DoEor
  ASL A
  BEQ SetRandomNumber
  BLT SetRandomNumber
DoEor:
  EOR #$1d
SetRandomNumber:
  STA RandomNumber
  PLA
  RTS

;----------

WaitForNextNmiToFinish:
  INC IsMainWaitingForNmiToFinish
WaitForNextNmiToFinishLoop:
  LDA IsMainWaitingForNmiToFinish
  BNE WaitForNextNmiToFinishLoop
  RTS

;----------

AdvancePopcornDropping:
  ; Need to rethink whole popcorn-dropping design.  Popcorns from next row can start dropping
  ; before current row finishes falling and at a slightly faster speed.  So we need to track
  ; speed per Active Popcorn, and track more than just the current row.
  ; 
  ; Speed based on level.  Speed increases by 1/8 pixel per frame per level.  Need to track
  ; level for each Active Popcorn.
  ; 
  ; Active Popcorn needs to be dynamic in size. Only holds Popcorn waiting to fall in current
  ; row Popcorn moves from Active Popcorn list to Falling Popcorn list when it starts falling.
  ; When last popcorn in Active Popcorn list moves to Falling list, refill Active Popcorn from
  ; next row.  These can start falling while there are still Popcorn from previous row in the
  ; Falling Popcorn list.  Active Popcorn list will always be Popcorn of same level (speed).
  ; Falling Popcorn list can have Popcorn of different levels (speeds) in it.
  ; 
  ; Every 40 frames (should be based on a base speed adjusted by row next popcorn drops from)
  ; we start next popcorn falling.
  ; 
  ; Before dropping the next popcorn, we need to check if any popcorns are left.  If not, Stop
  ; dropping popcorns.  Once the last popcorn is caught, reset play and adjust speeds as
  ; appropriate.
  ; 
  ; If popcorn is missed and hits conveyor, clear all Falling popcorn sprites and continue
  ; animating Conveyor popcorn until off screen.  Then decrease # of paddles and reset play
  ; adjusting speed as appropriate.
  ; 
  ; Need to keep track of where active Popcorn sprites are in PpuSpriteBuffer so we can update
  ; their Y (or X if on conveyor).
  ; 
  ; Need to control speed of popcorn.
  ; 1/8 pixel per frame per speed
  ; Start speed 0 at 1 pixel per frame
  ; Speed 1+: Increase pixels per 1st, 3rd, 5th, 7th, 2nd, 4th, 6th, 8th frame.   Repeat.
  ; Speed  0 = 1 1 1 1 1 1 1 1  (Speed+7)/8+1 = 0
  ; Speed  1 = 2 1 1 1 1 1 1 1  (Speed+7)/8+1 = 0
  ; Speed  2 = 2 1 2 1 1 1 1 1  (Speed+7)/8+1 = 0
  ; Speed  3 = 2 1 2 1 2 1 1 1  (Speed+7)/8+1 = 0
  ; Speed  4 = 2 1 2 1 2 1 2 1  (Speed+7)/8+1 = 0
  ; Speed  5 = 2 2 2 1 2 1 2 1  (Speed+7)/8+1 = 0
  ; Speed  6 = 2 2 2 2 2 1 2 1  (Speed+7)/8+1 = 0
  ; Speed  7 = 2 2 2 2 2 2 2 1  (Speed+7)/8+1 = 0
  ; Speed  8 = 2 2 2 2 2 2 2 2  (Speed+7)/8+1 = 0
  ; Speed  9 = 3 2 2 2 2 2 2 2  (Speed+7)/8+1 = 0
  ; Speed 10 = 3 2 3 2 2 2 2 2  (Speed+7)/8+1 = 0
  ; Speed 11 = 3 2 3 2 3 2 2 2  (Speed+7)/8+1 = 0
  ; Speed 12 = 3 2 3 2 3 2 3 2  (Speed+7)/8+1 = 0
  ; Speed 13 = 3 3 3 2 3 2 3 2  (Speed+7)/8+1 = 0
  ; Speed 14 = 3 3 3 3 3 2 3 2  (Speed+7)/8+1 = 0
  ; Speed 15 = 3 3 3 3 3 3 3 2  (Speed+7)/8+1 = 0
  ; Speed 16 = 3 3 3 3 3 3 3 3  (Speed+7)/8+1 = 0
  ; Subtract 8 until cant.  # of subtractions +1 initializes all 8 numbers.
  ; Follow algorithm with remainder to get 8 numbers for current level.
  RTS

;----------

.segment "RODATA"
StartBackground:
  .incbin "popcorn_start.nam";
PlayBackground:
  .incbin "popcorn_play.nam";

;----------

Palette:
  .incbin "popcorn.pal" ; Sprite palette
  .incbin "popcorn.pal" ; Background palette

;----------

PaddleSprites:
     ;vert tile attr horiz
  .byte $67, $0A, $00, $70
  .byte $67, $0A, $00, $78
  .byte $67, $0A, $00, $80
  .byte $67, $0A, $00, $88

  .byte $7F, $0A, $00, $70
  .byte $7F, $0A, $00, $78
  .byte $7F, $0A, $00, $80
  .byte $7F, $0A, $00, $88

  .byte $97, $0A, $00, $70
  .byte $97, $0A, $00, $78
  .byte $97, $0A, $00, $80
  .byte $97, $0A, $00, $88

  .byte $AF, $0A, $00, $70
  .byte $AF, $0A, $00, $78
  .byte $AF, $0A, $00, $80
  .byte $AF, $0A, $00, $88

  .byte $C7, $0A, $00, $70
  .byte $C7, $0A, $00, $78
  .byte $C7, $0A, $00, $80
  .byte $C7, $0A, $00, $88

;----------

PopcornBackgroundRowOffsets:  ; Indexed by Row (0-4)
  .byte $21, $41, $61, $81, $A1

PopcornSpriteStartX:  ; Indexed by column (0-14)
  .byte $08, $18, $28, $38, $48, $58, $68, $78, $88, $98, $A8, $B8, $C8, $D8, $E8

PopcornSpriteStartY:  ; Indexed by Row (0-4)
  .byte $07, $0F, $17, $1F, $27

PixelsPerFrameIntermediateCalculationLookup:
  .byte $0F, $0B, $0E, $0A, $0D, $09, $0C, $08


;----------

.segment "VECTORS"
	.word	NmiInterruptHandler   ; When VBlank begins
	.word	ResetInterruptHandler ; When the processor first turns on or is reset
	.word	0                     ; External interrupt IRQ is not used

;----------

.segment "CHARS"
  .incbin "popcorn.chr"   ;includes 8KB graphics file
