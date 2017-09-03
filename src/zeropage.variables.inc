.segment "ZEROPAGE"  
NmiNeedDma:                     .res 1   ; $00
NmiNeedDraw:                    .res 1   ; $01
NmiNeedPpuRegistersUpdated:     .res 1   ; $02
IsMainWaitingForNmiToFinish:    .res 1   ; $03
Ppu2000Buffer:                  .res 1   ; $04
Ppu2001Buffer:                  .res 1   ; $05
PpuScrollXBuffer:               .res 1   ; $06
PpuScrollYBuffer:               .res 1   ; $07
PpuSpriteBufferIndex:           .res 1   ; $08
BackgroundPointer:              .res 2   ; $09
GameSpritePointer:              .res 2   ; $0B
Player1PreviousButtons:         .res 1   ; $0D
Player2PreviousButtons:         .res 1   ; $0E
Player1Buttons:                 .res 1   ; $0F
Player2Buttons:                 .res 1   ; $10
NormalPaddleSpeed:              .res 1   ; $11 - Not to exceed $1F (31) or calculations for right wall limit fail. :)
CurrentPaddleSpeed:             .res 1   ; $12
PaddleCount:                    .res 1   ; $13
RandomNumber:                   .res 1   ; $14
TempPointer:                    .res 2   ; $15
Temp:                           .res 1   ; $17
Temp2:                          .res 1   ; $18
GameState:                      .res 1   ; $19
ConveyorTile:                   .res 1   ; $1A - Current Conveyor tile in conveyor animation
ConveyorFrameCount:             .res 1   ; $1B - # of frames since last Conveyor advance
ActivePopcornIndex:             .res 1   ; $1C - Index into ActivePopcornStates where falling popcorn start (0-14)
ActivePopcornCount:             .res 1   ; $1D - # of popcorns from ActivePopcornIndex that are falling or on conveyor
ActivePopcornRow:               .res 1   ; $1E - Active row popcorn is falling from (0-4).  Tile is Row*2 (0, 2, 4, 6 or 8)
ActivePopcornAdvanceFrameSpeed: .res 1   ; $1F - # frames before next popcorn starts falling
ActivePopcornFrameCount:        .res 1   ; $20 - # frames - for starting next popcorn falling
ActivePopcornPixelsPerFrame:    .res 8   ; $21 - # pixels falling popcorn falls per frame #
ActivePopcornFallingFrame:      .res 1   ; $29 - Frame # for falling popcorn; cycles betwen 0 and 7
ActivePopcornLevel:             .res 1   ; $2A - Level for next falling popcorn; determines falling speed.
ActivePopcornColumns:           .res 15  ; $2B - Column # (0-14).  Filled with shuffled 0-14.  Indicates the order popcorn falls in row.
ActivePopcornPositions:         .res 15  ; $3A - Y when State is Background or Falling, X when State is Conveyor
ActivePopcornStates:            .res 15  ; $49 - Background, Falling, or Conveyor
