InitializeVariables:
  LDX #$08  ; Not to exceed $1F (31)
  STX NormalPaddleSpeed
  LDX #$05
  STX PaddleCount
  LDX #40
  STX ActivePopcornAdvanceFrameSpeed
  LDA #12
  JSR SetPixelsPerFrameForLevel
  LDX #ConveyorFirstTile
  STX ConveyorTile
  LDX #$00
  STX Player1PreviousButtons
  STX Player2PreviousButtons
  STX ActivePopcornLevel
  STX ActivePopcornFallingFrame
  STX ActivePopcornFrameCount
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


SetPixelsPerFrameForLevel:
  ; A = level
  LDY #$01
DivideBy8:
  CMP #$08
  BLT DivideBy8Done
  SEC
  SBC #$08
  INY
  JMP DivideBy8
DivideBy8Done:
  ; Y has value to initialize ActivePopcornPixelsPerFrame[0]-[7] with.
  LDX #$00
SetInitialePixelsFrames:
  STY ActivePopcornPixelsPerFrame, X
  INX
  CPX #$08
  BNE SetInitialePixelsFrames
  ; A has number of frames to advance from there (see IncreaseLevel for algorithm).
  LDX #$00
AdvanceToCurrentFrame:
  STA Temp
  CPX Temp
  BEQ AdvanceToCurrentFrameDone
  LDA PixelsFrameIncrement, X ; Indexed by frame (0-7) to get index for element in ActivePopcornPixelsPerFrame to increment
  STX Temp2
  TAX
  LDA Temp
  INC ActivePopcornPixelsPerFrame, x
  LDX Temp2
  INX
  JMP AdvanceToCurrentFrame
AdvanceToCurrentFrameDone:
  RTS
