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
