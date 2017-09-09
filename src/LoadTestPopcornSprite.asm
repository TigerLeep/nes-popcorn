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
