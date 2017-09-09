AdvanceTestPopcorn:
  LDX ActivePopcornFallingFrame

  LDA ActivePopcornPixelsPerFrame, X
  CLC
  ADC PpuSpriteBuffer + 80
  STA PpuSpriteBuffer + 80

  LDA ActivePopcornPixelsPerFrame, X
  CLC
  ADC PpuSpriteBuffer + 84
  STA PpuSpriteBuffer + 84

  LDX #$01
  STX NmiNeedDma            ; Trigger sprite buffer to be sent to PPU next NMI.

  INC ActivePopcornFallingFrame
  LDA ActivePopcornFallingFrame
  CMP #$08
  BNE AdvanceTestPopcornDone
  LDA #$00
  STA ActivePopcornFallingFrame

AdvanceTestPopcornDone:
  RTS
