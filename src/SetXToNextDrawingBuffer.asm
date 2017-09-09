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
