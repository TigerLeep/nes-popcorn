IncreaseLevel:
  INC ActivePopcornLevel
  LDA ActivePopcornLevel
  AND #%111
  TAX
  LDA PixelsFrameIncrement, X
  TAX
  INC ActivePopcornPixelsPerFrame, x
  RTS

IncreaseLevelWhenBReleased:
  LDA Player1PreviousButtons
  AND #ButtonB
  BEQ IncreaseLevelWhenBReleasedDone
  LDA Player1Buttons
  AND #ButtonB
  BNE IncreaseLevelWhenBReleasedDone
  JMP IncreaseLevel
IncreaseLevelWhenBReleasedDone:
  RTS
