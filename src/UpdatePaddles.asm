UpdatePaddles:
  LDA Player1Buttons
  AND #ButtonLeft
  BNE MovePaddlesLeft
  LDA Player1Buttons
  AND #ButtonRight
  BNE MovePaddlesRight
  RTS

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
