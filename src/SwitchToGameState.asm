SwitchToGameStateInitializeStartScreen:
  LDA #GameState_InitializeStart
  STA GameState
  RTS

SwitchToGameStateStart:
  LDA #GameState_Start
  STA GameState
  RTS

SwitchToGameStateIntializePlayScreen:
  LDA #GameState_InitializePlay
  STA GameState
  RTS

SwitchToGameStatePlay:
  LDA #GameState_Play
  STA GameState
  RTS

SwitchToGameStateInitializeGameOverScreen:
  LDA #GameState_InitializeGameOver
  STA GameState
  RTS

SwitchToGameStateGameOver:
  LDA #GameState_GameOver
  STA GameState
  RTS

SwitchToPlayStateWhenStartIsPressed:
  LDA Player1Buttons
  AND #ButtonStart
  BNE SwitchToGameStateIntializePlayScreen
  RTS
