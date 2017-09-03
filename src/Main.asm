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
  JSR SwitchToGameStatePlay
  JSR InitializePopcorn
  JSR WaitForNextNmiToFinish
  JSR EnableRendering
  JMP Main
HandlePlay:
  JSR UpdateConveyor
  JSR UpdatePaddles
  JSR ReadControllers
  JSR IncreaseLevelWhenBReleased
  JSR AdvanceTestPopcorn
  JSR WaitForNextNmiToFinish
  JMP Main
HandleInitializeGameOver:
HandleGameOver:
  JSR UpdateConveyor
  JSR ReadControllers
  JSR WaitForNextNmiToFinish
  JMP Main
