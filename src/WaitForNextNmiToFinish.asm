WaitForNextNmiToFinish:
  INC IsMainWaitingForNmiToFinish
WaitForNextNmiToFinishLoop:
  LDA IsMainWaitingForNmiToFinish
  BNE WaitForNextNmiToFinishLoop
  RTS
