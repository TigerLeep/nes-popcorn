GenerateRandomNumber:
  PHA
  LDA RandomNumber
  BEQ DoEor
  ASL A
  BEQ SetRandomNumber
  BLT SetRandomNumber
DoEor:
  EOR #$1d
SetRandomNumber:
  STA RandomNumber
  PLA
  RTS
