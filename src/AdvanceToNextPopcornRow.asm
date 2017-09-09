AdvanceToNextPopcornRow:
  DEC ActivePopcornRow
  LDX #15
  STX ActivePopcornIndex
  LDX #$00
  STX ActivePopcornCount
  JSR InitializeActivePopcornRow
  JMP ShuffleActivePopcornPosits


InitializeActivePopcornRow:
  ; States
  ; Positions
  ; Columns
  LDX #00
InitializeActivePopcornRowLoop:
  LDA #PopcornState_Background
  STA ActivePopcornStates, X
  LDY ActivePopcornRow
  LDA PopcornSpriteStartY, Y
  STA ActivePopcornPositions, X
  TXA
  STA ActivePopcornColumns, X
  INX
  CPX #15
  BNE InitializeActivePopcornRowLoop
  RTS


ShuffleActivePopcornPosits:
  LDA #00
ShuffleActivePopcornPositsLoop:
  PHA
  JSR GenerateRandomNumber
  LDA RandomNumber
GetValidRandomNumber:
  CMP #15
  BLT GotValidRandomNumber
  SEC
  SBC #14
  JMP GetValidRandomNumber
GotValidRandomNumber:
  TAY
  PLA
  TAX ; X and Y hold the indexes of the two Position elements to swap
  LDA ActivePopcornColumns, X
  STA Temp
  LDA ActivePopcornColumns, Y
  STA ActivePopcornColumns, X
  LDA Temp
  STA ActivePopcornColumns, Y

  TXA
  CLC
  ADC #01
  CMP #15
  BNE ShuffleActivePopcornPositsLoop
  RTS
