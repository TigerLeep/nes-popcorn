WaitForVBlank:
  BIT $2002
  BPL WaitForVBlank
  RTS
