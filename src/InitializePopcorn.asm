InitializePopcorn:
  LDX #05
  STX ActivePopcornRow
  JSR AdvanceToNextPopcornRow
  RTS
