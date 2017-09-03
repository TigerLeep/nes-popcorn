ResetInterruptHandler:
  LDX #$FF
  TXS
  SEI                 ; disable IRQs
  CLD                 ; disable decimal mode
  LDX #$00
  STX $2000           ; disable NMI
  JSR DisableRendering
  JSR WaitForVBlank
  JSR ClearMemory
  JSR WaitForVBlank
  JSR InitializeVariables
  LDA #%10000000      ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
  STA $2000           ; need NMI enabled now so buffers will get handled
  STA Ppu2000Buffer   ; need to also buffer so when buffers are handled, the value we just set isn't changed
  LDA #$00            ; no scrolling so always set background display position to 0,0
  STA PpuScrollXBuffer
  STA PpuScrollYBuffer
  INC NmiNeedPpuRegistersUpdated
  JSR WaitForNextNmiToFinish
  JSR LoadPalettes
  JSR SwitchToGameStateInitializeStartScreen
  JMP Main
