NmiInterruptHandler:
  PHA                               ; back up registers (important)
  TXA
  PHA
  TYA
  PHA

  LDA NmiNeedDma
  BEQ DoneNeedDma
  LDA #0                            ; do sprite DMA
  STA $2003                         ; conditional via the 'needdma' flag
  LDA #>PpuSpriteBuffer
  STA $4014
  DEC NmiNeedDma
DoneNeedDma:

  LDA NmiNeedDraw                   ; do other PPU drawing (NT/Palette/whathaveyou)
  BEQ DoneNeedDraw                  ; conditional via the 'needdraw' flag
  BIT $2002                         ; clear VBl flag, reset $2005/$2006 toggle
  JSR XferDrawingsToPpu             ; draw the stuff from the drawing buffer
  DEC NmiNeedDraw
DoneNeedDraw:

  ;LDA NmiNeedPpuRegistersUpdated
  ;BEQ DoneNeedRegisters
  LDA Ppu2001Buffer                 ; copy buffered $2000/$2001 (conditional via needppureg)
  STA $2001
  LDA Ppu2000Buffer
  STA $2000

  BIT $2002
  LDA PpuScrollXBuffer              ; set X/Y scroll (conditional via needppureg)
  STA $2005
  LDA PpuScrollYBuffer
  STA $2005
  ;DEC NmiNeedPpuRegistersUpdated
DoneNeedRegisters:

  JSR GenerateRandomNumber

  LDA #0                            ; clear the Main Waiting flag so that Main will continue
  STA IsMainWaitingForNmiToFinish   ; note that you should not 'dec' here, as it might
                                    ; already be zero (will be the case during slowdown)

  PLA                               ; restore regs and exit
  TAY
  PLA
  TAX
  PLA
  RTI
