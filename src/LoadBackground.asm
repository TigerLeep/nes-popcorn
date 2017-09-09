LoadStartBackground:
  LDA #<StartBackground
  STA BackgroundPointer
  LDA #>StartBackground
  STA BackgroundPointer + 1
  JSR LoadBackground
  RTS

LoadPlayBackground:
  LDA #<PlayBackground
  STA BackgroundPointer
  LDA #>PlayBackground
  STA BackgroundPointer + 1
  JSR LoadBackground
  RTS

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of PPU address
  LDA #$00
  STA $2006             ; write the low byte of PPU address

  LDX #$04
  LDY #$00

LoadBackgroundLoop:
  LDA (BackgroundPointer), y    ; copy one background byte from address in pointer plus Y
  STA $2007                   ; this runs 256 * 4 times
  
  INY                         ; inside loop counter
  BNE LoadBackgroundLoop      ; run the inside loop 256 times before continuing down
  
  INC BackgroundPointer + 1   ; low byte went 0 to 256, so high byte needs to be changed now
  
  DEX
  BNE LoadBackgroundLoop      ; Until X drops to #$00, we keep looping back for another 256 bytes.
  RTS
