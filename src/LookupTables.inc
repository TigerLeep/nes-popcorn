PopcornBackgroundRowOffsets:  ; Indexed by Row (0-4)
  .byte $21, $41, $61, $81, $A1

PopcornSpriteStartX:  ; Indexed by column (0-14)
  .byte $08, $18, $28, $38, $48, $58, $68, $78, $88, $98, $A8, $B8, $C8, $D8, $E8

PopcornSpriteStartY:  ; Indexed by Row (0-4)
  .byte $07, $0F, $17, $1F, $27

PixelsFrameIncrement: ; Lookup table for which PixelsFrame to increment for frame # mod 8 (0 - 7)
  .byte $00, $02, $04, $06, $01, $03, $05, $07
