.segment "HEADER"
  .byte "NES", $1A  ; iNES header identifier
  .byte 1           ; 1x 16KB PRG code
  .byte 1           ; 1x  8KB CHR data
  .byte $01, $00    ; mapper 0, vertical mirroring
