; Input data has the following format:           
;   Byte 0  = length                             
;   Byte 1  = high byte of the PPU address       
;   Byte 2  = low byte of the PPU address        
;   Byte 3  = reserved for now
;   Byte 4+ = {length} bytes                     
;                                                
; Repeat until length == 0 is found.             
XferDrawingsToPpu:
  LDX #$00
  LDA $2002                 ; read PPU status to reset the high/low latch

XferDrawingsToPpuLoop:
  LDY PpuDrawingBuffer, X   ; load the length of the data to the Y register
  BEQ DoneXferDrawingsToPpu ; length equal 0 means that the drawing is done  
  
  INX                       ; X = 1
  LDA PpuDrawingBuffer, X   ; load the high byte of the target address
  STA $2006                 ; write the high byte to PPU
  
  INX                       ; X = 2
  LDA PpuDrawingBuffer, X   ; load the low byte of the target address
  STA $2006                 ; write the low byte to PPU
  
  INX                       ; X = 3 (reserved for now)
      
XferDrawingToPpuLoop:
  INX                       ; increment X so it points to the next byte
  LDA PpuDrawingBuffer, X   ; load a byte of the data
  STA $2007                 ; write it to PPU
  DEY                       ; decrement Y
  BNE XferDrawingToPpuLoop  ; if Y != 0 jump to .setLoop
    
  INX                       ; increment X so it points to the next segment      
  JMP XferDrawingsToPpuLoop ; jump back to .drawLoop
 
DoneXferDrawingsToPpu:
  LDA #$00
  STA PpuDrawingBuffer ; Reset buffer
  RTS
