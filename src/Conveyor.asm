UpdateConveyor:
  LDX ConveyorFrameCount
  INX
  CPX #ConveyorFrameSpeed
  BGE AdvanceConveyor
  STX ConveyorFrameCount
  RTS
AdvanceConveyor:
  LDX #$00
  STX ConveyorFrameCount
  LDX ConveyorTile
  INX
  CPX #ConveyorLastTile + 1
  BNE UpdateConveyorContinue
  LDX #ConveyorFirstTile
UpdateConveyorContinue:
  STX ConveyorTile
  JSR BufferConveyor
  RTS

BufferConveyor:
  ;   Byte 0  = length                             
  ;   Byte 1  = high byte of the PPU address       
  ;   Byte 2  = low byte of the PPU address        
  ;   Byte 3  = reserved for now
  ;   Byte 4+ = {length} bytes
  JSR SetXToNextDrawingBuffer
  LDA #$20
  STA PpuDrawingBuffer, X
  INX
  LDA #>ConveyorBackgroundPpuAddress
  STA PpuDrawingBuffer, X
  INX
  LDA #<ConveyorBackgroundPpuAddress
  STA PpuDrawingBuffer, X
  INX
  INX
  LDY #$20
  LDA ConveyorTile
BufferConveyorLoop:
  STA PpuDrawingBuffer, X
  INX
  DEY
  BNE BufferConveyorLoop
  LDA #$00
  STA PpuDrawingBuffer, X;  Flag end of buffers
  LDA #$01
  STA NmiNeedDraw
  RTS
