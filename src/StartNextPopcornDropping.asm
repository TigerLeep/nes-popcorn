StartNextPopcornDropping:
  DEC ActivePopcornIndex
  INC ActivePopcornCount
  LDX ActivePopcornIndex
  LDA #PopcornState_Falling
  STA ActivePopcornStates, X

ClearIndexPopcornFromBackground:
  LDX ActivePopcornIndex
  LDA ActivePopcornColumns, X
  ASL A
  LDY ActivePopcornRow
  CLC
  ADC PopcornBackgroundRowOffsets, Y
  STA TempPointer+1
  LDA #$20
  STA TempPointer

BufferPopcornBlanking:
  ;   Byte 0  = length                             
  ;   Byte 1  = high byte of the PPU address       
  ;   Byte 2  = low byte of the PPU address        
  ;   Byte 3  = reserved for now
  ;   Byte 4+ = {length} bytes
  JSR SetXToNextDrawingBuffer
  LDA #$02
  STA PpuDrawingBuffer, X
  INX
  LDA TempPointer
  STA PpuDrawingBuffer, X
  INX
  LDA TempPointer+1
  STA PpuDrawingBuffer, X
  INX
  INX
  LDA #BlankTile
  STA PpuDrawingBuffer, X
  INX
  STA PpuDrawingBuffer, X
  INX
  LDA #$00
  STA PpuDrawingBuffer, X;  Flag end of buffers
  LDA #$01
  STA NmiNeedDraw
  RTS
