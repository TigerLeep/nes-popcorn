.include "macros.inc"
.include "NesHeader.inc"
.include "constants.inc"
.include "zeropage.variables.inc"

.segment "STARTUP"
.segment "CODE"

.include "ResetInterruptHandler.asm"
.include "NmiInterruptHandler.asm"
.include "XferDrawingsToPpu.asm"
.include "Main.asm"
.include "DisableRendering.asm"
.include "EnableRendering.asm"
.include "WaitForVBlank.asm"
.include "InitializeVariables.asm"
.include "InitializePopcorn.asm"
.include "ClearMemory.asm"
.include "ClearSprites.asm"
.include "LoadPalettes.asm"
.include "LoadInitialPaddleSprites.asm"
.include "LoadTestPopcornSprite.asm"
.include "AdvanceTestPopcorn.asm"
.include "SwitchToGameState.asm"
.include "LoadBackground.asm"
.include "ReadControllers.asm"
.include "UpdatePaddles.asm"
.include "IncreaseLevel.asm"
.include "Conveyor.asm"
.include "AdvanceToNextPopcornRow.asm"
.include "StartNextPopcornDropping.asm"
.include "SetXToNextDrawingBuffer.asm"
.include "GenerateRandomNumber.asm"
.include "WaitForNextNmiToFinish.asm"

AdvancePopcornDropping:
  ; Need to rethink whole popcorn-dropping design.  Popcorns from next row can start dropping
  ; before current row finishes falling and at a slightly faster speed.  So we need to track
  ; speed per Active Popcorn, and track more than just the current row.
  ; 
  ; Speed based on level.  Speed increases by 1/8 pixel per frame per level.  Need to track
  ; level for each Active Popcorn.
  ; 
  ; Active Popcorn needs to be dynamic in size. Only holds Popcorn waiting to fall in current
  ; row Popcorn moves from Active Popcorn list to Falling Popcorn list when it starts falling.
  ; When last popcorn in Active Popcorn list moves to Falling list, refill Active Popcorn from
  ; next row.  These can start falling while there are still Popcorn from previous row in the
  ; Falling Popcorn list.  Active Popcorn list will always be Popcorn of same level (speed).
  ; Falling Popcorn list can have Popcorn of different levels (speeds) in it.
  ; 
  ; Every 40 frames (should be based on a base speed adjusted by row next popcorn drops from)
  ; we start next popcorn falling.
  ; 
  ; Before dropping the next popcorn, we need to check if any popcorns are left.  If not, Stop
  ; dropping popcorns.  Once the last popcorn is caught, reset play and adjust speeds as
  ; appropriate.
  ; 
  ; If popcorn is missed and hits conveyor, clear all Falling popcorn sprites and continue
  ; animating Conveyor popcorn until off screen.  Then decrease # of paddles and reset play
  ; adjusting speed as appropriate.
  ; 
  ; Need to keep track of where active Popcorn sprites are in PpuSpriteBuffer so we can update
  ; their Y (or X if on conveyor).
  ; 
  ; Need to control speed of popcorn.
  ; 1/8 pixel per frame per level
  ; Start Level 0 at 1 pixel per frame
  ; Level 1+: Increase pixels per 1st, 3rd, 5th, 7th, 2nd, 4th, 6th, 8th frame.   Repeat.
  ; Level  0 = 1 1 1 1 1 1 1 1
  ; Level  1 = 2 1 1 1 1 1 1 1
  ; Level  2 = 2 1 2 1 1 1 1 1
  ; Level  3 = 2 1 2 1 2 1 1 1
  ; Level  4 = 2 1 2 1 2 1 2 1
  ; Level  5 = 2 2 2 1 2 1 2 1
  ; Level  6 = 2 2 2 2 2 1 2 1
  ; Level  7 = 2 2 2 2 2 2 2 1
  ; Level  8 = 2 2 2 2 2 2 2 2
  ; Level  9 = 3 2 2 2 2 2 2 2
  ; Level 10 = 3 2 3 2 2 2 2 2
  ; Level 11 = 3 2 3 2 3 2 2 2
  ; Level 12 = 3 2 3 2 3 2 3 2
  ; Level 13 = 3 3 3 2 3 2 3 2
  ; Level 14 = 3 3 3 3 3 2 3 2
  ; Level 15 = 3 3 3 3 3 3 3 2
  ; Level 16 = 3 3 3 3 3 3 3 3
  ; Subtract 8 until cant.  # of subtractions +1 initializes all 8 numbers.
  ; Follow algorithm with remainder to get 8 numbers for current level.
  RTS


.segment "RODATA"
.include "Backgrounds.inc"
.include "Palettes.inc"
.include "PaddleSprites.inc"
.include "LookupTables.inc"


.segment "VECTORS"
	.word	NmiInterruptHandler   ; When VBlank begins
	.word	ResetInterruptHandler ; When the processor first turns on or is reset
	.word	0                     ; External interrupt IRQ is not used
  
  
.segment "CHARS"
  .incbin "popcorn.chr"   ;includes 8KB graphics file
