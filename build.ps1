New-Item -ItemType Directory -Force -Path .\obj\ | Out-Null
New-Item -ItemType Directory -Force -Path .\bin\ | Out-Null
ca65 -g -o .\obj\popcorn.o -t nes -v .\src\popcorn.asm
ld65.exe --config .\nes.cfg --mapfile .\popcorn.map -vm -Ln .\popcorn.lbl --obj .\obj\popcorn.o -o .\bin\popcorn.nes
$rows = (Get-Content .\popcorn.lbl) -replace '^al 00(....) \.(.*)$', '$1#$2#' -replace '^', '$'
$rows = $rows | Select-String -Pattern '__' -NotMatch | % { $_.Line }
$rows = $rows | Sort-Object
$rows | Out-File .\bin\popcorn.nes.0.nl -Encoding ascii