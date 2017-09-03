New-Item -ItemType Directory -Force -Path .\obj\ | Out-Null
New-Item -ItemType Directory -Force -Path .\bin\ | Out-Null
ca65 -g -o .\obj\popcorn.o -t nes -v .\src\popcorn.asm
ld65.exe --config .\nes.cfg --mapfile .\popcorn.map -vm -Ln .\popcorn.lbl --obj .\obj\popcorn.o -o .\bin\popcorn.nes
$rows = (Get-Content .\popcorn.lbl) -replace '^al 00(....) \.(.*)$', '$1#$2#' -replace '^', '$'
$rows = $rows | Select-String -Pattern '__' -NotMatch | % { $_.Line }
$rows = $rows | Sort-Object

#for ($index = 0; $index -lt $rows.Length - 1; $index++) {
#    $currentAddress = ($rows[$index] -match '\$(....)' | % { $Matches[1] })
#    if ($nextAddress.SubString(0, 2) -eq "00") {
#        $currentAddressAsInteger = [System.Convert]::ToInt32($currentAddress, 16)
#        $nextAddress = ($rows[$index + 1] -match '\$(....)' | % { $Matches[1] })
#        $nextAddressAsInteger = [System.Convert]::ToInt32($nextAddress, 16)
#        $size = $nextAddressAsInteger - $currentAddressAsInteger
#        #echo "$currentAddress/$size"
#        $rows[$index] = "$($rows[$index].Substring(0,1))$currentAddress/$size$($rows[$index].SubString(5))"
#    }
#}

$rows | Out-File .\bin\popcorn.nes.0.nl -Encoding ascii
