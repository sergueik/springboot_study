param(
  [string]$path = 'C:\tools\nunit\nunit-console.exe'
)
$bytes = Get-Content -Path $path -Encoding Byte -TotalCount 256
# The offset 0x3C contains the pointer to the PE header
$peOffset = [System.BitConverter]::ToUInt32($bytes, 0x3C)
# 4 bytes after 'PE\0\0' is the Machine type (offset 4 from PE header)
$machineType = [System.BitConverter]::ToUInt16($bytes, $peOffset + 4)

if ($machineType -eq 0x8664) { write-host '64-bit (x64)' }
elseif ($machineType -eq 0x014c) { write-host  '32-bit (x86)' }
elseif ($machineType -eq 0xAA64) { write-host '64-bit (ARM64)' }
else { write-host  'Unknown Machine Type: $machineType' }
