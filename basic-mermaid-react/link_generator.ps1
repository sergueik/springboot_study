function Create-Shortcut {
  param (
    [string]$Source,
    [string]$DestinationLnk,
    [string]$IconLocation = (resolve-path -path '.').path + '\launch-resource.dll,0' ,
    [string]$Arguments
  )

  begin {
    $WshShell = New-Object -ComObject WScript.Shell
  }

  process {
    if (!$Source) {Throw 'No Source'}
    if (!$DestinationLnk) {Throw 'No DestinationLnk'}
    # TODO: the file extensions in the file path string passed to `CreateShortcut` *must* end in 
    # `.lnk` or `.url`, otherwise the method will fail

    $Shortcut = $WshShell.CreateShortcut($DestinationLnk)
    $Shortcut.TargetPath = $Source
    write-host $Shortcut.IconLocation
    $Shortcut.IconLocation = $IconLocation 
    if ($Arguments) {
      $Shortcut.Arguments = $Arguments
    }
    $Shortcut.Save()
  }

  end {
    function Release-Ref ($ref) {
      ([System.Runtime.InteropServices.Marshal]::ReleaseComObject([System.__ComObject]$ref) -gt 0)
      [System.GC]::Collect()
      [System.GC]::WaitForPendingFinalizers()
    }
    $Shortcut, $WshShell | % {$null = Release-Ref $_}
  }
}


$filepath = (resolve-path -path 'dist\index.html').path
[Uri] $fileUri = new-object Uri($filepath)
new-item -path 'launch.lnk' -itemtype file -erroraction silentlycontinue
$destination = (resolve-path -path 'launch.lnk').path
$arguments =  ('--user-data-dir=C:\temp\chrome-file-test --allow-file-access-from-files {0}' -f $fileUri.AbsoluteUri)
$key = 'HKLM:\SOFTWARE\Classes\ChromeHTML\shell\open\command'
$source =  (Get-Item -Path $key).GetValue('') -replace ' *--.*', ''
Create-Shortcut -Source $Source -Arguments "$arguments" -DestinationLnk "$destination" -Verbose

# [enum]::GetValues([Environment+SpecialFolder])

$desktop_directory = [Environment]::GetFolderPath( [Environment+SpecialFolder]::DesktopDirectory )

if ([string]::IsNullOrEmpty($desktop_directory)) {
  throw 'Cannot resolve user Desktop directory'
}

if (-not (Test-Path -LiteralPath $desktop_directory -PathType Container)) {
  throw "Desktop directory is unavailable: $desktop_directory"
}

$desktop_link = Join-Path $desktop_directory 'launch.lnk'

Create-Shortcut -Source $destination -DestinationLnk $desktop_link
