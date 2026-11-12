$url = 'https://github.com/gangstanthony/PowerShell/blob/master/Create-Shortcut.ps1'

$builder = new-object System.UriBuilder($url)
$builder.Path = $builder.Path -replace '/blob/', '/'
$builder.Host = 'raw.githubusercontent.com'
$raw_url = $builder.Uri.AbsoluteUri
# NOTE: not handling invoke-WebRequest : The remote name could not be resolved
$webResponseObject = (invoke-WebRequest -usebasicparsing -uri $raw_url)
$scriptCode = $webResponseObject.Content

$scriptCode = ('{0}{3}{1}{3}{2}' -f 'param ( [string]$Source, [string]$DestinationLnk, [string]$Arguments )', $scriptCode ,'Create-Shortcut -Source $Source -Arguments $Arguments -DestinationLnk $DestinationLnk -Verbose', [char]10 )
$scriptBlock = [scriptblock]::Create($scriptCode)
$filepath = (resolve-path -path 'dist\index.html').path
[Uri] $fileUri = new-object Uri($filepath)
new-item -path 'launch.lnk' -itemtype file -erroraction silentlycontinue
$destination = (resolve-path -path 'launch.lnk').path
$arguments =  ('--user-data-dir=C:\temp\chrome-file-test --allow-file-access-from-files {0}' -f $fileUri.AbsoluteUri)
$key = 'HKLM:\SOFTWARE\Classes\ChromeHTML\shell\open\command'
$source =  (Get-Item -Path $key).GetValue('') -replace ' *--.*', ''
& $scriptBlock -Source $source -Arguments "$arguments" -DestinationLnk "$destination" -Verbose

# [enum]::GetValues([Environment+SpecialFolder])

$desktop_directory = [Environment]::GetFolderPath( [Environment+SpecialFolder]::DesktopDirectory )

if ([string]::IsNullOrEmpty($desktop_directory)) {
  throw 'Cannot resolve user Desktop directory'
}

if (-not (Test-Path -LiteralPath $desktop_directory -PathType Container)) {
  throw "Desktop directory is unavailable: $desktop_directory"
}
# TODO update
# NOTE: the https://github.com/gangstanthony/PowerShell/blob/master/Create-Shortcut.ps1 does not handle one
# https://learn.microsoft.com/en-us/troubleshoot/windows-client/admin-development/create-desktop-shortcut-with-wsh
# Calling CreateShortcut on an existing path opens the file so you can read properties like TargetPath, Arguments, WorkingDirectory, IconLocation, and Hotkey
# changes you make to the object's properties stay in memory until the .Save method explicitly called.
# File extensions in the file path string passed to `CreateShortcut` must end in `.lnk` or `.url`, otherwise the method will fail
#
# $destination.IconLocation

        $WshShell = New-Object -ComObject WScript.Shell
 
        $Shortcut = $WshShell.CreateShortcut($DestinationLnk)
        write-host $Shortcut.IconLocation
        $Shortcut.IconLocation = (resolve-path -path 'launch.ico').path
        $Shortcut.Save()
         function Release-Ref ($ref) {
            ([System.Runtime.InteropServices.Marshal]::ReleaseComObject([System.__ComObject]$ref) -gt 0)
            [System.GC]::Collect()
            [System.GC]::WaitForPendingFinalizers()
        }
        $Shortcut, $WshShell | % {$null = Release-Ref $_}

$desktop_link = Join-Path $desktop_directory 'launch.lnk'

& $scriptBlock -Source $destination -DestinationLnk $desktop_link
