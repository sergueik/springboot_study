# https://stackoverflow.com/questions/22770213/using-powershell-invoke-restmethod-to-post-large-binary-multipart-form-data
# https://stackoverflow.com/questions/25075010/upload-multiple-files-from-powershell-script
# states: PowerShell itself cannot do multipart form uploads
# PowerShell does not (yet) have built-in support for making 'multipart' (i.e. binary file upload compatible)
# alternatively one can pass the payload filename via -InFile
# Invoke-RestMethod -uri $uri -method POST -inFile $filePath -contentType 'multipart/form-data'

# https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-5.1
param(  
    # [parameter(Mandatory=$true,Position=1)] [ValidateScript({ Test-Path -PathType Leaf $_ })] [String] $filePath,
    [String] $filePath,
    # [parameter(Mandatory=$true,Position=2)] [System.URI] $url,
    [System.URI] $url,
    $timeout = 10,
  [switch]$debug
)

function getPayload{
  param (
    [string]$filePath = $null,
    [bool]$debug = $false
  )
  [String]$payload = ''
  if(($filePath -ne $null ) -and ($filePath -ne '')){
    [byte[]]$bytes = [IO.File]::ReadAllBytes($filePath)
    [string]$payload = [System.Text.Encoding]::GetEncoding("UTF-8").GetString($bytes)
  }
  if ($debug){
    write-host ('payload:' + [char]10 + $payload)
  }
  return $payload
}

function sendfile {
  param (
    [String] $filePath,
    [String] $url,
    $timeout = 10,
    [bool]$debug
  )
  [String]$payload = getPayload -filePath $filePath -debug $debug
  # pass an extra form argument to have an example
  $computer = $env:COMPUTERNAME
  $boundary = [System.Guid]::NewGuid().ToString()

  $LF = "`n"
  $LF = "`r`n";
  # Note: The Multipart Content-Type protocol https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html is very precise about getting the number of line feeds correct (both CRLF or LF work).
  $body = @(
    "--$boundary",
    "Content-Disposition: form-data; name=`"operation`"$LF",
    'send',
    "--$boundary",
    "Content-Disposition: form-data; name=`"param`"$LF",
    'data',
    "--$boundary",
    "Content-Disposition: form-data; name=`"servername`"$LF",
    $computer,
    "--$boundary",
    "Content-Disposition: form-data; name=`"file`"$LF",   # filename= is optional
   'Content-Type: application/octet-stream',
   '',
    $payload,
    "--$boundary--$LF"
    ) -join $LF
  # TODO: fix 'file' query parameter
  if ($debug) {
    write-host ('invoke-restmethod -uri {0} -method POST -contenttype "{1}" -body {2}' -f $uri,  ('multipart/form-data; boundary="{0}"' -f $boundary), [char]10 + $body)
  }
  try {
    # Returns the response gotten from the server (we pass it on).
    #
    $result = invoke-RestMethod -uri $url -method POST -contentType ( 'multipart/form-data; boundary="{0}"' -f $boundary) -timeoutSec $timeout -body $body
    return $result
  }
  catch [System.Net.WebException] {
    Write-Error(( 'Exception: ' + $_ ))
    throw $_
  }
}

[bool]$debug_flag = [bool]$psboundparameters['debug'].ispresent
$result = sendfile -url $url -filePath $filePath -debug $debug_flag
