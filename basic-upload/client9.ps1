param(
  [string]$foo = $null,
  [string]$bar = $null,
  [string]$content_type = 'text/plain',
  [string]$filename = $null

)

function Get-FileName {

  param (
    [string]$initialDirectory
  )
    
   [System.Reflection.Assembly]::LoadWithPartialName('System.windows.forms') | out-null
  $o = new-object System.Windows.Forms.OpenFileDialog
  $o.InitialDirectory = $initialDirectory
  $o.Filter = 'All files (*.*)| *.*'
  $o.ShowDialog() | Out-Null
  $FileName = $o.filename
  $o = $null
  return $fileName

}


if ($filename -eq $null){
  $filename = Get-FileName
}

# NOTE: corrupts binary files
# [string]$data = get-content -raw -path $filename
# write-host ('converting {0}' -f $data)
[byte[]]$bytes = [IO.File]::ReadAllBytes($filename)
[String]$contentBase64 = [Convert]::ToBase64String($bytes)
[PSCustomObject]$upload_request = @{}
$upload_request.foo = $foo
$upload_request.bar = $bar
$upload_request.filename = $filename
$upload_request.contentType = $content_type
$upload_request.contentBase64 =  $contentBase64 


$payload = (ConvertTo-Json -InputObject $upload_request -Depth 10) -join ''
write-output $payload
