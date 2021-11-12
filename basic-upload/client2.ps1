# origin: https://stackoverflow.com/questions/36268925/powershell-invoke-restmethod-multipart-form-payload
# see also: https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
# http://blog.majcica.com/2016/01/13/powershell-tips-and-tricks-multipartform-data-requests/
# https://lwp.interglacial.com/ch05_07.htm
# https://www.codeproject.com/Articles/17449/Send-a-content-type-multipart-form-data-request-fr CustomWebRequest.cs

param(
  [string]$datafile = (resolve-path 'data.txt'),
  [switch]$print,
  [switch]$debug,
  [string]$url = 'http://localhost:8085/basic/upload'
)

function sendfile {
  param(
    [string]$filePath = (resolve-path 'data.txt'),
    [string]$boundary = [System.Guid]::NewGuid().ToString(),
    [string]$url = 'http://localhost:8085/basic/upload',
    [bool]$debug = $false,
    [System.Collections.Hashtable]$params = @{
      'operation' = 'send';
      'param'   = 'data';
    }
  )
  $date = get-date -format 'yyyy-MM-dd HH:mm'
  $filename = ($filePath -replace '^.*\\', '') + '_' + ($date -replace '[\-: ]', '_')
  $payload = [System.Text.Encoding]::GetEncoding('UTF-8').GetString([System.IO.File]::ReadAllBytes($filePath))

  $LF = "`r`n";
  $B = '--' + $boundary
  $body_lines = @()
  $body_lines += $B
  $params.keys | foreach-object {
    $key = $_
    $val = $params.Item($key)
    $body_lines += ('Content-Disposition: form-data; name="{0}"' -f $key)
    $body_lines += ''
    $body_lines += $val
    $body_lines += $B
  }
  $body_lines += ('Content-Disposition: form-data; name="file"; filename="{0}"' -f $filename)
  $body_lines += 'Content-Type: application/octet-stream'
  $body_lines += ''
  $body_lines += $payload
  $body_lines += $B + '--'
  $body_lines += ''
  $body = $body_lines -join $LF

  # NOTE: Powershell does not allow dash in variables names
  $content_type = ('multipart/form-payload; boundary="{0}"' -f $boundary)
  if ($debug)  {
    write-host ('invoke-restmethod -uri {0} -method Post -contenttype "{1}" -body {2}' -f $uri, $content_type, [char]10 + $body)
  }
  # quotes aroung content_type arguments are optional

  $result = invoke-restmethod -uri $URL -method Post -contenttype "$content_type" -body $body
  return $result
}
[bool]$debug_flag = [bool]$psboundparameters['debug'].ispresent
$result = sendfile -url $url -filePath $datafile -debug $debug_flag
# NOTE: the following code may only be correct processing with WCF derived REST services that favor XML
if ([bool]$psboundparameters['print'].ispresent) {
  write-output ('The sendfile result is: {0}' -f $result)
  if ($result.html -ne $null){
    write-output $result.SelectSingleNode('//*/text()').payload
  }
}
