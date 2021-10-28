# origin: https://stackoverflow.com/questions/36268925/powershell-invoke-restmethod-multipart-form-payload
# see also: https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
# http://blog.majcica.com/2016/01/13/powershell-tips-and-tricks-multipartform-data-requests/
# https://lwp.interglacial.com/ch05_07.htm
param(
  [string]$datafile = (resolve-path 'data.txt'), 
  [switch]$print,
  [string]$url = 'http://localhost:8085/basic/upload'
)

function sendfile {
  param(
    [string]$filePath = (resolve-path 'data.txt'), 
    [string]$boundary = [System.Guid]::NewGuid().ToString(),
    [string]$url = 'http://localhost:8085/basic/upload'
  )
  $date = get-date -format 'yyyy-MM-dd HH:mm'
  $filename = ($filePath -replace '^.*\\', '') + '_' + ($date -replace '[\-: ]', '_')
  $inputs = @{
    "operation" = "send";
  }
  $payload = [System.Text.Encoding]::GetEncoding('UTF-8').GetString([System.IO.File]::ReadAllBytes($filePath))

  $LF = "`r`n";

  $body = (@( 
    "--$boundary",
    "Content-Disposition: form-data; name=`"operation`"",
    '',
    'send',
    "--$boundary",
    "Content-Disposition: form-data; name=`"file`"; filename=`"${filename}`"",
    'Content-Type: application/octet-stream',
    '',
    $payload,
    "--$boundary--" ,
   ''
  ) -join $LF)

  # NOTE: Powershell does not allow dash in variables names 
  $content_type = ('multipart/form-payload; boundary="{0}"' -f $boundary)
  write-host ('invoke-restmethod -uri {0} -method Post -contenttype "{1}" -body {2}' -f $uri, $content_type, "`n" + $body)
  # quotes aroung content_type arguments are optional

  $result = invoke-restmethod -uri $URL -method Post -contenttype "$content_type" -body $body
  return $result
}
  
$result = sendfile -url $url -filePath $datafile
# NOTE: the following code may only be correct processing with WCF derived REST services that favor XML
if ([bool]$psboundparameters['print'].ispresent) {
  write-output ('result: {0}' -f $result)
  if ($result.html -ne $null){
    write-output $result.SelectSingleNode('//*/text()').payload
  }
}

