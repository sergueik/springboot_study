# origin: https://stackoverflow.com/questions/36268925/powershell-invoke-restmethod-multipart-form-data
# see also: https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
param(
  [string]$datafile = (resolve-path 'data.txt'), 
  [switch]$print,
  [string]$url = 'http://localhost:8085/basic/upload'
)
$date = get-date -format 'yyyy-MM-dd HH:mm'
$filename = ($datafile -replace '^.*\\', '') + '_' + ($date -replace '[\-: ]', '_')

$data = [System.Text.Encoding]::GetEncoding('UTF-8').GetString([System.IO.File]::ReadAllBytes($datafile))
$boundary = [System.Guid]::NewGuid().ToString()
$LF = "`r`n";

$body = (@( 
  "--$boundary",
  "Content-Disposition: form-data; name=`"file`"; filename=`"${filename}`"",
  'Content-Type: application/octet-stream',
  '',
  $data,
  "--$boundary--" ,
 ''
) -join $LF)

# NOTE: Powershell does not allow dash in variables names 
$content_type = ('multipart/form-data; boundary="{0}"' -f $boundary)
write-output ('invoke-restmethod -uri {0} -method Post -contenttype "{1}" -body {2}' -f $uri, $content_type, "`n" + $body)
# quotes aroung content_type arguments are optional

$result = invoke-restmethod -uri $URL -method Post -contenttype "$content_type" -body $body

# NOTE: the following code may only be correct processing with WCF derived REST services that favor XML
if ([bool]$psboundparameters['print'].ispresent) {
  write-output $result.SelectSingleNode('//*/text()').Data
}

