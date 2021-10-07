# origin: https://stackoverflow.com/questions/36268925/powershell-invoke-restmethod-multipart-form-data

param(
  [string]$filePath = 'c:\temp\pstest.txt',
  [string]$url = 'http://localhost:8085/basic/upload'
)

$fileBytes = [System.IO.File]::ReadAllBytes($FilePath);
$fileEnc = [System.Text.Encoding]::GetEncoding('UTF-8').GetString($fileBytes);
$boundary = [System.Guid]::NewGuid().ToString(); 
$LF = "`r`n";

$bodyLines = ( 
    "--$boundary",
    "Content-Disposition: form-data; name=`"file`"; filename=`"temp.txt`"",
    "Content-Type: application/octet-stream$LF",
    $fileEnc,
    "--$boundary--$LF" 
) -join $LF

write-output "Invoke-RestMethod -Uri $URL -Method Post -ContentType `"multipart/form-data; boundary=```"$boundary```"`" -Body $bodyLines"
Invoke-RestMethod -Uri $URL -Method Post -ContentType "multipart/form-data; boundary=`"$boundary`"" -Body $bodyLines
