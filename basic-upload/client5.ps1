# origin: https://qna.habr.com/q/1116090 (in Russian)

param(
  [string]$fieldName = 'file'
  [string]$filePath = 'C:\Temp\test.pdf'
  $url = 'http://posttestserver.com/post.php'
)
Add-Type -AssemblyName System.Web
Add-Type -AssemblyName System.Net.Http

# $uri = "https://api.telegram.org/bot$code/sendDocument"
$form = New-Object System.Net.Http.MultipartFormDataContent
$chatId = New-Object System.Net.Http.StringContent $Chat_ID
$FileName = "D:\mydoc.txt"

$form.Add($chatId, 'chat_id')
$fileContent = Get-Content $FileName -Encoding Byte
$byteContent = New-Object System.Net.Http.ByteArrayContent ($fileContent, 0, $fileContent.Length)

$byteContent.Headers.Add('Content-Type',[System.Web.MimeMapping]::GetMimeMapping($FileName))
$form.Add($byteContent, 'document', (Split-Path $filename -Leaf))

$ms = New-Object System.IO.MemoryStream
$ca = $form.CopyToAsync($ms)
$ca.Wait()

Invoke-WebRequest -Method Post -Body $ms.ToArray() -Uri $uri -ContentType $form.Headers.ContentType.ToString()

