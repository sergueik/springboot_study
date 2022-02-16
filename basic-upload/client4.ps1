# origin: https://stackoverflow.com/questions/22491129/how-to-send-multipart-form-data-with-powershell-invoke-restmethod
# see also: https://qna.habr.com/q/1116090 (in Russian)

param(
  [string]$fieldName = 'file',
  [string]$filePath = "${env:USERPROFILE}\Downloads\document.pdf",
  $url = 'http://httpbin.org/post',
  [switch]$debug
)
$ErrorActionPreference = 'Stop'
try {
  add-type -AssemblyName 'System.Net.Http'

  $client = new-object System.Net.Http.HttpClient
  $content = new-object System.Net.Http.MultipartFormDataContent
  $fileStream = [System.IO.File]::OpenRead($filePath)
  $fileName = [System.IO.Path]::GetFileName($filePath)
  $fileContent = new-object System.Net.Http.StreamContent($fileStream)
  $content.Add($fileContent, $fieldName, $fileName)

  $result = $client.PostAsync($url, $content).Result
  $result.EnsureSuccessStatusCode()  | out-null
  write-host 'Done'
  if ($debug){
    $result
  }
}
catch {
  write-host 'Exception:'
  write-host $_
  exit 1
}
finally {
  if ($client -ne $null) { 
    $client.Dispose() 
  }
  if ($content -ne $null) {
    $content.Dispose() 
  }
  if ($fileStream -ne $null) { 
    $fileStream.Dispose() 
  }
  if ($fileContent -ne $null) {
    $fileContent.Dispose() 
  }
}
