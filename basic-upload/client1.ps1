# origin: https://newbedev.com/how-to-send-multipart-form-data-with-powershell-invoke-restmethod
# see also: https://dotsh.no/2021/04/20/powershell-uploading-files-with-multipart-form-data/


param(
  [string]$filePath = 'c:\temp\pstest.txt',
  [string]$url = 'http://localhost:8085/basic/upload'
)
$ErrorActionPreference = 'Stop'
$fieldName = 'file'
try {
    add-type -AssemblyName 'System.Net.Http'
    $client = new-object System.Net.Http.HttpClient
    $content = new-object System.Net.Http.MultipartFormDataContent
    $fileStream = [System.IO.File]::OpenRead($filePath)
    $fileName = [System.IO.Path]::GetFileName($filePath)
    $fileContent = new-object System.Net.Http.StreamContent($fileStream)
    $content.Add($fileContent, $fieldName, $fileName)

    $result = $client.PostAsync($url, $content).Result
    $result.EnsureSuccessStatusCode()
}
catch {
    Write-Error $_
    exit 1
}
Finally {
    if ($client -ne $null) { $client.Dispose() }
    if ($content -ne $null) { $content.Dispose() }
    if ($fileStream -ne $null) { $fileStream.Dispose() }
    if ($fileContent -ne $null) { $fileContent.Dispose() }
}
