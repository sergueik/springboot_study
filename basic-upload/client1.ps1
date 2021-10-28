# origin: https://newbedev.com/how-to-send-multipart-form-data-with-powershell-invoke-restmethod
# see also: https://dotsh.no/2021/04/20/powershell-uploading-files-with-multipart-form-data/

param(
  [string]$datafile = (resolve-path 'data.txt'),
  [switch]$print,
  [string]$url = 'http://localhost:8085/basic/upload'
)

function sendfile {
  param(
    [string]$filePath = (resolve-path 'data.txt'),
    [string]$url = 'http://localhost:8085/basic/upload'
  )
  $result = $null
  $ErrorActionPreference = 'Stop'
  $fieldName = 'file'
  $params = @{
    'operation' = 'send';
  }
  # NOTE: try catch everything is considered bad practice
  try {
    add-type -assemblyname 'System.Net.Http'
    # https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netframework-4.5
    $client = new-object System.Net.Http.HttpClient
    # https://docs.microsoft.com/en-us/dotnet/api/system.net.http.multipartformdatacontent?view=netframework-4.5
    $content = new-object System.Net.Http.MultipartFormDataContent
    $params.keys | foreach-object {
      $key = $_
      $val = $params.Item($key)
      # https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpcontent?view=netframework-4.5
      $content.Add((new-object System.Net.Http.StringContent($val)), $key)
    }
    $fileStream = [System.IO.File]::OpenRead($filePath)
    $fileName = [System.IO.Path]::GetFileName($filePath)
    # NOTE: Encoding ?
    $fileContent = new-object System.Net.Http.StreamContent($fileStream)
    $filename = ($filePath -replace '^.*\\', '') + '_' + ($date -replace '[\-: ]', '_')

    # https://docs.microsoft.com/en-us/dotnet/api/system.net.http.multipartcontent.add?view=netframework-4.5
    $content.Add($fileContent, $fieldName, $fileName)
    # https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient.postasync?view=netframework-4.5
    $resultObj = $client.PostAsync($url, $content).Result
    # https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpresponsemessage?view=netframework-4.5
    # https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpresponsemessage.ensuresuccessstatuscode?view=netframework-4.5
    [System.Net.Http.HttpResponseMessage]$responseMessage = $resultObj.EnsureSuccessStatusCode()
    write-host ('Status: {0}' -f $responseMessage.StatusCode)
    # https://docs.microsoft.com/en-us/dotnet/api/system.net.http.streamcontent?view=netframework-4.5
    [System.Net.Http.StreamContent]$responseContent = $responseMessage.Content
    # NOTE $responseResult is a generic System.Threading.Tasks.Task
    $responseResult = $responseContent.ReadAsStringAsync()
    $result = $responseResult.Result
    write-host ('Result: {0}' -f $result)
    # NOTE: unclear how to do it the below way, may be worth trying
    # [System.Net.Http.FormUrlEncodedContent] $arg = new-object System.Net.Http.FormUrlEncodedContent($params)
    # see also: https://www.codeproject.com/Questions/1228835/How-to-post-file-and-data-to-api-using-httpclient
  } catch {
    write-error $_
    exit 1
  } finally {
    if ($responseResult -ne $null) {
      $responseResult.Dispose()
    }
    if ($responseContent -ne $null) {
      $responseContent.Dispose()
    }
    if ($responseContent -ne $null) {
      $responseContent.Dispose()
    }
    if ($responseMessage -ne $null) {
      $responseMessage.Dispose()
    }
    if ($resultObj -ne $null) {
      $resultObj.Dispose()
    }
    if ($fileContent -ne $null) {
      $fileContent.Dispose()
    }
    if ($fileStream -ne $null) {
      $fileStream.Dispose()
    }
    if ($content -ne $null) {
      $content.Dispose()
    }
    if ($client -ne $null) {
      $client.Dispose()
    }
  }
  return $result
}

$result = sendfile -url $url -filePath $datafile
# NOTE: the following code may only be correct processing with WCF derived REST services that favor XML
if ([bool]$psboundparameters['print'].ispresent) {
  write-output ('The sendfile result is: {0}' -f $result)
  if ($result.html -ne $null){
    write-output $result.SelectSingleNode('//*/text()').payload
  }
}

