#Copyright (c) 2026 Serguei Kouzmine
#
#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:
#
#The above copyright notice and this permission notice shall be included in
#all copies or substantial portions of the Software.
#
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#THE SOFTWARE.

param(
  [string] $filename,
  [int] $chunk_size = 256,
  [string] $url = 'http://localhost:8080/api/upload',
  [bool] $debug = $true
)

function sendfile {
  param(
    [byte[]] $data,
    [int] $chunk_size = 256,
    [int] $offset = 0,
    [string]$url = 'http://localhost:8085/basic/upload',
    [bool] $debug = $true
  )


  if ($data.length -eq $offset) {
    return $false	
  }
  if ($chunk_size -gt $data.length - $offset) {
    $chunk_size = $data.length - $offset
  }
  [byte[]] $payload = [byte[]]::new($chunk_size)
  [System.Array]::Copy($data, $offset, $payload, 0, $chunk_size)
  if ($debug) {
    write-host ('payload:' + [char]10 + [System.Text.Encoding]::GetEncoding('UTF-8').GetString($payload))
  }

  $request = [System.Net.HttpWebRequest]( [System.Net.WebRequest]::Create($url) )

  $request.Method = 'PATCH'

  $request.Headers.Add('Tus-Resumable','1.0.0')
  $request.Headers.Add('Upload-Offset', $offset )

  $request.ContentType = 'application/offset+octet-stream'
  $request.ContentLength = $payload.Length

  $stream = $request.GetRequestStream()
  $stream.Write($payload,0,$payload.Length)
  $stream.Flush()
  $stream.Close()
  try {

    $response = $request.GetResponse()
    [System.Net.HttpStatusCode]$statuscode = $response.StatusCode
    if ($debug)  {
      write-host ('Response status code: {0}' -f $statuscode.value__)
    }
    if (($statusCode.value__ -eq 204)){
      [System.Net.WebHeaderCollection]$response_headers = $response.Headers
      $result = $response_headers['Upload-Offset']
      if ($debug)  {
       write-host ('Response: Upload-Offset: {0}' -f $result )
       # write-host ($response.Headers -join [char]10)
       # TUS responses are mostly communicated via headers:
       # $response.Headers.AllKeys | foreach-object { write-host ('{0} = {1}' -f $_ , $response.Headers[$_]) }
      }
    }
  } catch [System.Net.WebException] {

    $e = $_.Exception
    write-host $e.Status

    if ($e.Response -ne $null) {
        $reader =  new-object System.IO.StreamReader( $e.Response.GetResponseStream() )
        write-host $reader.ReadToEnd()
        $reader.Close()
    }
    return $false
  }
  return $true
}

function getHead{
  param(
    [string]$url = 'http://localhost:8085/basic/upload',
    [bool] $debug = $true
  )
  $result = $null
  [System.Net.HttpWebRequest] $request = [System.Net.HttpWebRequest]( [System.Net.WebRequest]::Create($url) )

   $request.Method = 'HEAD'

  $request.Headers.Add('Tus-Resumable','1.0.0')
  $request.Headers.Add('Upload-Offset','0')

  try {

    $response = $request.GetResponse()
    [System.Net.HttpStatusCode]$statuscode = $response.StatusCode
    if ($debug)  {
      write-host ('Response status code: {0}' -f $statuscode.value__)
    }
    if (($statusCode.value__ -eq 204)){
      [System.Net.WebHeaderCollection]$response_headers = $response.Headers
      $result = $response_headers['Upload-Offset']
      if ($debug)  {
       write-host ('Response: Upload-Offset: {0}' -f $result )
       # write-host ($response.Headers -join [char]10)
	   # TUS responses are mostly communicated via headers:
	   # $response.Headers.AllKeys | foreach-object { write-host ('{0} = {1}' -f $_ , $response.Headers[$_]) }
      }
    }
  } catch [System.Net.WebException] {

    $e = $_.Exception
    write-host $e.Status

    if ($e.Response -ne $null) {
        $reader =  new-object System.IO.StreamReader( $e.Response.GetResponseStream() )
        write-host $reader.ReadToEnd()
        $reader.Close()
    }
  }
  return $result
}
function getPayload{
  param (
    [string]$file_path = $null,
    [bool]$debug = $false
  )
  [byte[]]$payload = $null
  if(($file_path -ne $null ) -and ($file_path -ne '')){
    $payload = [System.IO.File]::ReadAllBytes($file_path)
  }
  if ($debug){
    write-host ('full payload:' + [char]10 + [System.Text.Encoding]::GetEncoding('UTF-8').GetString($payload))
  }
  # NOTE: need to truncate console log for large files
  return $payload
}


function getLocation{
  param(
    [string]$url = 'http://localhost:8085/basic/upload',
    [bool] $debug = $true
  )

  $webRequest = [System.Net.WebRequest]::Create($url)
  $webRequest.Method = 'POST'
  $headers = @{ 'Tus-Resumable' = '1.0.0'; 'Upload-Defer-Length' = '1' }

  [System.Collections.Specialized.NameValueCollection] $obj = new-object System.Collections.Specialized.NameValueCollection
  $headers.Keys | foreach-object { $key = $_; $value = $headers.Item($key ) ; $obj.Add($key, $value) }

  $webRequest.Headers.Add($obj)
  try {
	# https://learn.microsoft.com/en-us/dotnet/api/system.net.webresponse?view=netframework-4.5
    [System.Net.WebResponse] $response =  $webRequest.GetResponse()
    [System.Net.HttpStatusCode]$statuscode = $response.StatusCode
    if ($debug)  {
      write-host ('Response status code: {0}' -f $statuscode.value__)
    }
    if (($statusCode.value__ -eq 200) -or ($statusCode.value__ -eq 201)){
      [System.Net.WebHeaderCollection]$response_headers = $response.Headers
      $result = $response_headers['Location']
      if ($debug)  {
        write-host ('Response:{1}{0}' -f $result, [char]10 )
      }
    }
  } catch [Exception] {
     # System.Management.Automation.ErrorRecord -> System.Net.WebException
     $e = $_[0].Exception
     write-host ('Exception:{3}Status: {0}{3}StatusCode: {1}{3}Message: {2}' -f  $e.Status, $e.Response.StatusCode, $e.Message,[char]10 )
  } finally {
    if ($response -ne $null){
      $response.Dispose()
    }
  }
  return $result
}

function finalize {
  param(
    [string]$url = 'http://localhost:8080/api/uploads/finalize',
    [string]$uploadId = $null,
    [bool] $debug = $true
  )

  [String]$payload = @{'uploadId' = $uploadId } | convertto-json
  if ($debug) {
    write-host ('connecting to {0}' -f $url)
  }
  $webRequest = [System.Net.WebRequest]::Create($url)
  $webRequest.Method = 'POST'

  # https://learn.microsoft.com/en-us/dotnet/api/system.net.webrequest.contenttype?view=netframework-4.5
  $webRequest.ContentType = 'application/json'

  [System.IO.Stream]$requestStream = $webRequest.GetRequestStream()
  [byte[]]$payloadBytes = [System.Text.Encoding]::GetEncoding('ASCII').GetBytes($payload)
  $requestStream.Write($payloadBytes, 0, $payloadBytes.Length)
  $requestStream.Close()
  try {
    # https://learn.microsoft.com/en-us/dotnet/api/system.net.webresponse?view=netframework-4.5
    [System.Net.WebResponse] $response =  $webRequest.GetResponse()
    [System.Net.HttpStatusCode]$statuscode = $response.StatusCode
    if ($debug)  {
      write-host ('Response status code: {0}' -f $statuscode.value__)
    }
    [System.IO.StreamReader] $responseStream = new-object System.IO.StreamReader($response.GetResponseStream())
    $result = $responseStream.ReadToEnd()
    $responseStream.Close()
    if ($debug)  {
      write-host ('Response:{1}{0}' -f $result, [char]10 )
    }
  } catch [Exception] {
     # System.Management.Automation.ErrorRecord -> System.Net.WebException
     $e = $_[0].Exception
     write-host ('Exception:{3}Status: {0}{3}StatusCode: {1}{3}Message: {2}' -f  $e.Status, $e.Response.StatusCode, $e.Message,[char]10 )
  } finally {
    if ($response -ne $null){
      $response.Dispose()
    }
  }
  return $result
}

function validate {
  param(
    [string]$url = 'http://localhost:8080/api/uploads/validate',
    [string]$uploadId = $null,
    [string]$hash = $null,
    [bool] $debug = $true
  )

  [String]$payload = @{'uploadId' = $uploadId; 'hash' = $hash } | convertto-json
  if ($debug) {
    write-host ('connecting to {0}' -f $url)
  }
  $webRequest = [System.Net.WebRequest]::Create($url)
  $webRequest.Method = 'POST'

  # https://learn.microsoft.com/en-us/dotnet/api/system.net.webrequest.contenttype?view=netframework-4.5
  $webRequest.ContentType = 'application/json'
  [System.IO.Stream]$requestStream = $webRequest.GetRequestStream()
  [byte[]]$payloadBytes = [System.Text.Encoding]::GetEncoding('ASCII').GetBytes($payload)
  $requestStream.Write($payloadBytes, 0, $payloadBytes.Length)
  $requestStream.Close()
  try {
	# https://learn.microsoft.com/en-us/dotnet/api/system.net.webresponse?view=netframework-4.5
    [System.Net.WebResponse] $response =  $webRequest.GetResponse()
    [System.Net.HttpStatusCode]$statuscode = $response.StatusCode
    if ($debug)  {
      write-host ('Response status code: {0}' -f $statuscode.value__)
    }
    [System.IO.StreamReader] $responseStream = new-object System.IO.StreamReader($response.GetResponseStream())
    $result = $responseStream.ReadToEnd()
    $responseStream.Close()
    if ($debug)  {
      write-host ('Response:{1}{0}' -f $result, [char]10 )
    }
  } catch [Exception] {
     # System.Management.Automation.ErrorRecord -> System.Net.WebException
     $e = $_[0].Exception
     write-host ('Exception:{3}Status: {0}{3}StatusCode: {1}{3}Message: {2}' -f  $e.Status, $e.Response.StatusCode, $e.Message,[char]10 )
  } finally {
    if ($response -ne $null){
      $response.Dispose()
    }
  }
  return $result
}

function checksum_file {
  param(
    [string]$file_path
  )
  # https://learn.microsoft.com/en-us/dotnet/api/system.security.cryptography.sha256cryptoserviceprovider?view=netframework-4.5
  # https://learn-powershell.net/2013/03/25/use-powershell-to-calculate-the-hash-of-a-file/
  return ( [System.BitConverter]::ToString((new-object -TypeName 'System.Security.Cryptography.SHA256CryptoServiceProvider').ComputeHash([System.IO.File]::ReadAllBytes((get-item -path $file_path).FullName))) -replace '-', '' )
}


# https://learn.microsoft.com/en-us/dotnet/api/system.security.cryptography.sha256cryptoserviceprovider?view=netframework-4.5
[System.Net.ServicePointManager]::ServerCertificateValidationCallback = { $true }


write-host 'Getting upload location'
$location = getLocation -debug $debug -url $url
$uploadId = ($location -replace '^.+/', '')
$url = 'http://localhost:8080' + $location
$file_path = (resolve-path $filename)
[byte[]]$data = getPayload -file_path $file_path -debug $debug

$offset  = 0
$status = $true
while ($status) {
  if ($debug) {
    write-host ('send the {0} bytes to {1}' -f $chunk_size, $url )
  }
  $status = sendfile -url $url -data $data -debug $debug -offset $offset
  $offset = getHead -url $url -debug $debug

  if ($debug) {
    write-host ('new offset: {0} length: {1}' -f $offset, $data.Length )
  }
}
write-host 'Done'
start-sleep -millisecond 1000
finalize -uploadId $uploadId
validate -uploadId $uploadId -hash (checksum_file -file_path $file_path)

