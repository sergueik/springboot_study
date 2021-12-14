#Copyright (c) 2021 Serguei Kouzmine
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
    },
    [System.Collections.Hashtable]$headers = @{ }
  )
  [string]$result = $null
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
  if ($debug)  {
    write-host ('Body:{1}{0}' -f $body, [char]10)
  }

  [System.Net.ServicePointManager]::ServerCertificateValidationCallback = { $true }

  $webRequest = [System.Net.WebRequest]::Create($url)
  $webRequest.Method = 'POST'
  $content_type = ('multipart/form-payload; boundary="{0}"' -f $boundary)

  $webRequest.ContentType = $content_type

  [System.Collections.Specialized.NameValueCollection] $obj = New-Object System.Collections.Specialized.NameValueCollection
  $headers.Keys | foreach-object {
    $key = $_
    $value = $headers.Item($key)
    $obj.Add($key, $value)
  }

  # $obj.Add($auth_key, $auth_value) # custom authentication

  $webRequest.Headers.Add($obj)
  # Write-output ("The HttpHeaders are \\n{0}" -f $webRequest.Headers )
  [System.IO.Stream]$requestStream = $webRequest.GetRequestStream()
  [string]$postData = $body
  [byte[]]$postArray = [System.Text.Encoding]::GetEncoding('ASCII').GetBytes($postData)
  $requestStream.Write($postArray, 0, $postArray.Length)
  $requestStream.Close()
  try {
    [System.Net.WebResponse] $response =  $webRequest.GetResponse()
    [System.Net.HttpStatusCode]$statuscode = $response.StatusCode
    if ($debug)  {
      write-host ('Response status code: {0}' -f $statuscode.value__)
    }
    if ($debug)  {$v
      write-host ('Reading response')
    }
    if ($statusCode.value__ -eq 200){
      [System.IO.StreamReader] $responseStream = new-object System.IO.StreamReader($response.GetResponseStream())
      $result = $responseStream.ReadToEnd()
      $responseStream.Close()
      if ($debug)  {$v
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

[bool]$debug_flag = [bool]$psboundparameters['debug'].ispresent
$result = sendfile -url $url -filePath $datafile -debug $debug_flag
if ([bool]$psboundparameters['print'].ispresent) {
  write-output ('The sendfile result is:{1}{0}' -f $result,[char]10)
}
