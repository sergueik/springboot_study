#Copyright (c) 2022 Serguei Kouzmine
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
  [switch]$debug,
  [string]$url = 'http://localhost:8080/data'
)


function get_data {
  param(
    [string]$url = 'http://localhost:8080/data',

    # currently unused - kept for possible future use
    [System.Collections.Hashtable]$params = @{
      'operation' = 'send';
      'param'   = 'data';
    },

    # currently unused - kept for possible future use
    [System.Collections.Hashtable]$headers = @{ },
    [bool]$debug = $false
  )
  if ($debug) {
    write-host ('POST to {0}' -f $url)
  }
  [string]$result = $null
  [System.Net.ServicePointManager]::ServerCertificateValidationCallback = { $true }

  $webRequest = [System.Net.WebRequest]::Create($url)
  $webRequest.Method = 'GET'
  $content_type = 'application/json'

  $webRequest.ContentType = $content_type

  [System.Collections.Specialized.NameValueCollection] $obj = New-Object System.Collections.Specialized.NameValueCollection
  $headers.Keys | foreach-object {
    $key = $_
    $value = $headers.Item($key)
    $obj.Add($key, $value)
  }

  # $obj.Add($auth_key, $auth_value) # custom authentication

  # $webRequest.Headers.Add($obj)
  # Write-output ("The HttpHeaders are \\n{0}" -f $webRequest.Headers )
  try {
    [System.Net.WebResponse] $response =  $webRequest.GetResponse()
    [System.Net.HttpStatusCode]$statuscode = $response.StatusCode
    $result = $statuscode # cast to String
    if ($debug)  {
      write-host ('Response status code: {0}/{1}' -f $statuscode.value__,$result)
    }
    if ($debug)  {
      write-host ('Reading response')
    }
    if ($statusCode.value__ -eq 200){
      [System.IO.StreamReader] $responseStream = new-object System.IO.StreamReader($response.GetResponseStream())
      $result = $responseStream.ReadToEnd()
      $responseStream.Close()
      if ($debug)  {
        write-host ('Response:{0}{1}' -f [char]10, $result )
      }
    }

   } catch [Exception] {
     # System.Management.Automation.ErrorRecord -> System.Net.WebException
     $exception = $_[0].Exception
     write-host ('Exception:{3}Status: {0}{3}StatusCode: {1}{3}Message: {2}' -f  $exception.Status, $exception.Response.StatusCode, $exception.Message,[char]10 )
    $result = $exception.Status
  } finally {
    if ($response -ne $null){
      $response.Dispose()
    }
  }
  return $result
}

[bool]$debug_flag = [bool]$psboundparameters['debug'].ispresent

# NOTE: 'data' is reserved word. If named function just 'data' get run time error
# The "url" parameter of the Data section is not valid. The valid Data section
# parameter is SupportedCommand.

$result = get_data -url $url -debug $debug_flag
if ($debug_flag) {
  write-output ('The result is:{1}{0}' -f $result,[char]10)
}

# possible result values:
# ConnectFailure
# ProtocolError
# raw content: 
# example.runner.CustomApplicationRunner@6f95cd51
# Also try
# data.ps1 debug -url http://localhost:8080/json
# data.ps1 -debug -url http://localhost:8080/dummyData
