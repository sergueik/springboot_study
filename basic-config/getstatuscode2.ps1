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

param (
  [string]$url = 'http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=208',
  [switch]$debug
)

# use Invoke-WebRequest cmdlet to read HTTP Status
function getHttpStatusCode {
  param(
    [string]$url,
    [bool]$debug = $false,
    [System.Collections.Hashtable]$headers = @{ }
  )
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

  $webRequest.Headers.Add($obj)
  # Write-output ("The HttpHeaders are \\n{0}" -f $webRequest.Headers )
  try {
    [System.Net.WebResponse] $response =  $webRequest.GetResponse()
    [System.Net.HttpStatusCode]$statuscode = $response.StatusCode
    # see also  discussion:
    # https://stackoverflow.com/questions/38622526/invoke-restmethod-how-do-i-get-the-return-code

    # question: how can one access the HTTP status of a successful REST call done via Invoke-RestMethod cmdlet
    # the short answer is: one can't. it returns String
    # workarounds exist when an exception occurs, then access the $_.Exception.Response.StatusCode.value__
    # https://stackoverflow.com/questions/29613572/error-handling-for-invoke-restmethod-powershell
    # one should use Invoke-WebRequest instead. it returns Microsoft.PowerShell.Commands.HtmlWebResponseObject
    # or use one of the direct C# classes in this directory
    #
    if ($debug)  {
      write-host ('Response status code: {0}' -f $statuscode.value__)
    }
    if ($debug)  {
      write-host ('Reading response')
    }
    if ($statusCode.value__ -eq 200){
      [System.IO.StreamReader] $responseStream = new-object System.IO.StreamReader($response.GetResponseStream())
      $result = $responseStream.ReadToEnd()
      $responseStream.Close()
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

[bool]$debug_flag = [bool]$psboundparameters['debug'].ispresent
$result = getHttpStatusCode -url $url -debug $debug_flag
write-output ('The sendfile result is:{1}{0}' -f $result,[char]10)
<#
 . .\getstatuscode2.ps1 -debug
Response status code: 208
Reading response
The sendfile result is:

 . .\getstatuscode2.ps1 -debug -url http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=304
Exception:
Status: ProtocolError
StatusCode: NotModified
Message: The remote server returned an error: (304) Not Modified.
The sendfile result is:

#>
