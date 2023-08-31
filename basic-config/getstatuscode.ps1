#Copyright (c) 2023 Serguei Kouzmine
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
  [string]$url = 'http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=208'
)

# use Invoke-WebRequest cmdlet to read HTTP Status
function getHttpStatusCode {
  param(
    [string]$url
  
  )
  # workaround for the error ininvoke-webrequest cmdlet:
  # the underlying connection was closed: could not establish trust relationship for the SSL/TLSsecure channel
  # see also: https://stackoverflow.com/questions/11696944/powershell-v3-invoke-webrequest-https-error
  # https://learn.microsoft.com/en-us/dotnet/api/system.net.icertificatepolicy?view=netframework-4.0
  $helper_class = 'TrustAllCertsPolicy'
  if ( -not ( $helper_class -as [type])) {
    # ICertificatePolicy Interface validates a server certificate
    # ignore self-signed certificates
    add-type @"
    using System.Net;
    using System.Security.Cryptography.X509Certificates;
    public class ${helper_class} : ICertificatePolicy {
      public bool CheckValidationResult( ServicePoint srvPoint, X509Certificate certificate, WebRequest request, int certificateProblem) {
        return true;
      }
    }
"@
# NOTE: the line above should not be indented
  }
  
  # https://www.cyberforum.ru/powershell/thread2589305.html
  [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]'Ssl3,Tls,Tls11,Tls12'
  [System.Net.ServicePointManager]::CertificatePolicy = new-object -typename $helper_class
  # alternatively define as a lambda
  # [System.Net.ServicePointManager]::ServerCertificateValidationCallback = {$true}
  $statuscode = $null
  try {
    $ProgressPreference = 'Stop'
    # NOTE : do not set to "Stop" or the status code error will abort 
    # The running command stopped because the preference variable "ProgressPreference" or common parameter is set to Stop: Reading web response
    $ProgressPreference = 'SilentlyContinue'
    $statuscode = (Invoke-WebRequest -uri $url).StatusCode
    $ProgressPreference = 'Continue'
  } catch [Exception]{
    write-host ('Exception (intercepted): {0}' -f $_.Exception.Message)
    $statuscode = -1
  }
  return  $statuscode
}

# use invoke-restmethod cmdlet to read page, but 
# NOTE: there is no way to get HTTP status with invoke-restmethod
function getPage{

  param(
    [string]$url
  
  )
  # workaround for the error invoke-restmethod cmdlet:
  # the underlying connection was closed: could not establish trust relationship for the SSL/TLSsecure channel
  # see also: https://stackoverflow.com/questions/11696944/powershell-v3-invoke-webrequest-https-error
  # https://learn.microsoft.com/en-us/dotnet/api/system.net.icertificatepolicy?view=netframework-4.0
  $helper_class = 'TrustAllCertsPolicy'
  if ( -not ( $helper_class -as [type])) {
    # ICertificatePolicy Interface validates a server certificate
    # ignore self-signed certificates
    add-type @"
    using System.Net;
    using System.Security.Cryptography.X509Certificates;
    public class ${helper_class} : ICertificatePolicy {
      public bool CheckValidationResult( ServicePoint srvPoint, X509Certificate certificate, WebRequest request, int certificateProblem) {
        return true;
      }
    }
"@
# NOTE: the line above should not be indented
  }
  
  # https://www.cyberforum.ru/powershell/thread2589305.html
  [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]'Ssl3,Tls,Tls11,Tls12'
  [System.Net.ServicePointManager]::CertificatePolicy = new-object -typename $helper_class
  # alternatively define as a lambda
  # [System.Net.ServicePointManager]::ServerCertificateValidationCallback = {$true}
  $content_type = 'application/json'
  if ($debug)  {
    write-host ('invoke-restmethod -uri {0} -method GET -contenttype "{1}"' -f $uri, $content_type)
  }
  # quotes around "content_type" argumen are optional
  
try {
  $ProgressPreference = 'SilentlyContinue'
  $page = invoke-restmethod -uri $url -method Get -contenttype "$content_type"
  $ProgressPreference = 'Continue'
} catch [Exception]{
  write-host ('Exception (intercepted): {0}' -f $_.Exception.Message)
  $page = ''
}
# undo the conversion to PSObjects done by invoke-restmethod by default
$page = $page | convertto-json
return $page
} 
$page = getPage -url $url
write-output ('Body: {0}' -f $page)
# main
$statuscode = getHttpStatusCode -url  $url
write-output ('HTTP Stasus: {0}' -f $statuscode)
# 208
<#
NOTE: the Powershell treats some(?) 30x status codes as exceptions:
  . .\getHttpStatusCode.ps1 -url http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=304
Exception (intercepted): The remote server returned an error: (304) Not Modified.
Page: ""

Most of the 30x codes are for URL Redirection.
https://www.softwaretestinghelp.com/rest-api-response-codes/
similar with curl:

 curl -sv http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=304
* Uses proxy env variable no_proxy == '192.168.99.100,192.168.99.101'
*   Trying 192.168.99.100:9090...
* Connected to 192.168.99.100 (192.168.99.100) port 9090 (#0)
> GET /cgi-bin/statuscode.cgi?code=304 HTTP/1.1
> Host: 192.168.99.100:9090
> User-Agent: curl/7.74.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 304 Not Modified
< Date: Thu, 31 Aug 2023 17:52:26 GMT
< Server: Apache/2.4.46 (Unix)



the payload is not delivered to the client when the HTTP Status Code 304 – Not Modified
similarly with HTTP Status Code 204 – No Content
#>
