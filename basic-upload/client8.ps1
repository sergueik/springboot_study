#Copyright (c) 2024 Serguei Kouzmine
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
  [string]$operation = 'send',
  [string]$param  = 'value',
  [string]$servername = 'server',
  [string]$url = 'http://localhost:8085/basic/upload',
  [switch]$debug

)


# origin: https://learn.microsoft.com/en-us/dotnet/api/system.net.webclient.uploadfile?view=netframework-4.5
# NOTE: there is also
# https://learn.microsoft.com/en-us/dotnet/api/system.net.webclient.uploaddata?view=netframework-4.5
# https://learn.microsoft.com/en-us/dotnet/api/system.net.webclient.uploadstring?view=netframework-4.5
# https://learn.microsoft.com/en-us/dotnet/api/system.net.webclient.uploadvalues?view=netframework-4.5
# and async variations

add-type -AssemblyName 'System.Net.Http'

$modified_url = $url + ('?operation={0}&param={1}&servername={2}' -f $operation, $param, $servername)
<#
NOTE: this server expects query parameters and returns
Exception calling "UploadFile" with "2" argument(s): "The remote server returned an error: (400) Bad Request."
if the "operation=send" query parameter is not provided
and when "servername" or "param" query parameter is absent or blank
#>

[byte[]]$result = (new-object System.Net.WebClient).UploadFile($modified_url, $datafile)
if ([bool]$psboundparameters['print'].ispresent) {
  # NOTE: the System.Text.Encoding.ASCII is probably extension method
  write-output ('The sendfile result is:{1}{0}' -f [System.Text.Encoding]::GetEncoding('ASCII').GetString($result, 0, $result.Count), [char]10)
}

