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
# [CmdletBinding()]
param(
  [parameter(Mandatory=$true,Position=2)] [System.URI] $url,
  $timeout = 10
  # ,
  # A parameter with the name 'Debug' was defined multiple times for the command
  #[switch]$debug
)
function sendData {
  param (
    [Object[]] $dataRows,
    [String] $url,
    $timeout = 10,
    [bool]$debug
  )
  $columns = @(
    'author',
    'title',
    'year',
    'isbn'
  )
  $rows = @()
  $dataRows | foreach-object { 
    $row = @()
    $dataRow = $_
    $columns | foreach-object {
      $column = $_
      if ($dataRow.ContainsKey($column)){
        $row += $dataRow[$column]
      } else {
        $row += ''    
      }
    }
  
    $rows += ($row -join ',')
  }
  $payload = $rows -join ([char]10);
  write-host $payload
  $computer = $env:COMPUTERNAME
  $boundary = [System.Guid]::NewGuid().ToString()

  $LF = "`n"
  $LF = "`r`n";
  # Note: The Multipart Content-Type protocol https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html is very precise about getting the number of line feeds correct (both CRLF or LF work).
  # pass an extra form argument to have an example
    [System.Collections.Hashtable]$params = @{
      'operation' = 'send';
      'param'   = 'data';
   }
    $params.keys | foreach-object {
      $key = $_
      $val = $params.Item($key)
      # $content.Add((new-object System.Net.Http.StringContent($val)), $key)
    }
  $param_lines = @()
  $B = '--' + $boundary
  $param_lines += $B
  $params.keys | foreach-object {
    $key = $_
    $val = $params.Item($key)
    $param_lines += ('Content-Disposition: form-data; name="{0}"' -f $key)
    $param_lines += ''
    $param_lines += $val
    $param_lines += $B
  }
  $param_part = $param_lines -join $LF
  $body = @(
    "--$boundary",
    "Content-Disposition: form-data; name=`"file`"$LF",   # filename= is optional
    $payload,
    $param_part,
#    "--$boundary",
    "Content-Disposition: form-data; name=`"computer`"$LF",
    $computer,
    "--$boundary--$LF"
    ) -join $LF
   write-host $body
  try {
    # Returns the response gotten from the server (we pass it on).
    #
    $result = invoke-RestMethod -uri $url -method POST -contentType ( 'multipart/form-data; boundary="{0}"' -f $boundary) -timeoutSec $timeout -body $body
    return $result
  }
  catch [System.Net.WebException] {
    Write-Error( ('Exception: ' + $_ ))
    throw $_
  }
}

$dataRows = @( 
  @{
    author = 'Dan Simmons';
    title = 'Hyperion';
    isbn = '978-8576576013';
  },
  @{
    author = 'Douglas Adams'
    title = "The Hitchhiker's Guide to the Galaxy";
    year =  1979;
    isbn = '978-0345391803';
  },
  @{
    author = 'Lynne Truss';
    title = "Eats, Shoots and Leaves";
    year =  2003;
    isbn = '978-1861976123';
  }
)

[bool]$debug_flag = [bool]$psboundparameters['debug'].ispresent
$result = sendData -url $url -dataRows $dataRows -debug $debug_flag
