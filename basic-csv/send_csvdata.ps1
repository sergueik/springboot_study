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
    [string]$filePath = (resolve-path 'data.txt'),
    [Object[]] $dataRows,
    [string]$boundary = [System.Guid]::NewGuid().ToString(),
    [string]$url = 'http://localhost:8085/basic/upload',
    [System.Collections.Hashtable]$params = @{
      'operation' = 'send';
      'param'   = 'data';
    },
    $timeout = 10,
    [bool]$debug = $false
  )
  $date = get-date -format 'yyyy-MM-dd HH:mm'
  $filename = ($filePath -replace '^.*\\', '') + '_' + ($date -replace '[\-: ]', '_')
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
  write-host ('body:' + [char]10 + $body)
  # NOTE: Powershell does not allow dash in variables names
  $content_type = ('multipart/form-data; boundary="{0}"' -f $boundary)
  if ($debug)  {
    write-host ('invoke-restmethod -uri {0} -method Post -contenttype "{1}" -body {2}' -f $uri, $content_type, [char]10 + $body)
  }
  # quotes aroung content_type arguments are optional

  try {
    # Returns the response gotten from the server (we pass it on).
    #
    $result = invoke-restmethod -uri $URL -method Post -contenttype "$content_type"  -timeoutSec $timeout -body $body
    return $result
  }
  catch [System.Net.WebException] {
    Write-Error( ('Exception: ' + $_ ))
    throw $_
  }
}
# TODO: org.apache.commons.csv.CSVRecord losing one row considering it header
$dataRows = @( 
  @{
    author = 'author';
    title = 'title';
    isbn = 'isbn';
    year = 'year';
  },
  @{
    author = 'Dan Simmons';
    title = 'Hyperion';
    isbn = '978-8576576013';
  },
  @{
    author = 'Douglas Adams'
    title = "`"The Hitchhiker's Guide to the Galaxy`"";
    year =  1979;
    isbn = '978-0345391803';
  },
  @{
    author = 'Lynne Truss';
    title = "`"Eats, Shoots and Leaves`"";
    year =  2003;
    isbn = '978-1861976123';
  }
)

[bool]$debug_flag = [bool]$psboundparameters['debug'].ispresent
$result = sendData -url $url -dataRows $dataRows -debug $debug_flag
write-output $result
