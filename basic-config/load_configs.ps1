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


param(
  # currently unused, placeholder
  [String]$list_url = "/configs/{hostname}/list?newer={last_run_epoch}",
  # server /configs/*/list response mockup
  [string]$filelist = ([System.IO.Path]::Combine((resolve-path -path '.').path,'filelist.json')),
  # currently unused, placeholder
  [String]$url = "/load/{filename}?newer={last_run_epoch}",
  [String]$local_config_path = 'configurations',
  [string]$hostname = "${env:COMPUTERNAME}".ToLower(),
  [long]$interval = 10,
  [switch]$debug
)

# origin: https://4sysops.com/archives/convert-json-to-a-powershell-hash-table/
# see also: https://github.com/sergueik/powershell_ui_samples/blob/master/external/parse_json_hash.ps1

function ConvertTo-Hashtable {
    [CmdletBinding()]
    [OutputType('hashtable')]
    param (
        [Parameter(ValueFromPipeline)]
        $InputObject
    )

    process {
        if ($null -eq $InputObject) {
            return $null
        }

        if ($InputObject -is [System.Collections.IEnumerable] -and $InputObject -isnot [string]) {
            $collection = @(
                foreach ($object in $InputObject) {
                    ConvertTo-Hashtable -InputObject $object
                }
            )
            Write-Output -NoEnumerate $collection
        } elseif ($InputObject -is [psobject]) { 
            # If the object has properties that need enumeration
            # Convert it to its own dictionary table and return it
            # convertFrom-Json produces System.Management.Automation.PSCustomObject 
            # https://stackoverflow.com/questions/14012773/difference-between-psobject-hashtable-and-pscustomobject
            # which properties enumeration is fastest through its PSObject
            $dictionary = @{}
            foreach ($property in $InputObject.PSObject.Properties) {
                $dictionary[$property.Name] = ConvertTo-Hashtable -InputObject $property.Value
            }
            $dictionary
        } else {
            # the object isn't an array, collection, or other object, it's already a dictionary table
            # so just return it.
            $InputObject
        }
    }
}

$debug_flag = [bool]$PSBoundParameters['debug'].IsPresent -bor $debug.ToBool()

$data = (Get-Content -Path $filelist) -join "`n"
  if ($debug){
     Write-host ('data: "{0}"' -f $data)
  }
$json_obj = $data | convertfrom-json
$response = $json_obj | ConvertTo-HashTable
if ( -not ($response['status'] -eq 'OK' ) ) {
   write-host ('Exit response status: {0}' -f $response['status'])
   exit
}
if ($debug){
  write-output 'Response:'
  $response | format-list
}
$result = $response['result']
$count = $result.count

if ($debug){
  write-output 'Result:'  
  $result | format-list
}
if ($count -gt 0) {
  if ($debug){
    write-host ('downloading {0} files' -f $count)
  }
  # NOTE: need to enforce precedence to avoid error
  # "because [System.Object[]] does not contain a method named 'op_Subtraction'."
  0..($count - 1)| foreach-object {
    $i = $_
    $filename = $result[$i]
    if ($debug){
      write-output ('config[{0}]:' -f $i)
    }
    write-output ('downloading file: "{0}"' -f ($result[$i]))
  }
} else { 
  write-host ('nothing to download ')
}

# https://java2blog.com/powershell-convert-epoch-time-to-datetime/

$epochTime = 1615395869
$dateTimeObject = (New-Object DateTime 1970,1,1,0,0,0).AddSeconds($epochTime)
Write-Output $dateTimeObject

($d = get-date)|out-null
$d = $d.AddMinutes((-1 *$interval))
# NOTE: the ToUnixTimeSeconds() API requires
# PSVersion 5.1.14409.1005
# will not work under
# PSVersion 5.0.10586.117
try {
 $last_run_epoch = $([DateTimeOffset]$d).ToUnixTimeSeconds() 
} catch [Exception] {
  # https://stackoverflow.com/questions/4192971/in-powershell-how-do-i-convert-datetime-to-unix-time
  $d1 = Get-Date -Date "01/01/1970"
  $last_run_epoch = [Convert]::ToInt32( (new-timespan -start $d1 -end $d).TotalSeconds )
}
# The epoch logic
# https://unix.stackexchange.com/questions/2987/how-do-i-convert-an-epoch-timestamp-to-a-human-readable-format-on-the-cli

[string]$config_path = ([System.IO.Path]::Combine((resolve-path -path '.').path,$local_config_path))

# NOTE: not -include
$count = @(get-childitem -path $config_path -filter '*.json' -erroraction silentlycontinue).count

if ($count -gt 0 ){
  $url = ( $list_url -replace '{hostname}', $hostname ) -replace '{last_run_epoch}', $last_run_epoch 
  write-host ('Will query new configurations: "{0}"' -f $url)
} else {
  $url = ( $list_url -replace '{hostname}', $hostname ) -replace '\?newer=.*$', ''
  write-host ('Will query all configurations: "{0}"' -f $url)
}
