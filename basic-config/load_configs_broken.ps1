param(
  [string]$filelist = ([System.IO.Path]::Combine((resolve-path -path '.').path,'filelist.json')),
  [switch]$debug
)



# origin: https://4sysops.com/archives/convert-json-to-a-powershell-hash-table/
# see also: https://github.com/sergueik/powershell_ui_samples/blob/master/external/parse_json_hash.ps1

# NOTE: breaking change in the iterator found to lead the function to not work with a simple JSON provided below. 

<#
Found defect with coverting to hash such a minimal JSON:

{
  "status": "OK",
  "result": [
    "file1",
    "file2",
    "file4"
  ]
}

Response:

Name                           Value
----                           -----
result                         {System.Collections.Hashtable}
status                         OK

#>



function Broken-ConvertTo-Hashtable {
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
	    # https://learn.microsoft.com/en-us/dotnet/api/system.collections.ienumerable?view=netframework-4.5
	    # NOTE: cannot use 
	    # $InputObject | foreach-object... 
	    # nor 
            # $InputObject.GetEnumerator() | foreach-object { ...
	    # shortcut
               $InputObject | foreach-object { 
                    $object = $_ 
		    write-host ('object  is {0}' -f $object.gettype().Name)
                    Broken-ConvertTo-Hashtable -InputObject $object
                }
            )
            Write-Output -NoEnumerate $collection
        } elseif ($InputObject -is [psobject]) { 
            # convertFrom-Json produces System.Management.Automation.PSCustomObject 
            # https://stackoverflow.com/questions/14012773/difference-between-psobject-hashtable-and-pscustomobject#:~:text=%5BPSCustomObject%5D%20is%20a%20type%20accelerator,called%20with%20no%20constructor%20parameters.
            # which properties enumeration is fastest through its PSObject
            $dictionary = @{}
            foreach ($property in $InputObject.PSObject.Properties) {
                $dictionary[$property.Name] = Broken-ConvertTo-Hashtable -InputObject $property.Value
            }
            $dictionary
        } else {
            # the object is likely a hash table
            $InputObject
        }
    }
}
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
                # NOTE: this is a breaking change
                # $InputObject | foreach-object { 
                #     $object = $_ 
                foreach ($object in $InputObject) {
		    write-host ('object  is {0}' -f $object.gettype().Name)
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
#
write-host 'Using "Broken-ConvertTo-HashTable"'
$data = (Get-Content -Path $filelist) -join "`n"
Write-host ('data: "{0}"' -f $data)
$json_obj = $data | convertfrom-json
$response = $json_obj | Broken-ConvertTo-HashTable
if ( -not ($response['status'] -eq 'OK' ) ) {
   write-host ('Exit response status: {0}' -f $response['status'])
   exit
}
write-host 'Response:'
write-host $response

$result = $response['result']
write-host '$response["result"]:'
write-host $result


$count = $result.Count
write-host ('downloading {0} files' -f $count)

# NOTE: need to ennfoce precedence
# "because [System.Object[]] does not contain a method named 'op_Subtraction'."
0..($count - 1)| foreach-object {
  $i = $_
  $filename = $result[$i]
  write-host ('downloading config[{0}]: {1}' -f $i, ($result[$i]))
}

write-host 'Using "ConvertTo-HashTable"'
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
write-host 'Response:'
write-host $response

$result = $response['result']
write-host '$response["result"]:'
write-host $result

$count = $result.Count
write-host ('downloading {0} files' -f $count)

# NOTE: need to ennfoce precedence
# "because [System.Object[]] does not contain a method named 'op_Subtraction'."
0..($count - 1)| foreach-object {
  $i = $_
  $filename = $result[$i]
  write-host ('downloading config[{0}]: {1}' -f $i, ($result[$i]))
}

