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
  [Parameter(Mandatory = $false,Position = 1)]
  [String]$datafile = 'catalog.txt',
  [String]$templatefile = 'catalog.html',
  [String]$outputfile = 'output.html',
  [String]$datafile_filename = 'catalog-template.xlsx',
  [String[]]$fields =  @( 'Skill_Name', 'Category','Technology','Repository', 'Link','Select','GUID', 'Id'),
  [int]$count = 0
)
# NOTE: the original path remains the __source of truth__, while the derived columns are just search *aids*


[System.Collections.Hashtable]$technology = @{
    # Languages;
    'java'        = 'java';
    'kotlin'      = 'kotlin';
    'scala'       = 'scala';
    'groovy'      = 'groovy';
    'python'      = 'python';
    'py'          = 'python';
    'javascript'  = 'javascript';
    'typescript'  = 'typescript';
    'node'        = 'node';
    'cobol'       = 'cobol';
    'fortran'     = 'fortran';

    # Java ecosystem;
    'spring'      = 'spring';
    'springboot'  = 'spring';
    'hibernate'   = 'hibernate';
    'maven'       = 'maven';
    'gradle'      = 'gradle';

    # Python ecosystem;
    'fastapi'     = 'fastapi';
    'django'      = 'django';
    'flask'       = 'flask';
    'pandas'      = 'pandas';

    # Web;
    'react'       = 'react';
    'angular'     = 'angular';
    'vue'         = 'vue';

    # Cloud / DevOps;
    'docker'      = 'docker';
    'aws'         = 'aws';
    'azure'       = 'azure';
    'kubernetes'  = 'kubernetes';
    'k8s'         = 'kubernetes';
    'terraform'   = 'terraform';

    # AI / Agent ecosystem;
    'mcp'         = 'mcp';
    'model-context-protocol' = 'mcp';
    'claude'      = 'claude';
    'openai'      = 'openai';
};

[String[]]$columns =  @( 'Skill_Name', 'Category','Technology','Repository', 'Link');
[bool]$debug_flag  = $false
# git clone  --depth 1 https://github.com/majiayu000/claude-skill-registry
# write-host ('written {0}' -f $filepath)
$filepath = (resolve-path -path '.').path + '\' + $datafile
$stopwatch = [System.Diagnostics.Stopwatch]::StartNew()
function proces_file {

param(
)

$cnt = 0
# NOTE: += cannot handle large data in +=
# $results = @();
[System.Collections.ArrayList]$results = new-object System.Collections.ArrayList
# [System.Collections.Generic.List[object]]$results = [System.Collections.Generic.List[object]]::new()
# NOTE: for Powerhell 2.0
# $results = new-object 'System.Collections.Generic.List[object]'
$debug_flag = $false

# NOTE:
# PSBoundParameters.ContainsKey does not work here
if ($PSBoundParameters.ContainsKey('count')) {
  write-host ('reading {0} rows from {1}' -f $count, $filepath)
  [string[]]$input_lines = get-content $filepath | select-object -first $count
} elseif ($count -eq 0) {
  write-host ('reading {0}' -f $filepath)
  [string[]]$input_lines = get-content $filepath
} else {
  write-host ('reading {0} rows from {1}' -f $count, $filepath)
  [string[]]$input_lines = get-content $filepath | select-object -first $count
}

$spinIndex = 0
$input_lines |
foreach-object {
    $line = $_
    $cnt = $cnt + 1

    write-verbose ('read Data (raw):' + [char]10 + '"' + $line + '"' + [char]10)

    if (($cnt -ne 0 ) -and ((($cnt % 1000) -eq 0 ) -or ($cnt -eq $input_lines.Count-1 ))) {
      write-host -nonewline ("`rReading {0} {1}  Elapsed: {2:hh\:mm\:ss}" -f ($cnt), $spin[$spinIndex], $stopwatch.Elapsed)
      $spinIndex = ($spinIndex + 1) % $spin.Count
    }

    $pattern = '^claude-skill-registry/skills/([^/]+)(?:/[^/]+)*/([^/]+)/SKILL.md$'
    $m = select-string -pattern $pattern -InputObject $line
    $name = $null
    $category = $null
    if (($m -ne $null ) -and ($m.Matches -ne $null) -and $m.Matches.Success ) {
      $g = $m.Matches.Groups
      $category = $g.Item(1).Value
      $name = $g.Item(2).Value
    }
    if ($name -eq $null) {
      $regex = new-object System.Text.RegularExpressions.Regex($pattern) 
      $match = $regex.Match($line)
      if ($match.Success) {
        $category = $match.Groups.Item(1).Value
        $name = $match.Groups.Item(2).Value
      }
    }
    $a = @()
    $technology.keys | foreach-object { $p = $_
      if (($name -match "${p}[^a-z]" ) -or ($name -match "${p}$" )){ 
        $a +=$technology[$p]
      }
    }
    write-verbose ('Category: {0} Skill_Name: {1}'-f $category , $name )
    if ($a.count -ne 0 ) {
      write-verbose('Technology: {0}' -f ($a -join ',' ))
    }
    $r = @{
      'Skill_Name' = $name;
      'Category' = $category;
      'Technology' = ( $a -join ',' );
      'Link' = $line;
      'Id'   = $cnt;
    };
    [void]$results.Add($r)
    write-verbose ('Skill_Name: {0}' -f $r['Skill_Name'])
  }
  write-host ('Returning: {0} results' -f $results.Count)
  # write-host ('example: {0}' -f ($results[0]|format-list))
  return ([ref]$results)
}

function initialize_data_reader {
  param(
    [string]$format = 'excel',
    [string]$datafile_filename,
    [string]$sheet_name,
    [string]$query,
    [System.Management.Automation.PSReference]$connection_ref,
    [System.Management.Automation.PSReference]$command_ref,
    [System.Management.Automation.PSReference]$data_table_ref,
    [bool]$debug

  )

  [string]$datafile_directory = (resolve-path -path '.').Path
  [string]$datafile_fullpath = ('{0}\{1}' -f $datafile_directory,$datafile_filename)

  switch ($format) {
    'excel' {
      [string]$oledb_provider = 'Provider=Microsoft.ACE.OLEDB.16.0'
      [string]$data_source = "Data Source = ${datafile_fullpath}"
      [string]$ext_arg = 'Extended Properties=Excel 8.0'
      [string]$table = $sheet_name
    }
    'excel_legacy' {
      # 32-bit instances only, Jet Engine has been included with core image for Windows XP, Server 2013
      [string]$oledb_provider = 'Provider=Microsoft.Jet.OLEDB.4.0'
      [string]$data_source = "Data Source = ${datafile_fullpath}"
      [string]$ext_arg = 'Extended Properties=Excel 8.0;IMEX=1;'
      [string]$table = $sheet_name
    }
    'csv' {
      [string]$oledb_provider = 'Provider=Microsoft.ACE.OLEDB.16.0'
      [string]$ext_arg = 'Extended Properties="Text;IMEX=1;HDR=Yes;FMT=Delimited(,)";'
      [string]$data_source = "Data Source = ${$datafile_directory}"
      [string]$table = $datafile_filename
    }
    'csv_legacy' {
      # 32-bit instances only:
      [string]$oledb_provider = 'Provider=Microsoft.Jet.OLEDB.4.0'
      [string]$ext_arg = 'Extended Properties="Text;IMEX=1;HDR=Yes;FMT=Delimited(,)";'
      [string]$data_source = "Data Source = ${$datafile_directory}"
      [string]$table = $datafile_filename
    }
    default { throw }
  }
  $connection_string = "$oledb_provider;$data_source;$ext_arg"
  
  [string]$query = "SELECT * FROM [${table}] WHERE ISNULL(guid)"

  [System.Data.OleDb.OleDbConnection]$local:connection = new-object System.Data.OleDb.OleDbConnection($connection_string)
  [System.Data.OleDb.OleDbCommand]$local:command = new-object System.Data.OleDb.OleDbCommand($query)

  [System.Data.DataTable]$local:data_table = new-object System.Data.DataTable
  [System.Data.OleDb.OleDbDataAdapter]$ole_db_adapter = new-object System.Data.OleDb.OleDbDataAdapter
  $ole_db_adapter.SelectCommand = $local:command

  $local:command.Connection = $connection

  [void]$ole_db_adapter.Fill($local:data_table)
  # Exception calling "Fill" with "1" argument(s): "'Catalog$' is not a valid name.  Make sure that it does not include invalid characters or punctuation and that it is not too long."
  $local:connection.open()
  # http://stackoverflow.com/questions/24648081/error-the-type-system-data-oledb-oledbdatareader-has-no-constructors-defined
  $global:data_reader = $local:command.ExecuteReader()
  $data_table_ref.Value = $local:data_table
  $connection_ref.Value = $local:connection
  $command_ref.Value = $local:command
  return $local:data_reader
}

function insert_row_new {
  param(
    [string]$sql,
    [System.Data.OleDb.OleDbConnection]$connection,
    [System.Collections.Hashtable]$new_row_data
  )

  [string[]]$columns = [string[]]($new_row_data.Keys)

  [System.Data.OleDb.OleDbCommand]$local:command = new-object System.Data.OleDb.OleDbCommand
  $local:command.Connection = $connection

  $local:insert_name_part = @()
  $local:insert_value_part = @()

  $columns | foreach-object {
    $column_name = $_
    if ($column_name -eq $null) { return }
    $column_data = $new_row_data[$column_name]
    $local:command.Parameters.Add(('@{0}' -f $column_name),$column_data['type']).Value = $column_data['value']
    write-verbose ('@{0} = {1}' -f $column_name,$column_data['value'])
    $local:insert_name_part += ('[{0}]' -f $column_name)
    $local:insert_value_part += ('@{0}' -f $column_name)
  }

  $local:generated_sql = (($sql -replace '@insert_name_part',($local:insert_name_part -join ',')) -replace '@insert_value_part',($local:insert_value_part -join ','))

  write-verbose ('Insert query: {0}' -f $local:generated_sql)

  $new_row_data.Keys | ForEach-Object {
    $column_name = $_
    $column_data = $new_row_data[$column_name]
    write-verbose ('@{0} = {1}' -f $column_name,$column_data['value'])
  }
  $local:command.CommandText = $local:generated_sql

  try {
      $local:result = $local:command.ExecuteNonQuery()
  }
  catch [System.Data.OleDb.OleDbException] {
    # Exception calling "ExecuteNonQuery" with "0" argument(s): "Spreadsheet is full."	
    # other possible exceptions, from the error in the caller code / data	
    # Exception calling "ExecuteNonQuery" with "0" argument(s): "Invalid bracketing of name '[]'."
    # Exception calling "ExecuteNonQuery" with "0" argument(s): "Syntax error (missing operator) in query expression '@Skill Name'."
    # Exception calling "ExecuteNonQuery" with "0" argument(s): "Parameter @id has no default value."
  
    write-host ('ERROR inserting row: {0}' -f $new_row_data['id']['value'])
    write-host ('Skill: {0}' -f $new_row_data['Skill_Name']['value'])
    write-host ('Exception: {0}' -f $_.Exception.Message)

    throw
  }
  write-verbose ('Insert result: {0}' -f $local:result)
  $local:command.Dispose()
  return $local:result
}

[System.Collections.Hashtable]$row = ${ }
# NOTE: Braille spinner characters are Unicode code points U+280B through U+284F
# each is represented by one char
$spin = @(
    [char]0x280B, [char]0x2819, [char]0x2839, [char]0x2838,
    [char]0x283C, [char]0x2834, [char]0x2826, [char]0x2827,
    [char]0x2807, [char]0x280F
)
# $spin = @('|','/','-','\')


$results_ref = proces_file
write-host ('Exporting {0} entries' -f $results_ref.value.Count)

$command = new-object System.Data.OleDb.OleDbCommand
$connection = new-object System.Data.OleDb.OleDbConnection

$sheet_name = 'Catalog$'
$data_table = new-object System.Data.DataTable

initialize_data_reader -datafile_filename $datafile_filename -sheet_name $sheet_name -connection_ref ([ref]$connection) -command_ref ([ref]$command) -data_table_ref ([ref]$data_table)
# https://learn.microsoft.com/en-us/dotnet/api/system.data.oledb.oledbtype?view=netframework-4.5
# https://learn.microsoft.com/en-us/dotnet/api/system.data.oledb.oledbparameter.oledbtype?view=netframework-4.5
$rows = $results_ref.Value
$spinIndex = 0

@(0..($rows.Count-1)) | foreach-object {
  $cnt = $_
  $row = $rows[$cnt]
  
  $new_row_data = @{
    'id' = @{
      'value' = $row['Id'];
      'type' = [System.Data.OleDb.OleDbType]::Numeric;
    };
    'Skill_Name' = @{
      'value' = $row['Skill_Name'];
      'type' = [System.Data.OleDb.OleDbType]::VarChar;
    };
    'Category' = @{
      'value' = $row['Category'];
      'type' = [System.Data.OleDb.OleDbType]::VarChar;
    };
    'Technology' = @{
      'value' = $row['Technology'];
      'type' = [System.Data.OleDb.OleDbType]::VarChar;
    };
    'Repository' = @{
      'value' = '';
      'type' = [System.Data.OleDb.OleDbType]::VarChar;
    };
    
    'Link' = @{
      'value' = '';
      'type' = [System.Data.OleDb.OleDbType]::Variant;
    };
    'Select' = @{
      'value' = $false;
      'type' = [System.Data.OleDb.OleDbType]::Boolean;
    };
  
    'guid' = @{
      'value' = ([guid]::NewGuid()).ToString();
      'type' = [System.Data.OleDb.OleDbType]::VarChar;
    };
  
  }
  [void](insert_row_new -new_row_data $new_row_data -connection $connection -sql "Insert into [${sheet_name}] (@insert_name_part) values (@insert_value_part)"    )
  if (($cnt -ne 0 ) -and ((($cnt % 1000) -eq 0 ) -or ($cnt -eq $input_lines.Count-1 ))) {
    write-host -nonewline ("`rInserted {0} {1} Elapsed: {2:hh\:mm\:ss}" -f ($cnt), $spin[$spinIndex], $stopwatch.Elapsed)
    $spinIndex = ($spinIndex + 1) % $spin.Count
  }
}
$command.Dispose()

$connection.close()


exit
$template = (get-content -raw ((resolve-path -path '.' ).path + '\' + $templatefile))
[xml]$template_xml = [xml]$template
[System.Xml.XmlElement]$documentElement = $template_xml.documentElement

[System.Xml.XmlElement]$template_row = $documentElement.SelectNodes("//*[contains(@class, ""template"")]").Item(0)
# Simple class lookup. Safe because this template generates the HTML and no other class names contain "template"
# [System.Xml.XmlElement]$template_row = $documentElement.SelectNodes("//*[contains(concat("" "", normalize-space(@class), "" ""), "" template "")]").Item(0)
if ($template_row -eq $null) {
  # TODO: report error with template	
}
[string]$template_row_html = $template_row.outerXML

[Object[]]$rows = $results_ref.value
[string]$html = $null
# NOTE: You must provide a value expression following the '..' operator.

$spinIndex = 0

@(0..($rows.Count-1)) | foreach-object {
  $cnt = $_
  $row = $rows[$cnt]
  $html = $template_row_html
  $fields | foreach-object {
    $field = $_
    if ( -not $row.ContainsKey($field) ){
      $html = $html.Replace($field.ToUpper(), '')
    } else {
      $html = $html.Replace($field.ToUpper(), $row.Item($field))
    }
  }
  [System.Xml.XmlDocumentFragment]$fragment = $template_xml.CreateDocumentFragment()
  ($fragment.InnerXml = $html ) |out-null
  $template_row.ParentNode.InsertAfter($fragment, $template_row)|out-null
  if ((($cnt % 1000) -eq 0 ) -or ($cnt -eq $rows.Count-1 )) {
    # NOTE: performance
    $rate = [math]::Round(($cnt + 1) / $stopwatch.Elapsed.TotalSeconds)
    write-host  -nonewline ("`rReading {0} {1}  Elapsed: {2:hh\:mm\:ss} | {3} rows/sec " -f ($cnt + 1), $spin[$spinIndex], $stopwatch.Elapsed, $rate)
    $spinIndex = ($spinIndex + 1) % $spin.Count
    <#
      write-Progress -activity 'Generating HTML' -status "$($cnt + 1) of $($rows.Count)" -percentComplete (($cnt + 1) * 100 / $rows.Count)
    #>
  }
}
$result_filepath = ((resolve-path -path '.' ).path + '\' + $outputfile )

$settings = new-object System.Xml.XmlWriterSettings
$settings.Indent = $true
$settings.IndentChars = '  '

$writer = [System.Xml.XmlWriter]::Create($result_filepath, $settings)
try {
  $template_xml.WriteContentTo($writer)
}
finally {
  $writer.Flush()
  $writer.Close()
}
