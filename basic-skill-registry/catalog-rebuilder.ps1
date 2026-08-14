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
  [String]$location = 'https://github.com/membranedev/application-skills',
  [Parameter(Mandatory = $false,Position = 2)]
  [String]$datafile = 'catalog.txt',
  [Parameter(Mandatory = $false,Position = 2)]
  [String]$format = 'html',
  [String]$outputfile = 'result.xls',
 # [String]$template_filename = 'catalog.html',
  [String]$template_filename = 'catalog-template.xlsx',
  [String[]]$fields = @( 'Skill_Name', 'Category', 'Technology', 'Repository', 'Link', 'Select', 'GUID', 'Id'),
  [int]$count = 0
)

Add-Type -TypeDefinition @'
using System;
using System.Diagnostics;
using System.IO;

namespace Utils {
	// NOTE:
	// will cannot declare instance members in a static class
	public class Program {
		private string filename = "SKILL.md";
		public string Filename {
			get { return filename; }
			set { filename = value; } }
		private string[] files;

		public string[] Files { get { return files; }}
		private string tempPath;
		public string TempPath {
			get { return tempPath; }
		}
		private string location;
		private string project;
		public string Project {
			get { return project; }
    }
		public string Location {
			get { return location; }
			set { location = value;
				string[] parts = location.Split('/');
				project = parts[parts.Length - 1 ];
			}
		}
		// .Net 4.0: Default parameter specifiers are not permitted
		public static string CreateTempSubdirectory() {
			return CreateTempSubdirectory("");
		}

		public static string CreateTempSubdirectory(string prefix) {
			// Get the system temp path
			string tempRoot = Path.GetTempPath();

			// Create a unique folder name (optional prefix + GUID)
			string uniqueFolderName = string.IsNullOrEmpty(prefix)
            ? Guid.NewGuid().ToString()
            : prefix + "_" + Guid.NewGuid().ToString();

			// Combine root and unique subfolder name
			string uniquePath = Path.Combine(tempRoot, uniqueFolderName);

			// Create and return the physical directory
			Directory.CreateDirectory(uniquePath);
			return uniquePath;
		}

		public void Run() {

			// NOTE: custom extension returning String, not DirectryInfo hence no FullPath
			// built-in Directory.CreateTempSubdirectory() method does not exist in .NET 4.5 (it was introduced later in .NET Core 3.0 / .NET Standard 2.1).
			// therefore the following
			// https://learn.microsoft.com/en-us/dotnet/api/system.io.directory.createtempsubdirectory?view=net-10.0?view=netframework-4.5
			//	is redirect
			// https://learn.microsoft.com/en-us/dotnet/api/system.io.directory.createtempsubdirectory?view=net-10.0&viewFallbackFrom=net-10.0%3Fview%3Dnetframework-4.5
			this.tempPath = CreateTempSubdirectory(Path.GetTempPath());
			// .Net 4.0  A new expression requires () or [] after type
			// .Net 4.0 The type or namespace name 'var' could not be found
			Process process = new Process();
			// https://learn.microsoft.com/en-us/dotnet/api/system.diagnostics.processstartinfo?view=netframework-4.5
			process.StartInfo = new ProcessStartInfo();

			process.StartInfo.WorkingDirectory = tempPath;
			// NOTE: @"" does not appear to work under add-type
			// FileName = @"C:\Program Files\Git\bin\git.exe";
			process.StartInfo.FileName = "C:\\Program Files\\Git\\bin\\git.exe";
			process.StartInfo.Arguments = String.Format("clone --depth 1 \"{0}\"", this.location);
			process.StartInfo.UseShellExecute = false;

			process.Start();
			process.WaitForExit();
      if (!Directory.Exists(Path.Combine(tempPath, project)))
        return;
			this.files = Directory.GetFiles( Path.Combine(tempPath, project), filename, SearchOption.AllDirectories);
			try {
				Directory.Delete(tempPath, true);
			} catch (Exception) {
			}
		}
	}
}
'@
# $git_install_path = ( Get-ItemProperty -Path 'HKLM:\SOFTWARE\GitForWindows').InstallPath

function create_temporaryfile {
    param (
      # [Parameter(Mandatory)]
        [string]$template_fullpath
    )
    $temp_file = [System.IO.Path]::GetTempFileName()
    $template_extension = [System.IO.Path]::GetExtension($template_fullpath)
    $temp_fullpath = [System.IO.Path]::ChangeExtension($temp_file, $template_extension)
    rename-item -path $temp_file -newname $temp_fullpath -force
    # The term 'new-temporaryfile' is not recognized as the name of a cmdlet, function, script file, or operable program. Check the spelling of the name, or if a path was included, verify that the path is correct and try again.
    <#
    $temp_file = new-temporaryfile
    copy-item -path $template_fullpath -destination $temp_file.FullName -force
    return $temp_file.FullName
    #>
    copy-item -path $template_fullpath -destination $temp_fullpath -force
    return $temp_fullpath
}

function read_location {

  param(
    $helper_ref = $null,
    [string]$location = $null,
    [string]$logfile = $null
  )
  $helper = $helper_ref.Value
  $helper = new-object Utils.Program
  # -ArgumentList (([int]$window_handle))

  $helper.Location = $location
  $helper.Run()
  # Exception calling "Run" with "0" argument(s): "Could not find a part of the path 'C:\Documents and Settings\Admin\Local Settings\Temp\_d6a741e8-48b4-48fe-a141-26ef3d393b86\application-skills'."

  # Powershell 2.x The term 'new-temporaryfile' is not recognized

  if ($PSBoundParameters.ContainsKey('Verbose')) {
    tee-object -filepath $tempFile -inputObject @($helper.Files)
  } else {
    out-file -filepath $tempFile -inputObject @($helper.Files)
  }
  # why (measure-object -inputObject @($helper.Files)).Count = 1
  $project = $helper.Project
  
  write-host ('{0} items ({1} bytes) of "{2}" written' -f ($helper.Files.Count), (get-item $tempFile).Length, $project)
  return $project
}


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
$stopwatch = [System.Diagnostics.Stopwatch]::StartNew()


function proces_file {

param(
  [string] $filepath = $null,
  [string] $repository_name = 'claude-skill-registry'
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

    write-verbose ('line: "{0}"' -f $line)
    $line = $line.Replace('\', '/')
    $pattern1 = ('^.+/(?={0})' -f  $repository_name )
    $r1 = new-object System.Text.RegularExpressions.Regex($pattern1)
    $line = $r1.replace($line, '')
    write-verbose ('parsing prepared line: "{0}"' -f $line)
    # claude-skill-registry/skills/agent/adk/SKILL.md
    # application-skills/skills/zype/SKILL.md
    $pattern = ('^{0}/skills(?:/([^/]+))*(?:/[^/]+)*/([^/]+)/SKILL.md$' -f $repository_name )
    write-verbose $pattern
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
    [string]$template_fullpath,
    [string]$sheet_name,
    [string]$query,
    [System.Management.Automation.PSReference]$connection_ref,
    [System.Management.Automation.PSReference]$command_ref,
    [System.Management.Automation.PSReference]$data_table_ref,
    [bool]$debug

  )

  $template_filename = split-path -path $template_fullpath -leaf
  [string]$oledb_provider = $null
  [string]$data_source = $null
  [string]$table = $null
  [string]$ext_arg = $null
  switch ($format) {
    'excel' {
      $oledb_provider = 'Provider=Microsoft.ACE.OLEDB.12.0'
      $data_source = ('Data Source = {0}' -f $template_fullpath )
      $ext_arg = 'Extended Properties=Excel 8.0'
      $table = $sheet_name
    }
    'excel_legacy' {
      # 32-bit instances only, Jet Engine has been included with core image for Windows XP, Server 2013
      $oledb_provider = 'Provider=Microsoft.Jet.OLEDB.4.0'
      $data_source = ('Data Source = {0}' -f $template_fullpath )
      $ext_oarg = 'Extended Properties=Excel 8.0;IMEX=1;'
      $table = $sheet_name
    }
    'csv' {
      $oledb_provider = 'Provider=Microsoft.ACE.OLEDB.12.0'
      $data_source = ('Data Source = {0}' -f $working_directory )
      $ext_arg = 'Extended Properties="Text;IMEX=1;HDR=Yes;FMT=Delimited(,)";'
      $table = $template_filename
    }
    'csv_legacy' {
      # 32-bit instances only:
      $oledb_provider = 'Provider=Microsoft.Jet.OLEDB.4.0'
      $data_source = ('Data Source = {0}' -f $working_directory )
      $ext_arg = 'Extended Properties="Text;IMEX=1;HDR=Yes;FMT=Delimited(,)";'
      $table = $template_filename
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

    write-host ("ERROR inserting row: {0}$([Environment]::NewLine)Skill: {1}$([Environment]::NewLine)Exception: {2}" -f $new_row_data['id']['value'], $new_row_data['Skill_Name']['value'], $_.Exception.Message)
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
[string]$working_directory = (resolve-path -path '.').Path


# NOTE: use the extension as the primary contract. Can perform magic bytes also but only as a sanity check:
$template_extension = [IO.Path]::GetExtension($template_filename).ToLower()

if ($format -eq 'html' -and $template_extension -match '.xlsx?') {
  throw ('Inconsistent arguments: format "{0}" cannot be used with Excel template "{1}"' -f $format, $template_filename  )
}

if ($format -eq 'excel' -and (-not ($template_extension -ne '.xlsx?'))) {
  throw ('Inconsistent arguments: format "{0}" requires an Excel (.xlsx|.xls) template' -f $format)
}

$inferred_format = switch ($template_extension) {
  '.xlsx' { 'excel' }
  '.xls'  { 'excel' }
  '.html' { 'html' }
  '.htm'  { 'html' }
  default { throw ('Unsupported template extension: {0}' -f $template_extension )}
}

if ($format -ne $inferred_format) {
  throw ('Template "{0}" inferred format ({1}) mismatch with supplied format argument {2}' -f $template_filename, $inferred_format, $format )
}

if ($format -like 'excel') {
  # [byte[]]$bytes = [byte[]]::new(8)
  $bytes = new-object byte[] 8
  $file_probe_stream = new-object IO.FileStream( $template_filename, [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::Read )

  try {
    [void]$file_probe_stream.Read($bytes, 0, 8)
    write-host ('successfully read bytea {0}' -f  [String]::join("", $bytes.ForEach('ToString', 'X2')) )
  }
  finally {
    $file_probe_stream.Close()
  }

  if ($bytes[0] -ne 0x50 -or $bytes[1] -ne 0x4B) {
    throw ('Template "{0}" does not appear to be a valid XLSX file' -f $template_filename)
  }
}
if ($location -ne $null) {
  <#
    $git = Get-Command git.exe -ErrorAction Stop

    & $git.Source clone --depth 1 $repository $destination
    if ($LASTEXITCODE -ne 0) {
        throw "git clone failed: $LASTEXITCODE"
    }
    git clone  --depth 1 https://github.com/majiayu000/claude-skill-registry
    write-host ('written {0}' -f $filepath)
  #>
  $tempfile = (new-temporaryfile)
  # $window_handle = [System.Diagnostics.Process]::GetCurrentProcess().MainWindowHandle

  $repository_name = read_location -helper_ref [ref]($helper) -logfile $tempfile -location $location
  $filepath = $tempFile.fullName
  # Exception calling "run" with "0" argument(s): "Access to the path 'C:\Users\kouzm\AppData\Local\Temp\_e57611bf-0351-4731-916f-f082e1bd671e' is denied."
  <#
    Cloning into 'application-skills'...
    fatal: unable to access 'https://github.com/membranedev/application-skills/': Couldn't resolve host 'github.com'
    C:\Documents and Settings\Admin\Local Settings\Temp\_1b1db0d8-4001-457d-8128-6f8655389fe8
  #>
} else {
   # TODO move code
   $repository_name = 'claude-skill-registry' 
   $filepath = (resolve-path -path '.').path + '\' + $datafile
}
$results_ref = proces_file -filepath $filepath -repository_name $repository_name
write-host ('Exporting {0} entries' -f $results_ref.value.Count)


if ($format -like 'excel') {
$command = new-object System.Data.OleDb.OleDbCommand
$connection = new-object System.Data.OleDb.OleDbConnection

$sheet_name = 'Catalog$'
$data_table = new-object System.Data.DataTable

[string]$template_fullpath = join-path -path $working_directory -childpath  $template_filename
$template_fullpath = create_temporaryfile -template_fullpath $template_fullpath
write-host ('writing temporary file: {0}' -f $template_fullpath)

initialize_data_reader -template_fullpath $template_fullpath -sheet_name $sheet_name -connection_ref ([ref]$connection) -command_ref ([ref]$command) -data_table_ref ([ref]$data_table)
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
# NOTE: in Powershell 2.0
# Rename-Item : Cannot rename because the target specified represents a path or device name.
# rename-item -path $template_fullpath -newname (join-path -path $working_directory -childpath $outputfile) -force
rename-item -path $template_fullpath -newname $outputfile -force -erroraction silentlycontinue
# NOTE: in PowerShell 2.0, the Rename-Item cmdlet does not support overwriting existing files
# Rename-Item : Cannot create a file when that file already exists
# NOTE: the $outputfile still in $env:TEMP (split-path -path $template_fullpath -parent)
copy-item -path (join-path -path (split-path -path $template_fullpath -parent) -childpath $outputfile ) -destination $working_directory -force
move-item -literalpath $template_fullpath -destination (join-path -path $working_directory -childpath $outputfile) -force
exit
} else {
  <#
   TODO: probe to avoid
   Cannot convert value "PK^C^D^T^@... to type "System.Xml.XmlDocument". Error: "'^C', hexadecimal value 0x03, is an invalid character. Line 1, position 3."
   when arguments are inconsistent
 #>
$template = (get-content -raw (join-path -path $working_directory -childpath $template_filename))
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
  ($fragment.InnerXml = $html)|out-null
  # logic
  # $template_row.ParentNode.InsertAfter($fragment, $template_row)|out-null
  $template_row.ParentNode.InsertBefore($fragment, $template_row)|out-null
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
# write to $outputfile directly
$result_filepath = (join-path -path $working_directory -childpath $outputfile )

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

}