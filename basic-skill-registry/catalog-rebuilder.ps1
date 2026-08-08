
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
  [String[]]$fields =  @( 'Skill Name', 'Category','Technology','Repository', 'Link','Select','GUID', 'Id'),
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

[String[]]$columns =  @( 'Skill Name', 'Category','Technology','Repository', 'Link');
[bool]$debug_flag  = $false
# git clone  --depth 1 https://github.com/majiayu000/claude-skill-registry
# write-host ('written {0}' -f $filepath)
$filepath = (resolve-path -path '.').path + '\' + $datafile
$stopwatch = [System.Diagnostics.Stopwatch]::StartNew()
function proces_file {

param(
)

$cnt = 0
# NOTE: cannot handle large data in +=
# very += creates a new array and copies the previous contents.
# with 2 million entries, this becomes catastrophic.
# $results = @();
[System.Collections.ArrayList]$results = new-object System.Collections.ArrayList
# Windows XP .net 4.0 Powerhell 2.0
# Method invocation failed because [System.Collections.Generic.List`1[[System.Object, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]] doesn't contain a method named 'new'.
# [System.Collections.Generic.List[object]]$results = [System.Collections.Generic.List[object]]::new()
# $results = new-object 'System.Collections.Generic.List[object]'
write-host ('reading {0} rows from {1}' -f $count, $filepath)
$debug_flag = $false

# NOTE:


# Limit the upstream pipeline rather than exiting from inside ForEach-Object.
#
# Returning from a pipeline callback does not terminate the pipeline; it emits
# a value back to the caller and subsequent input objects continue to invoke
# the callback. The function returns only after the pipeline completes.
#
# This is analogous to Java Stream.limit(count), where the upstream iterator is
# truncated before the callback executes:
#
#   Files.lines(path)
#       .limit(count)
#       .forEach(...);
#
# or LINQ:
#
#   File.ReadLines(path)
#       .Take(count)
#       .ToList();
#
# The below are code smells: treating map() as if it were a for loop.
# .stream()
#     .map(x -> {
#         if (enough(x))
#             return null;   // hoping to stop
#         return transform(x);
#     })
#     .collect(...);
# .stream
#    .map(x -> {
#        if (found)
#            return ...;    // hoping to terminate
#        ...
#    });
# The key idea is to stop the producer, not to escape from the consumer

# PSBoundParameters.ContainsKey does not work
if ($PSBoundParameters.ContainsKey('count')) {
  [string[]]$input_lines = Get-Content $filepath | Select-Object -First $count
} elseif ($count -eq 0)  {
  [string[]]$input_lines = Get-Content $filepath
} else {
  [string[]]$input_lines = Get-Content $filepath | Select-Object -First $count
  # [string[]]$input_lines = Get-Content $filepath
}

$spinIndex = 0
$input_lines |
foreach-object {
    $line = $_
    $cnt = $cnt + 1

    write-verbose ('read Data (raw):' + [char]10 + '"' + $line + '"' + [char]10)

    if ((($cnt % 1000) -eq 0 ) -or ($cnt -eq $input_lines.Count-1 )) {
	  write-host  -nonewline ("`rReading {0} {1}  Elapsed: {2:hh\:mm\:ss}" -f ($cnt + 1), $spin[$spinIndex], $stopwatch.Elapsed)
	  $spinIndex = ($spinIndex + 1) % $spin.Count
    }

    $pattern = '^claude-skill-registry/skills/([^/]+)(?:/[^/]+)*/([^/]+)/SKILL.md$'
    $m = select-string -pattern $pattern -InputObject $line
	$name = $null
	$category = $null
    # if (($m -ne $null ) -and ($m.matches -ne $null) ) {
    # You cannot call a method on a null-valued expression.
    # CategoryInfo : InvalidOperation: (Item:String) [], RuntimeException FullyQualifiedErrorId : InvokeMethodOnNull
    # if (($m -ne $null ) -and ($m.Matches -ne $null) -and $m.Matches.Success -and ($m.Matches.Groups -ne $null)) {
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
    write-verbose ('Category: {0} Skill Name: {1}'-f $category , $name )
    if ($a.count -ne 0 ) {
      write-verbose('Technology: {0}' -f ($a -join ',' ))
    }
    $r = @{
      'Skill Name' = $name;
      'Category' = $category;
      'Technology' = ( $a -join ',' );
      'Link' = $line;
      'Id'   = $cnt;
    };
    [void]$results.Add($r)
    write-verbose ('Skill Name: {0}' -f $r['Skill Name'])
  }
  write-host ('Returning: {0} results' -f $results.Count)
  # write-host ('example: {0}' -f ($results[0]|format-list))
  return ([ref]$results)
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
