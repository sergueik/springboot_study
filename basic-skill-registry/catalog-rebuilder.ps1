
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
  [String]$datafile = 'catalog2.txt',
  [String]$templatefile = 'catalog.html',
  [String[]]$fields =  @( 'Skill Name', 'Category','Technology','Repository', 'Link','Select','GUID', 'Id'),
  [int]$count =  10
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

function proces_file { 

param(
)

$cnt = 0
$results = @();
write-host ('reading {0} rows from {1}' -f $count, $filepath)
$debug_flag = $false
get-content $filepath | foreach-object {
    $line = $_
    $cnt = $cnt + 1
    # [Void]$i.Items.Add($line)

    # claude-skill-registry/skills/agent/35-google-adk-reliable-agents/SKILL.md
    if ($debug_flag) {
      write-host ('read Data (raw):' + [char]10 + '"' + $line + '"' + [char]10)
    }
    $o = $null
    if ($cnt -gt $count ){
      # write-host ('count: {0}' -f $results.Count)
      # write-host ('example: {0}' -f ($results[0]|format-list))
      return ([ref]$results)
      # write-output ($results | format-list ) 
      # $results | foreach-object { write-output $_} 
      # WARNING - it is not what is appears
      # WARNING: old Powershell 2.0 parser understands it differently than 5.1  
      continue
      # https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_continue?view=powershell-5.1
      # continue :label
    }

    $pattern =  '^claude-skill-registry/skills/([^/]+)(?:/[^/]+)*/([^/]+)/SKILL.md$'
    # is not a valid regular expression: parsing
    # parsing "..." - Not enough )'s
    [Microsoft.PowerShell.Commands.MatchInfo]$m = $null
    $m = select-string -pattern $pattern -InputObject $line
    if (($m -ne $null ) -and ($m.matches -ne $null)) {
      try {
        $g = $m.Matches.Groups
        $c = $g.Item(1).Value
        $n = $g.Item(2).Value
        $a = @()
        $technology.keys | foreach-object {
  	      $p = $_
  	      if (($n -match "${p}[^a-z]" ) -or  ($n -match "${p}$" )){
  		 $a +=$technology[$p]
  	      }
        }
        write-host ('Category: {0} Skill Name: {1}'-f $c , $n )
        if ($a.count -ne 0 ) {
          write-host('Technology: {0}' -f ($a -join ',' ))
        }
  
        $r = @{ 'Skill Name' = $n;
                 'Category' = $c;
                 'Technology' = ( $a -join ',' );
                 'Link' = $line;
		 'Id'   = $cnt;
  	      };
        $results += $r 
        write-host ('Skill Name: {0}' -f $r['Skill Name'])
      } catch [InvalidOperationException] {
        # not outermost
      } catch [System.Management.Automation.RuntimeException] {
  	    write-host ("Exception (ignored): {0} {1}" -f $_.Exception.GetType().FullName, $_.Exception.Message)
        # https://devblogs.microsoft.com/scripting/troubleshoot-the-invokemethodonnull-error-with-powershell/
        # PowerShell FullyQualifiedErrorId : InvokeMethodOnNull (commonly stated as "You cannot call a method on a null-valued expression") means your code tries to run a method on a variable, object, or property that evaluates to $null
      }
      } 
    # Warning: retrofit
    # :label [void] 1
  }
}
[System.Collections.Hashtable]$row = ${ }
$results_ref = proces_file
write-output ('Exporting {0} entries' -f $results_ref.value.Count)
$template = (get-content -raw ((resolve-path -path '.' ).path + '\' + $templatefile))
# write-output $template
[xml]$template_xml = [xml]$template
[System.Xml.XmlElement]$documentElement = $template_xml.documentElement
# Error: "The specified node cannot be inserted as the valid child of this node, because the specified node is the wrong type."
[System.Xml.XmlElement]$template_row = $documentElement.SelectNodes("//*[@id=""template""]").Item(0)
[string]$template_row_html = $template_row.outerXML
# $template_xml.documentElement.SelectNodes("//*[@id=""template""]").Item(0).innerText
# IDNAMECATEGORYTECHNOLOGYREPOSITORYLINKSELECTGUID
[Object[]]$rows = $results_ref.value
[string]$html = $null
# NOTE: You must provide a value expression following the '..' operator.
@(0..($rows.Count-1)) | foreach-object {
  $cnt = $_
  $row = $rows[$cnt]
  $html = $template_row_html
  # write-output ('before: {0}' -f $html)
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
  # write-output ('after: {0}' -f $html)
  $template_row.ParentNode.InsertAfter($fragment, $template_row)|out-null
	  <#
  write-output $row.Item('Skill Name')
  write-output $row.Item('Category')
  write-output $row.Item('Technology')
  write-output $row.Item('Link' )
  #>
}

$result_filepath = ((resolve-path -path '.' ).path + '\' + 'output.html' )

$settings = new-object System.Xml.XmlWriterSettings
$settings.Indent = $true
$settings.IndentChars = "  "

$writer = [System.Xml.XmlWriter]::Create($result_filepath, $settings)
try {
    $template_xml.WriteContentTo($writer)
}
finally {
    $writer.Flush()
    $writer.Close()
}
