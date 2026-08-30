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
  [string]$filename = $null
)

add-type -AssemblyName System.Windows.Forms
add-type -AssemblyName System.Drawing

if (-not ('VisioAxHost' -as [type])) {
    Add-Type @'
using System.Windows.Forms;

public class VisioAxHost : AxHost {
    public VisioAxHost() : base("{F8CF7A98-2C45-4c8d-9151-2D716989DDAB}") { }
}
'@ -ReferencedAssemblies 'System.Windows.Forms.dll'
}

$f = New-Object System.Windows.Forms.Form
$f.Text = 'Visio Viewer'
$f.Width = 1000
$f.Height = 700

$v = New-Object VisioAxHost
write-host ((($v | Get-Member | select-object -expandproperty name | sort-object ) -join [char]12) | out-string)
$v.Dock = [System.Windows.Forms.DockStyle]::Fill
$f.Controls.Add($v)

$f_Load = $f.add_Load
$f_Load.Invoke({
  $o = $v.GetOcx()  
  if ($filename -eq $null) {
    $filename = (get-childitem -path "${env:USERPROFILE}\Downloads" -filter '*.vsdx'|select-object -last 1).FullName
  }
  write-host ('Loading {0}' -f $filename)
  $o.Load($filename)
  write-host $o.DocumentLoaded
  write-host $o.PageName(0)
})

$f.ShowDialog()


