# CreateFlowchart.ps1
# based on:
# https://dellenny.com/approaches-to-automatically-generate-visio-diagrams-from-text/
# https://www.visguy.com/2006/09/13/create-visio-flowcharts-programmatically
# Visio COM Automation
# Compatible with Windows PowerShell 2.0 / Visio 2007

$ErrorActionPreference = 'Stop'

# --------------------------------------------------
# Step 1: Get the flowchart template and stencil
# --------------------------------------------------

$flowchartTemplateName = 'C:\Program Files\Microsoft Office\Office12\1049\BASFLO_M.VST'
$flowchartStencilName = 'BASFLO_M.VSS'

$masterProcessName = 'Process'
$masterDecisionName = 'Decision'

$visApp = $null
$docFlowTemplate = $null
$docFlowStencil = $null

try {

  # Start Visio

  $visApp = New-Object -ComObject Visio.Application
  $visApp.Visible = $true

  # Open a new document using the flowchart template

  $docFlowTemplate = $visApp.Documents.Add($flowchartTemplateName)

  # Search open documents for the flowchart stencil

  foreach ($doc in $visApp.Documents) {

    Write-Host 'Document:' $doc.Name

    if ($doc.Name -eq $flowchartStencilName) {
      $docFlowStencil = $doc
      break
    }
  }

  # Fallback: open stencil explicitly if not found

  if ($null -eq $docFlowStencil) {

    # https://learn.microsoft.com/en-us/visualstudio/vsto/how-to-programmatically-open-visio-documents?view=visualstudio&tabs=csharp
    # NOTE: ACP locale is used in Office application resource path and name attributes
    # NOTE: may be different from [System.Globalization.CultureInfo]::CurrentCulture.LCID
    # en-us 0x409 1033
    # ru-ru 0x419 1049
    $programFiles = [System.Environment]::GetFolderPath([System.Environment+SpecialFolder]::ProgramFiles)
    $stencilPath = $programFiles + '\Microsoft Office\Office12\1049\BASFLO_M.VSS'
    # https://learn.microsoft.com/en-us/dotnet/api/system.environment.specialfolder?view=netframework-4.5
    # https://learn.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo.currentculture?view=netframework-4.5
    # https://learn.microsoft.com/sr-latn-rs/office/vba/api/visio.visopensaveargs
    # NOTE: may choose to open with visOpenHidden flag instead
    $docFlowStencil = $visApp.Documents.OpenEx( $stencilPath, [Microsoft.Office.Interop.Visio.VisOpenSaveArgs]::visOpenDocked )
  }

  # --------------------------------------------------
  # Step 2: Get masters and connector
  # --------------------------------------------------

  $mstProcess = $docFlowStencil.Masters.ItemU($masterProcessName)

  $mstDecision = $docFlowStencil.Masters.ItemU($masterDecisionName)

  # Built-in connector object (not a Master)

  $conn = $visApp.ConnectorToolDataObject

  # --------------------------------------------------
  # Step 3: Drop the masters
  # --------------------------------------------------

  $numShapes = 7
  $decisionStepNumber = 3

  $dx = 1.5
  $dy = 1.0

  # First page of the document

  $pg = $docFlowTemplate.Pages.Item(1)

  # Get center and top of page

  $x = $pg.PageSheet.CellsU( 'PageWidth').ResultIU / 2

  $y = $pg.PageSheet.CellsU( 'PageHeight').ResultIU - 1

  $shpLast = $null
  $shpDec = $null
  $shpConn = $null

  for ($i = 1; $i -le $numShapes; $i++) {

    # Drop Process or Decision shape

    if ($i -eq $decisionStepNumber) {
      # https://learn.microsoft.com/en-us/office/vba/api/visio.page.drop
      $shpNew = $pg.Drop( $mstDecision, $x, $y)

      $shpNew.Text = ('{0}?' -f $i )

      # Remember decision shape

      $shpDec = $shpNew
    } else {
      $shpNew = $pg.Drop( $mstProcess, $x, $y)

      $shpNew.Text = ('Do Step {0}' -f $i)
    }

    # Set custom properties

    $shpNew.CellsU( 'Prop.Cost').ResultIU = $i
    $cell = $shpNew.CellsU('Prop.Duration')

    # Duration expressed in elapsed hours
    # https://learn.microsoft.com/en-us/office/vba/api/visio.cell.result
    $cell.Result([Microsoft.Office.Interop.Visio.VisUnitCodes]::visElapsedHour) = $i
<#
    $shpNew.CellsU( 'Prop.Duration').FormulaU = ('$i h') 

leading to
Exception setting 'FormulaU': '
#NAME?'

- this is ShapeSheet formula parsing issue
#>
    # Connect shapes

    if ($i -ne 1) {

      # Drop connector

      $shpConn = $pg.Drop( $conn, 0, 0)

      # Glue BeginX

      if ($i -eq ($decisionStepNumber + 1)) {

        # Glue to right-side decision connection
        # https://learn.microsoft.com/en-us/office/vba/api/visio.cell.glueto
        $shpConn.CellsU( 'BeginX').GlueTo( $shpDec.CellsU('Connections.X2'))
      } else {
        # Dynamic glue to previous shape

        $shpConn.CellsU( 'BeginX').GlueTo( $shpLast.CellsU('PinX'))
      }

      # Glue EndX to new shape

      $shpConn.CellsU( 'EndX').GlueTo( $shpNew.CellsU('PinX'))
    }

    # Prepare for next iteration

    $shpLast = $shpNew

    # Adjust coordinates according to branch

    if ($i -eq $decisionStepNumber) {
      $x = $x + $dx
    } elseif ($i -eq ($decisionStepNumber + 1)) {

      $x = $x - $dx
      $y = $y - $dy

      # Label decision branch

      $shpConn.Text = 'No'

      # Next connector starts at decision shape

      $shpLast = $shpDec
    } else {
      $y = $y - $dy
    }
  }

  # --------------------------------------------------
  # Step 4: Layout and deselect
  # --------------------------------------------------

  # Original example leaves automatic layout disabled.
  # Uncomment to experiment:
  #
  $pg.Layout()

  # Deselect all shapes

  $visApp.ActiveWindow.DeselectAll()

  # Original example:
  #
  # Visio.Windows.Arrange(
  #   Visio.visArrangeTileVertical
  # )
  #
  # This requires resolving the Visio enum value
  # or using the installed Visio interop assembly.

  Write-Host 'Flowchart created successfully.'

  foreach ($shape in $pg.Shapes) {

    [Console]::Error.WriteLine(('ID: {0} Name: "{1}" NameU: "{2}" Text: "{3}" OneD: {4} Master ID: {5} Master Name: "{6}" Master NameU: "{7}"' -f $shape.ID , $shape.Name, $shape.NameU, $shape.Text, $shape.OneD, $shape.Master.ID, $shape.Master.Name,$shape.Master.NameU ))
	[Console]::Error.WriteLine(('{0} connects' -f $shape.Connects.Count))

    if ($shape.OneD) {

      # https://learn.microsoft.com/en-us/office/client-developer/visio/gluetype-cell-glue-info-section
	  [Console]::Error.WriteLine( ('EndX: "{0}" {1}' -f  $shape.CellsU('EndX').ResultIU, $shape.CellsU('EndX').GlueType))
	  [Console]::Error.WriteLine(('BeginX: "{0}" {1}' -f $shape.CellsU('BeginX').ResultIU, $shape.CellsU('BeginX').GlueType))
    } else {

      [Console]::Error.WriteLine( (Write-Host 'Master:'  + $shape.Master.Name ))
      [Console]::Error.WriteLine( ('MasterU:' + $shape.Master.NameU))

      [Console]::Error.WriteLine( ('PinX:' + $shape.CellsU('PinX').ResultIU))
      [Console]::Error.WriteLine( ( 'PinY:'+  $shape.CellsU('PinY').ResultIU))
    }
  }

} finally {

  # Close documents and quit Visio
  # NOTE: also saves when docyment management was unsuccessful
  if ($null -ne $docFlowTemplate) {
    $docFlowTemplate.SaveAs( [System.IO.Path]::GetTempPath()  + '\AutomatedFlowchart.vsd')
    # $docFlowTemplate.Saved = $true
    # not needed
    $docFlowTemplate.Close()
  }

  if ($null -ne $docFlowStencil) {
    try {
      $docFlowStencil.Close()
    } catch [Exception] {
      # System.Reflection.TargetInvocationException: Exception has been thrown by the target of an invocation. ---> System.Runtime.InteropServices.COMException (0x86DB0898):
      # the stencil is a shared document, already owned or managed by the template/document session. Closing it independently may be rejected by Visio
      write-host $_.Exception
    }
  }

  if ($null -ne $visApp) {
    $visApp.Quit()
  }
}

<#
Write-Host 'Output encoding:'
Write-Host [Console]::OutputEncoding.WebName

Write-Host 'Error encoding:'
Write-Host [Console]::Error.Encoding.WebName

Write-Host 'Output code page:'
Write-Host [Console]::OutputEncoding.CodePage

Write-Host 'Error code page:'
Write-Host [Console]::Error.Encoding.CodePage

$encoding = New-Object System.Text.UTF8Encoding($false)
$writer = New-Object System.IO.StreamWriter(
  [Console]::OpenStandardError(),
  $encoding
)
$writer.AutoFlush = $true
[Console]::SetError($writer)

[Console]::Error.WriteLine($shape.Name)

#>