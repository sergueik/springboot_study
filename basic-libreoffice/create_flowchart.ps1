
# CreateFlowchart.ps1
# based on:
# https://dellenny.com/approaches-to-automatically-generate-visio-diagrams-from-text/
# https://www.visguy.com/2006/09/13/create-visio-flowcharts-programmatically
# Visio COM Automation
# Compatible with Windows PowerShell 2.0 / Visio 2007

$ErrorActionPreference = "Stop"

# --------------------------------------------------
# Step 1: Get the flowchart template and stencil
# --------------------------------------------------

$flowchartTemplateName = "C:\Program Files\Microsoft Office\Office12\1049\BASFLO_M.VST"
$flowchartStencilName = "BASFLO_M.VSS"

$masterProcessName = "Process"
$masterDecisionName = "Decision"

$visApp = $null
$docFlowTemplate = $null
$docFlowStencil = $null

try {

    # Start Visio

    $visApp = New-Object -ComObject Visio.Application
    $visApp.Visible = $true

    # Open a new document using the flowchart template

    $docFlowTemplate = $visApp.Documents.Add(
        $flowchartTemplateName
    )

    # Search open documents for the flowchart stencil

    foreach ($doc in $visApp.Documents) {

        Write-Host "Document:" $doc.Name

        if ($doc.Name -eq $flowchartStencilName) {
            $docFlowStencil = $doc
            break
        }
    }

    # Fallback: open stencil explicitly if not found
    
    if ($null -eq $docFlowStencil) {

        # NOTE: system ACP locale is used in Office application resource path and name attributes 
        # en-us 0x409 1033
        # ru-ru 0x419 1049
        $stencilPath = "C:\Program Files\Microsoft Office\Office12\1049\BASFLO_M.VSS"

        $docFlowStencil = $visApp.Documents.OpenEx(
            $stencilPath,
            4
        )
    }

    # --------------------------------------------------
    # Step 2: Get masters and connector
    # --------------------------------------------------

    $mstProcess = $docFlowStencil.Masters.ItemU(
        $masterProcessName
    )

    $mstDecision = $docFlowStencil.Masters.ItemU(
        $masterDecisionName
    )

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

    $x = $pg.PageSheet.CellsU(
        "PageWidth"
    ).ResultIU / 2

    $y = $pg.PageSheet.CellsU(
        "PageHeight"
    ).ResultIU - 1

    $shpLast = $null
    $shpDec = $null
    $shpConn = $null

    for ($i = 1; $i -le $numShapes; $i++) {

        # Drop Process or Decision shape

        if ($i -eq $decisionStepNumber) {

            $shpNew = $pg.Drop(
                $mstDecision,
                $x,
                $y
            )

            $shpNew.Text = "$i?"

            # Remember decision shape

            $shpDec = $shpNew
        }
        else {

            $shpNew = $pg.Drop(
                $mstProcess,
                $x,
                $y
            )

            $shpNew.Text = "Do Step $i"
        }

        # Set custom properties

        $shpNew.CellsU(
            "Prop.Cost"
        ).ResultIU = $i

        # Duration expressed in elapsed hours
<#
        $shpNew.CellsU(
            "Prop.Duration"
        ).FormulaU = ("$i h")
		#>
<# Exception setting "FormulaU": "
#NAME?"
#>
        # Connect shapes

        if ($i -ne 1) {

            # Drop connector

            $shpConn = $pg.Drop(
                $conn,
                0,
                0
            )

            # Glue BeginX

            if ($i -eq ($decisionStepNumber + 1)) {

                # Glue to right-side decision connection

                $shpConn.CellsU(
                    "BeginX"
                ).GlueTo(
                    $shpDec.CellsU("Connections.X2")
                )
            }
            else {

                # Dynamic glue to previous shape

                $shpConn.CellsU(
                    "BeginX"
                ).GlueTo(
                    $shpLast.CellsU("PinX")
                )
            }

            # Glue EndX to new shape

            $shpConn.CellsU(
                "EndX"
            ).GlueTo(
                $shpNew.CellsU("PinX")
            )
        }

        # Prepare for next iteration

        $shpLast = $shpNew

        # Adjust coordinates according to branch

        if ($i -eq $decisionStepNumber) {

            $x = $x + $dx
        }
        elseif ($i -eq ($decisionStepNumber + 1)) {

            $x = $x - $dx
            $y = $y - $dy

            # Label decision branch

            $shpConn.Text = "No"

            # Next connector starts at decision shape

            $shpLast = $shpDec
        }
        else {

            $y = $y - $dy
        }
    }

    # --------------------------------------------------
    # Step 4: Layout and deselect
    # --------------------------------------------------

    # Original example leaves automatic layout disabled.
    # Uncomment to experiment:
    #
    # $pg.Layout()

    # Deselect all shapes

    $visApp.ActiveWindow.DeselectAll()

    # Original example:
    #
    # Visio.Windows.Arrange(
    #     Visio.visArrangeTileVertical
    # )
    #
    # This requires resolving the Visio enum value
    # or using the installed Visio interop assembly.

    Write-Host "Flowchart created successfully."

}
finally {

    # Close documents and quit Visio

    if ($null -ne $docFlowTemplate) {
		$docFlowTemplate.Layout
		$docFlowTemplate.SaveAs( "C:\DOCUME~1\Admin\LOCALS~1\Temp\AutomatedFlowchart.vsd")
		$docFlowTemplate.Saved = $true
        $docFlowTemplate.Close()
    }

    if ($null -ne $docFlowStencil) {
      try {
        # $docFlowStencil.Close
        # MemberType          : Method
        # OverloadDefinitions : {void Close ()}
            $docFlowStencil.Close()
      } catch [Exception] { 
        # System.Reflection.TargetInvocationException: Exception has been thrown by the target of an invocation. ---> System.Runtime.InteropServices.COMException (0x86DB0898):
        write-host $_.Exception
      }
    }

    if ($null -ne $visApp) {
        $visApp.Quit()
    }

}
	