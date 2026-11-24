### Info

This directory contains a replica of alpine based
[alpine based libreoffice container](https://github.com/woahbase/alpine-libreoffice) with fixes

For binary Microsoft Office `.xlsx`/`.xls` files, to extract the VB  sceipt / macros one first converts the subject to OpenDocument format via
using [Libreoffice](https://documentation.libreoffice.org/en/english-documentation/) in `--headless` mode in particular for converting files between formats, including PDF, HTML, DOC, DOCX, EPUB, plain text, etc.
using various built-in filters

The [unoconv](http://dag.wiee.rs/home-made/unoconv/) __Universal Office Converter__ is a command-line tool that converts document formats supported by __LibreOffice__ or [OpenOffice](https://en.wikipedia.org/wiki/Apache_OpenOffice) using [Univerdsal Network Object](https://en.wikipedia.org/wiki/Universal_Network_Objects) __UNO__ bindings - efectively runs a extra LibreOffice process via `popen()` prinarily to convert a caller provided document that format __LibreOffice__ can import to any document format that __LibreOffice__ can produce. It is deprecated in favor of next generation rewrite of Unoconv called [Unoserver](https://github.com/unoconv/unoserver/).
The __unoconv__ supervises __LibreOffice__, which owns the socket listener.
Both [unoserver](https://pypi.org/project/unoserver/) and [unoconv](https://pypi.org/project/unoconv/) avilable as pip packages. There is a remark that Windows and Mac support is as of yet untested: Unoserver needs to be installed by and run with the same Python installation that LibreOffice uses, to properly run the unoserver command.

### Visio Support

__LibreOffice__ first added support for opening [Microsoft Visio] files in 3.5 - released in February 2012 -
with full support for newer `.vsdx` OpenDoc Visio combined semantics files arriving in LibreOffice 4.0 -
Release [timeline](https://www.libreoffice.org/media/OOo-major-derivatives.png) lists expanded coverage to support all existing Visio file formats, spanning from original __Visio__ __1.0__ (1992) up through the OpenDoc XML-based `.vsdx`
format introduced in __Microsoft Visio__ __2013__.

LibreOffice Desktop application features input only for Visio:  can read/import .vsdx (and other Visio formats), but not write/export Visio formats.
• Native drawing formats: Save as OpenDocument Drawing (.odg) or OpenDocument Drawing Template (.otg) to keep editable diagram layers and objects in LibreOffice Draw.
• Document & vector export: Export or convert to PDF (.pdf), Scalable Vector Graphics (.svg), Encapsulated PostScript (.eps), Enhanced Metafile (.emf), or Windows Metafile (.wmf).
• Raster image export: Export to PNG (.png), JPEG (.jpg/.jpeg), BMP (.bmp), TIFF (.tif/.tiff), or GIF (.gif).


### Samle Data

One can find the early format of Visio x.y  released in 1992 by __Shapeware Corporation__ (later __Visio Corporation__)
and acquired by Microsoft in 2000 samples known as
[20 Visio 2003 Sample Diagrams](https://legacyupdate.net/download-center/download/24023/visio-2003-sample-20-sample-diagrams)
from Microsoft Download Center Archive:

|Description | Filename|
|------------|---------|
|Active Directory diagram | ActvDir.vsd|
|Basic Flowchart | Bflowcht.vsd|
|Block Diagram - BlkDiagm.vsd|
|Building Floor Plan | BldgPlan.vsd|
|Basic Network diagram | Bnetwork.vsd|
|Brainstorming/Mind map | Brainstm.vsd|
|Calendar | Calendar.vsd|
|Database diagram | Database.vsd|
|Detailed Network diagram | Dnetwork.vsd|
|Electrical Engineering Control diagram | EECtrl.vsd|
|Fluid Power diagram | FluidPwr.vsd|
|Meeting Room layout | MeetRoom.vsd|
|Organization Chart | OrgChart.vsd|
|Process Engineering diagram | ProcEng.vsd|
|IT Rack diagram | Rack.vsd|
|Timeline diagram | Timeline.vsd|
|UML diagram | UML.vsd|
|Web Design diagram | WebDsgn.vsd|
|Web Site diagram | WebSite.vsd|
|Window UI diagram | WinUI.vsd |


|Filename|Description| Comment |
|----|-----|------|
|`ActvDir.vsd`|Active Directory diagram||
|`Bflowcht.vsd`|Basic Flowchart|Two pages, used as `sample.vsd`|
|`BlkDiagm.vsd`|Block Diagram||
|`BldgPlan.vsd`|Building Floor Plan||
|`Bnetwork.vsd`|Basic Network diagram||
|`Brainstm.vsd`|Brainstorming/Mind map||
|`Calendar.vsd`|Calendar||
|`Database.vsd`|Database diagram||
|`Dnetwork.vsd`|Detailed Network diagram||
|`EECtrl.vsd`|Electrical Engineering Control diagram||
|`FluidPwr.vsd`|Fluid Power diagram||
|`MeetRoom.vsd`|Meeting Room layout||
|`OrgChart.vsd`|Organization Chart||
|`ProcEng.vsd`|Process Engineering diagram||
|`Rack.vsd`|IT Rack diagram||
|`Timeline.vsd`|Timeline diagram||
|`UML.vsd`|UML diagram||
|`WebDsgn.vsd`|Web Design diagram||
|`WebSite.vsd`|Web Site diagram||
|`WinUI.vsd`|Window UI diagram||

packaged as `VisioSam.exe`

> NOTE: This download is no longer available on microsoft.com.
The downloads below links provided by the Internet Archive Wayback Machine from the Microsoft Download Center are also not workig,
Link listed on https://www.visiocafe.com/microsoft.htm is no longer available.
The https://download.cnet.com/visio-2003-sample-20-sample-diagrams has the non expired __Visio 2003 Sample: 20 Sample Diagrams for Windows__ copy.

One can get a [Open Packaging Conventions](https://en.wikipedia.org/wiki/Open_Packaging_Conventions) __OPC__ Visio 365 [Microsoft Visio VSDX Files Download](https://www.howtoanalyzedata.net/microsoft-visio-vsdx-files-download)
and from  [featured Visio templates and diagrams](https://support.microsoft.com/en-us/visio/featured-visio-templates-and-diagrams)

```text
'BPMN cross-functional chart tf16393606.vstx'
'Bank Account database tf22876660.vsdx'
'Basic UML communication tf22725921.vsdx'
'Basic UML component tf33765020.vsdx'
'Basic UML deployment tf22667440.vsdx'
'Basic UML use case tf11670301.vsdx'
'Basic flowchart tf11712902.vstx'
'Basic home network tf78855695.vstx'
'Computer block diagram tf16403437.vsdx'
'Cross-functional flowchart tf56636647.vstx'
'Detailed network tf33958593.vstx'
'Ethernet LAN diagram tf16393633.vstx'
'Login-registry activity tf56142499.vsdx'
'Office network plan tf16403889.vsdx'
'Property buying flow tf16403429.vsdx'
'Star network diagram tf16403888.vsdx'
'UML database for employees tf33998165.vsdx'
'UML state ATM tf11406762.vsdx'
```
### Usage

```sh
docker pull alpine:3.22
```
```sh
export IMAGE=basic-libreoffice
docker build -f Dockerfile -t $IMAGE .
```
> NOTE this Dockerfile is one-shot direct conversion: installs __LibreOffice__ and starts a shell. The __LibreOffice__  is invoked once fot onr conversion and container exits and is removed
```sh
export IMAGE=basic-libreoffice
export NAME=$IMAGE
mkdir -p ~/data; cp sample.xlsm ~/data
docker run --entrypoint sh --rm -it --name $NAME --hostname libreoffice -e PGID=1000 -e PUID=1000 -c 512 -m 2096m -v /c/Users/${USERNAME}/data:/home/alpine $IMAGE /usr/bin/libreoffice --headless --convert-to ods:"calc8" sample.xlsm
docker container prune -f
```
```text
convert /home/alpine/sample.xlsm as a Calc document -> /home/alpine/sample.ods using filter: calc8
Overwriting: /home/alpine/sample.ods
```
```sh
pushd ~/data/
file sample.ods
```
```text
sample.ods: OpenDocument Spreadsheet
```
"wird es interessanter und sogar  höchst interessant":

```sh
unzip -ql sample.ods
```
```text
  Length      Date    Time    Name
---------  ---------- -----   ----
       46  2026-11-21 22:55   mimetype
     2018  2026-11-21 22:55   Basic/VBAProject/ThisWorkbook.xml
      405  2026-11-21 22:55   Basic/VBAProject/Sheet1 (Sheetname1).xml
...
     2491  2026-11-21 22:55   Thumbnails/thumbnail.png
     1707  2026-11-21 22:55   META-INF/manifest.xml
---------                     -------
    50364                     21 files
```
> NOTE: the original VBA project is not necessarily preserved "byte-for-byte" - it is LibreOffice's representation of the imported project.
```sh
unzip -uo sample.ods  Basic/VBAProject/ThisWorkbook.xml
```
```text
Archive:  sample.ods
  inflating: Basic/VBAProject/ThisWorkbook.xml
```
```sh
file Basic/VBAProject/ThisWorkbook.xml
```
```text
Basic/VBAProject/ThisWorkbook.xml: XML 1.0 document, ASCII text
```
```sh
FILE=Basic/VBAProject/ThisWorkbook.xml
FILE=Basic/VBAProject/Module1.xml
xmlstarlet fo -D $FILE | xmlstarlet sel -t -v "//script:module" -| head -40
```
> NOTE: on Windows xmlstarlet binary is called `xml.exe`
on RHEL based system use
```sh
xmllint --format Basic/VBAProject/ThisWorkbook.xml | sed -n '/<script:module/,/<\/script:module>/p' | head -40
```

this will yield extract the raw  VB-like content:
```text
Rem Attribute VBA_ModuleType=VBADocumentModule
Option VBASupport 1
Sub NameSheetsFromAList()

    'iterate from 2 to last row count, name them. then name sheet 1
    'then delete content of sheet 1

    'list of sheet names headings should start from cells(1,1) of first sheet in the active workbook
    'should not contain blank cells within the list.
    'macro renames the first sheet with the value of first sheet's first cell from top left

    Sheets(1).Select

    Dim sname1 As String
    Dim numberOfSheets As Integer
    Dim numiterations As Integer
    Dim sheetnamearay() As String
    numberOfSheets = Cells(Rows.Count, 1).End(xlUp).Row



    sname1 = Cells(1, 1).Value
    numiterations = 1
    ReDim sheetnamearray(numberOfSheets)


    For numiterations = 1 To numberOfSheets
        sheetnamearray(numiterations - 1) = Cells(numiterations, 1).Value
    Next numiterations

    For numiterations = 2 To numberOfSheets
        'add number of elements in array - 1 sheets. name them.
        ActiveWorkbook.Sheets.Add(After:=ActiveWorkbook.Sheets(ActiveWorkbook.Sheets.Count)).Name = sheetnamearray(numiterations - 1)
    Next numiterations

    Sheets(1).Name = sheetnamearray(0)
    Sheets(1).Select

    For numiterations = 1 To numberOfSheets
        Cells(numiterations, 1).Value = ""
```

which matches quite close to what one find in the desktop application or Microsoft Office Excel.

![Capture Libreoffice Macro Explore](screenshots/capture-libreoffice-macros.png)

### Using Uno Conv

> NOTE this Dockerfile is intended for repeaded conversions

```sh
export IMAGE=basic-libreoffice-uno
docker build -t $IMAGE -f Dockerfile.uno .
```
```sh
docker images $IMAGE
```
```text
REPOSITORY              TAG                 IMAGE ID            CREATED             SIZE
basic-libreoffice-uno   latest              a919aa25fd55        2 minutes ago       1.15GB
```
```sh
export IMAGE=basic-libreoffice-uno
export NAME=$IMAGE
docker run -d --name $NAME -p 8100:8100 $IMAGE
```
or

```sh
docker start $NAME
```
 
```sh
docker cp sample.xls $NAME:/tmp/
```

```sh
docker exec -it $NAME unoconv --connection 'socket,host=127.0.0.1,port=8100,tcpNoDelay=1;urp;StarOffice.ComponentContext' -f ods /tmp/sample.xls
```
```sh
docker cp $NAME:/tmp/sample.ods .
```
```sh
unzip -ql sample.ods
```
```text
  Length      Date    Time    Name
---------  ---------- -----   ----
       46  2026-11-23 21:45   mimetype
      364  2026-11-23 21:45   Basic/VBAProject/ThisWorkbook.xml
      358  2026-11-23 21:45   Basic/VBAProject/Sheet1.xml
     3887  2026-11-23 21:45   Basic/VBAProject/Module1.xml
      440  2026-11-23 21:45   Basic/VBAProject/script-lb.xml
      287  2026-11-23 21:45   Basic/Standard/script-lb.xml
      405  2026-11-23 21:45   Basic/script-lc.xml
     5021  2026-11-23 21:45   settings.xml
        0  2026-11-23 21:45   Configurations2/toolpanel/
        0  2026-11-23 21:45   Configurations2/floater/
        0  2026-11-23 21:45   Configurations2/statusbar/
        0  2026-11-23 21:45   Configurations2/toolbar/
        0  2026-11-23 21:45   Configurations2/progressbar/
        0  2026-11-23 21:45   Configurations2/popupmenu/
        0  2026-11-23 21:45   Configurations2/menubar/
    35858  2026-11-23 21:45   styles.xml
      899  2026-11-23 21:45   manifest.rdf
    63166  2026-11-23 21:45   content.xml
      901  2026-11-23 21:45   meta.xml
   106311  2026-11-23 21:45   Pictures/100000010000025D0000033A421DE03B.png
    12202  2026-11-23 21:45   Pictures/10002FAA00006A2C000090EF5DA07E0C.wmf
    22254  2026-11-23 21:45   Thumbnails/thumbnail.png
     1943  2026-11-23 21:45   META-INF/manifest.xml
---------                     -------
   254342                     23 files
```
```sh
unzip -d /tmp -uqq sample.ods
```
```text
Archive:  sample.ods
 extracting: mimetype
  inflating: Basic/VBAProject/ThisWorkbook.xml
  inflating: Basic/VBAProject/Sheet1.xml
  inflating: Basic/VBAProject/Module1.xml
  inflating: Basic/VBAProject/script-lb.xml
  inflating: Basic/Standard/script-lb.xml
  inflating: Basic/script-lc.xml
  inflating: settings.xml
   creating: Configurations2/toolpanel/
   creating: Configurations2/floater/
   creating: Configurations2/statusbar/
   creating: Configurations2/toolbar/
   creating: Configurations2/progressbar/
   creating: Configurations2/popupmenu/
   creating: Configurations2/menubar/
  inflating: styles.xml
  inflating: manifest.rdf
  inflating: content.xml
  inflating: meta.xml
 extracting: Pictures/100000010000025D0000033A421DE03B.png
  inflating: Pictures/10002FAA00006A2C000090EF5DA07E0C.wmf
 extracting: Thumbnails/thumbnail.png
  inflating: META-INF/manifest.xml

```
```sh
$file = join-path -path $env:TEMP -childpath '/Basic/VBAProject/Module1.xml'
$namespace = 'http://openoffice.org/2000/script'
[xml]$xml = get-content -raw $file
$document_type = $xml.DocumentType

if ($null -ne $document_type) {
  [Console]::Error.WriteLine("Name:     " + $document_type.Name)
  [Console]::Error.WriteLine("PublicId: " + $document_type.PublicId)
  [Console]::Error.WriteLine("SystemId: " + $document_type.SystemId)
}
$node = $xml.SelectSingleNode("//*[local-name()='module']")
if ($null -ne $node) {

  $namespace = $node.GetAttribute('xmlns:script')
  $ns = New-Object System.Xml.XmlNamespaceManager($xml.NameTable)
  $ns.AddNamespace('script', $namespace)

  $node = $xml.SelectSingleNode('//script:module', $ns)
  $node.GetNamespaceOfPrefix('script')
  if ($null -ne $node) {
    [Console]::Error.WriteLine(($node.InnerText -split "`r?`n" | select-object -first 40) -join [char]10)
  }
}
```
```vb
Rem Attribute VBA_ModuleType=VBAModule
Option VBASupport 1
Function checkSales(inThisRange As Range) As Boolean
    'checks if the sales of at least one store are entered
    checkSales = WorksheetFunction.CountA(inThisRange) > 0
End Function

Sub processSales()
    'This is a macro to process the daily sales of We Are Nuts store chain.
    'In this macro, we do the following activities

    '   1. Check if at least one store sales are entered. If not exit
    '   2. For each store's sales, check if the value is < 500 or > 5000
    '       2.1 If so, ask and capture the reason for deviation
    '   3. Show summary statistics of the sales
    '   4. Save a snapshot of sales log to current folder as PDF

    If checkSales(Range("C7:C30")) = False Then
        Debug.Print "No Sales"
        Exit Sub
    End If

    Dim storeSales As Range
    Dim sumOfSales As Double, curSale As Double
    Dim countOfSales As Integer
    Dim minSale As Double, maxSale As Double
    Dim reason As String, message As String, minStore As String, maxStore As String

    sumOfSales = 0
    countOfSales = 0
    minSale = 999999#
    maxSale = 0
    minStore = 0
    maxStore = 0

    For Each storeSales In Range("C7:C30")
        If storeSales.Value <> "" And storeSales.Value <> 0 Then
            curSale = storeSales.Value
            countOfSales = countOfSales + 1
            sumOfSales = sumOfSales + curSale
```
### Visio to LibreOffice Draw Conversion (Lossy?)

```sh
export IMAGE=basic-libreoffice-uno
docker build -t $IMAGE -f Dockerfile.uno .
```
```sh
export IMAGE=basic-libreoffice-uno
export NAME=$IMAGE
docker run -d --name $NAME -p 8100:8100 $IMAGE
```

```sh
docker cp sample.vsd $NAME:/tmp/
```
```sh
docker exec -ti $NAME unoconv --connection 'socket,host=127.0.0.1,port=8100,tcpNoDelay=1;urp;StarOffice.ComponentContext' -f odg /tmp/sample.vsd
```
```text
if product.ooName not in ('LibreOffice', 'LOdev') or LooseVersion(product.ooSetupVersion) <= LooseVersion('3.3'):
```
```sh
docker cp $NAME:/tmp/sample.odg .
```
```sh
file sample.odg
```
```text
sample.odg: OpenDocument Drawing
```

The result

![capture libreoffice draw](screenshots/capture-libreoffice-draw.png)

looks very well match to the original

![capture visio 2003](screenshots/capture-visio2003.png)

and the edges/nodes selectable individually

However the Semantic fidelity is not preserved:
As this becomes immediately apparent when trying to
__move a connected node around and inspect whether its connectors remain attached__

While native __Visio__ retains the connector attachments:

![capture Editing in visio 2003](screenshots/capture-visio2003-editing.png)

__LibreOffice__ does not:

![capture Editing in Libre Office Draw](screenshots/capture-libreoffice-draw-editing.png)


One can download individual templates and diagram examples from from  [featured Visio templates and diagrams](https://support.microsoft.com/en-us/visio/featured-visio-templates-and-diagrams)
 using curl command / `Invoke-WebRequest` Powershell cmdlet / `[System.Net.Http.HttpClient]` with resource URLs
`https://createcatalog.public.onecdn.static.microsoft/support/templates/en-us/tf33958593.vstx` and similar or download in the browser
 - for some files download will be made pending the approval.


#### Data augmentation through controlled perturbation of the original diagrams


Bar analogy is good:

- Same person.
- Same underlying identity.
- Same driver ID.
- Progressively different appearance after each shot.
- Eventually, identifying the person from the observed appearance becomes increasingly difficult


|Term|Meaning in Visio Conversion experiment|
|----|--------------------------|
|Data augmentation|Generate additional examples from existing diagrams|
|Controlled perturbation|Change geometry while preserving diagram semantics|
|Invariant representation|Identify the same logical relationships despite geometric changes|
|Domain randomization|Expose the model to many variations of the same underlying structure|
|Ground-truth labeling|Retain the original Visio connection graph as the correct answer|


### Automation Exercise
```powershell

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

    Write-Host ('ID: {0} Name: "{1}" NameU: "{2}" Text: "{3}" OneD: {4} Master ID: {5} Master Name: "{6}" Master NameU: "{7}"' -f $shape.ID , $shape.Name, $shape.NameU, $shape.Text, $shape.OneD, $shape.Master.ID, $shape.Master.Name,$shape.Master.NameU )

    if ($shape.OneD) {

      # https://learn.microsoft.com/en-us/office/client-developer/visio/gluetype-cell-glue-info-section
      Write-Host ('EndX: "{0}" {1}' -f  $shape.CellsU('EndX').ResultIU, $shape.CellsU('EndX').GlueType)
      Write-Host ('BeginX: "{0}" {1}' -f $shape.CellsU('BeginX').ResultIU, $shape.CellsU('BeginX').GlueType)

    } else {

      Write-Host 'Master:' $shape.Master.Name
      Write-Host 'MasterU:' $shape.Master.NameU

      Write-Host 'PinX:' $shape.CellsU('PinX').ResultIU
      Write-Host 'PinY:' $shape.CellsU('PinY').ResultIU
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
	
```
```powershell
. .\create_flowchart.ps1
```
```text
Flowchart created successfully.
ID: 1 Name: "Процесс" NameU: "Process" Text: "Do Step 1" OneD: 0 Master ID: 0 Master Name: "Процесс" Master NameU: "Process"
Master: Процесс
MasterU: Process
PinX: 4.13385826771654
PinY: 8.26033464566929
ID: 2 Name: "Процесс.2" NameU: "Process.2" Text: "Do Step 2" OneD: 0 Master ID: 0 Master Name: "Процесс" Master NameU: "Process"
Master: Процесс
MasterU: Process
PinX: 4.13385826771654
PinY: 7.29478346456693
ID: 3 Name: "Динамическая соединительная линия" NameU: "Dynamic connector" Text: "" OneD: -1 Master ID: 2 Master Name: "Динамическая соединительная линия" Master NameU: "Dynamic connector"
EndX: "4.13385826771654"
BeginX: "4.13385826771654"
ID: 4 Name: "Решение" NameU: "Decision" Text: "3?" OneD: 0 Master ID: 3 Master Name: "Решение" Master NameU: "Decision"
Master: Решение
MasterU: Decision
PinX: 4.13385826771654
PinY: 6.32923228346457
ID: 5 Name: "Динамическая соединительная линия.5" NameU: "Dynamic connector.5" Text: "" OneD: -1 Master ID: 2 Master Name: "Динамическая соединительная линия" Master NameU: "Dynamic connector"
EndX: "4.13385826771654"
BeginX: "4.13385826771654"
ID: 6 Name: "Процесс.6" NameU: "Process.6" Text: "Do Step 4" OneD: 0 Master ID: 0 Master Name: "Процесс" Master NameU: "Process"
Master: Процесс
MasterU: Process
PinX: 4.8134842519685
PinY: 5.3636811023622
ID: 7 Name: "Динамическая соединительная линия.7" NameU: "Dynamic connector.7" Text: "No" OneD: -1 Master ID: 2 Master Name: "Динамическая соединительная линия" Master NameU: "Dynamic connector"
EndX: "4.8134842519685"
BeginX: "4.6259842519685"
ID: 8 Name: "Процесс.8" NameU: "Process.8" Text: "Do Step 5" OneD: 0 Master ID: 0 Master Name: "Процесс" Master NameU: "Process"
Master: Процесс
MasterU: Process
PinX: 3.45423228346457
PinY: 5.3636811023622
ID: 9 Name: "Динамическая соединительная линия.9" NameU: "Dynamic connector.9" Text: "" OneD: -1 Master ID: 2 Master Name: "Динамическая соединительная линия" Master NameU: "Dynamic connector"
EndX: "3.45423228346457"
BeginX: "4.13385826771654"
ID: 10 Name: "Процесс.10" NameU: "Process.10" Text: "Do Step 6" OneD: 0 Master ID: 0 Master Name: "Процесс" Master NameU: "Process"
Master: Процесс
MasterU: Process
PinX: 3.45423228346457
PinY: 4.39812992125984
ID: 11 Name: "Динамическая соединительная линия.11" NameU: "Dynamic connector.11" Text: "" OneD: -1 Master ID: 2 Master Name: "Динамическая соединительная линия" Master NameU: "Dynamic connector"
EndX: "3.45423228346457"
BeginX: "3.45423228346457"
ID: 12 Name: "Процесс.12" NameU: "Process.12" Text: "Do Step 7" OneD: 0 Master ID: 0 Master Name: "Процесс" Master NameU: "Process"
Master: Процесс
MasterU: Process
PinX: 3.45423228346457
PinY: 3.43257874015748
ID: 13 Name: "Динамическая соединительная линия.13" NameU: "Dynamic connector.13" Text: "" OneD: -1 Master ID: 2 Master Name: "Динамическая соединительная линия" Master NameU: "Dynamic connector"
EndX: "3.45423228346457"
BeginX: "3.45423228346457"
0


System.Reflection.TargetInvocationException: Exception has been thrown by the target of an invocation. ---> System.Runtime.InteropServices.COMException (0x86DB0898):

Возникло исключение.
   --- End of inner exception stack trace ---
   at System.RuntimeType.InvokeDispMethod(String name, BindingFlags invokeAttr,Object target, Object[] args, Boolean[] byrefModifiers, Int32 culture, String[]namedParameters)
   at System.RuntimeType.InvokeMember(String name, BindingFlags bindingFlags, Binder binder, Object target, Object[] providedArgs, ParameterModifier[] modifiers, CultureInfo culture, String[] namedParams)
   at System.Management.Automation.ComMethod.InvokeMethod(PSMethod method, Object[] arguments)
```
```text
 Directory of C:\DOCUME~1\Admin\LOCALS~1\Temp

11/22/2026  03:49 PM    <DIR>          WPDNSE
11/22/2026  05:34 PM                81 dw.log
11/22/2026  05:42 PM            38,400 AutomatedFlowchart.vsd
11/22/2026  05:42 PM    <DIR>          ..
11/22/2026  05:42 PM    <DIR>          .
               2 File(s)         38,481 bytes
```

![Capture generated diagram](screenshots/capture-visio-scripted.png)

### Macro Enabled Office Documents

  * https://learn.microsoft.com/en-us/answers/questions/5834185/enable-macro-in-excel
  * https://support.microsoft.com/en-us/office/vba/enable-or-disable-macros-in-microsoft-365-files
  * [Parikshit-Hooda/VBA-excel-examples](https://github.com/Parikshit-Hooda/VBA-excel-examples) - code examples written in VBA to interact with Excel - includes Macro Enabled Workbooks

```sh
/c/Program\ Files/7-Zip/7z.exe  x WordFileTool/Test/macro-enabled_workbook1.xlsm
```
```
├───docProps
├───xl
│   ├───theme
│   ├───worksheets
│   └───_rels
└───_rels
```
```sh
/c/Program\ Files/7-Zip/7z.exe  t xl/vbaProject.bin
```
```
7-Zip 21.07 (x64) : Copyright (c) 1999-2021 Igor Pavlov : 2021-12-26

Scanning the drive for archives:
1 file, 16896 bytes (17 KiB)

Testing archive: xl\vbaProject.bin
--
Path = xl\vbaProject.bin
Type = Compound
Physical Size = 16896
Extension = compound
Cluster Size = 512
Sector Size = 64

Everything is Ok

Folders: 1
Files: 12
Size:       12354
Compressed: 16896


```

```sh
find .
```
```txt
./PROJECT
./PROJECTwm
./VBA
./VBA/dir
./VBA/Sheet1
./VBA/ThisWorkbook
./VBA/_VBA_PROJECT
./VBA/__SRP_0
./VBA/__SRP_1
./VBA/__SRP_2
./VBA/__SRP_3
./VBA/__SRP_4
./VBA/__SRP_5
```
![Capture Excel 2003 Macro Editor](screenshots/capture-excel-2003-macros.png)
```
Rem Attribute VBA_ModuleType=VBADocumentModule
Option VBASupport 1
Sub NameSheetsFromAList()

    'iterate from 2 to last row count, name them. then name sheet 1
    'then delete content of sheet 1

    'list of sheet names headings should start from cells(1,1) of first sheet in the active workbook
    'should not contain blank cells within the list.
    'macro renames the first sheet with the value of first sheet's first cell from top left

    Sheets(1).Select

    Dim sname1 As String
    Dim numberOfSheets As Integer
    Dim numiterations As Integer
    Dim sheetnamearay() As String
    numberOfSheets = Cells(Rows.Count, 1).End(xlUp).Row



    sname1 = Cells(1, 1).Value
    numiterations = 1
    ReDim sheetnamearray(numberOfSheets)


    For numiterations = 1 To numberOfSheets
        sheetnamearray(numiterations - 1) = Cells(numiterations, 1).Value
    Next numiterations

    For numiterations = 2 To numberOfSheets
        'add number of elements in array - 1 sheets. name them.
        ActiveWorkbook.Sheets.Add(After:=ActiveWorkbook.Sheets(ActiveWorkbook.Sheets.Count)).Name = sheetnamearray(numiterations - 1)
    Next numiterations

    Sheets(1).Name = sheetnamearray(0)
    Sheets(1).Select

    For numiterations = 1 To numberOfSheets
        Cells(numiterations, 1).Value = ""
    Next numiterations

   ' Debug.Print numberOfSheets
    'check if capturing right number of sheets
   ' Sheets.Add
  '  Sheets(1).Select
   ' ActiveSheet.Move After:=Worksheets(Worksheets.Count)
  '  Sheets(2).Name = "Number of sheets"
  '  Cells(1, 1).Value = numberOfSheets

End Sub

```
![Capture LibreOffice Macro Editor](screenshots/capture-libreoffice-calc-macros.png)
```vba
Sub NameSheetsFromAList()

    'iterate from 2 to last row count, name them. then name sheet 1
    'then delete content of sheet 1

    'list of sheet names headings should start from cells(1,1) of first sheet in the active workbook
    'should not contain blank cells within the list.
    'macro renames the first sheet with the value of first sheet's first cell from top left

    Sheets(1).Select

    Dim sname1 As String
    Dim numberOfSheets As Integer
    Dim numiterations As Integer
    Dim sheetnamearay() As String
    numberOfSheets = Cells(Rows.Count, 1).End(xlUp).Row



    sname1 = Cells(1, 1).Value
    numiterations = 1
    ReDim sheetnamearray(numberOfSheets)


    For numiterations = 1 To numberOfSheets
        sheetnamearray(numiterations - 1) = Cells(numiterations, 1).Value
    Next numiterations

    For numiterations = 2 To numberOfSheets
        'add number of elements in array - 1 sheets. name them.
        ActiveWorkbook.Sheets.Add(After:=ActiveWorkbook.Sheets(ActiveWorkbook.Sheets.Count)).Name = sheetnamearray(numiterations - 1)
    Next numiterations

    Sheets(1).Name = sheetnamearray(0)
    Sheets(1).Select

    For numiterations = 1 To numberOfSheets
        Cells(numiterations, 1).Value = ""
    Next numiterations

   ' Debug.Print numberOfSheets
    'check if capturing right number of sheets
   ' Sheets.Add
  '  Sheets(1).Select
   ' ActiveSheet.Move After:=Worksheets(Worksheets.Count)
  '  Sheets(2).Name = "Number of sheets"
  '  Cells(1, 1).Value = numberOfSheets

End Sub


```

Not attempting to modernize the legacy application; you are establishing an inexpensive, repeatable early-cycle inspection mechanism

You open what looks like an ordinary Excel document. It has an unfamiliar extension. Suddenly, instead of a spreadsheet, you discover an entire application environment: VBA modules, forms, dialogs, menus, event handlers, editor panels, and a programming model you did not expect to encounter.

The immediate reaction is not necessarily enthusiasm. It is: How much of this new environment am I going to have to learn before I can even understand what I have opened?

And that leads to a sensible engineering response: find a smaller entry point


### A lightweight entry point into legacy Office applications

The initial objective is not necessarily to build an inexpensive tool in absolute terms. LibreOffice itself is a substantial application environment, and its automation infrastructure should not be underestimated.

The more relevant objective is to establish a repeatable, unattended inspection mechanism.

Microsoft Office offers extensive automation capabilities, but its COM-based automation model introduces a considerable execution and operational surface, particularly when attempting to scale unattended processing.

LibreOffice provides an alternative worth evaluating: whether its headless execution and UNO interfaces can expose enough of the embedded Office application structure to perform an initial inventory without launching an interactive development environment.

The experiment is deliberately narrow: determine whether a macro-enabled workbook can be inspected, its embedded project enumerated, and its source material exported in a repeatable headless workflow.

The desired economy is therefore not necessarily smaller software. It is less human intervention, less interactive exploration, and a shorter path to useful technical evidence.



### See Also

  * https://stackoverflow.com/questions/50982064/converting-docx-to-pdf-with-pure-python-on-linux-without-libreoffice
  * https://stackoverflow.com/questions/72434000/running-libreoffice-converter-on-docker

  * https://docs.linuxserver.io/images/docker-libreoffice
  * https://medium.com/codex/libreoffice-on-docker-1a64245468c
  * [known bug](https://bugs.documentfoundation.org/show_bug.cgi?id=37531&redirected_from=fdo) of Libreoffice will not run in "batch mode" when there is another instance open
  * [document](https://www.baeldung.com/spring-file-upload) and [source](https://github.com/eugenp/tutorials/tree/master/spring-web-modules/spring-mvc-java) on Spring App file Upoad - too many projects in the same umbrella
  * [upload files through curl](https://medium.com/@petehouston/upload-files-with-curl-93064dcccc76)
  * [spring uloading files getting started](https://spring.io/guides/gs/uploading-files/)
  * [1 pixel png used as spacer](https://commons.wikimedia.org/wiki/File:1x1.png)
  * https://stackoverflow.com/questions/35406213/how-to-copy-data-from-docker-volume-to-host
  * [standalone Office Development Suites for .Net](https://www.e-iceblue.com/Introduce/spire-office-for-net-free.html)
  * https://github.com/linuxserver/docker-libreoffice/blob/master/Dockerfile
  * https://github.com/hannesdejager/docker-libreoffice-api/blob/master/Dockerfile - an headless LibreOffice in Docker listening for API requests on port 8100
  * https://hub.docker.com/r/libreofficedocker/alpine
  * https://github.com/ipunkt/docker-libreoffice-headless
  * [converting doc/docx Format to PDF With Help of LibreOffice and LaTeX](https://www.baeldung.com/linux/latex-doc-docx-pdf-conversion)
  * [converting PDF to Word Documents in Linux](https://www.baeldung.com/linux/pdf-word-conversion)
  * [conversion of a PowerPoint PPT File to PDF From the Command Line](https://www.baeldung.com/linux/convert-powerpoint-ppt-to-pdf)
  * [convert CSV to XLS or XSLX in Linux](https://www.baeldung.com/linux/csv-xls-xslx)
  * [Excel VBA Examples](https://chandoo.org/wp/excel-vba/examples/)
  * __Excel VBA Crash Course__
    + [Part 1 of 5](https://chandoo.org/wp/introduction-to-vba-macros/) - what is VBA & Writing your First VBA Macro in Excel
    + [Part 2 of 5](https://chandoo.org/wp/variables-conditions-loops-in-vba/) - uloading
    + [Part 3 of 5](https://chandoo.org/wp/using-objects-in-vba-howto/) - how to use Cells, Ranges & Other Objects in Excel VBA
    + [Part 4 of 5](https://chandoo.org/wp/excel-vba-demo-application/) - Putting It All Together – Our First VBA Application
    + [Part 5 of 5](https://chandoo.org/wp/top-10-tips-for-excel-vba/) - top 10 Tips for Mastering VBA & Excel Macros
    + [Demo file](https://img.chandoo.org/vba/crash-course/our-first-vba-app-demo.xls) - Daily Sales Tracker VBA Workbook

--
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

