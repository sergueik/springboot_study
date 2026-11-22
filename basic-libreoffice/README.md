### Info

This directory contains a replica of alpine based
[alpine based libreoffice container](https://github.com/woahbase/alpine-libreoffice) with fixes

For binary Microsoft Office `.xlsx`/`.xls` files, to extract the VB  sceipt / macros one first converts the subject to OpenDocument format via
using [Libreoffice](https://documentation.libreoffice.org/en/english-documentation/) in `--headless` mode in particular for converting files between formats, including PDF, HTML, DOC, DOCX, EPUB, plain text, etc.
using various built-in filters

### Usage

```sh
docker pull alpine/java:11-jre
```
```sh
export IMAGE=basic-libreoffice
docker build -f Dockerfile -t $IMAGE .
```

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
xmlstarlet fo -D Basic/VBAProject/ThisWorkbook.xml | xmlstarlet sel -t -v "//script:module" -| head -40
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
The `unoconv` implementation is not merely another way to run the same command:
it introduces a persistent UNO service, allowing multiple clients to submit conversion requests without repeatedly launching LibreOffice.
```sh
export IMAGE=basic-libreoffice-uno
docker build -t $IMAGE  -f Dockerfile.uno .
```
```sh
export IMAGE=basic-libreoffice-uno
export NAME=$IMAGE
docker run -d --name $NAME -p 8100:8100 $IMAGE
```
```sh
docker cp sample.xlsm $NAME:/tmp/
```

```sh
docker exec -ti $NAME unoconv --connection 'socket,host=127.0.0.1,port=8100,tcpNoDelay=1;urp;StarOffice.ComponentContext' -f ods /tmp/sample.xlsm
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
       46  2026-11-22 02:59   mimetype
     2018  2026-11-22 02:59   Basic/VBAProject/ThisWorkbook.xml
      405  2026-11-22 02:59   Thumbnails/thumbnail.png
...
     1707  2026-11-22 02:59   META-INF/manifest.xml
---------                     -------
    50364                     21 files
```
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
---

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

