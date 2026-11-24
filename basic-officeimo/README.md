### Info

The OfficeIMO – Free cross-platform Microsoft Word .NET library

Assemblies for COM-free .NET app for creating, reading, editing, converting, rendering, and extracting Office, Visio, CHM, PDF, and other formants also including email, OneNote

### Usage

To install [OfficeIMO Studio](https://officeimo.com/downloads):
```sh
curl -skLo ~/Downloads/offesimo-stidio.deb https://github.com/EvotecIT/OfficeIMO/releases/download/Studio-v0.1.9767/officeimo-studio_0.1.9767_amd64.deb
```

To install Nuget dependencies (preserve the original filename - this may be reconsidered if your tool chain requires `.zip` extension):

```sh
curl -skLo ~/Downloads/OfficeIMO.Chm.3.6.0.nupkg https://github.com/EvotecIT/OfficeIMO/releases/download/OfficeIMO-v3.6.0/OfficeIMO.Chm.3.6.0.nupkg
```
```sh
curl -skLo ~/Downloads/OfficeIMO.Chm.Markdown.3.6.0.nupkg https://github.com/EvotecIT/OfficeIMO/releases/download/OfficeIMO-v3.6.0/OfficeIMO.Chm.Markdown.3.6.0.nupkg
``` 

```sh
curl -skLo ~/Downloads/OfficeIMO.Word.3.6.0.nupkg https://github.com/EvotecIT/OfficeIMO/releases/download/OfficeIMO-v3.6.0/OfficeIMO.Word.3.6.0.nupkg
```
```sh
curl -skLo ~/Downloads/OfficeIMO.PowerPoint.3.6.0.nupkg https://github.com/EvotecIT/OfficeIMO/releases/download/OfficeIMO-v3.6.0/OfficeIMO.PowerPoint.3.6.0.nupkg
```
```sh
curl -skLo ~/Downloads/OfficeIMO.Excel.3.6.0.nupkg https://github.com/EvotecIT/OfficeIMO/releases/download/OfficeIMO-v3.6.0/OfficeIMO.Excel.3.6.0.nupkg
```
```sh
curl -skLo ~/Downloads/OfficeIMO.Access.3.6.0.nupkg https://github.com/EvotecIT/OfficeIMO/releases/download/OfficeIMO-v3.6.0/OfficeIMO.Access.3.6.0.nupkg  
```
```sh
curl -skLo ~/Downloads/OfficeIMO.Pdf.3.6.0.nupkg https://github.com/EvotecIT/OfficeIMO/releases/download/OfficeIMO-v3.6.0/OfficeIMO.Pdf.3.6.0.nupkg
```
```sh
curl -skLo ~/Downloads/OfficeIMO.Visio.3.6.0.nupkg https://github.com/EvotecIT/OfficeIMO/releases/download/OfficeIMO-v3.6.0/OfficeIMO.Visio.3.6.0.nupkg
```

```sh
curl -skLo ~/Downloads/OfficeIMO-v3.6.0-src.tar.gz https://github.com/EvotecIT/OfficeIMO/archive/refs/tags/OfficeIMO-v3.6.0.tar.gz
```

```sh
unzip -ql ~/Downloads/OfficeIMO.Access.3.6.0.nupkg
```
```text
  Length      Date    Time    Name
---------  ---------- -----   ----
      483  2026-11-06 10:53   _rels/.rels
     1463  2026-11-06 10:53   OfficeIMO.Access.nuspec
     4670  2026-11-05 10:20   CAPABILITIES.md
     6956  2026-11-05 10:20   OfficeIMO.png
    17606  2026-11-05 10:20   README.md
    29896  2026-11-05 10:20   SUPPORT.md
     5705  2026-11-05 10:20   operations.json
     1084  2026-11-05 10:21   Licenses/Polyfill-LICENSE.txt
   291808  2026-11-06 09:53   lib/net10.0/OfficeIMO.Access.dll
    84341  2026-11-06 09:53   lib/net10.0/OfficeIMO.Access.xml
   297440  2026-11-06 09:53   lib/net472/OfficeIMO.Access.dll
    84341  2026-11-06 09:53   lib/net472/OfficeIMO.Access.xml
   291808  2026-11-06 09:53   lib/net8.0/OfficeIMO.Access.dll
    84341  2026-11-06 09:53   lib/net8.0/OfficeIMO.Access.xml
   296928  2026-11-06 09:53   lib/netstandard2.0/OfficeIMO.Access.dll
    84341  2026-11-06 09:53   lib/netstandard2.0/OfficeIMO.Access.xml
      780  2026-11-06 10:53   [Content_Types].xml
      783  2026-11-06 10:53   package/services/metadata/core-properties/nuget.psmdcp
    13114  2026-11-06 11:04   .signature.p7s

```
alternatively pull the dependency from nuget or its mirror - this may give slightly older versions:
```sh
curl -skLo ~/Downloads/officimo.excel.opendocument.3.1.0.zip https://www.nuget.org/api/v2/package/OfficeIMO.Excel.OpenDocument/3.1.0
```
> NOTE: the earliers release on nuget is only 7/14/2026. The downnlevel __.Net Framework__ supported is __4.7.2__
Based on age of the product and the timeline of their nuget package releases the team is quite likely be [vibe coders](https://en.wikipedia.org/wiki/Vibe_coding)


### See Also:

  * https://evotec.xyz/blog/officeimo-free-cross-platform-microsoft-word-net-library/
