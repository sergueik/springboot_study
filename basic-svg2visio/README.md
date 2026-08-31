### Info

Replica of [McMarius11/svgtovisio](https://github.com/McMarius11/svgtovisio) the standalone browser hosted __SVG__ & __Draw.io__ to __Visio__ Converter
useful for creating Visio resources *without* the __Visio__ itself


### Usage

```sh
docker pull node:22.12.0-alpine
docker pull nginx:1.30.3-alpine3.23
```
```sh
IMAGE=svg2visio
docker build -t $IMAGE -f Dockerfile .
```
```text
Sending build context to Docker daemon  1.192MB
Step 1/15 : FROM node:22.12.0-alpine AS builder
 ---> 3448d7ddbc59
Step 2/15 : WORKDIR /app
 ---> Running in cf76642e0bac
Removing intermediate container cf76642e0bac
 ---> 1edd1d0ab760
Step 3/15 : COPY package*.json /app/
 ---> c8a612bbf175
Step 4/15 : ARG NPM_REGISTRY
 ---> Running in bd5a1bc2e18f
Removing intermediate container bd5a1bc2e18f
 ---> aa04ca504fd3
Step 5/15 : RUN if [ -n "$NPM_REGISTRY" ]; then npm config set registry "$NPM_REGISTRY"; fi
 ---> Running in 6dcc84b1705d
Removing intermediate container 6dcc84b1705d
 ---> 409d7c0bd5f0
Step 6/15 : RUN npm install || { cat /root/.npm/_logs/*.log; exit 1; }
 ---> Running in a23bd4c675da
npm warn EBADENGINE Unsupported engine {
npm warn EBADENGINE   package: 'jsdom@29.0.1',
npm warn EBADENGINE   required: { node: '^20.19.0 || ^22.13.0 || >=24.0.0' },
npm warn EBADENGINE   current: { node: 'v22.12.0', npm: '10.9.0' }
npm warn EBADENGINE }

added 52 packages, and audited 53 packages in 8s

8 packages are looking for funding
  run `npm fund` for details

1 high severity vulnerability

To address all issues, run:
  npm audit fix

Run `npm audit` for details.
npm notice
npm notice New major version of npm available! 10.9.0 -> 12.0.2
npm notice Changelog: https://github.com/npm/cli/releases/tag/v12.0.2
npm notice To update run: npm install -g npm@12.0.2
npm notice
Removing intermediate container a23bd4c675da
 ---> 1a5ce95d8ba2
Step 7/15 : COPY app.js build.js index.html *-parser.js vsdx-builder.js test.js test-samples/ ./
 ---> 2c345a1b7427
Step 8/15 : RUN node build.js
 ---> Running in 3cbe8f56456c
Inlined: jszip.min.js (95.3 KB)
Inlined: pako.min.js (45.8 KB)
Inlined: svg-parser.js (25.3 KB)
Inlined: drawio-parser.js (14.1 KB)
Inlined: vsdx-builder.js (27.6 KB)
Inlined: app.js (13.0 KB)

Built: dist/index.html (227.2 KB)
Removing intermediate container 3cbe8f56456c
 ---> 4a775be93f35
Step 9/15 : FROM nginx:1.30.3-alpine3.23
 ---> d0701bd41f82
Step 10/15 : WORKDIR /usr/share/nginx/html
 ---> Running in 2153ef848ea8
Removing intermediate container 2153ef848ea8
 ---> 9c300351b656
Step 11/15 : COPY --from=builder /app/dist/index.html ./
 ---> de642adf477e
Step 12/15 : COPY --from=builder /app/*js ./
 ---> dc11548d027f
Step 13/15 : RUN  touch .nojekyll
 ---> Running in 6dfe576d5340
Removing intermediate container 6dfe576d5340
 ---> 791e23de7407
Step 14/15 : EXPOSE 80
 ---> Running in cde655b8ced2
Removing intermediate container cde655b8ced2
 ---> 3f68d61c0686
Step 15/15 : CMD ["nginx", "-g", "daemon off;"]
 ---> Running in fb8eb250e46b
Removing intermediate container fb8eb250e46b
 ---> 2c4efd63f39c
Successfully built 2c4efd63f39c
Successfully tagged svg2visio:latest
```
```sh    
NAME=svg2visio
docker run --name $NAME -d -p 8080:80 $IMAGE
```

![capture app in container](screenshots/capture-app-docker.png)

### Background

Convert [SVG](https://en.wikipedia.org/wiki/SVG) diagrams and [Draw.io](https://en.wikipedia.org/wiki/Diagrams.net) files to editable legacy [Visio](https://en.wikipedia.org/wiki/Microsoft_Visio) `.vsdx` files (a well documented [DatadiagramML](https://github.com/slebok/zoo/blob/master/zoo/markup/graphical/datadiagramml/xform/README.txt) XML schema honoring successor of the original Visio's `VSD` proprietary binary-file format that is still the default here and there).

###NAME=svg2visio
docker run --name $NAME -d -p 8080:80 $IMAGE
 Note


 * Download the __Graphviz__ binaries from [Graphviz](https://graphviz.org/download/)

```sh
curl -skLo ~/Downloads/graphviz-installer.exe https://gitlab.com/api/v4/projects/4207231/packages/generic/graphviz-releases/15.1.1/windows_10_cmake_Release_graphviz-install-15.1.1-win64.exe
```
and install it

* Pick any of gallery examples e.e. [Finite Automaton](https://graphviz.org/Gallery/directed/fsm.html) and convert it to SVG remotely or locally.
```sh
curl -skLo ~/Downloads/fsm.svg https://graphviz.org/Gallery/directed/fsm.svg
```
* convert to VISIO using the static page form:

![capture app in container](screenshots/capture-app2-docker.png)

The app simply downloads the result

![Saving the result](screenshots/capture-app3-docker.png)

Use MS [Free Visio Viewer](https://www.microsoft.com/en-us/microsoft-365/visio/free-visio-viewer) 
> CAUTION! Desktop app requires some kind of installation. Consider the risks!
> NOTE:  the default page contains *no* download link. The real download is available on [old page](https://www.microsoft.com/en-us/microsoft-365/blog/2012/11/28/download-the-free-microsoft-visio-viewer) [link](https://www.microsoft.com/en-us/download/details.aspx?id=35811)
```cmd
curl -skLo ~/Downloads/visioviewer64bit.exe https://download.microsoft.com/download/a/6/8/a682a83b-4866-4f88-ae83-61388629a891/visioviewer64bit.exe
```
```text
write-host (get-item -path  "C:\Program Files\Microsoft Office\Office15\VPREVIEW.EXE" | select-object -property * | format-list |out-string)


PSPath            : Microsoft.PowerShell.Core\FileSystem::C:\Program Files\Microsoft Office\Office15\VPREVIEW.EXE
PSParentPath      : Microsoft.PowerShell.Core\FileSystem::C:\Program Files\Microsoft Office\Office15
PSChildName       : VPREVIEW.EXE
PSDrive           : C
PSProvider        : Microsoft.PowerShell.Core\FileSystem
PSIsContainer     : False
Mode              : -a----
VersionInfo       : File:             C:\Program Files\Microsoft Office\Office15\VPREVIEW.EXE
                    InternalName:     VPREVIEW.EXE
                    OriginalFilename: VPREVIEW.EXE
                    FileVersion:      15.0.4420.1017
                    FileDescription:  Microsoft Office Visio Previewer
                    Product:          Microsoft Office 2013
                    ProductVersion:   15.0.4420.1017
                    Debug:            False
                    Patched:          False
                    PreRelease:       False
                    PrivateBuild:     False
                    SpecialBuild:     False
                    Language:         Language Neutral

BaseName          : VPREVIEW
Target            : {}
LinkType          :
Name              : VPREVIEW.EXE
Length            : 694928
DirectoryName     : C:\Program Files\Microsoft Office\Office15
Directory         : C:\Program Files\Microsoft Office\Office15
IsReadOnly        : False
Exists            : True
FullName          : C:\Program Files\Microsoft Office\Office15\VPREVIEW.EXE
Extension         : .EXE
CreationTime      : 10/1/2012 8:35:08 PM
CreationTimeUtc   : 10/2/2012 12:35:08 AM
LastAccessTime    : 8/29/2026 9:53:57 AM
LastAccessTimeUtc : 8/29/2026 1:53:57 PM
LastWriteTime     : 10/1/2012 8:35:08 PM
LastWriteTimeUtc  : 10/2/2012 12:35:08 AM
Attributes        : Archive
```
![app launch error](screenshots/capture-visio-viewer-error.png)

turns out the `VPREVIEW.EXE` isn't really a conventional standalone viewer application.

The line "You betcha" is featured in the surreal final musical sequence of the 2014 dark comedy The Voices, occurring in an afterlife scene as Jerry reunites with his victims.


It's essentially the executable/component that __Office__ / __IE__ uses to provide __Visio__ *preview*
functionality, and it expects to be invoked exclusively through its Office integration/host mechanism.


blind dump registry 
```cmd
reg.exe query HKCR /s | findstr /ic:"Visio Viewer"
```
```text
    (Default)    REG_SZ    Visio Viewer CAD Drawing
    (Default)    REG_SZ    Visio Viewer DWG Display
    (Default)    REG_SZ    Visio Viewer DWG Display Creator
    (Default)    REG_SZ    Visio Viewer CAD Drawing
    ProductName    REG_SZ    Microsoft Visio Viewer 2013
    DiskPrompt    REG_SZ    Microsoft Visio Viewer 2013
    (Default)    REG_SZ    Microsoft Visio Viewer 15.0 Type Library
    (Default)    REG_SZ    Visio Viewer DWG Display
    (Default)    REG_SZ    Visio Viewer DWG Display
    (Default)    REG_SZ    Visio Viewer DWG Display Creator
    (Default)    REG_SZ    Visio Viewer DWG Display Creator
    (Default)    REG_SZ    Visio Viewer CAD Drawing
    (Default)    REG_SZ    Visio Viewer DWG Display
    (Default)    REG_SZ    Visio Viewer DWG Display Creator
    (Default)    REG_SZ    Microsoft Visio Viewer 15.0 Type Library
```
targeted registry peek

```powershell
HKEY_CLASSES_ROOT\Typelib\{BA35B84E-A623-471B-8B09-6D72DD072F25}\1.5
Microsoft Visio Viewer 15.0 Type Library
```
```powershell
$p = 'SOFTWARE\Classes\CLSID\{F8CF7A98-2C45-4c8d-9151-2D716989DDAB}\InprocServer32'
get-itemproperty -path "HKLM:\${p}" -name '(default)'|select-object -expandproperty '(default)'
```
```text
C:\PROGRA~1\MICROS~4\Office15\VVIEWER.DLL
```
```powershell
get-itemproperty -path 'HKLM:\SOFTWARE\Classes\WOW6432Node\CLSID\{F8CF7A98-2C45-4c8d-9151-2D716989DDAB}\InprocServer32' -name '(default)'|select-object -expandproperty '(default)'
```
```text
C:\PROGRA~1\MICROS~4\Office15\VVIEWER.DLL
```

```powershell
$p = 'SOFTWARE\Classes\CLSID\{F8CF7A98-2C45-4c8d-9151-2D716989DDAB}\VersionIndependentProgID'
get-itemproperty -path "HKLM:\${p}" -name '(default)'|select-object -expandproperty '(default)'
```
```text
VisioViewer.Viewer
```

> NOTE: the following will fail
> ```powershell
> $p = 'SOFTWARE\Classes\CLSID\{F8CF7A98-2C45-4c8d-9151-2D716989DDAB}'
> get-itemproperty -path "HKLM:\${p}" -name 'ProgID'|select-object -expandproperty 'ProgID'
> ```
> ```text
> Property ProgID does not exist at path ...
> ```

```powershell
p = 'SOFTWARE\Classes\Typelib\{BA35B84E-A623-471B-8B09-6D72DD072F25}\1.5\0\win32'
get-itemproperty -path "HKLM:\${p}" -name '(default)'|select-object -expandproperty '(default)'
```
```text
C:\PROGRA~1\MICROS~4\Office15\VVIEWER.DLL
```

```powershell
$p = 'SOFTWARE\Classes\CLSID\{21E17C2F-AD3A-4b89-841F-09CFE02D16B7}\LocalServer32'
get-itemproperty -path "HKLM:\${p}" -name '(default)'|select-object -expandproperty '(default)'
```
```text
C:\PROGRA~1\MICROS~4\Office15\VPREVIEW.EXE
```
```powershell
$p = 'SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_ADDON_MANAGEMENT'
get-itemproperty -path "HKLM:\${p}" -name 'VPREVIEW.EXE'|select-object -expandproperty 'VPREVIEW.EXE'
```
```text
1
```

```powershell
$p = 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_HTTP_USERNAME_PASSWORD_DISABLE'
$p  = $p -replace 'HKEY_LOCAL_MACHINE\\', ''
get-itemproperty -path "HKLM:\${p}" -name 'VPREVIEW.EXE'|select-object -expandproperty 'VPREVIEW.EXE'
```
```text
1
```

```powershell
$p = 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_LOCALMACHINE_LOCKDOWN'
$p  = $p -replace 'HKEY_LOCAL_MACHINE\\', ''
get-itemproperty -path "HKLM:\${p}" -name 'VPREVIEW.EXE'|select-object -expandproperty 'VPREVIEW.EXE'
```
```text
1
```

```powershell
$p = 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_RESTRICT_ACTIVEXINSTALL'
$p  = $p -replace 'HKEY_LOCAL_MACHINE\\', ''
get-itemproperty -path "HKLM:\${p}" -name 'VPREVIEW.EXE'|select-object -expandproperty 'VPREVIEW.EXE'
```
```text
1
```
...more of the kind found under IE's own small *feature registry*, skipped

![app aprose render](screenshots/capture-aprose-viewer.png)

> NOTE: aprose is server side

### Cleanup

```sh
IMAGE=svg2visio
NAME=$IMAGE
docker stop $NAME
docker container prune -f
docker image prune -f
docker image rm $IMAGE node:22.12.0-alpine nginx:1.30.3-alpine3.23
```
### NOTE

**QA in the era of agentic development**

As agentic coding makes implementation increasingly cheap, QA value shifts from code-level verification toward **product-level semantic understanding**: architecture awareness, knowledge of the product’s failure history, recognition of business-critical behavior, and the ability to identify risks that are not explicit in the current specification.

This makes QA not less relevant, but potentially **more strategic**: the QA function becomes a source of accumulated product knowledge and an adversarial semantic layer around increasingly autonomous software development.


Agentic development makes implementation increasingly cheap, shifting the value of QA toward the semantic layer: understanding product architecture, reconstructing intent behind requirements and failures, and recognizing risks from accumulated product history.

QA therefore becomes both **an independent adversarial layer and a keeper of product wisdom**—preserving what is learned by digging from a visible symptom through its apparent cause to the actual RCA, and applying that knowledge to future changes.

In this sense, QA plays for software failures a role somewhat analogous to semantic transcription for speech: **not merely recording what happened, but reconstructing what was actually meant or caused.**


Transcribe reconstructs intent behind imperfect speech; QA reconstructs causality behind imperfect software behavior.

A test failure is often just the surface manifestation. The real QA value begins when someone refuses to stop at:

Test failed.”

and keeps asking:

  * Why?
  * Why really?
  * Why did the system allow this?
  * Why didn't we see it earlier?
  * What does this tell us about the product?

That last answer is the thing worth __keeping__.

### See Also


  * [live](https://McMarius11.github.io/svgtovisio)
  * [related reddit discussion](https://www.reddit.com/r/drawio/comments/1r9j05c/free_tool_to_convert_between_visio_and_drawio/)
  * __Google Transcribe__  latest speech-to-text model designed for precise and intelligent real-time transcription.
    + [release new feature info](https://habr.com/ru/news/1075046/) (in Russian)
 - the model able to understand not only the words, but also what the person meant to convey
 + [original](https://blog.google/innovation-and-ai/models-and-research/gemini-models/gemini-3-5-transcribe/)

---  

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
