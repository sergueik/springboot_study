### Info
this builds [graphviz with pure java](https://github.com/nidi3/graphviz-java) pruned from native dependencies
### Usage
```sh
mvn clean package
```
review the fat jar:
```sh
unzip -ql target/example.graphviz-java.jar 
```
test the app on the developer machine
```sh
java -jar target/example.graphviz-java.jar -inputfile color.dot -outputfile example.png
```
> NOTE: when testing on Windows can use Unix path separaror, but still have to use Windows slement separator (`;` instead of `:`)
```cmd
docker pull eclipse-temurin:11-jre-alpine
export NAME=example-graphviz-java
docker build -t $NAME -f Dockerfile . 
```
```sh
docker run --rm $NAME -help
```
```text
Usage: jar -outputFile <filename> -inputfile <filename>
```
```sh
docker run --rm -w /app -v $(pwd):/app $NAME -inputfile example.dot -outputfile example.png
```
```text
03:47:15.860 INFO  example.Example - converting example.dot example.png
```

### Troubleshooting

```sh
docker run --rm -w /app -v $(pwd):/app --entrypoint '' $NAME java -jar /app/app.jar -inputfile example.dot -outputfile example.png --help
```
```text
Error: Unable to access jarfile /app/app.jar
```
### See Also
  * https://github.com/omerio/graphviz-server
  * https://github.com/omerio/graphviz-webapp
  * https://hub.docker.com/r/mejran/graphviz-server
  * https://github.com/blackears/svgSalamander SVG engine for Java
  * https://mvnrepository.com/artifact/guru.nidi/graphviz-java/0.18.1
  * __COBOL Control Flow__ [Visual Studio Code Extension](https://marketplace.visualstudio.com/items?itemName=broadcomMFD.ccf) and [repository](https://github.com/BroadcomMFD/cobol-control-flow) - is an extension for Visual Studio Code that provides graphical visualization of program flow for programs written in COBOL. Techically speaking the extension generates a control-flow graph for COBOL and can export to DOT format, but the exact implementation isn’t documented in terms of which graph engine it uses internally
  To study the extrnsion contents one can run the command in console:
```cmd
code --install-extension broadcomMFD.ccf --force
```
NOTE > every VS Code Marketplace extension has a unique identifier in the form `publisher.extensionName`. This comman works the same regardless of:

* where VS Code itself is installed (MSI, ZIP, portable)
* whether you are admin or not
* which OS  is run (Windows/Linux/macOS)

To discovder the short name (the canonical ID) of the extension examine the url to the page
`https://marketplace.visualstudio.com/items?itemName=broadcomMFD.ccf`.

this leads to success message 
```text
Installing extensions...
Installing extension 'broadcommfd.ccf'...
Extension 'broadcommfd.ccf' v1.2.2 was successfully installed.
Extension 'broadcommfd.cobol-language-support' v2.4.3 was successfully installe
```
then note, that VS Code is very much an “anti-MSI” / user-profile–centric tool when it comes to extensions.

later on can recall the list via

```sh
code --list-extensions --show-versions
```

this will bring the short list of the form
```text
broadcommfd.ccf@1.2.2
broadcommfd.cobol-language-support@2.4.3
bruno-api-client.bruno@4.5.0
github.copilot@1.388.0
github.copilot-chat@0.36.2
golang.go@0.52.2
hashicorp.terraform@2.37.6
ms-python.debugpy@2025.18.0
ms-python.python@2026.0.0
ms-python.vscode-pylance@2025.10.4
ms-python.vscode-python-envs@1.16.0
```
one can confirm by running the command
```cmd
>dir /b %USERPROFILE%\.vscode\extensions\
```
this will output
```text
.obsolete
broadcommfd.ccf-1.2.2
broadcommfd.cobol-language-support-2.4.3-win32-x64
bruno-api-client.bruno-4.5.0
extensions.json
github.copilot-1.388.0
github.copilot-chat-0.36.1
github.copilot-chat-0.36.2
golang.go-0.52.1
golang.go-0.52.2
hashicorp.terraform-2.37.6-win32-x64
ms-python.debugpy-2025.18.0-win32-x64
ms-python.python-2026.0.0-win32-x64
ms-python.vscode-pylance-2025.10.4
ms-python.vscode-python-envs-1.16.0-win32-x64
```
>NOTE Windows portable mode is a special case  and its directory layout not covered here

It turns out there is no 

and found no references to:

  * `viz.js`
  * `@viz-js/viz`
  * `d3-graphviz`
  * `graphviz.wasm`
  * `hpcc-js/wasm`
  * `dot`

inside `%USERPROFILE%\.vscode\extensions\broadcommfd.ccf-1.2.2`
making one assume it does not use the vanilla `Viz.js`.

There are functions like
 * `dotAdapter.getSimpleDot`
 * `exportControlGraphToDot`

it is likely that DOT is an export format only; rendering is custom or opaque.

#### Why this makes sense for COBOL specifically

COBOL control flow is not arbitrary graphs. It’s:

sequential blocks

IF / ELSE

PERFORM loops

GO TO branches

Which maps nicely to:

vertical layered layout

deterministic positioning

block diagrams

Graphviz is overkill and unpredictable for that

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

