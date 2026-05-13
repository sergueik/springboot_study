### Info

__Imixs-BPMN.io__ __Modeler__ is a web based BPMN modeler based on the [bpmn.io](https://bpmn.io/) project.
There is also the online version somewhere in: `https://bpmn.imixs.org`.

The __BPMN__ ( __Business Process Model and Notation__ ) tools are software applications used to create,
visualize, and analyze business processes using a standardized, easy-to-understand steep learning curve 
vintage flowcharting language. 


### Usage

NOTE:  intentionally pull pinned, not `latest` from Docker hub [location](https://hub.docker.com/r/imixs/imixs-bpmn.io/tags)


```sh
docker pull imixs/imixs-bpmn.io:1.0.0
```

```text
docker run -p 8080:8080 imixs/imixs-bpmn.io:1.0.0
```
will log bootstrap progress to console, then continue detached:
 
```text
> Imixs-BPMN.io@1.0.0 start /home/node
> run-s dev


> Imixs-BPMN.io@1.0.0 dev /home/node
> webpack-dev-server --content-base=public --open --host 0.0.0.0

ℹ ｢wds｣: Project is running at http://0.0.0.0:8080/
ℹ ｢wds｣: webpack output is served from /
ℹ ｢wds｣: Content not from webpack is served from /home/node/public
ℹ ｢wdm｣: wait until bundle finished: /
⚠ ｢wdm｣: Hash: d7856c2f279debd64dfd
Version: webpack 4.46.0
Time: 44760ms
Built at: 05/12/2026 10:59:51 PM
                                                Asset       Size  Chunks                          Chunk Names
                                               app.js    714 KiB       0  [emitted]        [big]  bundle
                                           app.js.map   3.04 MiB       0  [emitted] [dev]         bundle
                                          css/app.css  988 bytes          [emitted]
                                 css/custom-print.css   34 bytes          [emitted]
                          css/fonts/OpenSans-Bold.ttf    219 KiB          [emitted]
                    css/fonts/OpenSans-BoldItalic.ttf    208 KiB          [emitted]
                     css/fonts/OpenSans-ExtraBold.ttf    217 KiB          [emitted]
               css/fonts/OpenSans-ExtraBoldItalic.ttf    208 KiB          [emitted]
                        css/fonts/OpenSans-Italic.ttf    208 KiB          [emitted]
                         css/fonts/OpenSans-Light.ttf    217 KiB          [emitted]
                   css/fonts/OpenSans-LightItalic.ttf    208 KiB          [emitted]
                       css/fonts/OpenSans-Regular.ttf    212 KiB          [emitted]
                      css/fonts/OpenSans-SemiBold.ttf    216 KiB          [emitted]
                css/fonts/OpenSans-SemiBoldItalic.ttf    208 KiB          [emitted]
                              css/fonts/open_sans.css   3.84 KiB          [emitted]
                               css/fonts/typicons.css   22.1 KiB          [emitted]
                               css/fonts/typicons.ttf   97.6 KiB          [emitted]
                              css/fonts/typicons.woff   58.5 KiB          [emitted]
                                  css/imixs-basic.css    5.7 KiB          [emitted]
                                   css/imixs-bpmn.css    5.9 KiB          [emitted]
                                 css/imixs-layout.css   5.66 KiB          [emitted]
                                           index.html   3.45 KiB          [emitted]
   vendor/bpmn-js/assets/bpmn-font/css/bpmn-codes.css   7.73 KiB          [emitted]
vendor/bpmn-js/assets/bpmn-font/css/bpmn-embedded.css   92.3 KiB          [emitted]
         vendor/bpmn-js/assets/bpmn-font/css/bpmn.css   9.39 KiB          [emitted]
        vendor/bpmn-js/assets/bpmn-font/font/bpmn.eot   46.7 KiB          [emitted]
        vendor/bpmn-js/assets/bpmn-font/font/bpmn.svg    131 KiB          [emitted]
        vendor/bpmn-js/assets/bpmn-font/font/bpmn.ttf   46.6 KiB          [emitted]
       vendor/bpmn-js/assets/bpmn-font/font/bpmn.woff   15.6 KiB          [emitted]
      vendor/bpmn-js/assets/bpmn-font/font/bpmn.woff2   12.8 KiB          [emitted]
                 vendor/bpmn-js/assets/diagram-js.css   11.5 KiB          [emitted]
Entrypoint bundle [big] = app.js app.js.map
 [0] ./node_modules/inherits/inherits_browser.js 753 bytes {0} [built]
 [1] ./node_modules/jquery/dist/jquery.js 282 KiB {0} [built]
 [6] (webpack)-dev-server/client/clients/SockJSClient.js 4.08 KiB {0} [built]
 [9] (webpack)-dev-server/client/utils/log.js 964 bytes {0} [built]
[11] ./resources/new_process.bpmn 11.3 KiB {0} [built]
[12] multi (webpack)-dev-server/client?http://0.0.0.0:8080 ./app/app.js 40 bytes {0} [built]
[13] (webpack)-dev-server/client?http://0.0.0.0:8080 4.29 KiB {0} [built]
[14] (webpack)-dev-server/node_modules/strip-ansi/index.js 161 bytes {0} [built]
[16] (webpack)-dev-server/client/socket.js 1.53 KiB {0} [built]
[19] (webpack)-dev-server/client/overlay.js 3.51 KiB {0} [built]
[26] (webpack)-dev-server/client/utils/sendMessage.js 402 bytes {0} [built]
[27] (webpack)-dev-server/client/utils/reloadApp.js 1.59 KiB {0} [built]
[30] (webpack)-dev-server/client/utils/createSocketUrl.js 2.91 KiB {0} [built]
[39] (webpack)/hot sync nonrecursive ^\.\/log$ 170 bytes {0} [built]
[42] ./app/app.js + 334 modules 1.22 MiB {0} [built]
     | ./app/app.js 3.78 KiB [built]
     | ./node_modules/bpmn-js/lib/Modeler.js 6.42 KiB [built]
     | ./node_modules/bpmn-js/lib/BaseModeler.js 2 KiB [built]
     | ./node_modules/bpmn-js/lib/Viewer.js 2.22 KiB [built]
     | ./node_modules/bpmn-js/node_modules/diagram-js/lib/navigation/keyboard-move/index.js 233 bytes [built]
     | ./node_modules/bpmn-js/node_modules/diagram-js/lib/navigation/movecanvas/index.js 126 bytes [built]
     | ./node_modules/bpmn-js/node_modules/diagram-js/lib/navigation/zoomscroll/index.js 126 bytes [built]
     | ./node_modules/bpmn-js/lib/NavigatedViewer.js 733 bytes [built]
     | ./node_modules/bpmn-js/node_modules/diagram-js/lib/navigation/touch/index.js 105 bytes [built]
     | ./node_modules/bpmn-js/node_modules/diagram-js/lib/features/align-elements/index.js 142 bytes [built]
     | ./node_modules/bpmn-js/lib/features/auto-place/index.js 244 bytes [built]
     | ./node_modules/bpmn-js/lib/features/auto-resize/index.js 309 bytes [built]
     | ./node_modules/bpmn-js/node_modules/diagram-js/lib/features/auto-scroll/index.js 212 bytes [built]
     | ./node_modules/bpmn-js/node_modules/diagram-js/lib/features/bendpoints/index.js 742 bytes [built]
     | ./node_modules/bpmn-js/node_modules/diagram-js/lib/features/connect/index.js 420 bytes [built]
     |     + 320 hidden modules
    + 30 hidden modules

WARNING in asset size limit: The following asset(s) exceed the recommended size limit (244 KiB).
This can impact web performance.
Assets:
  app.js (714 KiB)

WARNING in entrypoint size limit: The following entrypoint(s) combined asset size exceeds the recommended limit (244 KiB). This can impact web performance.
Entrypoints:
  bundle (714 KiB)
      app.js


WARNING in webpack performance recommendations:
You can limit the size of your bundles by using import() or require.ensure to lazy load some parts of your application.
For more info visit https://webpack.js.org/guides/code-splitting/
ℹ ｢wdm｣: Compiled with warnings.

```
confirm port:
```sh
docker container ls
```
```
CONTAINER ID        IMAGE                       COMMAND                  CREATED             STATUS              PORTS                    NAMES
d3ba574dfdec        imixs/imixs-bpmn.io:1.0.0   "docker-entrypoint.s…"   30 minutes ago      Up 30 minutes       0.0.0.0:8080->8080/tcp   elated_lalande
```
confirm hostname
```sh
docker-machine ip
```
```text
192.168.99.100
```
open in the browser: `http://192.168.99.100:8080/`

create a bpnm diagram (`diagram.bpnm`) to illustrate some other task, drag and drop into design area and work from there.


### Background Info


#### BPMN 2.0
  * A standard notation
  * Designed for business process modeling
  * Focus: “what should happen in a process”
  * Not execution-dependent (usually)


Used in:

  * process design
  * workflow documentation
  * orchestration systems (occationally)

####  Windows Workflow [Foundation] (WF)


  * A runtime + programming model
  * Part of .NET ecosystem (introduced in .NET 3.0 era, tied to “Indigo”/WCF era)
  * Focus: execution of workflows in code
  * Very state-machine / activity-driven

  * strongly code-centric
  * compiled workflows
  * not a business notation standard
  * deeply integrated into .NET runtime model
  
#### Best Known Implementsations - UiPath (modern RPA layer)


  * A commercial RPA platform
  * Uses a workflow designer that visually resembles BPMN
  * But internally it is:
    + execution engine for UI automation
    + not BPMN compliant runtime

All three somewhat share a core abstraction:

*"Directed graph of ativities with conditional flow"*

### See Also

  * [Imixs-Workflow Microservice](https://www.imixs.org/sub_microservice.html?ref=bpmn.imixs.org)
  * [ekifox/bpmn-io-with-extensions-docker](https://hub.docker.com/r/ekifox/bpmn-io-with-extensions-docker/tags)
  * [eduardoluizgs/bpmn-io-docker](https://github.com/eduardoluizgs/bpmn-io-docker)

