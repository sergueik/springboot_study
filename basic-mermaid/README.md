### Info

replica of the [mermaid-live-editor](https://github.com/mermaid-js/mermaid-live-editor) container for Edit, preview and share mermaid charts/diagrams taken
at `efafad1e8709854e77bea2d6f1abf212ed7482a9` to use node 18

### Usage

```sh
docker build -t test-alpine -f Dockerfile .
```
```text

Sending build context to Docker daemon  564.2kB
Step 1/10 : FROM node:18.1.0-alpine AS builder
18.1.0-alpine: Pulling from library/node
...
Digest: sha256:f4d6916c5625853e81994b5cb53ad3eb27e5fec9451c579d298fee0c508fe621
Status: Downloaded newer image for node:18.1.0-alpine
 ---> d94913fe64df
Step 2/10 : RUN apk update     && apk add --update --no-cache python3 py3-pip g++ make
 ---> Running in 571a493a3296
fetch https://dl-cdn.alpinelinux.org/alpine/v3.15/main/x86_64/APKINDEX.tar.gz
fetch https://dl-cdn.alpinelinux.org/alpine/v3.15/community/x86_64/APKINDEX.tar.gz
v3.15.11-31-g2a2ec278a7b [https://dl-cdn.alpinelinux.org/alpine/v3.15/main]
v3.15.11-31-g2a2ec278a7b [https://dl-cdn.alpinelinux.org/alpine/v3.15/community]
OK: 15865 distinct packages available
fetch https://dl-cdn.alpinelinux.org/alpine/v3.15/main/x86_64/APKINDEX.tar.gz
fetch https://dl-cdn.alpinelinux.org/alpine/v3.15/community/x86_64/APKINDEX.tar.gz
(1/51) Upgrading musl (1.2.2-r7 -> 1.2.2-r9)
(2/51) Installing binutils (2.37-r3)
...
(51/51) Installing py3-pip (20.3.4-r1)
Executing busybox-1.34.1-r5.trigger
OK: 257 MiB in 66 packages
 ---> Removed intermediate container 571a493a3296
 ---> 5e3077a07ebb
Step 3/10 : COPY --chown=node:node . /home
 ---> 61da5dbd7a70
Step 4/10 : WORKDIR /home
 ---> Running in 6cee117a33c8
 ---> Removed intermediate container 6cee117a33c8
 ---> c25f23c9468e
Step 5/10 : RUN yarn install --ignore-scripts
 ---> Running in 0b18a4b0f925
yarn install v1.22.18
warning package.json: No license field
warning mermaid-live-editor@2.0.67: No license field
[1/4] Resolving packages...
[2/4] Fetching packages...
[3/4] Linking dependencies...
warning "analytics > @analytics/core > analytics-utils@1.0.10" has unmet peer dependency "@types/dlv@^1.0.0".
warning "mermaid > cypress-image-snapshot@4.0.1" has incorrect peer dependency "cypress@^4.5.0".
warning "mermaid > cypress-image-snapshot > jest-image-snapshot@4.2.0" has unmet peer dependency "jest@>=20 <=26".
[4/4] Building fresh packages...
warning Ignored scripts due to flag.
Done in 58.95s.
 ---> Removed intermediate container 0b18a4b0f925
 ---> 86d27a57da55
Step 6/10 : RUN cd node_modules/deasync &&     node /usr/local/lib/node_modules/npm/node_modules/node-gyp/bin/node-gyp.js configure
 ---> Running in ec9fb22604bd
gyp info it worked if it ends with ok
gyp info using node-gyp@9.0.0
gyp info using node@18.1.0 | linux | x64
gyp info find Python using Python version 3.9.18 found at "/usr/bin/python3"
gyp http GET https://unofficial-builds.nodejs.org/download/release/v18.1.0/node-v18.1.0-headers.tar.gz
gyp http 200 https://unofficial-builds.nodejs.org/download/release/v18.1.0/node-v18.1.0-headers.tar.gz
gyp http GET https://unofficial-builds.nodejs.org/download/release/v18.1.0/SHASUMS256.txt
gyp http 200 https://unofficial-builds.nodejs.org/download/release/v18.1.0/SHASUMS256.txt
gyp info spawn /usr/bin/python3
gyp info spawn args [
gyp info spawn args   '/usr/local/lib/node_modules/npm/node_modules/node-gyp/gyp/gyp_main.py',
gyp info spawn args   'binding.gyp',
gyp info spawn args   '-f',
gyp info spawn args   'make',
gyp info spawn args   '-I',
gyp info spawn args   '/home/node_modules/deasync/build/config.gypi',
gyp info spawn args   '-I',
gyp info spawn args   '/usr/local/lib/node_modules/npm/node_modules/node-gyp/addon.gypi',
gyp info spawn args   '-I',
gyp info spawn args   '/root/.cache/node-gyp/18.1.0/include/node/common.gypi',
gyp info spawn args   '-Dlibrary=shared_library',
gyp info spawn args   '-Dvisibility=default',
gyp info spawn args   '-Dnode_root_dir=/root/.cache/node-gyp/18.1.0',
gyp info spawn args   '-Dnode_gyp_dir=/usr/local/lib/node_modules/npm/node_modules/node-gyp',
gyp info spawn args   '-Dnode_lib_file=/root/.cache/node-gyp/18.1.0/<(target_arch)/node.lib',
gyp info spawn args   '-Dmodule_root_dir=/home/node_modules/deasync',
gyp info spawn args   '-Dnode_engine=v8',
gyp info spawn args   '--depth=.',
gyp info spawn args   '--no-parallel',
gyp info spawn args   '--generator-output',
gyp info spawn args   'build',
gyp info spawn args   '-Goutput_dir=.'
gyp info spawn args ]
gyp info ok
 ---> Removed intermediate container ec9fb22604bd
 ---> f1bd279b6651
Step 7/10 : RUN yarn build
 ---> Running in aeef5dfad0f5
yarn run v1.22.18
warning package.json: No license field
$ svelte-kit build
vite v2.9.1 building for production...
transforming...
Browserslist: caniuse-lite is outdated. Please run:
  npx browserslist@latest --update-db
  Why you should do it regularly: https://github.com/browserslist/browserslist#browsers-data-updating

🌼 daisyUI components 2.14.2  https://github.com/saadeghi/daisyui
  ✔︎ Including:  base, components, themes[29], utilities


🌼 daisyUI components 2.14.2  https://github.com/saadeghi/daisyui
  ✔︎ Including:  base, components, themes[29], utilities

Use of eval is strongly discouraged, as it poses security risks and may cause issues with minification
✓ 72 modules transformed.
rendering chunks...
.svelte-kit/output/client/_app/manifest.json                                       3.70 KiB
...
.svelte-kit/output/client/_app/chunks/util-3a497aa7.js                             1150.19 KiB / gzip: 325.54 KiB

(!) Some chunks are larger than 500 KiB after minification. Consider:
- Using dynamic import() to code-split the application
- Use build.rollupOptions.output.manualChunks to improve chunking: https://rollupjs.org/guide/en/#outputmanualchunks
- Adjust chunk size limit for this warning via build.chunkSizeWarningLimit.
vite v2.9.1 building SSR bundle for production...
transforming...
Use of eval is strongly discouraged, as it poses security risks and may cause issues with minification
✓ 51 modules transformed.
rendering chunks...
.svelte-kit/output/server/manifest.json                                      2.73 KiB
.svelte-kit/output/server/index.js                                           77.13 KiB
...
.svelte-kit/output/server/chunks/hooks-4d9cb55e.js                           0.15 KiB

Run npm run preview to preview your production build locally.

> Using @sveltejs/adapter-static
  You should set `config.kit.prerender.default` to `true` if no fallback is specified
  Wrote site to "docs"
  ✔ done
Done in 16.49s.
 ---> Removed intermediate container aeef5dfad0f5
 ---> 2481935dfd7e
Step 8/10 : FROM nginx:1.30.3-alpine3.23 as runner
1.30.3-alpine3.23: Pulling from library/nginx
e6f31ffc071e: Pulling fs layer
...
f72ca0003135: Verifying Checksum
f72ca0003135: Download complete
f72ca0003135: Pull complete
Digest: sha256:0d3b80406a13a767339fbe2f41406d6c7da727ab89cf8fae399e81f780f814d1
Status: Downloaded newer image for nginx:1.30.3-alpine3.23
 ---> d0701bd41f82
Step 9/10 : COPY ./nginx.conf /etc/nginx/conf.d/default.conf
 ---> 461931964b11
Step 10/10 : COPY --from=builder --chown=nginx:nginx /home/docs /usr/share/nginx/html
 ---> acb997d6ff06
Successfully built acb997d6ff06
Successfully tagged test-alpine:latest
```

```sh
docker image ls
```
```text
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
test-alpine         latest              1e52490045c7        6 hours ago         63.8MB
<none>              <none>              6c2f2d0a9aad        6 hours ago         1.41GB
nginx               1.30.3-alpine3.23   d0701bd41f82        4 weeks ago         62.2MB
node                18.1.0-alpine       d94913fe64df        4 years ago         171MB
```
```sh
docker run --name mermaid-live -p 8080:80 -d test 
```
```sh
docker ps 
```
```text
CONTAINER ID        IMAGE               COMMAND                  CREATED             STATUS              PORTS                  NAMES
653671f1d219        test-alpine         "/docker-entrypoint.…"   6 seconds ago       Up 6 seconds        0.0.0.0:8080->80/tcp   mermaid-live

```

```sh
curl -I http://192.168.99.100:8080/
```
```text
HTTP/1.1 200 OK
Server: nginx/1.30.3
Date: Fri, 24 Jul 2026 12:32:42 GMT
Content-Type: text/html
Content-Length: 2550
Last-Modified: Fri, 24 Jul 2026 05:51:14 GMT
Connection: keep-alive
ETag: "6a62fd52-9f6"
Accept-Ranges: bytes
```
open in the browser
![editor](screenshots/capture-editor.png)

#### Using the generated static site without Docker

```sh
docker cp mermaid-live:/usr/share/nginx/html dist
```
The application is generated as a static web site (`docs/`). It can be served by any HTTP server.

However, Simply opening `index.html` with the `file://` protocol will not work because modern browsers block module loading and requests such as `manifest.json` from the local file system.


```cmd
pushd dist
start "c:\Program Files\Google\Chrome\Application\chrome.exe" file://%cd%\index.html
```
will fail with
```text
Access to manifest at 'file:///C:/developer/sergueik/springboot_study/basic-mermaid/dist/manifest.json' from origin 'null' has been blocked by CORS policy: Cross origin requests are only supported for protocol schemes: chrome, chrome-extension, chrome-untrusted, data, http, https, isolated-app.
manifest.json:1  Failed to load resource: net::ERR_FAILED
index.html:65 Access to script at 'file:///C:/_app/start-d8e4d14a.js' from origin 'null' has been blocked by CORS policy: Cross origin requests are only supported for protocol schemes: chrome, chrome-extension, chrome-untrusted, data, http, https, isolated-app.
start-d8e4d14a.js:1  Failed to load resource: net::ERR_FAILED
index.html:65 Access to script at 'file:///C:/_app/chunks/index-3ccfa173.js' from origin 'null' has been blocked by CORS policy: Cross origin requests are only supported for protocol schemes: chrome, chrome-extension, chrome-untrusted, data, http, https, isolated-app.
index-3ccfa173.js:1  Failed to load resource: net::ERR_FAILED
index.html:65 Access to script at 'file:///C:/_app/chunks/preload-helper-d25c422b.js' from origin 'null' has been blocked by CORS policy: Cross origin requests are only supported for protocol schemes: chrome, chrome-extension, chrome-untrusted, data, http, https, isolated-app.
preload-helper-d25c422b.js:1
```

The generated site must be served over HTTP. Any static web server can be used, for example:
```cmd
pushd dist
python.exe -m http.server
```
```text
Serving HTTP on :: port 8000 (http://[::]:8000/) ...
```
then open the site in the browser `http://localhost:8000`

Alternatively install [Live Server VS Code extension](https://marketplace.visualstudio.com/items?itemName=ritwickdey.LiveServer), and 

![Install Live Server](screenshots/capture-vscode1.png)

run the `index.html` with Live server 

![Run Live Server](screenshots/capture-vscode2.png)

and it will automatically open `http://localhost:5000/index.html` in the browser. 
There is an error
```text
Live reload enabled.
undefined:1  Failed to load resource: the server responded with a status of 404 (Not Found)
start-d8e4d14a.js:1 Uncaught (in promise) Error: Version check failed: 404
    at Object.l [as check] (start-d8e4d14a.js:1:10429)
    at async me (start-d8e4d14a.js:1:13827)
    at async we (start-d8e4d14a.js:1:19667)
```
### Background
[Mermaid](https://en.wikipedia.org/wiki/Mermaid_(software)) - is an open-source JavaScript-based diagramming and charting software from a Markdown-like syntax flow DSL similar and more advanded than Graphviz scored in the Most Exciting Use of Technology
[Mermaid Wiki](https://mermaid.ai/open-source/intro/index.html)

[Live App](https://mermaid.live/) to learn practice


There official __Mermaid Live Editor__  Docker image is hosted on [GitHub Container Registry](ghcr.io/mermaid-js/mermaid-live-editor/mermaid-live-editor) (__GHCR__) 

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
