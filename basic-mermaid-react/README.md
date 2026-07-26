### Info

Latest [Mermaid](https://github.com/mermaid-js/mermaid) bundled with [React](https://github.com/react/react) library for web and native user interfaces and [Vite](https://github.com/vitejs/vite) fast next generation frontend tooling in order to run dist as a static resource (partial success)

### Background

### Usage


```sh
docker pull node:22.12.0-alpine
docker pull nginx:1.30.3-alpine3.23
```
```sh
IMAGE=mermaid-react
docker build -t $IMAGE -f Dockerfile .
```
```sh    
NAME=mermaid-react
docker run --name $NAME -d -p 8080:80 $IMAGE
```

![Run Nginx Docker](screenshots/capture-nginx-docker.png)

### Note

```sh
docker cp $NAME:/usr/share/nginx/html dist
```
```cmd
pushd dist
python.exe -m http.server
```
```text
Serving HTTP on :: port 8000 (http://[::]:8000/) ...
```
then open the site in the browser `http://localhost:8000`

![Running Python Http Server](screenshots/capture-python.png)

### Local File

Cannot open in the browser via `file://` when using the real DOS Drive letters:

when launched with 

```cmd
"C:\Program Files\Google\Chrome\Application\chrome.exe" --user-data-dir=C:\temp\chrome-file-test --allow-file-access-from-files file:///C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\index.html
```
or
```cmd

"C:\Program Files\Google\Chrome\Application\chrome.exe" --user-data-dir=C:\temp\chrome-file-test --allow-file-access-from-files file:///C:/developer/sergueik/springboot_study/basic-mermaid-react/dist/index.html
```
it fails with `net::ERR_FILE_NOT_FOUND` error for multiple asset:

```text
Failed to load resource: net::ERR_FILE_NOT_FOUND index-BGiIaHVw.js:1 
Failed to load resource: net::ERR_FILE_NOT_FOUND chunk-Y2CYZVJY-DsF7k-Jl.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND src-BMa7vLb8.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND chunk-WYO6CB5R-C36byBU-.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND dist-Q9n2Bb2K.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND chunk-ICXQ74PX-_B4UKQEp.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND path-BWPyau1x.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND array-BifhSqXX.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND line-BjeXKALW.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND chunk-C7G6YPKG-WgqYOC9I.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND chunk-OGEWGWER-q1FVTapY.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND chunk-HOUHSVGY-BrlsNa-I.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND chunk-Q4XR5HBZ-DuMv4AAJ.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND rough.esm-CSKSodPl.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND chunk-ZGVPDNZ5-7E3CyR1q.js:1  
Failed to load resource: net::ERR_FILE_NOT_FOUND chunk-7BUUIJ7U-Bb538aSH.js:1  
```


all files are present:
```cmd
dir /b/s /a-d assets | findstr -i "index-BGiIaHVw.js chunk-Y2CYZVJY-DsF7k-Jl.js src-BMa7vLb8.js  chunk-WYO6CB5R-C36byBU-.js dist-Q9n2Bb2K.js chunk-ICXQ74PX-_B4UKQEp.js  path-BWPyau1x.js array-BifhSqXX.js line-BjeXKALW.js chunk-C7G6YPKG-WgqYOC9I.js  chunk-OGEWGWER-q1FVTapY.jschunk-OGEWGWER-q1FVTapY.js chunk-HOUHSVGY-BrlsNa-I.js chunk-Q4XR5HBZ-DuMv4AAJ.js rough.esm-CSKSodPl.js chunk-ZGVPDNZ5-7E3CyR1q.js chunk-7BUUIJ7U-Bb538aSH.js"
```
```text
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\array-BifhSqXX.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\chunk-7BUUIJ7U-Bb538aSH.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\chunk-C7G6YPKG-WgqYOC9I.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\chunk-HOUHSVGY-BrlsNa-I.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\chunk-ICXQ74PX-_B4UKQEp.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\chunk-Q4XR5HBZ-DuMv4AAJ.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\chunk-WYO6CB5R-C36byBU-.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\chunk-Y2CYZVJY-DsF7k-Jl.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\chunk-ZGVPDNZ5-7E3CyR1q.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\dist-Q9n2Bb2K.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\index-BGiIaHVw.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\line-BjeXKALW.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\path-BWPyau1x.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\rough.esm-CSKSodPl.js
C:\developer\sergueik\springboot_study\basic-mermaid-react\dist\assets\src-BMa7vLb8.js

```
> NOTE: moving the files around does not help:

![Directory Listing With C: Drive](screenshots/captute-local-c-2.png)


![Trouble With C: Drive](screenshots/capture-local-c.png)

However it works successfully with new instance launched with option  --allow-file-access-from-files from e: drive. 

```cmd
cd dist
subst E: %CD%
"C:\Program Files\Google\Chrome\Application\chrome.exe" --user-data-dir=C:\temp\chrome-file-test --allow-file-access-from-files file:///E:/index.html
```
![Running from E: Drive](screenshots/capture-local-e.png)

> NOTE: without the Chrome option `--allow-file-access-from-files` the error becomes:

```text
Access to script at 'file:///C:/assets/index-BGiIaHVw.js' from origin 'null' has been blocked by CORS policy: Cross origin requests are only supported for protocol schemes: chrome, chrome-extension, chrome-untrusted, data, http, https, isolated-app.
```
(repeated for other resources)

For option to have effect one has to close all running instances of Chrome browser, otherwise the new url will be served by the already launched instance.

---

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
