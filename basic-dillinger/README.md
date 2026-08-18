### Dillinger Live As Static



```
Full Dillinger container
788 MB
    │
    ├── Node
    ├── Express
    ├── plugins
    ├── OAuth integrationsa
    ├── node_modules
    ├── build machinery
    └── public/  ← 6.4 MB
                   │
                   ▼
             browser application
```
> The useful artifact for the target audience is the public/ browser application, not the 788 MB container.

| Deployment | Prerequisite |
|---|---|
| container | Docker/Podman, Virtualization (WSL2) |
| Express application | Node.js |
| SPA | **None** - browser only |


### Background

__Dillinger__ - free, online, browser-based AngularJS-powered HTML5 live Markdown editor featuring a split-pane interface with real-time live preview.


### Usage



```sh
docker pull linuxserver/dillinger:3.39.1
```
```sh
docker image ls
```
```text
REPOSITORY              TAG                 IMAGE ID            CREATED             SIZE
linuxserver/dillinger   3.39.1              ba7ab914577c        2 years ago         788MB
```
```sh
docker run -d --name=linuxserver-dillinger -p 9090:8080 linuxserver/dillinger:3.39.1
```
```sh
docker inspect linuxserver-dillinger |jq '.[0].Config.Entrypoint'
```
```json
[
  "/init"
]
```
```sh
docker inspect linuxserver-dillinger |jq '.[0].Config.Cmd'
```
```
null
```


> **NOTE:** You can examine the container's `init` script if desired:
>
> ```sh
> docker exec -it linuxserver-dillinger cat /init
> ```
>
> The important point is that the Dillinger application is ultimately
> running under Node.js:
>
> ```sh
> ps ax | grep node
> ```
>
> ```text
>   159 ?        Ssl    0:05 node app
>   326 pts/0    S+     0:00 grep node
> ```
>

> and is located in the `app/dilinger` folder:
> ```sh
> ls /app/dillinger/
> ```
> ```text
> Dockerfile  config.js           karma.conf.js      public
> LICENSE     configs             nginx              routes
> Procfile    dillinger.service   node_modules       snapcraft.yaml
> README.md   docker-compose.yml  package-lock.json  views
> app.js      gulp                package.json       webpack.config.js
> bin         gulpfile.js         plugins
> ```

#### Copy Application

* try to copy locally
```sh
docker cp linuxserver-dillinger:/app/dillinger/ .
```
cannot continue - most of the files never copied:
```
symlink \config\configs C:\...dillinger\configs: A required privilege is not held by the client.
```

* copy locally
```sh
docker exec -it linuxserver-dillinger tar cf /tmp/a.tar -C /app dillinger
```

```sh
docker cp  linuxserver-dillinger:/tmp/a.tar .
```

```sh
ls -hl a.tar
```
```text
-rw-r--r-- 1 kouzm 197610 122M Aug 18 12:50 a.tar
```
```sh
tar xf a.tar
```
```text
tar: dillinger/configs: Cannot create symlink to ‘/config/configs’: No such file or directory
tar: Exiting with failure status due to previous errors
```
> NOTE: attempt to modify flags to let tar run quite does not work, but was unnecessary 
```sh
 tar --ignore-command-error -xf a.tar
```

```sh
 find ./dillinger/ -type f |wc -l
```

Examining `package.json` reveals imporant info:

  * AngularJS 1.7.9
  * Node 14 as the declared engine
  * old Webpack/Gulp toolchain
  * markdown-it
  * the large dependency set

#### Run Application

```sh
node app
```
```text
Dropbox config not found at C:\developer\sergueik\springboot_study\basic-mermaid-react\dillinger\configs\dropbox\dropbox-config.json. Plugin disabled.
Bitbucket config not found at C:\developer\sergueik\springboot_study\basic-mermaid-react\dillinger\configs\bitbucket\bitbucket-config.json. Plugin disabled.
Github config not found at C:\developer\sergueik\springboot_study\basic-mermaid-react\dillinger\configs\github\github-config.json. Plugin disabled.
Medium config not found at C:\developer\sergueik\springboot_study\basic-mermaid-react\dillinger\configs\medium\medium-config.json. Plugin disabled.
Google Drive config not found at C:\developer\sergueik\springboot_study\basic-mermaid-react\dillinger\configs\googledrive\googledrive-config.json. Plugin disabled.
OneDrive config not found at C:\developer\sergueik\springboot_study\basic-mermaid-react\dillinger\configs\onedrive\onedrive-config.json. Plugin disabled.
Sponsored config not found at C:\developer\sergueik\springboot_study\basic-mermaid-react\dillinger\configs\sponsored\sponsored-config.json. Plugin disabled.
GoogleAnalytics config not found at C:\developer\sergueik\springboot_study\basic-mermaid-react\dillinger\configs\googleanalytics\googleanalytics-config.json. Plugin disabled.
```
```
Express server listening on port 8080
http://localhost:8080
```

open in the browser `http://localhost:8080`

![Run Locally](screenshots/capture-local.png)

Server logs are healthy

```text
GET / 200 239.417 ms - -
GET /css/app.css 200 6.312 ms - -
GET /js/main.bundle.js 200 5.265 ms - -
GET /theme-github.js 404 5.611 ms - 154
GET /img/icons/file.svg 200 3.601 ms - -
GET /img/icons/code.svg 200 3.117 ms - 902
GET /img/icons/enter-zen.svg 200 4.376 ms - -
```

```

#### Run as File


```cmd
cd ..\basic-dillinger
subst E: /d
subst E: %CD%
"C:\Program Files\Google\Chrome\Application\chrome.exe" --user-data-dir=C:\temp\chrome-file-test --allow-file-access-from-files file:///E:/index.html
```

![Run as File](screenshots/capture-file.png)

> NOTE: use 
```sh
cygpath -wa .
```

if necesary can package the files and distribute

```sh
"c:\Program Files\7-Zip\7z.exe" a ..\dillinger.zip -r .
```
```text
7-Zip 21.07 (x64) : Copyright (c) 1999-2021 Igor Pavlov : 2021-12-26

Scanning the drive:
77 folders, 388 files, 5661824 bytes (5530 KiB)

Creating archive: ..\dillinger.zip

Add new data to archive: 77 folders, 388 files, 5661824 bytes (5530 KiB)


Files read from disk: 388
Archive size: 1718996 bytes (1679 KiB)
Everything is Ok
```
### Cleanup

```sh
docker stop linuxserver-dillinger
docker container prune -f
docker image prune -f
docker image rm linuxserver/dillinger:3.39.1
```
The latest revisions of `joemccann/dillinger` are using [Next.js](https://en.wikipedia.org/wiki/Next.js) while originally if has been a plain
 [AngularJS](https://en.wikipedia.org/wiki/AngularJS) powered HTML5 Markdown editor
 
### See Also:

  * [joemccann/dillinger](https://hub.docker.com/r/joemccann/dillinger) (NOTE: latest releases __3.41.0__ are significantly heavier than __3.39.0__ or earlier 
  * [live](https://dillinger.io) 
  * [joemccann/dillinger](https://github.com/joemccann/dillinger)
  * [dillinger](https://hub.docker.com/r/linuxserver/dillinger) - smaller Docker image (older, deprecated) and [repository](https://github.com/linuxserver-archive/docker-dillinger)


---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


