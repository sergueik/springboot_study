### Info

replica of [tus-java-server-spring-demo](https://github.com/tomdesair/tus-java-server-spring-demo) 
and [tus-java-server](https://github.com/tomdesair/tus-java-server)
with minimal twaks to trigger chunking and logging added
server side

### Intro

[Uppy](https://uppy.io/) has excellent support for resumable, chunked uploads. It handles this primarily through the [Tus](https://tus.io/) protocol, which splits files into smaller chunks and sends them sequentially, ensuring that dropped connections or browser crashes don't force you to start from scratch


The __tus__ protocol was [created](https://tus.io/blog) in 2013 (the __tus 1.0__ was released in 2015) by the team at Transloadit (Kevin van Zonneveld and Tim Koschützki). It was designed as an open-source standard to allow resumable file uploads over HTTP, meaning large file transfers can be paused or recovered after network drops without restarting

![TUS in Action](screenshots/capture-tus-chunks.png)

The protocol has:

  - official specification
  - reference server
  - official JS client
  - official Java client
  - official Node server
  - iOS client
  - Android client
  - conformance testing tools
  - cloud storage integrations
  - documented production users including [Vimeo](https://en.wikipedia.org/wiki/Vimeo)


#### Publicly documented users

__Vimeo__

* Early adopter
* Contributor to protocol design
* Uses __TUS__ for large video uploads
* Uses __TUS__-related infrastructure internally as well

__Transloadit__

* Creator of __TUS__
* Uses it in production for its own upload/processing platform
* Reports moving very large volumes of uploaded content through the protocol

__San Diego Supercomputer Center__ (__SDSC__)

* Mentioned as having rolled out __TUS__ support for scientific data transfer workloads

Should one use a mature upload ecosystem instead of inventing one?

> NOTE: most Java shops do not expose Java as the public upload endpoint.  A very common architecture is:

```sh
Browser (Uppy)
        |
        v
TUS endpoint
        |
        v
S3 / Blob Storage
        |
        v
Java backend processing
```
 
### Usage

> NOTE: some of the original project workflow is currently unused (webpack, maven `frontend-maven-plugin` plugin pending replacement with `exec-maven-plugin` plugin). The `tus-java-server` source is not currently used - the jar is uploaded from [maven central](https://mvnrepository.com/artifact/me.desair.tus/tus-java-server)


```cmd
pushd uppy-file-upload
npm install
npm run build
copy /y target\classes\META-INF\resources\webjars\uppy-spring-file-upload\0.0.1-SNAPSHOT\app-bundle.js ..\spring-boot-rest\src\main\resources\public
popd
pushd spring-boot-rest
mvn clean spring-boot:run
```
navigate to `http://localhost:8080`

```sh
dd if=/dev/urandom of=test.bin bs=1M count=100
```
### Background

With a tus upload, the interesting part is not the usual Spring MVC controller logging because the upload is handled by the `TusFileUploadService` directly through the `servlet` `request`/`response` objects. The protocol consists of multiple HTTP requests:

  * `OPTIONS` `/api/upload`
  * `POST` `/api/upload` (create upload)
  * `HEAD` `/api/upload/{id}` (query offset)
  * `PATCH` `/api/upload/{id}` (upload chunk)
  * additional `HEAD` / `PATCH` cycles until completion.

To see these requests, I would start with a request logging filter
{endpoint:"http://localhost:8080/api/upload"

```
2026-06-09 19:53:41.782  INFO 19720 --- [  restartedMain] me.desair.spring.tus.App                 : Starting App on sergueik59 with PID 19720 (C:\developer\sergueik\springboot_study\basic-uppy-tus\spring-boot-rest\target\classes started by kouzm in C:\developer\sergueik\springboot_study\basic-uppy-tus\spring-boot-rest)
2026-06-09 19:53:41.790  INFO 19720 --- [  restartedMain] me.desair.spring.tus.App                 : The following profiles are active: dev
2026-06-09 19:53:41.889  INFO 19720 --- [  restartedMain] .e.DevToolsPropertyDefaultsPostProcessor : Devtools property defaults active! Set 'spring.devtools.add-properties' to 'false' to disable
2026-06-09 19:53:41.890  INFO 19720 --- [  restartedMain] .e.DevToolsPropertyDefaultsPostProcessor : For additional web related logging consider setting the 'logging.level.web' property to 'DEBUG'
2026-06-09 19:53:44.002  INFO 19720 --- [  restartedMain] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat initialized with port(s): 8080 (http)
2026-06-09 19:53:44.029  INFO 19720 --- [  restartedMain] o.apache.catalina.core.StandardService   : Starting service [Tomcat]
2026-06-09 19:53:44.030  INFO 19720 --- [  restartedMain] org.apache.catalina.core.StandardEngine  : Starting Servlet engine: [Apache Tomcat/9.0.38]
2026-06-09 19:53:44.203  INFO 19720 --- [  restartedMain] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring embedded WebApplicationContext
2026-06-09 19:53:44.205  INFO 19720 --- [  restartedMain] w.s.c.ServletWebServerApplicationContext : Root WebApplicationContext: initialization completed in 2312 ms
2026-06-09 19:53:44.280 DEBUG 19720 --- [  restartedMain] o.s.w.f.CommonsRequestLoggingFilter      : Filter 'requestLoggingFilter' configured for use
2026-06-09 19:53:44.592  INFO 19720 --- [  restartedMain] o.s.s.concurrent.ThreadPoolTaskExecutor  : Initializing ExecutorService 'applicationTaskExecutor'
2026-06-09 19:53:44.693  INFO 19720 --- [  restartedMain] o.s.b.a.w.s.WelcomePageHandlerMapping    : Adding welcome page: class path resource [public/index.html]
2026-06-09 19:53:44.819  INFO 19720 --- [  restartedMain] o.s.b.d.a.OptionalLiveReloadServer       : LiveReload server is running on port 35729
2026-06-09 19:53:44.889  INFO 19720 --- [  restartedMain] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8080 (http) with context path ''
2026-06-09 19:53:44.908  INFO 19720 --- [  restartedMain] me.desair.spring.tus.App                 : =======================================
2026-06-09 19:53:44.910  INFO 19720 --- [  restartedMain] me.desair.spring.tus.App                 : App running with active profiles: dev
2026-06-09 19:53:44.917  INFO 19720 --- [  restartedMain] me.desair.spring.tus.App                 : =======================================
2026-06-09 19:53:44.930  INFO 19720 --- [  restartedMain] me.desair.spring.tus.App                 : Started App in 4.155 seconds (JVM running for 5.039)
```
after file upload starts the log shows:
```
2026-06-09 19:56:41.053 DEBUG 25120 --- [nio-8080-exec-6] o.s.w.f.CommonsRequestLoggingFilter      : Before request [POST /api/upload, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"0", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", upload-length:"52428800", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-metadata:"relativePath bnVsbA==,name dGVzdC5iaW4=,type YXBwbGljYXRpb24vb2N0ZXQtc3RyZWFt,filetype YXBwbGljYXRpb24vb2N0ZXQtc3RyZWFt,filename dGVzdC5iaW4=", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9"]]
2026-06-09 19:56:41.243  INFO 25120 --- [nio-8080-exec-6] m.d.t.s.c.CreationPostRequestHandler     : Created upload with ID 76494ffe-ba65-49af-8d4a-f60f045ad76a at 1781049401170 for ip address 0:0:0:0:0:0:0:1 with location /api/upload/76494ffe-ba65-49af-8d4a-f60f045ad76a
2026-06-09 19:56:41.251 DEBUG 25120 --- [nio-8080-exec-6] o.s.w.f.CommonsRequestLoggingFilter      : After request [POST /api/upload, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"0", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", upload-length:"52428800", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-metadata:"relativePath bnVsbA==,name dGVzdC5iaW4=,type YXBwbGljYXRpb24vb2N0ZXQtc3RyZWFt,filetype YXBwbGljYXRpb24vb2N0ZXQtc3RyZWFt,filename dGVzdC5iaW4=", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9"]]
2026-06-09 19:56:41.289 DEBUG 25120 --- [nio-8080-exec-4] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/76494ffe-ba65-49af-8d4a-f60f045ad76a, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"52428800", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"0", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 19:56:42.504  INFO 25120 --- [nio-8080-exec-4] m.d.t.s.core.CorePatchRequestHandler     : Upload with ID 76494ffe-ba65-49af-8d4a-f60f045ad76a at location /api/upload/76494ffe-ba65-49af-8d4a-f60f045ad76a finished successfully
2026-06-09 19:56:42.514 DEBUG 25120 --- [nio-8080-exec-4] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/76494ffe-ba65-49af-8d4a-f60f045ad76a, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"52428800", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"0", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
```

there is only one Upload-Offset

because the file was only 50MB

Patched the client
```javascript
    .use(Tus, { endpoint: 'http://localhost:8080/api/upload', chunkSize: 5 * 1024 * 1024  })

```

and observe TUS protocol in action:
```text

.   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v2.3.4.RELEASE)

2026-06-09 20:08:19.196  INFO 23540 --- [  restartedMain] me.desair.spring.tus.App                 : Starting App on sergueik59 with PID 23540 (C:\developer\sergueik\springboot_study\basic-uppy-tus\spring-boot-rest\target\classes started by kouzm in C:\developer\sergueik\springboot_study\basic-uppy-tus\spring-boot-rest)
2026-06-09 20:08:19.197  INFO 23540 --- [  restartedMain] me.desair.spring.tus.App                 : The following profiles are active: dev
2026-06-09 20:08:19.227  INFO 23540 --- [  restartedMain] .e.DevToolsPropertyDefaultsPostProcessor : Devtools property defaults active! Set 'spring.devtools.add-properties' to 'false' to disable
2026-06-09 20:08:19.227  INFO 23540 --- [  restartedMain] .e.DevToolsPropertyDefaultsPostProcessor : For additional web related logging consider setting the 'logging.level.web' property to 'DEBUG'
2026-06-09 20:08:19.880  INFO 23540 --- [  restartedMain] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat initialized with port(s): 8080 (http)
2026-06-09 20:08:19.887  INFO 23540 --- [  restartedMain] o.apache.catalina.core.StandardService   : Starting service [Tomcat]
2026-06-09 20:08:19.888  INFO 23540 --- [  restartedMain] org.apache.catalina.core.StandardEngine  : Starting Servlet engine: [Apache Tomcat/9.0.38]
2026-06-09 20:08:19.934  INFO 23540 --- [  restartedMain] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring embedded WebApplicationContext
2026-06-09 20:08:19.937  INFO 23540 --- [  restartedMain] w.s.c.ServletWebServerApplicationContext : Root WebApplicationContext: initialization completed in 709 ms
2026-06-09 20:08:19.971 DEBUG 23540 --- [  restartedMain] o.s.w.f.CommonsRequestLoggingFilter      : Filter 'requestLoggingFilter' configured for use
2026-06-09 20:08:20.083  INFO 23540 --- [  restartedMain] o.s.s.concurrent.ThreadPoolTaskExecutor  : Initializing ExecutorService 'applicationTaskExecutor'
2026-06-09 20:08:20.117  INFO 23540 --- [  restartedMain] o.s.b.a.w.s.WelcomePageHandlerMapping    : Adding welcome page: class path resource [public/index.html]
2026-06-09 20:08:20.157  INFO 23540 --- [  restartedMain] o.s.b.d.a.OptionalLiveReloadServer       : LiveReload server is running on port 35729
2026-06-09 20:08:20.180  INFO 23540 --- [  restartedMain] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8080 (http) with context path ''
2026-06-09 20:08:20.185  INFO 23540 --- [  restartedMain] me.desair.spring.tus.App                 : =======================================
2026-06-09 20:08:20.185  INFO 23540 --- [  restartedMain] me.desair.spring.tus.App                 : App running with active profiles: dev
2026-06-09 20:08:20.186  INFO 23540 --- [  restartedMain] me.desair.spring.tus.App                 : =======================================
2026-06-09 20:08:20.189  INFO 23540 --- [  restartedMain] me.desair.spring.tus.App                 : Started App in 1.234 seconds (JVM running for 1.515)
2026-06-09 20:08:48.182  INFO 23540 --- [nio-8080-exec-1] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring DispatcherServlet 'dispatcherServlet'
2026-06-09 20:08:48.187  INFO 23540 --- [nio-8080-exec-1] o.s.web.servlet.DispatcherServlet        : Initializing Servlet 'dispatcherServlet'
2026-06-09 20:08:48.211  INFO 23540 --- [nio-8080-exec-1] o.s.web.servlet.DispatcherServlet        : Completed initialization in 21 ms
...
2026-06-09 20:09:05.471 DEBUG 23540 --- [nio-8080-exec-5] o.s.w.f.CommonsRequestLoggingFilter      : Before request [POST /api/upload, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"0", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", upload-length:"52428800", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-metadata:"relativePath bnVsbA==,name dGVzdC5iaW4=,type YXBwbGljYXRpb24vb2N0ZXQtc3RyZWFt,filetype YXBwbGljYXRpb24vb2N0ZXQtc3RyZWFt,filename dGVzdC5iaW4=", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9"]]
2026-06-09 20:09:05.568  INFO 23540 --- [nio-8080-exec-5] m.d.t.s.c.CreationPostRequestHandler     : Created upload with ID d3ad2a50-7b4f-4179-8a3e-28d74366ad17 at 1781050145527 for ip address 0:0:0:0:0:0:0:1 with location /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17
2026-06-09 20:09:05.571 DEBUG 23540 --- [nio-8080-exec-5] o.s.w.f.CommonsRequestLoggingFilter      : After request [POST /api/upload, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"0", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", upload-length:"52428800", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-metadata:"relativePath bnVsbA==,name dGVzdC5iaW4=,type YXBwbGljYXRpb24vb2N0ZXQtc3RyZWFt,filetype YXBwbGljYXRpb24vb2N0ZXQtc3RyZWFt,filename dGVzdC5iaW4=", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9"]]
2026-06-09 20:09:05.584 DEBUG 23540 --- [nio-8080-exec-9] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"0", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:05.748 DEBUG 23540 --- [nio-8080-exec-9] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"0", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:05.755 DEBUG 23540 --- [nio-8080-exec-7] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"5242880", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:05.818 DEBUG 23540 --- [nio-8080-exec-7] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"5242880", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:05.828 DEBUG 23540 --- [nio-8080-exec-8] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"10485760", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:05.892 DEBUG 23540 --- [nio-8080-exec-8] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"10485760", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:05.901 DEBUG 23540 --- [nio-8080-exec-6] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"15728640", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:05.945 DEBUG 23540 --- [nio-8080-exec-6] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"15728640", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:05.954 DEBUG 23540 --- [io-8080-exec-10] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"20971520", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:06.040 DEBUG 23540 --- [io-8080-exec-10] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"20971520", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:06.057 DEBUG 23540 --- [nio-8080-exec-1] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"26214400", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:06.177 DEBUG 23540 --- [nio-8080-exec-1] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"26214400", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:06.191 DEBUG 23540 --- [nio-8080-exec-3] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"31457280", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:06.271 DEBUG 23540 --- [nio-8080-exec-3] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"31457280", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:06.279 DEBUG 23540 --- [nio-8080-exec-2] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"36700160", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:06.319 DEBUG 23540 --- [nio-8080-exec-2] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"36700160", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:06.326 DEBUG 23540 --- [nio-8080-exec-4] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"41943040", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:06.365 DEBUG 23540 --- [nio-8080-exec-4] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"41943040", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:06.370 DEBUG 23540 --- [nio-8080-exec-5] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"47185920", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-09 20:09:06.411  INFO 23540 --- [nio-8080-exec-5] m.d.t.s.core.CorePatchRequestHandler     : Upload with ID d3ad2a50-7b4f-4179-8a3e-28d74366ad17 at location /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17 finished successfully
2026-06-09 20:09:06.413 DEBUG 23540 --- [nio-8080-exec-5] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"5242880", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/148.0.0.0 Safari/537.36", sec-ch-ua:""Chromium";v="148", "Google Chrome";v="148", "Not/A)Brand";v="99"", upload-offset:"47185920", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://localhost:8080", sec-fetch-site:"same-origin", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://localhost:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
```
Server creates:
```
Location: /api/upload/$ID
```
(`ID` is `d3ad2a50-7b4f-4179-8a3e-28d74366ad17`)
Then a sequence of PATCH requests:
```
PATCH
Upload-Offset: 0
Content-Length: 5242880
```
followed by:
```
PATCH
Upload-Offset: 5242880
Content-Length: 5242880
```
then:
```
PATCH
Upload-Offset: 10485760
Content-Length: 5242880
```
and so on until:
```
Upload-Offset: 47185920
```
for the final chunk of a 50 MB file.

TUS upload is usually not initiated by uploading any file bytes. The first request is typically a resource creation request.

The simplest TUS flow is:

1. Create upload
```
POST /api/upload
Tus-Resumable: 1.0.0
Upload-Length: 1234567
Upload-Metadata: filename Zm9vLnppcA==
```
No file body is sent.

Server responds:
```
HTTP/1.1 201 Created
Location: /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17
Tus-Resumable: 1.0.0
```
The important thing is the `Location` header.

At this point the server has created an upload resource but has received zero bytes.

2. Upload first chunk
```
PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17
Tus-Resumable: 1.0.0
Content-Type: application/offset+octet-stream
Upload-Offset: 0
```
followed by
```
<binary bytes>
```
Server replies:
```
204 No Content
Upload-Offset: 5242880
```
meaning *I have `5 MB` now*.

3. Continue

Client sends another PATCH:
```
PATCH /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17
Upload-Offset: 5242880
```
with the next chunk.

4. Resume after interruption

Before sending more data, client asks:
```
HEAD /api/upload/d3ad2a50-7b4f-4179-8a3e-28d74366ad17
Tus-Resumable: 1.0.0
```
Server replies:

```text
204 No Content
Upload-Offset: 5242880
```

Client learns where to continue.

Internally __tus__ was doing roughly:

| step| effect |
|---|---|
|`POST`  | create upload|
|`PATCH` | chunk #1|
|`PATCH` | chunk #2|
|`PATCH` | chunk #3|
|...     |         |
|`HEAD`  | recover after interruption|
|`PATCH` | continue |

How to explore with curl

You can manually create an upload:
```
curl -i \
  -X POST \
  -H "Tus-Resumable: 1.0.0" \
  -H "Upload-Length: 1000" \
  http://localhost:8080/api/upload
```
Look at the `Location:` header in the response:

Then pipe the data through curl:
```sh
dd if=test.bin bs=100 count=1 | \
curl -i -X PATCH -H 'Tus-Resumable: 1.0.0' -H 'Content-Type: application/offset+octet-stream' -H 'Upload-Offset: 0' \
http://localhost:8080/api/upload/<id>  --data-binary @- 
```			
Then:
```sh
curl -i -X HEAD -H 'Tus-Resumable: 1.0.0' http://localhost:8080/api/upload/<id>
```
to see the current offset.

How I'd learn it

Since you've already got an Uppy client talking to your Spring server, open your browser DevTools Network tab and filter by:

  * `POST`
  * `PATCH`
  * `HEAD`

Then upload a small file (10 MB is good).


### TUS ecosystem

|Component	|Language	|GitHub Stars| Forks|
|-----------|-----------|------------|------|
|Uppy       |TypeScript<br/>Browser upload framework|30k| |
|tusd *          | 	Go          | 3800      | 550 |
|tus-js-client   | JavaScript	| 2600      | 338  |
|tus-node-server |	TypeScript/Node	|1100   | 227  |
|tusdotnet       |.NET Server       | 750   |      |
|tus-java-client |	Java         	| 230    |      |
| tus-java-server |  Java           | 170    |      |
| TUSKit         | iOS              | 240    |      |
|tus-android-client|Android         |180     |      |


notably,
Uppy and TUS solve different problems:
```
Uppy
  |
  +-- Dashboard UI
  +-- Drag & Drop
  +-- Progress bars
  +-- Retry handling
  +-- File selection
  +-- React integration
  +-- Companion integrations
  |
  +-- TUS plugin
          |
          +-- TUS protocol
```

Notable about __.NET__:

The .NET implementation is not a tiny hobby project.

  * tusdotnet GitHub
  * ASP.NET Core support
  * .NET Framework support
  * Implements all major TUS extensions
  * Recent commits in 2026
  * Active NuGet releases in 2026


The .NET implementation is particularly useful as a reference implementation because modern .NET projects often emphasize API discoverability, strong typing, XML/API documentation, unit testing, and readable framework integration. Even in organizations that do not deploy .NET in production, the tusdotnet source can be a valuable learning resource for understanding protocol internals and extension points.

What this says about adoption

The ecosystem center of gravity is clearly:

  * Browser / JavaScript
  * Go servers
  * Node servers
  * Mobile clients
  * Java

This is not surprising because resumable uploads are primarily a browser problem.

### See Also

  * https://blog.rasc.ch/2019/06/upload-with-tus.html
  * https://tus.io/protocols/resumable-upload#core-protocol
  * https://aiundecided.com/posts/tus-uppy-resumable-upload-architecture/
  * [tus implementations](https://tus.io/implementations). Notably, GitHub's tus-protocol topic currently shows roughly 60+ public repositories implementing or extending the protocol across multiple languages
  * `PATCH` Method for `HTTP` [RFC5789](https://www.rfc-editor.org/info/rfc5789/)
  * [tusdotnet/tusdotnet](https://github.com/tusdotnet/tusdotnet) .Net implementation of TUS Server supporting platformes ranging from __net452__, __net6.0__
  * [nuget package](https://www.nuget.org/packages/tusdotnet). NOTE: latest suported version is __2.11.1__, first release __1.0.0__ is from 2016. This versioning schema is unrelated to tus protocol version which is currenty __1.0.0__ (the __1.0.3__ relies on Java 17 features. Running on a Windows machine might require configuring IIS.
  * [gerdus/tus-dotnet-client](https://github.com/gerdus/tus-dotnet-client) - tus.io Client for .Net (supported range of NDP from .Net __4.0__)
  * .Net __5.0__ [Tus.Net.Client](https://github.com/hoss-green/Tus.Net.Client)
  * .Net __9.0__ [Newex/solidTUS](https://github.com/Newex/solidTUS) - .net TUS Server
  * .Net  __5.0__ and newer [bluetianx/BirdMessenger](https://github.com/bluetianx/BirdMessenger) Tus client
  * https://github.com/FuGuangzhi1/tus-demo - uses `tusdotnet`
  * [TwistingTwists/tusd-node-uppy---resumable-upload](https://github.com/TwistingTwists/tusd-node-uppy---resumable-upload) - node.js backend (presumably swappable) and uppy/tus frontend (elementary)

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
