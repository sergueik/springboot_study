### Info
https://github.com/judsonc/react-upload-uppy
https://uppy.io/
https://github.com/transloadit/uppy with over 10K commits

### Usage


```sh
docker pull node:22.18.0-alpine
docker pull maven:3.9.5-eclipse-temurin-11-alpine
docker pull eclipse-temurin:11-jre-alpine
```

```sh
docker build -t example -f Dockerfile .
```
the `Dockerfile` compiles `react` frontend and puts it into Spring as static resource then packages the jar:


```text
Step 1/15 : FROM node:22.18.0-alpine AS react_builder
 ---> 8a3ae2e7d0c5
Step 2/15 : WORKDIR /app
 ---> Running in c220d46b825d
Removing intermediate container c220d46b825d
 ---> e93a4c73beb2
Step 3/15 : COPY frontend /app/
 ---> 1b3ccda6b500
Step 4/15 : RUN cd /app   && rm -rf node_modules package-lock.json   && npm install     @uppy/core@5.2.0     @uppy/dashboard@5.1.1     @uppy/xhr-upload@5.2.0     @uppy/react@5.1.1
 ---> Running in 1bb8ac5018e6

added 95 packages, and audited 96 packages in 44s

16 packages are looking for funding
  run `npm fund` for details

found 0 vulnerabilities
npm notice
npm notice New major version of npm available! 10.9.3 -> 11.15.0
npm notice Changelog: https://github.com/npm/cli/releases/tag/v11.15.0
npm notice To update run: npm install -g npm@11.15.0
npm notice
Removing intermediate container 1bb8ac5018e6
 ---> baa8e4643a12
Step 5/15 : RUN npm run build
 ---> Running in ffcff40738c9

> uppy-react-upload@1.0.0 build
> vite build

vite v7.3.3 building client environment for production...
transforming...
✓ 212 modules transformed.
rendering chunks...
computing gzip size...
dist/index.html                   0.31 kB │ gzip:   0.22 kB
dist/assets/index-D9e3sqm0.css   65.60 kB │ gzip:  10.45 kB
dist/assets/index-BpCiVfFe.js   377.96 kB │ gzip: 118.41 kB
✓ built in 8.21s
Removing intermediate container ffcff40738c9
 ---> 812ae8962c65
Step 6/15 : FROM maven:3.9.5-eclipse-temurin-11-alpine as builder
 ---> 37ef041f8432
Step 7/15 : WORKDIR /app
 ---> Using cache
 ---> cb7dd4b470ab
Step 8/15 : COPY backend /app/
 ---> bfe19ff22fe1
Step 9/15 : COPY --from=react_builder /app/dist /app/src/main/resources/static/
 ---> d101b966ccef
Step 10/15 : RUN cd /app && mvn dependency:go-offline -q
 ---> Running in 436b669e40f5
Removing intermediate container 436b669e40f5
 ---> 0fec68d3a68a
Step 11/15 : RUN cd /app && mvn package -DskipTests -q
 ---> Running in bf520793b4a5
Removing intermediate container bf520793b4a5
 ---> 3dd394c259e7
Step 12/15 : FROM eclipse-temurin:11-jre-alpine as run
 ---> 642de1708b20
Step 13/15 : COPY --from=builder /app/target/example.uppy-react-multipart-upload-backend.jar /app/app.jar
Step 14/17 : RUN apk update     && apk add --update --no-cache curl     && rm -rf /var/cache/*     && m                                     kdir /var/cache/apk
 ---> Running in c0367ea9ce57
v3.23.4-268-gdcc713e014f [https://dl-cdn.alpinelinux.org/alpine/v3.23/main]
v3.23.4-271-g3e9e0da6943 [https://dl-cdn.alpinelinux.org/alpine/v3.23/community]
OK: 27581 distinct packages available
(1/5) Installing c-ares (1.34.6-r0)
(2/5) Installing nghttp2-libs (1.69.0-r0)
(3/5) Installing libpsl (0.21.5-r3)
(4/5) Installing libcurl (8.19.0-r0)
(5/5) Installing curl (8.19.0-r0)
Executing busybox-1.37.0-r30.trigger
OK: 41.8 MiB in 78 packages
Removing intermediate container c0367ea9ce57 
 ---> 77267e2fec25
Step 15/15 : ENTRYPOINT ["java", "-jar", "/app/app.jar"]
 ---> Running in 6791750bfdd7
Removing intermediate container 6791750bfdd7
 ---> f0605d87417e
Step 16/17 : HEALTHCHECK --interval=30s --timeout=5s --start-period=10s CMD curl -f http://localhost:80                                     80/upload || exit 1
 ---> Running in 6b05c52541ef
Removing intermediate container 6b05c52541ef
Step 17/15 : EXPOSE 8080
 ---> Running in c80e26adccc4
Removing intermediate container c80e26adccc4
 ---> 8e645532396e
Successfully built 8e645532396e
Successfully tagged example:latest
```

```sh
docker create --name example example
docker export example |tar tv | grep /app/app.jar
```
```text
-rw-r--r-- 0/0        18133112 2026-05-25 11:14 app/app.jar
```
```sh
docker export example |tar xv app/app.jar
[O
docker container rm example
```
```sh
unzip -ql app/app.jar |grep -E '(static|templates)'
```
```text
        0  2026-05-25 15:33   BOOT-INF/classes/templates/
        0  2026-05-25 15:33   BOOT-INF/classes/static/
        0  2026-05-25 15:33   BOOT-INF/classes/static/js/
        0  2026-05-25 15:33   BOOT-INF/classes/static/css/
        0  2026-05-25 15:33   BOOT-INF/classes/static/assets/
     1240  2026-05-25 15:33   BOOT-INF/classes/templates/upload.html
     3151  2026-05-25 15:33   BOOT-INF/classes/static/js/main.js
      311  2026-05-25 15:33   BOOT-INF/classes/static/index.html
       56  2026-05-25 15:33   BOOT-INF/classes/static/css/style.css
   378022  2026-05-25 15:33   BOOT-INF/classes/static/assets/index-C6DRwziu.js
    65603  2026-05-25 15:33   BOOT-INF/classes/static/assets/index-D9e3sqm0.css
```
it has both AngularJS (bare-bones non-styled, without drop zone) and [ReactJS](https://legacy.reactjs.org/) A JavaScript library for building user interfaces) driven upload pages with [uppy](https://uppy.io/) 

run both
```sh
docker run -d -p 8080:8080 --name example example
```
```sh
docker ps
```
```txt
CONTAINER ID        IMAGE               COMMAND                  CREATED             STATUS                                                                  PORTS                    NAMES
9fac7e1b533d        example             "java -jar /app/app.…"   27 seconds ago      Up 26 seconds (health: starting)   0.0.0.0:8080->8080/tcp   example
```
```txt
CONTAINER ID        IMAGE               COMMAND                  CREATED              STATUS                        PORTS                    NAMES
9fac7e1b533d        example             "java -jar /app/app.…"   About a minute ago   Up About a minute (healthy)   0.0.0.0:8080->8080/tcp   example

```
```sh
docker logs -f example
```
```text

  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::                (v2.7.8)

2026-05-23 18:48:01.808  INFO 1 --- [           main] example.Application                      : Starting Application v0.2.0-SNAPSHOT using Java 11.0.31 on 466676c927ab with PID 1 (/app/app.jar started by root in /)
2026-05-23 18:48:01.826  INFO 1 --- [           main] example.Application                      : No active profile set, falling back to 1 default profile: "default"
2026-05-23 18:48:01.834 DEBUG 1 --- [           main] o.s.boot.SpringApplication               : Loading source class example.Application
2026-05-23 18:48:02.172 DEBUG 1 --- [           main] ConfigServletWebServerApplicationContext : Refreshing org.springframework.boot.web.servlet.context.AnnotationConfigServletWebServerApplicationContext@561b6512
2026-05-23 18:48:09.922 DEBUG 1 --- [           main] .s.b.w.e.t.TomcatServletWebServerFactory : Code archive: /app/app.jar
2026-05-23 18:48:09.927 DEBUG 1 --- [           main] .s.b.w.e.t.TomcatServletWebServerFactory : Code archive: /app/app.jar
2026-05-23 18:48:09.931 DEBUG 1 --- [           main] .s.b.w.e.t.TomcatServletWebServerFactory : None of the document roots [src/main/webapp, public, static] point to a directory and will be ignored.
2026-05-23 18:48:10.142  INFO 1 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat initialized with port(s): 8080 (http)
2026-05-23 18:48:10.249  INFO 1 --- [           main] o.apache.catalina.core.StandardService   : Starting service [Tomcat]
2026-05-23 18:48:10.253  INFO 1 --- [           main] org.apache.catalina.core.StandardEngine  : Starting Servlet engine: [Apache Tomcat/9.0.71]
2026-05-23 18:48:10.926  INFO 1 --- [           main] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring embedded WebApplicationContext
2026-05-23 18:48:10.927 DEBUG 1 --- [           main] w.s.c.ServletWebServerApplicationContext : Published root WebApplicationContext as ServletContext attribute with name [org.springframework.web.context.WebApplicationContext.ROOT]
2026-05-23 18:48:10.927  INFO 1 --- [           main] w.s.c.ServletWebServerApplicationContext : Root WebApplicationContext: initialization completed in 8755 ms
2026-05-23 18:48:12.860 DEBUG 1 --- [           main] o.s.b.w.s.ServletContextInitializerBeans : Mapping filters: characterEncodingFilter urls=[/*] order=-2147483648, formContentFilter urls=[/*] order=-9900, requestContextFilter urls=[/*] order=-105
2026-05-23 18:48:12.866 DEBUG 1 --- [           main] o.s.b.w.s.ServletContextInitializerBeans : Mapping servlets: dispatcherServlet urls=[/]
2026-05-23 18:48:13.116 DEBUG 1 --- [           main] o.s.b.w.s.f.OrderedRequestContextFilter  : Filter 'requestContextFilter' configured for use
2026-05-23 18:48:13.122 DEBUG 1 --- [           main] s.b.w.s.f.OrderedCharacterEncodingFilter : Filter 'characterEncodingFilter' configured for use
2026-05-23 18:48:13.123 DEBUG 1 --- [           main] o.s.b.w.s.f.OrderedFormContentFilter     : Filter 'formContentFilter' configured for use
2026-05-23 18:48:14.540 DEBUG 1 --- [           main] s.w.s.m.m.a.RequestMappingHandlerAdapter : ControllerAdvice beans: 0 @ModelAttribute, 0 @InitBinder, 1 RequestBodyAdvice, 1 ResponseBodyAdvice
2026-05-23 18:48:14.864  INFO 1 --- [           main] o.s.b.a.w.s.WelcomePageHandlerMapping    : Adding welcome page: class path resource [static/index.html]
2026-05-23 18:48:15.255 DEBUG 1 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : 6 mappings in 'requestMappingHandlerMapping'
2026-05-23 18:48:15.528 DEBUG 1 --- [           main] o.s.w.s.handler.SimpleUrlHandlerMapping  : Patterns [/webjars/**, /**] in 'resourceHandlerMapping'
2026-05-23 18:48:15.614 DEBUG 1 --- [           main] .m.m.a.ExceptionHandlerExceptionResolver : ControllerAdvice beans: 0 @ExceptionHandler, 1 ResponseBodyAdvice
2026-05-23 18:48:16.168  INFO 1 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8080 (http) with context path ''
2026-05-23 18:48:16.273  INFO 1 --- [           main] example.Application                      : Started Application in 18.944 seconds (JVM running for 22.097)
2026-05-23 18:48:16.289 DEBUG 1 --- [           main] o.s.b.a.ApplicationAvailabilityBean      : Application availability state LivenessState changed to CORRECT
2026-05-23 18:48:16.313 DEBUG 1 --- [           main] o.s.b.a.ApplicationAvailabilityBean      : Application availability state ReadinessState changed to ACCEPTING_TRAFFIC

```
construct dummy upload not exceeding the `spring.servlet.multipart.max-request-size`:

```sh
dd if=/dev/urandom of=test.bin bs=1M count=100
```
```text
100+0 records in
100+0 records out
104857600 bytes (105 MB, 100 MiB) copied, 0.13182 s, 795 MB/s
```
launch application

#### Angular
![execute](screenshots/capture-upload-angular.png)


![execute](screenshots/capture-upload-success-angular.png)
#### React

![execute](screenshots/capture-upload-react.png)

![execute](screenshots/capture-upload-app.png)

![execute](screenshots/capture-upload-progress.png)

observe success on the frontend:
![execute](screenshots/capture-upload-success.png)

The application console log shows similar information in both scenarios:
```text
Servlet        : POST "/uploadMultipleFiles", parameters={multipart}
2026-05-23 18:49:11.469 DEBUG 1 --- [nio-8080-exec-4] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped to example.controller.FileUploadController#uploadMultipleFiles(MultipartFile[], Integer, Integer)
2026-05-23 18:49:11.602  INFO 1 --- [nio-8080-exec-4] example.controller.FileUploadController  : upload 1 files: [test.bin]
2026-05-23 18:49:11.610  INFO 1 --- [nio-8080-exec-4] example.controller.FileUploadController  : Received full payload (non-chunked): test.bin
2026-05-23 18:49:11.640  INFO 1 --- [nio-8080-exec-4] example.controller.FileUploadController  : upload file: test.bin
2026-05-23 18:49:11.646  INFO 1 --- [nio-8080-exec-4] example.service.FileStorageService       : UploadDir defined: /tmp/upload
2026-05-23 18:49:12.643  INFO 1 --- [nio-8080-exec-4] example.service.FileStorageService       : UploadDir: /tmp/upload
2026-05-23 18:49:12.656  INFO 1 --- [nio-8080-exec-4] example.controller.FileUploadController  : Listing: test.bin
2026-05-23 18:49:12.672 DEBUG 1 --- [nio-8080-exec-4] m.m.a.RequestResponseBodyMethodProcessor : Using 'application/json', given [*/*] and supported [application/json]
2026-05-23 18:49:12.722 DEBUG 1 --- [nio-8080-exec-4] m.m.a.RequestResponseBodyMethodProcessor : Writing [{success=true, uploaded=[test.bin], url=/upload-success}]
2026-05-23 18:49:12.880 DEBUG 1 --- [nio-8080-exec-4] o.s.web.servlet.DispatcherServlet        : Completed 200 OK
```
and the file is present on container:
```sh
docker exec -it example ls -hl /tmp/upload/test.bin
```
```
-rw-r--r-- 1 root root 100M May 23 17:07 /tmp/upload/test.bin
```
```sh
docker exec -it example sha256sum /tmp/upload/test.bin
```
```text
e37c10c86f56c0ca4778727e8bc8cdc7e428a68ac2daaddfd73429d134a7dd3e  /tmp/upload/test.bin
```
```sh
sha256sum.exe test.bin
```
```text
e37c10c86f56c0ca4778727e8bc8cdc7e428a68ac2daaddfd73429d134a7dd3e *test.bin
```
> NOTE: the "test.bin" is sufficiently large to test file chunking

with `300` __MB__  attempt, the `FileSizeLimitExceededException` error before  controller fully processed the request
```
org.apache.tomcat.util.http.fileupload.impl.FileSizeLimitExceededException: The field files exceeds its maximum permitted size of 209715200 bytes.
        at org.apache.tomcat.util.http.fileupload.impl.FileItemStreamImpl$1.raiseError(FileItemStreamImpl.java:117) ~[tomcat-embed-core-9.0.71.jar!/:na]

2026-05-23 19:25:52.569 ERROR 1 --- [io-8080-exec-10] o.a.c.c.C.[.[.[/].[dispatcherServlet]    : Servlet.service() for servlet [dispatcherServlet] in context with path [] threw exception [Request processing failed; nested exception is org.springframework.web.multipart.MaxUploadSizeExceededException: Maximum upload size exceeded; nested exception is java.lang.IllegalStateException: org.apache.tomcat.util.http.fileupload.impl.FileSizeLimitExceededException: The field files exceeds its maximum permitted size of 209715200 bytes.] with root cause

...

2026-05-23 19:25:52.576 DEBUG 1 --- [io-8080-exec-10] o.s.web.servlet.DispatcherServlet        : "ERROR" dispatch for POST "/error", parameters={multipart}
2026-05-23 19:25:52.580 DEBUG 1 --- [io-8080-exec-10] o.s.web.servlet.DispatcherServlet        : Multipart resolution previously failed for current request - skipping re-resolution for undisturbed error rendering
2026-05-23 19:25:52.582 DEBUG 1 --- [io-8080-exec-10] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped to org.springframework.boot.autoconfigure.web.servlet.error.BasicErrorController#error(HttpServletRequest)

```
- frontend still retries full upload 4 or so more times before concluding 0 of  file uploaded 

with `190` __MB__ - stil one  chunk:
```
HandlerMapping : Mapped to example.controller.FileUploadController#uploadMultipleFiles(HttpServletRequest, MultipartFile[], Integer, Integer)
2026-05-23 19:30:39.508  INFO 1 --- [nio-8080-exec-6] example.controller.FileUploadController  : Content-Type: multipart/form-data; boundary=----WebKitFormBoundary9MBEBwXB9rODKLN0
2026-05-23 19:30:39.509  INFO 1 --- [nio-8080-exec-6] example.controller.FileUploadController  : Content-Length: 199229637
2026-05-23 19:30:39.511  INFO 1 --- [nio-8080-exec-6] example.controller.FileUploadController  : Servlet parts count: 1
2026-05-23 19:30:39.512  INFO 1 --- [nio-8080-exec-6] example.controller.FileUploadController  : part name=files submittedFileName=test.bin size=199229440 contentType=application/octet-stream
2026-05-23 19:30:39.513  INFO 1 --- [nio-8080-exec-6] example.controller.FileUploadController  : upload 1 files: [test.bin]
2026-05-23 19:30:39.517  INFO 1 --- [nio-8080-exec-6] example.controller.FileUploadController  : Received full payload (non-chunked): test.bin
```
conclusion:

the config:
```js
chunking: {
  enabled: true,
  size: 5 * 1024 * 1024
}
```
in `XHRUpload` is not a guaranteed "server-visible chunk protocol" switch.

With `@uppy/xhr-upload`, it generally still behaves like:
```
ONE POST request
ONE multipart body
```
### Simulation

to simulate
  * chunking
  * resumability
  * extra safety
  * better reliability
in `curl`: 

```sh
i=0
for f in chunk_*; do
  curl -X POST http://$(docker-machine ip):8080/uploadChunk \
    -F "chunk=@$f" \
    -F "index=$i" \
    -F "fileName=test.bin"
  i=$((i+1))
done
```
one also needs to implement assbmbly the result on the backend.

### Cleanup
```sh
docker stop example; docker container prune -f ; docker image prune -f
```
### Background

__React__ is very unbeleivably complex - age?. Over the years.

creating the frontend from scratch with __Vite__ __React__ *template* is what is recmmended instead of manually assembling:

  * `package.json`
  * `Babel`
  * `webpack`
  * `React` bootstrap files
  * `ESLint`
  * `build config`


However this seems seriously feels backwards if you come from traditional build systems where source layout is explicit and inspectable. One has to accept it.


### See Also

  * [React Introduction](https://www.geeksforgeeks.org/reactjs/reactjs-introduction/)
  * [AngularJS Tutorial](https://www.geeksforgeeks.org/angular-js/angularjs/)
  * [AngularJS Examples](https://www.geeksforgeeks.org/angular-js/angularjs-examples/)






---

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
