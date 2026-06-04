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

### Profile

```sh
docker build -t example_build -f Dockerfile.build .
```
```text
Sending build context to Docker daemon  738.3kB
Step 1/7 : FROM maven:3.9.5-eclipse-temurin-11-alpine
 ---> 37ef041f8432
Step 2/7 : RUN apk add --no-cache nodejs npm
 ---> Running in b95eeab3274e
fetch https://dl-cdn.alpinelinux.org/alpine/v3.18/main/x86_64/APKINDEX.tar.gz
fetch https://dl-cdn.alpinelinux.org/alpine/v3.18/community/x86_64/APKINDEX.tar.gz
(1/7) Installing c-ares (1.19.1-r1)
(2/7) Installing libgcc (12.2.1_git20220924-r10)
(3/7) Installing icu-data-en (73.2-r2)
Executing icu-data-en-73.2-r2.post-install
*
* If you need ICU with non-English locales and legacy charset support, install
* package icu-data-full.
*
(4/7) Installing libstdc++ (12.2.1_git20220924-r10)
(5/7) Installing icu-libs (73.2-r2)
(6/7) Installing nodejs (18.20.1-r0)
(7/7) Installing npm (9.6.6-r0)
Executing busybox-1.36.1-r5.trigger
OK: 94 MiB in 56 packages
Removing intermediate container b95eeab3274e
 ---> 43886baaa5f9
Step 3/7 : WORKDIR /app
 ---> Running in 396508b3c916
Removing intermediate container 396508b3c916
 ---> 3b7935a06eba
Step 4/7 : COPY . /app
 ---> bd7f45adc722
Step 5/7 : RUN node --version
 ---> Running in c9fe445cb98a
v18.20.1
Removing intermediate container c9fe445cb98a
 ---> 20d4ab0ee57c
Step 6/7 : RUN npm --version
 ---> Running in d919df3cb533
9.6.6
Removing intermediate container d919df3cb533
 ---> 47aa9d060e05
Step 7/7 : RUN mvn -Preact clean package
 ---> Running in 470a2c9b3e3d
[INFO] Scanning for projects...
```
then
```text
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-parent/2.7.8/spring-boot-starter-parent-2.7.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-parent/2.7.8/spring-boot-starter-parent-2.7.8.pom (9.2 kB at 5.2 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-dependencies/2.7.8/spring-boot-dependencies-2.7.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-dependencies/2.7.8/spring-boot-dependencies-2.7.8.pom (110 kB at 413 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/datastax/oss/java-driver-bom/4.14.1/java-driver-bom-4.14.1.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/datastax/oss/java-driver-bom/4.14.1/java-driver-bom-4.14.1.pom (4.1 kB at 24 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/io/dropwizard/metrics/metrics-bom/4.2.15/metrics-bom-4.2.15.pom
Downloaded from central: https://repo.maven.apache.org/maven2/io/dropwizard/metrics/metrics-bom/4.2.15/metrics-bom-4.2.15.pom (6.9 kB at 19 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/io/dropwizard/metrics/metrics-parent/4.2.15/metrics-parent-4.2.15.pom
Downloaded from central: https://repo.maven.apache.org/maven2/io/dropwizard/metrics/metrics-parent/4.2.15/metrics-parent-4.2.15.pom (20 kB at 129 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/groovy/groovy-bom/3.0.14/groovy-bom-3.0.14.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/groovy/groovy-bom/3.0.14/groovy-bom-3.0.14.pom (26 kB at 128 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/infinispan/infinispan-bom/13.0.15.Final/infinispan-bom-13.0.15.Final.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/infinispan/infinispan-bom/13.0.15.Final/infinispan-bom-13.0.15.Final.pom (18 kB at 74 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/infinispan/infinispan-build-configuration-parent/13.0.15.Final/infinispan-build-configuration-parent-13.0.15.Final.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/infinispan/infinispan-build-configuration-parent/13.0.15.Final/infinispan-build-configuration-parent-13.0.15.Final.pom (16 kB at 104 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/jboss/jboss-parent/36/jboss-parent-36.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/jboss/jboss-parent/36/jboss-parent-36.pom (66 kB at 341 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-bom/2.13.4.20221013/jackson-bom-2.13.4.20221013.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-bom/2.13.4.20221013/jackson-bom-2.13.4.20221013.pom (17 kB at 116 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-parent/2.13/jackson-parent-2.13.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-parent/2.13/jackson-parent-2.13.pom (7.4 kB at 53 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/oss-parent/43/oss-parent-43.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/oss-parent/43/oss-parent-43.pom (24 kB at 91 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/glassfish/jersey/jersey-bom/2.35/jersey-bom-2.35.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/glassfish/jersey/jersey-bom/2.35/jersey-bom-2.35.pom (19 kB at 92 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/eclipse/ee4j/project/1.0.6/project-1.0.6.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/eclipse/ee4j/project/1.0.6/project-1.0.6.pom (13 kB at 97 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/eclipse/jetty/jetty-bom/9.4.50.v20221201/jetty-bom-9.4.50.v20221201.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/eclipse/jetty/jetty-bom/9.4.50.v20221201/jetty-bom-9.4.50.v20221201.pom (18 kB at 88 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/junit-bom/5.8.2/junit-bom-5.8.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/junit/junit-bom/5.8.2/junit-bom-5.8.2.pom (5.6 kB at 40 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/jetbrains/kotlin/kotlin-bom/1.6.21/kotlin-bom-1.6.21.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/jetbrains/kotlin/kotlin-bom/1.6.21/kotlin-bom-1.6.21.pom (9.3 kB at 51 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/jetbrains/kotlinx/kotlinx-coroutines-bom/1.6.4/kotlinx-coroutines-bom-1.6.4.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/jetbrains/kotlinx/kotlinx-coroutines-bom/1.6.4/kotlinx-coroutines-bom-1.6.4.pom (4.3 kB at 35 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j-bom/2.17.2/log4j-bom-2.17.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j-bom/2.17.2/log4j-bom-2.17.2.pom (8.1 kB at 41 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/logging/logging-parent/5/logging-parent-5.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/logging/logging-parent/5/logging-parent-5.pom (3.3 kB at 18 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/24/apache-24.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/apache/24/apache-24.pom (20 kB at 97 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/io/micrometer/micrometer-bom/1.9.7/micrometer-bom-1.9.7.pom
Downloaded from central: https://repo.maven.apache.org/maven2/io/micrometer/micrometer-bom/1.9.7/micrometer-bom-1.9.7.pom (7.1 kB at 52 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/mockito/mockito-bom/4.5.1/mockito-bom-4.5.1.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/mockito/mockito-bom/4.5.1/mockito-bom-4.5.1.pom (3.0 kB at 22 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-bom/4.1.87.Final/netty-bom-4.1.87.Final.pom
Downloaded from central: https://repo.maven.apache.org/maven2/io/netty/netty-bom/4.1.87.Final/netty-bom-4.1.87.Final.pom (13 kB at 99 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/oss/oss-parent/7/oss-parent-7.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/sonatype/oss/oss-parent/7/oss-parent-7.pom (4.8 kB at 32 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/squareup/okhttp3/okhttp-bom/4.9.3/okhttp-bom-4.9.3.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/squareup/okhttp3/okhttp-bom/4.9.3/okhttp-bom-4.9.3.pom (3.0 kB at 19 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/oracle/database/jdbc/ojdbc-bom/21.5.0.0/ojdbc-bom-21.5.0.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/oracle/database/jdbc/ojdbc-bom/21.5.0.0/ojdbc-bom-21.5.0.0.pom (13 kB at 69 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/io/prometheus/simpleclient_bom/0.15.0/simpleclient_bom-0.15.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/io/prometheus/simpleclient_bom/0.15.0/simpleclient_bom-0.15.0.pom (5.8 kB at 43 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/io/prometheus/parent/0.15.0/parent-0.15.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/io/prometheus/parent/0.15.0/parent-0.15.0.pom (12 kB at 93 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/querydsl/querydsl-bom/5.0.0/querydsl-bom-5.0.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/querydsl/querydsl-bom/5.0.0/querydsl-bom-5.0.0.pom (7.2 kB at 30 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/io/r2dbc/r2dbc-bom/Borca-SR2/r2dbc-bom-Borca-SR2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/io/r2dbc/r2dbc-bom/Borca-SR2/r2dbc-bom-Borca-SR2.pom (3.8 kB at 28 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/io/projectreactor/reactor-bom/2020.0.27/reactor-bom-2020.0.27.pom
Downloaded from central: https://repo.maven.apache.org/maven2/io/projectreactor/reactor-bom/2020.0.27/reactor-bom-2020.0.27.pom (4.6 kB at 22 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/io/rest-assured/rest-assured-bom/4.5.1/rest-assured-bom-4.5.1.pom
Downloaded from central: https://repo.maven.apache.org/maven2/io/rest-assured/rest-assured-bom/4.5.1/rest-assured-bom-4.5.1.pom (5.8 kB at 33 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/io/rsocket/rsocket-bom/1.1.3/rsocket-bom-1.1.3.pom
Downloaded from central: https://repo.maven.apache.org/maven2/io/rsocket/rsocket-bom/1.1.3/rsocket-bom-1.1.3.pom (2.6 kB at 21 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/data/spring-data-bom/2021.2.7/spring-data-bom-2021.2.7.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/data/spring-data-bom/2021.2.7/spring-data-bom-2021.2.7.pom (5.7 kB at 24 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-framework-bom/5.3.25/spring-framework-bom-5.3.25.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-framework-bom/5.3.25/spring-framework-bom-5.3.25.pom (5.7 kB at 45 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/integration/spring-integration-bom/5.5.16/spring-integration-bom-5.5.16.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/integration/spring-integration-bom/5.5.16/spring-integration-bom-5.5.16.pom (9.2 kB at 41 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/security/spring-security-bom/5.7.6/spring-security-bom-5.7.6.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/security/spring-security-bom/5.7.6/spring-security-bom-5.7.6.pom (5.7 kB at 30 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/session/spring-session-bom/2021.2.0/spring-session-bom-2021.2.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/session/spring-session-bom/2021.2.0/spring-session-bom-2021.2.0.pom (3.1 kB at 23 kB/s)
[INFO]
[INFO] ------------< example:uppy-react-multipart-upload-backend >-------------
[INFO] Building example:uppy-react-multipart-upload-backend 0.5.0-SNAPSHOT
[INFO]   from pom.xml
[INFO] --------------------------------[ jar ]---------------------------------
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-maven-plugin/2.7.8/spring-boot-maven-plugin-2.7.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-maven-plugin/2.7.8/spring-boot-maven-plugin-2.7.8.pom (3.0 kB at 15 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-maven-plugin/2.7.8/spring-boot-maven-plugin-2.7.8.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-maven-plugin/2.7.8/spring-boot-maven-plugin-2.7.8.jar (107 kB at 434 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/github/eirslett/frontend-maven-plugin/1.15.0/frontend-maven-plugin-1.15.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/github/eirslett/frontend-maven-plugin/1.15.0/frontend-maven-plugin-1.15.0.pom (4.9 kB at 23 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/github/eirslett/frontend-plugins/1.15.0/frontend-plugins-1.15.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/github/eirslett/frontend-plugins/1.15.0/frontend-plugins-1.15.0.pom (7.2 kB at 35 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/github/eirslett/frontend-maven-plugin/1.15.0/frontend-maven-plugin-1.15.0.jar
Downloaded from central: https://repo.maven.apache.org/maven2/com/github/eirslett/frontend-maven-plugin/1.15.0/frontend-maven-plugin-1.15.0.jar (46 kB at 242 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-clean-plugin/3.2.0/maven-clean-plugin-3.2.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-clean-plugin/3.2.0/maven-clean-plugin-3.2.0.pom (5.3 kB at 26 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-plugins/35/maven-plugins-35.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-plugins/35/maven-plugins-35.pom (9.9 kB at 85 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/35/maven-parent-35.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/35/maven-parent-35.pom (45 kB at 326 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/25/apache-25.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/apache/25/apache-25.pom (21 kB at 158 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-clean-plugin/3.2.0/maven-clean-plugin-3.2.0.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-clean-plugin/3.2.0/maven-clean-plugin-3.2.0.jar (36 kB at 255 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-resources-plugin/3.3.1/maven-resources-plugin-3.3.1.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-resources-plugin/3.3.1/maven-resources-plugin-3.3.1.pom (8.2 kB at 66 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-plugins/39/maven-plugins-39.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-plugins/39/maven-plugins-39.pom (8.1 kB at 63 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/39/maven-parent-39.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/39/maven-parent-39.pom (48 kB at 302 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/29/apache-29.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/apache/29/apache-29.pom (21 kB at 140 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-resources-plugin/3.3.1/maven-resources-plugin-3.3.1.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-resources-plugin/3.3.1/maven-resources-plugin-3.3.1.jar (31 kB at 135 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-compiler-plugin/3.10.1/maven-compiler-plugin-3.10.1.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-compiler-plugin/3.10.1/maven-compiler-plugin-3.10.1.pom (13 kB at 87 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-plugins/34/maven-plugins-34.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-plugins/34/maven-plugins-34.pom (11 kB at 89 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/34/maven-parent-34.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/34/maven-parent-34.pom (43 kB at 317 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/23/apache-23.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/apache/23/apache-23.pom (18 kB at 149 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-compiler-plugin/3.10.1/maven-compiler-plugin-3.10.1.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-compiler-plugin/3.10.1/maven-compiler-plugin-3.10.1.jar (62 kB at 445 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-surefire-plugin/2.22.2/maven-surefire-plugin-2.22.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-surefire-plugin/2.22.2/maven-surefire-plugin-2.22.2.pom (5.0 kB at 41 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire/2.22.2/surefire-2.22.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire/2.22.2/surefire-2.22.2.pom (26 kB at 178 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/33/maven-parent-33.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/33/maven-parent-33.pom (44 kB at 228 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/21/apache-21.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/apache/21/apache-21.pom (17 kB at 138 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-surefire-plugin/2.22.2/maven-surefire-plugin-2.22.2.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-surefire-plugin/2.22.2/maven-surefire-plugin-2.22.2.jar (41 kB at 268 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-jar-plugin/3.2.2/maven-jar-plugin-3.2.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-jar-plugin/3.2.2/maven-jar-plugin-3.2.2.pom (7.5 kB at 59 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-jar-plugin/3.2.2/maven-jar-plugin-3.2.2.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-jar-plugin/3.2.2/maven-jar-plugin-3.2.2.jar (29 kB at 230 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-thymeleaf/2.7.8/spring-boot-starter-thymeleaf-2.7.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-thymeleaf/2.7.8/spring-boot-starter-thymeleaf-2.7.8.pom (2.5 kB at 19 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter/2.7.8/spring-boot-starter-2.7.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter/2.7.8/spring-boot-starter-2.7.8.pom (3.0 kB at 25 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot/2.7.8/spring-boot-2.7.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot/2.7.8/spring-boot-2.7.8.pom (2.2 kB at 18 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-core/5.3.25/spring-core-5.3.25.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-core/5.3.25/spring-core-5.3.25.pom (2.0 kB at 17 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-jcl/5.3.25/spring-jcl-5.3.25.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-jcl/5.3.25/spring-jcl-5.3.25.pom (1.8 kB at 11 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-context/5.3.25/spring-context-5.3.25.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-context/5.3.25/spring-context-5.3.25.pom (2.6 kB at 20 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-aop/5.3.25/spring-aop-5.3.25.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-aop/5.3.25/spring-aop-5.3.25.pom (2.2 kB at 19 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-beans/5.3.25/spring-beans-5.3.25.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-beans/5.3.25/spring-beans-5.3.25.pom (2.0 kB at 17 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-expression/5.3.25/spring-expression-5.3.25.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-expression/5.3.25/spring-expression-5.3.25.pom (2.1 kB at 18 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-autoconfigure/2.7.8/spring-boot-autoconfigure-2.7.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-autoconfigure/2.7.8/spring-boot-autoconfigure-2.7.8.pom (2.1 kB at 8.5 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-logging/2.7.8/spring-boot-starter-logging-2.7.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-logging/2.7.8/spring-boot-starter-logging-2.7.8.pom (2.5 kB at 21 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-classic/1.2.11/logback-classic-1.2.11.pom
Downloaded from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-classic/1.2.11/logback-classic-1.2.11.pom (9.7 kB at 40 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-parent/1.2.11/logback-parent-1.2.11.pom
Downloaded from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-parent/1.2.11/logback-parent-1.2.11.pom (19 kB at 156 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-core/1.2.11/logback-core-1.2.11.pom
Downloaded from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-core/1.2.11/logback-core-1.2.11.pom (4.2 kB at 33 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.pom (2.7 kB at 20 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-parent/1.7.36/slf4j-parent-1.7.36.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-parent/1.7.36/slf4j-parent-1.7.36.pom (14 kB at 116 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j-to-slf4j/2.17.2/log4j-to-slf4j-2.17.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j-to-slf4j/2.17.2/log4j-to-slf4j-2.17.2.pom (7.3 kB at 50 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j/2.17.2/log4j-2.17.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j/2.17.2/log4j-2.17.2.pom (73 kB at 440 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j-api/2.17.2/log4j-api-2.17.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j-api/2.17.2/log4j-api-2.17.2.pom (14 kB at 58 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/jul-to-slf4j/1.7.36/jul-to-slf4j-1.7.36.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/slf4j/jul-to-slf4j/1.7.36/jul-to-slf4j-1.7.36.pom (991 B at 7.7 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/jakarta/annotation/jakarta.annotation-api/1.3.5/jakarta.annotation-api-1.3.5.pom
Downloaded from central: https://repo.maven.apache.org/maven2/jakarta/annotation/jakarta.annotation-api/1.3.5/jakarta.annotation-api-1.3.5.pom (16 kB at 80 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/jakarta/annotation/ca-parent/1.3.5/ca-parent-1.3.5.pom
Downloaded from central: https://repo.maven.apache.org/maven2/jakarta/annotation/ca-parent/1.3.5/ca-parent-1.3.5.pom (2.8 kB at 23 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/eclipse/ee4j/project/1.0.5/project-1.0.5.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/eclipse/ee4j/project/1.0.5/project-1.0.5.pom (13 kB at 78 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/yaml/snakeyaml/1.30/snakeyaml-1.30.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/yaml/snakeyaml/1.30/snakeyaml-1.30.pom (37 kB at 188 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/thymeleaf/thymeleaf-spring5/3.0.15.RELEASE/thymeleaf-spring5-3.0.15.RELEASE.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/thymeleaf/thymeleaf-spring5/3.0.15.RELEASE/thymeleaf-spring5-3.0.15.RELEASE.pom (14 kB at 85 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/thymeleaf/thymeleaf/3.0.15.RELEASE/thymeleaf-3.0.15.RELEASE.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/thymeleaf/thymeleaf/3.0.15.RELEASE/thymeleaf-3.0.15.RELEASE.pom (13 kB at 75 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/attoparser/attoparser/2.0.5.RELEASE/attoparser-2.0.5.RELEASE.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/attoparser/attoparser/2.0.5.RELEASE/attoparser-2.0.5.RELEASE.pom (10 kB at 54 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/unbescape/unbescape/1.1.6.RELEASE/unbescape-1.1.6.RELEASE.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/unbescape/unbescape/1.1.6.RELEASE/unbescape-1.1.6.RELEASE.pom (10.0 kB at 63 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/thymeleaf/extras/thymeleaf-extras-java8time/3.0.4.RELEASE/thymeleaf-extras-java8time-3.0.4.RELEASE.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/thymeleaf/extras/thymeleaf-extras-java8time/3.0.4.RELEASE/thymeleaf-extras-java8time-3.0.4.RELEASE.pom (14 kB at 118 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/ognl/ognl/3.1.26/ognl-3.1.26.pom
Downloaded from central: https://repo.maven.apache.org/maven2/ognl/ognl/3.1.26/ognl-3.1.26.pom (6.2 kB at 27 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/oss/oss-parent/9/oss-parent-9.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/sonatype/oss/oss-parent/9/oss-parent-9.pom (6.6 kB at 58 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/javassist/javassist/3.20.0-GA/javassist-3.20.0-GA.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/javassist/javassist/3.20.0-GA/javassist-3.20.0-GA.pom (9.8 kB at 58 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/commons-codec/commons-codec/1.13/commons-codec-1.13.pom
Downloaded from central: https://repo.maven.apache.org/maven2/commons-codec/commons-codec/1.13/commons-codec-1.13.pom (14 kB at 77 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/48/commons-parent-48.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/48/commons-parent-48.pom (72 kB at 232 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-web/2.7.8/spring-boot-starter-web-2.7.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-web/2.7.8/spring-boot-starter-web-2.7.8.pom (2.9 kB at 23 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-json/2.7.8/spring-boot-starter-json-2.7.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-json/2.7.8/spring-boot-starter-json-2.7.8.pom (3.1 kB at 25 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-web/5.3.25/spring-web-5.3.25.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-web/5.3.25/spring-web-5.3.25.pom (2.2 kB at 15 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.13.4.2/jackson-databind-2.13.4.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.13.4.2/jackson-databind-2.13.4.2.pom (17 kB at 110 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-base/2.13.4/jackson-base-2.13.4.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-base/2.13.4/jackson-base-2.13.4.pom (9.9 kB at 72 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-bom/2.13.4/jackson-bom-2.13.4.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-bom/2.13.4/jackson-bom-2.13.4.pom (17 kB at 142 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.13.4/jackson-annotations-2.13.4.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.13.4/jackson-annotations-2.13.4.pom (6.1 kB at 46 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-core/2.13.4/jackson-core-2.13.4.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-core/2.13.4/jackson-core-2.13.4.pom (5.5 kB at 43 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/datatype/jackson-datatype-jdk8/2.13.4/jackson-datatype-jdk8-2.13.4.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/datatype/jackson-datatype-jdk8/2.13.4/jackson-datatype-jdk8-2.13.4.pom (2.6 kB at 20 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/module/jackson-modules-java8/2.13.4/jackson-modules-java8-2.13.4.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/module/jackson-modules-java8/2.13.4/jackson-modules-java8-2.13.4.pom (3.2 kB at 27 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/datatype/jackson-datatype-jsr310/2.13.4/jackson-datatype-jsr310-2.13.4.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/datatype/jackson-datatype-jsr310/2.13.4/jackson-datatype-jsr310-2.13.4.pom (4.9 kB at 26 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/module/jackson-module-parameter-names/2.13.4/jackson-module-parameter-names-2.13.4.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/module/jackson-module-parameter-names/2.13.4/jackson-module-parameter-names-2.13.4.pom (4.4 kB at 32 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-tomcat/2.7.8/spring-boot-starter-tomcat-2.7.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-tomcat/2.7.8/spring-boot-starter-tomcat-2.7.8.pom (3.1 kB at 26 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-core/9.0.71/tomcat-embed-core-9.0.71.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-core/9.0.71/tomcat-embed-core-9.0.71.pom (1.7 kB at 14 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-el/9.0.71/tomcat-embed-el-9.0.71.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-el/9.0.71/tomcat-embed-el-9.0.71.pom (1.5 kB at 12 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-websocket/9.0.71/tomcat-embed-websocket-9.0.71.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-websocket/9.0.71/tomcat-embed-websocket-9.0.71.pom (1.7 kB at 14 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-webmvc/5.3.25/spring-webmvc-5.3.25.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-webmvc/5.3.25/spring-webmvc-5.3.25.pom (3.0 kB at 24 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-thymeleaf/2.7.8/spring-boot-starter-thymeleaf-2.7.8.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-thymeleaf/2.7.8/spring-boot-starter-thymeleaf-2.7.8.jar (4.8 kB at 44 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter/2.7.8/spring-boot-starter-2.7.8.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-logging/2.7.8/spring-boot-starter-logging-2.7.8.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-autoconfigure/2.7.8/spring-boot-autoconfigure-2.7.8.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot/2.7.8/spring-boot-2.7.8.jar
Downloading from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-classic/1.2.11/logback-classic-1.2.11.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-logging/2.7.8/spring-boot-starter-logging-2.7.8.jar (4.8 kB at 7.6 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-core/1.2.11/logback-core-1.2.11.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter/2.7.8/spring-boot-starter-2.7.8.jar (4.8 kB at 5.8 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j-to-slf4j/2.17.2/log4j-to-slf4j-2.17.2.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j-to-slf4j/2.17.2/log4j-to-slf4j-2.17.2.jar (18 kB at 14 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j-api/2.17.2/log4j-api-2.17.2.jar
Downloaded from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-classic/1.2.11/logback-classic-1.2.11.jar (232 kB at 172 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/jul-to-slf4j/1.7.36/jul-to-slf4j-1.7.36.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/slf4j/jul-to-slf4j/1.7.36/jul-to-slf4j-1.7.36.jar (4.5 kB at 3.1 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/jakarta/annotation/jakarta.annotation-api/1.3.5/jakarta.annotation-api-1.3.5.jar
Downloaded from central: https://repo.maven.apache.org/maven2/jakarta/annotation/jakarta.annotation-api/1.3.5/jakarta.annotation-api-1.3.5.jar (25 kB at 15 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-core/5.3.25/spring-core-5.3.25.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/logging/log4j/log4j-api/2.17.2/log4j-api-2.17.2.jar (303 kB at 161 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-jcl/5.3.25/spring-jcl-5.3.25.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-autoconfigure/2.7.8/spring-boot-autoconfigure-2.7.8.jar (1.7 MB at 824 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/yaml/snakeyaml/1.30/snakeyaml-1.30.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-jcl/5.3.25/spring-jcl-5.3.25.jar (24 kB at 12 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/thymeleaf/thymeleaf-spring5/3.0.15.RELEASE/thymeleaf-spring5-3.0.15.RELEASE.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/thymeleaf/thymeleaf-spring5/3.0.15.RELEASE/thymeleaf-spring5-3.0.15.RELEASE.jar (182 kB at 64 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/thymeleaf/thymeleaf/3.0.15.RELEASE/thymeleaf-3.0.15.RELEASE.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/yaml/snakeyaml/1.30/snakeyaml-1.30.jar (332 kB at 114 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/attoparser/attoparser/2.0.5.RELEASE/attoparser-2.0.5.RELEASE.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot/2.7.8/spring-boot-2.7.8.jar (1.5 MB at 488 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/unbescape/unbescape/1.1.6.RELEASE/unbescape-1.1.6.RELEASE.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/attoparser/attoparser/2.0.5.RELEASE/attoparser-2.0.5.RELEASE.jar (245 kB at 70 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/unbescape/unbescape/1.1.6.RELEASE/unbescape-1.1.6.RELEASE.jar (174 kB at 49 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/thymeleaf/extras/thymeleaf-extras-java8time/3.0.4.RELEASE/thymeleaf-extras-java8time-3.0.4.RELEASE.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar (41 kB at 11 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/commons-codec/commons-codec/1.13/commons-codec-1.13.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/thymeleaf/extras/thymeleaf-extras-java8time/3.0.4.RELEASE/thymeleaf-extras-java8time-3.0.4.RELEASE.jar (40 kB at 10 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-web/2.7.8/spring-boot-starter-web-2.7.8.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/thymeleaf/thymeleaf/3.0.15.RELEASE/thymeleaf-3.0.15.RELEASE.jar (871 kB at 208 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-json/2.7.8/spring-boot-starter-json-2.7.8.jar
Downloaded from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-core/1.2.11/logback-core-1.2.11.jar (449 kB at 107 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.13.4.2/jackson-databind-2.13.4.2.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-web/2.7.8/spring-boot-starter-web-2.7.8.jar (4.8 kB at 1.1 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.13.4/jackson-annotations-2.13.4.jar
Downloaded from central: https://repo.maven.apache.org/maven2/commons-codec/commons-codec/1.13/commons-codec-1.13.jar (344 kB at 79 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-core/2.13.4/jackson-core-2.13.4.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-core/5.3.25/spring-core-5.3.25.jar (1.5 MB at 315 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/datatype/jackson-datatype-jdk8/2.13.4/jackson-datatype-jdk8-2.13.4.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-json/2.7.8/spring-boot-starter-json-2.7.8.jar (4.7 kB at 978 B/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/datatype/jackson-datatype-jsr310/2.13.4/jackson-datatype-jsr310-2.13.4.jar
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/datatype/jackson-datatype-jdk8/2.13.4/jackson-datatype-jdk8-2.13.4.jar (35 kB at 7.2 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/module/jackson-module-parameter-names/2.13.4/jackson-module-parameter-names-2.13.4.jar
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/datatype/jackson-datatype-jsr310/2.13.4/jackson-datatype-jsr310-2.13.4.jar (121 kB at 23 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-tomcat/2.7.8/spring-boot-starter-tomcat-2.7.8.jar
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-core/2.13.4/jackson-core-2.13.4.jar (375 kB at 71 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-core/9.0.71/tomcat-embed-core-9.0.71.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-starter-tomcat/2.7.8/spring-boot-starter-tomcat-2.7.8.jar (4.8 kB at 882 B/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-el/9.0.71/tomcat-embed-el-9.0.71.jar
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/module/jackson-module-parameter-names/2.13.4/jackson-module-parameter-names-2.13.4.jar (9.5 kB at 1.7 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-websocket/9.0.71/tomcat-embed-websocket-9.0.71.jar
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.13.4.2/jackson-databind-2.13.4.2.jar (1.5 MB at 275 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-web/5.3.25/spring-web-5.3.25.jar
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.13.4/jackson-annotations-2.13.4.jar (76 kB at 13 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-beans/5.3.25/spring-beans-5.3.25.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-el/9.0.71/tomcat-embed-el-9.0.71.jar (256 kB at 40 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-webmvc/5.3.25/spring-webmvc-5.3.25.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-web/5.3.25/spring-web-5.3.25.jar (1.6 MB at 219 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-aop/5.3.25/spring-aop-5.3.25.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-aop/5.3.25/spring-aop-5.3.25.jar (383 kB at 47 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-context/5.3.25/spring-context-5.3.25.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-websocket/9.0.71/tomcat-embed-websocket-9.0.71.jar (279 kB at 34 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/springframework/spring-expression/5.3.25/spring-expression-5.3.25.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-beans/5.3.25/spring-beans-5.3.25.jar (703 kB at 85 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-webmvc/5.3.25/spring-webmvc-5.3.25.jar (1.0 MB at 109 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-context/5.3.25/spring-context-5.3.25.jar (1.3 MB at 130 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/springframework/spring-expression/5.3.25/spring-expression-5.3.25.jar (290 kB at 28 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/tomcat/embed/tomcat-embed-core/9.0.71/tomcat-embed-core-9.0.71.jar (3.4 MB at 323 kB/s)
[INFO]
[INFO] --- clean:3.2.0:clean (default-clean) @ uppy-react-multipart-upload-backend ---
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-utils/3.3.4/maven-shared-utils-3.3.4.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-utils/3.3.4/maven-shared-utils-3.3.4.pom (5.8 kB at 49 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-components/34/maven-shared-components-34.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-components/34/maven-shared-components-34.pom (5.1 kB at 44 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.6/commons-io-2.6.pom
Downloaded from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.6/commons-io-2.6.pom (14 kB at 78 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/42/commons-parent-42.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/42/commons-parent-42.pom (68 kB at 507 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/18/apache-18.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/apache/18/apache-18.pom (16 kB at 133 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-utils/3.3.4/maven-shared-utils-3.3.4.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-utils/3.3.4/maven-shared-utils-3.3.4.jar (153 kB at 870 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.6/commons-io-2.6.jar
Downloaded from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.6/commons-io-2.6.jar (215 kB at 1.1 MB/s)
[INFO]
[INFO] --- frontend:1.15.0:install-node-and-npm (install-node-and-npm) @ uppy-react-multipart-upload-backend ---
Downloading from central: https://repo.maven.apache.org/maven2/com/github/eirslett/frontend-plugin-core/1.15.0/frontend-plugin-core-1.15.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/github/eirslett/frontend-plugin-core/1.15.0/frontend-plugin-core-1.15.0.pom (3.2 kB at 27 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-core/2.13.0/jackson-core-2.13.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-core/2.13.0/jackson-core-2.13.0.pom (5.5 kB at 46 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-base/2.13.0/jackson-base-2.13.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-base/2.13.0/jackson-base-2.13.0.pom (9.7 kB at 42 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-bom/2.13.0/jackson-bom-2.13.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-bom/2.13.0/jackson-bom-2.13.0.pom (17 kB at 147 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-compress/1.21/commons-compress-1.21.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-compress/1.21/commons-compress-1.21.pom (20 kB at 149 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/52/commons-parent-52.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/52/commons-parent-52.pom (79 kB at 511 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.11.0/commons-io-2.11.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.11.0/commons-io-2.11.0.pom (20 kB at 155 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/junit-bom/5.7.2/junit-bom-5.7.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/junit/junit-bom/5.7.2/junit-bom-5.7.2.pom (5.1 kB at 40 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-exec/1.3/commons-exec-1.3.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-exec/1.3/commons-exec-1.3.pom (11 kB at 92 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/35/commons-parent-35.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/35/commons-parent-35.pom (58 kB at 385 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/15/apache-15.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/apache/15/apache-15.pom (15 kB at 129 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpclient/4.5.13/httpclient-4.5.13.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpclient/4.5.13/httpclient-4.5.13.pom (6.6 kB at 58 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcomponents-client/4.5.13/httpcomponents-client-4.5.13.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcomponents-client/4.5.13/httpcomponents-client-4.5.13.pom (16 kB at 124 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcomponents-parent/11/httpcomponents-parent-11.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcomponents-parent/11/httpcomponents-parent-11.pom (35 kB at 261 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcore/4.4.13/httpcore-4.4.13.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcore/4.4.13/httpcore-4.4.13.pom (5.0 kB at 40 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcomponents-core/4.4.13/httpcomponents-core-4.4.13.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcomponents-core/4.4.13/httpcomponents-core-4.4.13.pom (13 kB at 106 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/commons-logging/commons-logging/1.2/commons-logging-1.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/commons-logging/commons-logging/1.2/commons-logging-1.2.pom (19 kB at 154 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/34/commons-parent-34.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/34/commons-parent-34.pom (56 kB at 430 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/13/apache-13.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/apache/13/apache-13.pom (14 kB at 119 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/commons-codec/commons-codec/1.11/commons-codec-1.11.pom
Downloaded from central: https://repo.maven.apache.org/maven2/commons-codec/commons-codec/1.11/commons-codec-1.11.pom (14 kB at 106 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/4.0.0/plexus-utils-4.0.0.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/4.0.0/plexus-utils-4.0.0.pom (8.7 kB at 69 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/13/plexus-13.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/13/plexus-13.pom (27 kB at 230 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/junit-bom/5.9.3/junit-bom-5.9.3.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/junit/junit-bom/5.9.3/junit-bom-5.9.3.pom (5.6 kB at 50 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.5/slf4j-api-1.7.5.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.5/slf4j-api-1.7.5.pom (2.7 kB at 24 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-parent/1.7.5/slf4j-parent-1.7.5.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-parent/1.7.5/slf4j-parent-1.7.5.pom (12 kB at 110 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-build-api/0.0.7/plexus-build-api-0.0.7.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-build-api/0.0.7/plexus-build-api-0.0.7.pom (3.2 kB at 25 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/spice/spice-parent/15/spice-parent-15.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/sonatype/spice/spice-parent/15/spice-parent-15.pom (8.4 kB at 66 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/forge/forge-parent/5/forge-parent-5.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/sonatype/forge/forge-parent/5/forge-parent-5.pom (8.4 kB at 38 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/1.5.8/plexus-utils-1.5.8.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/1.5.8/plexus-utils-1.5.8.pom (8.1 kB at 45 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/2.0.2/plexus-2.0.2.pom
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/2.0.2/plexus-2.0.2.pom (12 kB at 99 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/github/eirslett/frontend-plugin-core/1.15.0/frontend-plugin-core-1.15.0.jar
Downloaded from central: https://repo.maven.apache.org/maven2/com/github/eirslett/frontend-plugin-core/1.15.0/frontend-plugin-core-1.15.0.jar (95 kB at 622 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-core/2.13.0/jackson-core-2.13.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-compress/1.21/commons-compress-1.21.jar
Downloading from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.11.0/commons-io-2.11.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-exec/1.3/commons-exec-1.3.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpclient/4.5.13/httpclient-4.5.13.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-exec/1.3/commons-exec-1.3.jar (54 kB at 316 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcore/4.4.13/httpcore-4.4.13.jar
Downloaded from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.11.0/commons-io-2.11.0.jar (327 kB at 399 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/commons-logging/commons-logging/1.2/commons-logging-1.2.jar
Downloaded from central: https://repo.maven.apache.org/maven2/commons-logging/commons-logging/1.2/commons-logging-1.2.jar (62 kB at 63 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/commons-codec/commons-codec/1.11/commons-codec-1.11.jar
Downloaded from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-core/2.13.0/jackson-core-2.13.0.jar (375 kB at 359 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/4.0.0/plexus-utils-4.0.0.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcore/4.4.13/httpcore-4.4.13.jar (329 kB at 297 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.5/slf4j-api-1.7.5.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.5/slf4j-api-1.7.5.jar (26 kB at 21 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-build-api/0.0.7/plexus-build-api-0.0.7.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-build-api/0.0.7/plexus-build-api-0.0.7.jar (8.5 kB at 6.2 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/4.0.0/plexus-utils-4.0.0.jar (192 kB at 131 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/commons-codec/commons-codec/1.11/commons-codec-1.11.jar (335 kB at 217 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-compress/1.21/commons-compress-1.21.jar (1.0 MB at 594 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpclient/4.5.13/httpclient-4.5.13.jar (780 kB at 244 kB/s)
[INFO] Installing node version v22.18.0
[INFO] Downloading https://unofficial-builds.nodejs.org/download/release/v22.18.0/node-v22.18.0-linux-x64-musl.tar.gz to /root/.m2/repository/com/github/eirslett/node/22.18.0/node-22.18.0-linux-x64-musl.tar.gz
[INFO] No proxies configured
[INFO] No proxy was configured, downloading directly
[INFO] Unpacking /root/.m2/repository/com/github/eirslett/node/22.18.0/node-22.18.0-linux-x64-musl.tar.gz into /app/frontend/node/tmp
[INFO] Copying node binary from /app/frontend/node/tmp/node-v22.18.0-linux-x64-musl/bin/node to /app/frontend/node/node
[INFO] Extracting NPM
[INFO] Installed node locally.
```
> NOTE: the node `v22.18.0` is installed by `frontend-maven-plugin` - the system version `v18.20.1` is ignored

then
```text
[INFO]
[INFO] --- frontend:1.15.0:npm (npm-install) @ uppy-react-multipart-upload-backend ---
[INFO] Running 'npm install' in /app/frontend
[INFO] Error relocating /app/frontend/node/node: _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE15_M_replace_coldEPcmPKcmm: symbol not found
[INFO] ------------------------------------------------------------------------
[INFO] BUILD FAILURE
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  03:20 min
[INFO] Finished at: 2026-06-04T14:55:20Z
[INFO] ------------------------------------------------------------------------
[ERROR] Failed to execute goal com.github.eirslett:frontend-maven-plugin:1.15.0:npm (npm-install) on project uppy-react-multipart-upload-backend: Failed to run task: 'npm install' failed. org.apache.commons.exec.ExecuteException: Process exited with an error: 127 (Exit value: 127) -> [Help 1]
[ERROR]

```

>NOTE: when the docker build is retried, all downloads will run again, making it quote time consuming

the longest is presumably the
```text
[INFO] Downloading https://unofficial-builds.nodejs.org/download/release/v22.18.0/node-v22.18.0-linux-x64-musl.tar.gz to /root/.m2/repository/com/github/eirslett/node/22.18.0/node-22.18.0-linux-x64-musl.tar.gz
[INFO] No proxies configured
[INFO] No proxy was configured, downloading directly
[INFO] Unpacking /root/.m2/repository/com/github/eirslett/node/22.18.0/node-22.18.0-linux-x64-musl.tar.gz into /app/frontend/node/tmp
[INFO] Copying node binary from /app/frontend/node/tmp/node-v22.18.0-linux-x64-musl/bin/node to /app/frontend/node/node
```

This is because `frontend-maven-plugin` is not using the system Node 18. It downloaded its own Node binary into `frontend/node/` and is attempting to execute it.

The downloaded binary appears incompatible with the __Alpine__ environment 


  * Alpine -> `musl` `libc`
  * Most Node binaries -> `glibc`

and the error often manifests as:

 * `symbol not found`
 * `Error relocating`
 * `ld-linux-x86-64.so.2` not found

or similar.


Therefore `frontend-maven-plugin` is probably the wrong plugin for the task

#### Try Debian Temurin Maven

```sh
docker pull maven:3.9.5-eclipse-temurin-11
```


```sh
docker run --rm maven:3.9.5-eclipse-temurin-11 cat /etc/os-release
```
```
PRETTY_NAME="Ubuntu 22.04.3 LTS"
NAME="Ubuntu"
VERSION_ID="22.04"
VERSION="22.04.3 LTS (Jammy Jellyfish)"
VERSION_CODENAME=jammy
ID=ubuntu
ID_LIKE=debian
HOME_URL="https://www.ubuntu.com/"
SUPPORT_URL="https://help.ubuntu.com/"
BUG_REPORT_URL="https://bugs.launchpad.net/ubuntu/"
PRIVACY_POLICY_URL="https://www.ubuntu.com/legal/terms-and-policies/privacy-policy"
UBUNTU_CODENAME=jammy

```


```
docker run --rm maven:3.9.5-eclipse-temurin-11 apt-key list
```
```
E: gnupg, gnupg2 and gnupg1 do not seem to be installed, but one of them is required for this operation
```
```sh
docker build -t uppy-react-build -f Dockerfile.build .
```
> NOTE: network prox issuses
```txt
Step 2/7 : RUN apt-get update && apt-get install -q -y nodejs npm
 ---> Running in 12a588ee67a3
Get:1 http://security.ubuntu.com/ubuntu jammy-security InRelease [129 kB]
Get:2 http://archive.ubuntu.com/ubuntu jammy InRelease [270 kB]
Err:1 http://security.ubuntu.com/ubuntu jammy-security InRelease
  The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
Get:3 http://archive.ubuntu.com/ubuntu jammy-updates InRelease [128 kB]
Err:2 http://archive.ubuntu.com/ubuntu jammy InRelease
  The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
Get:4 http://archive.ubuntu.com/ubuntu jammy-backports InRelease [127 kB]
Err:3 http://archive.ubuntu.com/ubuntu jammy-updates InRelease
  The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
Err:4 http://archive.ubuntu.com/ubuntu jammy-backports InRelease
  The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
Reading package lists...
```
```text
The command '/bin/sh -c apt-get update && apt-get install -q -y nodejs npm' returned a non-zero code: 100
```


```text

[INFO] Copying node binary from /app/frontend/node/tmp/node-v22.18.0-linux-x64/bin/node to /app/frontend/node/node
[INFO] Extracting NPM
[INFO] Installed node locally.
[INFO] 
[INFO] --- frontend:1.15.0:npm (npm-install) @ uppy-react-multipart-upload-backend ---
[INFO] Running 'npm install' in /app/frontend
[INFO] npm error code ERESOLVE
[INFO] npm error ERESOLVE unable to resolve dependency tree
[INFO] npm error
[INFO] npm error While resolving: uppy-react-upload@1.0.0
[INFO] npm error Found: @uppy/core@5.1.1
[INFO] npm error node_modules/@uppy/core
[INFO] npm error   @uppy/core@"5.1.1" from the root project
[INFO] npm error
[INFO] npm error Could not resolve dependency:
[INFO] npm error peer @uppy/core@"^5.2.0" from @uppy/dashboard@5.1.1
[INFO] npm error node_modules/@uppy/dashboard
[INFO] npm error   @uppy/dashboard@"5.1.1" from the root project
[INFO] npm error
[INFO] npm error Fix the upstream dependency conflict, or retry
[INFO] npm error this command with --force or --legacy-peer-deps
[INFO] npm error to accept an incorrect (and potentially broken) dependency resolution.
[INFO] npm error
[INFO] npm error
[INFO] npm error For a full report see:
[INFO] npm error /root/.npm/_logs/2026-06-04T16_22_26_410Z-eresolve-report.txt
[INFO] npm notice
[INFO] npm notice New major version of npm available! 10.9.3 -> 11.16.0
[INFO] npm notice Changelog: https://github.com/npm/cli/releases/tag/v11.16.0
[INFO] npm notice To update run: npm install -g npm@11.16.0
[INFO] npm notice
[INFO] npm error A complete log of this run can be found in: /root/.npm/_logs/2026-06-04T16_22_26_410Z-debug-0.log
[INFO] ------------------------------------------------------------------------
[INFO] BUILD FAILURE
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  01:15 min
[INFO] Finished at: 2026-06-04T16:22:27Z
[INFO] ------------------------------------------------------------------------
[ERROR] Failed to execute goal com.github.eirslett:frontend-maven-plugin:1.15.0:npm (npm-install) on project uppy-react-multipart-upload-backend: Failed to run task: 'npm install' failed. org.apache.commons.exec.ExecuteException: Process exited with an error: 1 (Exit value: 1) -> [Help 1]
[ERROR] 
```
```text
[INFO] --- frontend:1.15.0:npm (npm-install) @ uppy-react-multipart-upload-backend ---
[INFO] Running 'npm install' in /app/frontend
[INFO] npm error code ERESOLVE
[INFO] npm error ERESOLVE unable to resolve dependency tree
[INFO] npm error
[INFO] npm error While resolving: uppy-react-upload@1.0.0
[INFO] npm error Found: @uppy/core@5.1.1
[INFO] npm error node_modules/@uppy/core
[INFO] npm error   @uppy/core@"5.1.1" from the root project
[INFO] npm error
[INFO] npm error Could not resolve dependency:
[INFO] npm error peer @uppy/core@"^5.2.0" from @uppy/dashboard@5.1.1
[INFO] npm error node_modules/@uppy/dashboard
[INFO] npm error   @uppy/dashboard@"5.1.1" from the root project
[INFO] npm error
[INFO] npm error Fix the upstream dependency conflict, or retry
[INFO] npm error this command with --force or --legacy-peer-deps
[INFO] npm error to accept an incorrect (and potentially broken) dependency resolution.
[INFO] npm error
[INFO] npm error
[INFO] npm error For a full report see:
[INFO] npm error /root/.npm/_logs/2026-06-04T16_29_28_588Z-eresolve-report.txt
[INFO] npm notice
```
```sh
docker pull node:18-bookworm
```
```sh
docker build -t uppy-react-build2 -f Dockerfile.build2 .
```

```text
Step 1/8 : FROM node:18-bookworm
 ---> b50082bc3670
Step 2/8 : RUN apt-get update  && apt-cache search openjdk-11-jdk   && apt-get install -qqy maven openjdk-11-jdk
 ---> Running in 85bca5b004ed
Get:1 http://deb.debian.org/debian bookworm InRelease [151 kB]
Get:2 http://deb.debian.org/debian bookworm-updates InRelease [55.4 kB]
Get:3 http://deb.debian.org/debian-security bookworm-security InRelease [48.0 kB]
Get:4 http://deb.debian.org/debian bookworm/main amd64 Packages [8790 kB]
Get:5 http://deb.debian.org/debian bookworm-updates/main amd64 Packages [6924 B]
Get:6 http://deb.debian.org/debian-security bookworm-security/main amd64 Packages [306 kB]
Fetched 9358 kB in 7s (1377 kB/s)
Reading package lists...
E: Unable to locate package openjdk-11-jdk

```
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
