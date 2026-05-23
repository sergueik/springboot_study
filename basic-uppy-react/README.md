### Info
https://github.com/judsonc/react-upload-uppy
https://uppy.io/
https://github.com/transloadit/uppy with over 10K commits
### Usage


```
  docker build -t example -f Dockerfile  .
```
```text
Sending build context to Docker daemon  20.72MB
Step 1/14 : FROM node:22.18.0-alpine AS react_builder
 ---> 8a3ae2e7d0c5
Step 2/14 : WORKDIR /app
 ---> Using cache
 ---> adabb6bdda8e
Step 3/14 : COPY frontend /app/
 ---> d7150baf1e9b
Step 4/14 : RUN cd /app   && rm -rf node_modules package-lock.json   && npm install     @uppy/core@5.2.0     @uppy/dashboard@5.1.1     @uppy/xhr-upload@5.2.0     @uppy/react@5.1.1
 ---> Running in f015b46bbd16

added 95 packages, and audited 96 packages in 47s

16 packages are looking for funding
  run `npm fund` for details

found 0 vulnerabilities
npm notice
npm notice New major version of npm available! 10.9.3 -> 11.15.0
npm notice Changelog: https://github.com/npm/cli/releases/tag/v11.15.0
npm notice To update run: npm install -g npm@11.15.0
npm notice
Removing intermediate container f015b46bbd16
 ---> b1d0baa5640a
Step 5/14 : RUN npm run build
 ---> Running in ccdb5c8885ad

> uppy-react-upload@1.0.0 build
> vite build

vite v7.3.3 building client environment for production...
transforming...
✓ 212 modules transformed.
rendering chunks...
computing gzip size...
dist/index.html                   0.31 kB │ gzip:   0.22 kB
dist/assets/index-D9e3sqm0.css   65.60 kB │ gzip:  10.45 kB
dist/assets/index-WmiSNnvV.js   377.90 kB │ gzip: 118.39 kB
✓ built in 9.39s
Removing intermediate container ccdb5c8885ad
 ---> 3262381c6300
Step 6/14 : FROM maven:3.9.5-eclipse-temurin-11-alpine as builder
 ---> 37ef041f8432
Step 7/14 : WORKDIR /app
 ---> Running in 12f364c6e2ab
Removing intermediate container 12f364c6e2ab
 ---> cb7dd4b470ab
Step 8/14 : COPY backend /app/
 ---> 43bfd8bc7824
Step 9/14 : COPY --from=react_builder /app/dist /app/src/main/resources/static/
 ---> ab9f4b0ae722
Step 10/14 : RUN cd /app && mvn package -DskipTests

```



```
  docker build -t example -f Dockerfile  .
```
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
Step 11/15 : RUN cd /app && mvn package -DskipTests
 ---> Running in bf520793b4a5
[INFO] Scanning for projects...
[INFO]
[INFO] ------------< example:uppy-react-multipart-upload-backend >-------------
[INFO] Building example:uppy-react-multipart-upload-backend 0.1.0-SNAPSHOT
[INFO]   from pom.xml
[INFO] --------------------------------[ jar ]---------------------------------
Downloading from atlassian-3rd-P: https://maven.atlassian.com/3rdparty/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar
Downloading from ossrh: https://oss.sonatype.org/content/repositories/snapshots/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar
Downloading from maven-central: https://mvnrepository.com/repos/central/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar
Downloading from osgeo: https://download.osgeo.org/webdav/geotools/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar
Downloading from seasar: https://www.seasar.org/maven/maven2/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar
Downloading from jcenter: https://jcenter.bintray.com/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar
Downloaded from jcenter: https://jcenter.bintray.com/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar (41 kB at 54 kB/s)
[INFO]
[INFO] --- resources:3.2.0:resources (default-resources) @ uppy-react-multipart-upload-backend ---
[INFO] Using 'UTF-8' encoding to copy filtered resources.
[INFO] Using 'UTF-8' encoding to copy filtered properties files.
[INFO] Copying 1 resource
[INFO] Copying 3 resources
[INFO]
[INFO] --- compiler:3.10.1:compile (default-compile) @ uppy-react-multipart-upload-backend ---
[INFO] Changes detected - recompiling the module!
[INFO] Compiling 5 source files to /app/target/classes
[INFO]
[INFO] --- resources:3.2.0:testResources (default-testResources) @ uppy-react-multipart-upload-backend ---
[INFO] Using 'UTF-8' encoding to copy filtered resources.
[INFO] Using 'UTF-8' encoding to copy filtered properties files.
[INFO] skip non existing resourceDirectory /app/src/test/resources
[INFO]
[INFO] --- compiler:3.10.1:testCompile (default-testCompile) @ uppy-react-multipart-upload-backend ---
[INFO] No sources to compile
[INFO]
[INFO] --- surefire:2.22.2:test (default-test) @ uppy-react-multipart-upload-backend ---
[INFO] Tests are skipped.
[INFO]
[INFO] --- jar:3.2.2:jar (default-jar) @ uppy-react-multipart-upload-backend ---
[INFO] Building jar: /app/target/uppy-react-multipart-upload-backend-0.1.0-SNAPSHOT.jar
[INFO]
[INFO] --- spring-boot:2.7.8:repackage (repackage) @ uppy-react-multipart-upload-backend ---
[WARNING]  Parameter 'finalName' is read-only, must not be used in configuration
[INFO] Replacing main artifact with repackaged archive
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  18.926 s
[INFO] Finished at: 2026-05-23T15:34:28Z
[INFO] ------------------------------------------------------------------------
Removing intermediate container bf520793b4a5
 ---> 3dd394c259e7
Step 12/15 : FROM eclipse-temurin:11-jre-alpine as run
 ---> 642de1708b20
Step 13/15 : COPY --from=builder /app/target/example.uppy-react-multipart-upload-backend.jar /app/app.jar
 ---> 77267e2fec25
Step 14/15 : ENTRYPOINT ["java", "-jar", "/app/app.jar"]
 ---> Running in 6791750bfdd7
Removing intermediate container 6791750bfdd7
 ---> f0605d87417e
Step 15/15 : EXPOSE 8080
 ---> Running in c80e26adccc4
Removing intermediate container c80e26adccc4
```
```
docker run -d -p 8080:8080 --name example example
```
```
81c8222aebe1386779c724b6bea4b11f20e1511e4454ca99fbd0b8983bd363e8

```

![execute](screenshots/capture-app.png)

The error to debug:
![execute](screenshots/capture-upload-error.png)
```text
[Uppy] [11:47:28] @uppy/xhr-upload expects a JSON response (with a `url` property). To parse non-JSON responses, use `getResponseData` to turn your response into JSON.
```
The application console log:
```text
2026-05-23 15:47:31.783  INFO 1 --- [nio-8080-exec-7] example.controller.FileUploadController  : upload 1 files: [test.txt]
2026-05-23 15:47:31.787  INFO 1 --- [nio-8080-exec-7] example.controller.FileUploadController  : upload file: test.txt
2026-05-23 15:47:31.788  INFO 1 --- [nio-8080-exec-7] example.service.FileStorageService       : UploadDir defined: /tmp/upload
2026-05-23 15:47:31.797  INFO 1 --- [nio-8080-exec-7] example.service.FileStorageService       : UploadDir: /tmp/upload
2026-05-23 15:47:31.803  INFO 1 --- [nio-8080-exec-7] example.controller.FileUploadController  : Listing: test.txt

```
and the file is present on container:
```sh
docker exec -it example ls /tmp/upload
```
```text
test.txt
```
```sh

docker exec -it example sha256sum /tmp/upload/test.txt
```
```text
6b4a40d5711a17f884838a652a8b398b3e7bcaacc94c6c5ee801b585b19ead3f  /tmp/upload/test.txt
```
```sh
sha256sum.exe  test.txt
```
```text
6b4a40d5711a17f884838a652a8b398b3e7bcaacc94c6c5ee801b585b19ead3f *test.txt
```
> NOTE: the "test.txt" is small - but ready to test file which woild warrant chunking


> NOTE: the app is highly version sensitive:

![execute](screenshots/capture-error.png)

### Background



React is very complex. Over the years.

creating the frontend from scratch with Vite React template is what is recmmended instead of manually assembling:

`package.json`
`Babel`
`webpack`
`React` bootstrap files
`ESLint`
`build config`


However this seems seriously feels backwards if you come from:

Java
Maven
C/C++
PowerShell
traditional build systems

where source layout is explicit and inspectable

