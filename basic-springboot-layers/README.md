
https://www.baeldung.com/docker-layers-spring-boot
### Usage 

```sh
docker image pull gcr.io/distroless/java11-debian11
docker image pull maven:3.9.3-eclipse-temurin-11-alpine 
docker pull adoptopenjdk/openjdk11:alpine-jrehttps://www.youtube.com/watch?v=96llGp-0pEQ
```
```sh
IMAGE=basic-layered 
docker build -t $IMAGE -f Dockerfile.jre .
```
```sh
docker run --rm -d --name $IMAGE -p 8085:8085 $IMAGE
```
```sh
docker logs $IMAGE
```
this will display
```text
  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v2.3.4.RELEASE)

2026-03-15 17:12:27.887  INFO 1 --- [           main] example.Application                      : Starting Application v0.1.0-SNAPSHOT on a162287bf091 with PID 1 (/app/BOOT-INF/classes started by root in /app)
2026-03-15 17:12:27.912  INFO 1 --- [           main] example.Application                      : No active profile set, falling back to default profiles: default
...
2026-03-15 17:12:39.067  INFO 1 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8085 (http) with context path ''
2026-03-15 17:12:39.183  INFO 1 --- [           main] example.Application                      : Started Application in 14.163 seconds (JVM running for 16.711)
```
confirm the service is operational
```sh
curl -sL -o /dev/null -w "%{http_code}"  http://192.168.99.100:8085/basic
```
```text
200
```
```sh
IMAGE=basic-layered 
docker build -t $IMAGE -f Dockerfile.distroless .
```
```sh
docker run --rm -d --name $IMAGE -p 8085:8085 $IMAGE
```
```sh
docker logs $IMAGE
```
### Troublshooting

```text
Error: Unable to access jarfile target/example.layered.jar
```

create a separate `Dockerfile.builder` and explore the file layout and fix `Dockerfile`/`pom.xml`variable misalignment:

```sh
docker build -t $IMAGE  -f Dockerfile.builder .
```
```text
...
Step 10/12 : RUN echo "==== BOOT-INF/layers.idx ===="  && unzip -p $JAR_FILE BOOT-INF/layers.idx  || echo "layers.idx not found"
 ---> Running in abce1229ca47
==== BOOT-INF/layers.idx ====
- "dependencies":
  - "BOOT-INF/lib/"
- "spring-boot-loader":
  - "org/"
- "snapshot-dependencies":
- "application":
  - "BOOT-INF/classes/"
  - "BOOT-INF/classpath.idx"
  - "BOOT-INF/layers.idx"
  - "META-INF/"
Removing intermediate container abce1229ca47
Step 11/12 : RUN java -Djarmode=layertools -jar $JAR_FILE extract || echo "something went wrong but we are ignoring it"
 ---> Running in 3437e6bccfb5
Error: Unable to access jarfile target/example.layered.jar
something went wrong but we are ignoring it
Removing intermediate container 3437e6bccfb5
 ---> 3ea39a0df749
Successfully built 3ea39a0df749
Successfully tagged basic-layered:latest
SECURITY WARNING: You are building a Docker image from Windows against a non-Windows Docker host. All files and directories added to build context will have '-rwxr-xr-x' permissions. It is recommended to double check and reset permissions for sensitive files and directories.
```
```sh
docker run -it $IMAGE sh
```

```sh
/app # pwd
```
```text
/app
```
```sh
/app # ls target/
```
```text
classes                     generated-sources

example.pivot.jar           maven-archiver
example.pivot.jar.original  maven-status
```
after this issue is fixed, may explore the effect of `layered`:


```sh
docker run -it --entrypoint="" $IMAGE tree application/ snapshot-dependencies/ dependencies/ spring-boot-loader
```
```text
/
application/
├── BOOT-INF
│   ├── classes
│   │   ├── application.properties
│   │   ├── example
│   │   │   ├── Application.class
│   │   │   └── controller
│   │   │       └── Controller.class
│   │   └── test.txt
│   ├── classpath.idx
│   └── layers.idx
└── META-INF
    ├── MANIFEST.MF
    └── maven
        └── example
            └── layered
                ├── pom.properties
                └── pom.xml
snapshot-dependencies/
dependencies/
└── BOOT-INF
    └── lib
        ├── jackson-annotations-2.11.2.jar
        ├── jackson-core-2.11.2.jar
        ├── jackson-databind-2.11.2.jar
        ├── jackson-datatype-jdk8-2.11.2.jar
        ├── jackson-datatype-jsr310-2.11.2.jar
        ├── jackson-module-parameter-names-2.11.2.jar
        ├── jakarta.annotation-api-1.3.5.jar
        ├── jakarta.el-3.0.3.jar
        ├── jul-to-slf4j-1.7.30.jar
        ├── log4j-api-2.13.3.jar
        ├── log4j-to-slf4j-2.13.3.jar
        ├── logback-classic-1.2.3.jar
        ├── logback-core-1.2.3.jar
        ├── slf4j-api-1.7.30.jar
        ├── snakeyaml-1.26.jar
        ├── spring-aop-5.2.9.RELEASE.jar
        ├── spring-beans-5.2.9.RELEASE.jar
        ├── spring-boot-2.3.4.RELEASE.jar
        ├── spring-boot-autoconfigure-2.3.4.RELEASE.jar
        ├── spring-boot-jarmode-layertools-2.3.4.RELEASE.jar
        ├── spring-boot-starter-2.3.4.RELEASE.jar
        ├── spring-boot-starter-json-2.3.4.RELEASE.jar
        ├── spring-boot-starter-logging-2.3.4.RELEASE.jar
        ├── spring-boot-starter-tomcat-2.3.4.RELEASE.jar
        ├── spring-boot-starter-web-2.3.4.RELEASE.jar
        ├── spring-context-5.2.9.RELEASE.jar
        ├── spring-core-5.2.9.RELEASE.jar
        ├── spring-expression-5.2.9.RELEASE.jar
        ├── spring-jcl-5.2.9.RELEASE.jar
        ├── spring-web-5.2.9.RELEASE.jar
        ├── spring-webmvc-5.2.9.RELEASE.jar
        ├── tomcat-embed-core-9.0.38.jar
        └── tomcat-embed-websocket-9.0.38.jar
spring-boot-loader/
└── org
    └── springframework
        └── boot
            └── loader
                ├── ClassPathIndexFile.class
                ├── ExecutableArchiveLauncher.class
                ├── JarLauncher.class
                ├── LaunchedURLClassLoader$DefinePackageCallType.class
                ├── LaunchedURLClassLoader$UseFastConnectionExceptionsEnumeration.class
                ├── LaunchedURLClassLoader.class
                ├── Launcher.class
                ├── MainMethodRunner.class
                ├── PropertiesLauncher$1.class
                ├── PropertiesLauncher$ArchiveEntryFilter.class
                ├── PropertiesLauncher$ClassPathArchives.class
                ├── PropertiesLauncher$PrefixMatchingArchiveFilter.class
                ├── PropertiesLauncher.class
                ├── WarLauncher.class
                ├── archive
                │   ├── Archive$Entry.class
                │   ├── Archive$EntryFilter.class
                │   ├── Archive.class
                │   ├── ExplodedArchive$AbstractIterator.class
                │   ├── ExplodedArchive$ArchiveIterator.class
                │   ├── ExplodedArchive$EntryIterator.class
                │   ├── ExplodedArchive$FileEntry.class
                │   ├── ExplodedArchive$SimpleJarFileArchive.class
                │   ├── ExplodedArchive.class
                │   ├── JarFileArchive$AbstractIterator.class
                │   ├── JarFileArchive$EntryIterator.class
                │   ├── JarFileArchive$JarFileEntry.class
                │   ├── JarFileArchive$NestedArchiveIterator.class
                │   └── JarFileArchive.class
                ├── data
                │   ├── RandomAccessData.class
                │   ├── RandomAccessDataFile$1.class
                │   ├── RandomAccessDataFile$DataInputStream.class
                │   ├── RandomAccessDataFile$FileAccess.class
                │   └── RandomAccessDataFile.class
                ├── jar
                │   ├── AbstractJarFile$JarFileType.class
                │   ├── AbstractJarFile.class
                │   ├── AsciiBytes.class
                │   ├── Bytes.class
                │   ├── CentralDirectoryEndRecord$1.class
                │   ├── CentralDirectoryEndRecord$Zip64End.class
                │   ├── CentralDirectoryEndRecord$Zip64Locator.class
                │   ├── CentralDirectoryEndRecord.class
                │   ├── CentralDirectoryFileHeader.class
                │   ├── CentralDirectoryParser.class
                │   ├── CentralDirectoryVisitor.class
                │   ├── FileHeader.class
                │   ├── Handler.class
                │   ├── JarEntry.class
                │   ├── JarEntryCertification.class
                │   ├── JarEntryFilter.class
                │   ├── JarFile$1.class
                │   ├── JarFile$JarEntryEnumeration.class
                │   ├── JarFile.class
                │   ├── JarFileEntries$1.class
                │   ├── JarFileEntries$EntryIterator.class
                │   ├── JarFileEntries.class
                │   ├── JarFileWrapper.class
                │   ├── JarURLConnection$1.class
                │   ├── JarURLConnection$JarEntryName.class
                │   ├── JarURLConnection.class
                │   ├── StringSequence.class
                │   └── ZipInflaterInputStream.class
                ├── jarmode
                │   ├── JarMode.class
                │   ├── JarModeLauncher.class
                │   └── TestJarMode.class
                └── util
                    └── SystemPropertyUtils.class

19 directories, 107 files

```
> NOTE the [Busybox](https://en.wikipedia.org/wiki/BusyBox)'s `tree` does not recognize `-d` option 

```sh
docker history --format "{{.ID}} {{.CreatedBy}} {{.Size}}" $IMAGE
```
>NOTE: naturally the history will vary with whehter it the first or repeated run
```text
c1b7e8c6ba7d /bin/sh -c #(nop)  ENTRYPOINT ["java" "org.s… 0B
e63c2816f6a6 /bin/sh -c #(nop) COPY dir:000c671065acdc9f7… 17.7kB
987299b8a0b4 /bin/sh -c #(nop) COPY dir:82860366a20fb1f4f… 0B
3a4dd51f8876 /bin/sh -c #(nop) COPY dir:34fffe734ed638d06… 241kB
acc8549a5445 /bin/sh -c #(nop) COPY dir:a06c3500a0e17c527… 16.4MB
477a9a73f5bc /bin/sh -c #(nop) WORKDIR /app 0B
42b1763c76e2  175MB

```
### See Also

  * [optimized docker images for Spring Boot applications with layering feature](https://redamessoudi.com/optimized-docker-images-for-spring-boot-apps/) and [repository](https://github.com/redamessoudi/optimized-docker-image-for-springboot/tree/optimized_dockerimage) - NOTE: git branches introduced for stages covered in the story 


---  
