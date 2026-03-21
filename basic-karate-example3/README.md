### Usage
```sh
curl -sX POST -H "Content-Type: application/json" http://localhost:8085/token -d '{"username": "test", "password": "test"}' | jq '.'
```
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0ZXN0Iiwicm9sZSI6IlVTRVIiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwODUiLCJpYXQiOjE3NzQwNTk0NjMsImV4cCI6MTc3NDA2MzA2M30.ZJy6Pa4eU7MglhpIlV90iZVoKpOExwLn54s6Hlxl_I0",
  "expires_in": 3600,
  "token_type": "Bearer"
}

```

### Background 

What a JWT actually is

has 3 parts separated by dots: `header.payload.signature`

Header → algorithm info (Base64URL encoded JSON)

Payload → claims (Base64URL encoded JSON)

Signature → cryptographic signature (not human-readable)

🔍 How to decode (no libraries)

```sh
 curl -sX POST -H "Content-Type: application/json" http://localhost:8085/token -d '{"username": "test", "password": "test"}' |jq -rc '.access_token | split(".")|.[1]'  | base64 -d -  2> /dev/null | jq '.'

```
```json
{
  "sub": "test",
  "role": "USER",
  "iss": "http://localhost:8085",
  "iat": 1774059916,
  "exp": 1774063516
}

```

### Karate Run
#### Standalone
>NOTE Karate “fat JAR” is not something you pull as a single ready-made artifact from Maven Central in the sense of a self-contained executable for 1.4.x.

NOTE> What is available in Maven Central are the individual Karate modules, and the “fat jar” is typically something you assemble yourself via Maven (Shade / Assembly plugin).

```sh
mvn package -ntp -B
```

```text
[INFO] Scanning for projects...
[INFO]
[INFO] -------------------------< com.intuit:karate >--------------------------
[INFO] Building com.intuit:karate 0.1.0-SNAPSHOT
[INFO]   from pom.xml
[INFO] --------------------------------[ jar ]---------------------------------
[INFO]
[INFO] --- maven-resources-plugin:2.6:resources (default-resources) @ karate ---
[WARNING] Using platform encoding (Cp1252 actually) to copy filtered resources, i.e. build is platform dependent!
[INFO] skip non existing resourceDirectory C:\developer\sergueik\springboot_study\basic-karate-example3\src\main\resources
[INFO]
[INFO] --- maven-compiler-plugin:3.1:compile (default-compile) @ karate ---
[INFO] No sources to compile
[INFO]
[INFO] --- maven-resources-plugin:2.6:testResources (default-testResources) @ karate ---
[WARNING] Using platform encoding (Cp1252 actually) to copy filtered resources, i.e. build is platform dependent!
[INFO] skip non existing resourceDirectory C:\developer\sergueik\springboot_study\basic-karate-example3\src\test\resources
[INFO]
[INFO] --- maven-compiler-plugin:3.1:testCompile (default-testCompile) @ karate ---
[INFO] No sources to compile
[INFO]
[INFO] --- maven-surefire-plugin:2.12.4:test (default-test) @ karate ---
[INFO] No tests to run.
[INFO]
[INFO] --- maven-jar-plugin:2.4:jar (default-jar) @ karate ---
[WARNING] JAR will be empty - no content was marked for inclusion!
[INFO]
[INFO] --- maven-shade-plugin:3.5.0:shade (default) @ karate ---
[INFO] Including com.intuit.karate:karate-core:jar:1.4.1 in the shaded jar.
[INFO] Including org.graalvm.js:js-scriptengine:jar:22.3.3 in the shaded jar.
[INFO] Including org.graalvm.sdk:graal-sdk:jar:22.3.3 in the shaded jar.
[INFO] Including org.graalvm.js:js:jar:22.3.3 in the shaded jar.
[INFO] Including org.graalvm.regex:regex:jar:22.3.3 in the shaded jar.
[INFO] Including org.graalvm.truffle:truffle-api:jar:22.3.3 in the shaded jar.
[INFO] Including com.ibm.icu:icu4j:jar:71.1 in the shaded jar.
[INFO] Including org.thymeleaf:thymeleaf:jar:3.1.2.RELEASE in the shaded jar.
[INFO] Including ognl:ognl:jar:3.3.4 in the shaded jar.
[INFO] Including org.javassist:javassist:jar:3.29.0-GA in the shaded jar.
[INFO] Including org.attoparser:attoparser:jar:2.0.7.RELEASE in the shaded jar.
[INFO] Including org.unbescape:unbescape:jar:1.1.6.RELEASE in the shaded jar.
[INFO] Including org.slf4j:slf4j-api:jar:2.0.7 in the shaded jar.
[INFO] Including com.linecorp.armeria:armeria:jar:1.25.2 in the shaded jar.
[INFO] Including com.fasterxml.jackson.core:jackson-core:jar:2.15.2 in the shaded jar.
[INFO] Including com.fasterxml.jackson.core:jackson-annotations:jar:2.15.2 in the shaded jar.
[INFO] Including com.fasterxml.jackson.core:jackson-databind:jar:2.15.2 in the shaded jar.
[INFO] Including com.fasterxml.jackson.datatype:jackson-datatype-jdk8:jar:2.15.2 in the shaded jar.
[INFO] Including com.fasterxml.jackson.datatype:jackson-datatype-jsr310:jar:2.15.2 in the shaded jar.
[INFO] Including io.micrometer:micrometer-core:jar:1.11.3 in the shaded jar.
[INFO] Including io.micrometer:micrometer-commons:jar:1.11.3 in the shaded jar.
[INFO] Including io.micrometer:micrometer-observation:jar:1.11.3 in the shaded jar.
[INFO] Including org.hdrhistogram:HdrHistogram:jar:2.1.12 in the shaded jar.
[INFO] Including org.latencyutils:LatencyUtils:jar:2.0.3 in the shaded jar.
[INFO] Including io.netty:netty-transport:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-common:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-buffer:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-resolver:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-codec-haproxy:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-codec:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-codec-http2:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-handler:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-codec-http:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-resolver-dns:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-codec-dns:jar:4.1.96.Final in the shaded jar.
[INFO] Including org.reactivestreams:reactive-streams:jar:1.0.4 in the shaded jar.
[INFO] Including com.google.code.findbugs:jsr305:jar:3.0.2 in the shaded jar.
[INFO] Including io.netty:netty-handler-proxy:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-codec-socks:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-transport-native-unix-common:jar:linux-x86_64:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-transport-native-epoll:jar:linux-x86_64:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-transport-native-unix-common:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-transport-classes-epoll:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-transport-native-unix-common:jar:linux-aarch_64:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-transport-native-epoll:jar:linux-aarch_64:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-transport-native-unix-common:jar:osx-x86_64:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-transport-native-kqueue:jar:osx-x86_64:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-transport-classes-kqueue:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-resolver-dns-native-macos:jar:osx-x86_64:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-resolver-dns-classes-macos:jar:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-transport-native-unix-common:jar:osx-aarch_64:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-transport-native-kqueue:jar:osx-aarch_64:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-resolver-dns-native-macos:jar:osx-aarch_64:4.1.96.Final in the shaded jar.
[INFO] Including io.netty:netty-tcnative-boringssl-static:jar:linux-x86_64:2.0.61.Final in the shaded jar.
[INFO] Including io.netty:netty-tcnative-classes:jar:2.0.61.Final in the shaded jar.
[INFO] Including io.netty:netty-tcnative-boringssl-static:jar:linux-aarch_64:2.0.61.Final in the shaded jar.
[INFO] Including io.netty:netty-tcnative-boringssl-static:jar:osx-x86_64:2.0.61.Final in the shaded jar.
[INFO] Including io.netty:netty-tcnative-boringssl-static:jar:osx-aarch_64:2.0.61.Final in the shaded jar.
[INFO] Including io.netty:netty-tcnative-boringssl-static:jar:windows-x86_64:2.0.61.Final in the shaded jar.
[INFO] Including com.aayushatharva.brotli4j:brotli4j:jar:1.12.0 in the shaded jar.
[INFO] Including com.aayushatharva.brotli4j:service:jar:1.12.0 in the shaded jar.
[INFO] Including com.aayushatharva.brotli4j:native-windows-x86_64:jar:1.12.0 in the shaded jar.
[INFO] Including org.apache.httpcomponents:httpclient:jar:4.5.14 in the shaded jar.
[INFO] Including org.apache.httpcomponents:httpcore:jar:4.4.16 in the shaded jar.
[INFO] Including commons-codec:commons-codec:jar:1.16.0 in the shaded jar.
[INFO] Including ch.qos.logback:logback-classic:jar:1.4.11 in the shaded jar.
[INFO] Including ch.qos.logback:logback-core:jar:1.4.11 in the shaded jar.
[INFO] Including org.slf4j:jcl-over-slf4j:jar:2.0.9 in the shaded jar.
[INFO] Including org.antlr:antlr4-runtime:jar:4.11.1 in the shaded jar.
[INFO] Including com.jayway.jsonpath:json-path:jar:2.8.0 in the shaded jar.
[INFO] Including net.minidev:json-smart:jar:2.4.10 in the shaded jar.
[INFO] Including net.minidev:accessors-smart:jar:2.4.9 in the shaded jar.
[INFO] Including org.ow2.asm:asm:jar:9.3 in the shaded jar.
[INFO] Including org.yaml:snakeyaml:jar:2.0 in the shaded jar.
[INFO] Including de.siegmar:fastcsv:jar:2.2.1 in the shaded jar.
[INFO] Including info.picocli:picocli:jar:4.7.1 in the shaded jar.
[INFO] Including io.github.classgraph:classgraph:jar:4.8.160 in the shaded jar.
[INFO] Including io.github.t12y:resemble:jar:1.0.2 in the shaded jar.
[INFO] Including io.github.t12y:ssim:jar:1.0.0 in the shaded jar.
[WARNING] Discovered module-info.class. Shading will break its strong encapsulation.
[WARNING] Discovered module-info.class. Shading will break its strong encapsulation.
[WARNING] Discovered module-info.class. Shading will break its strong encapsulation.
[WARNING] Discovered module-info.class. Shading will break its strong encapsulation.
[WARNING] Discovered module-info.class. Shading will break its strong encapsulation.
[WARNING] Discovered module-info.class. Shading will break its strong encapsulation.
[WARNING] Discovered module-info.class. Shading will break its strong encapsulation.
[WARNING] Discovered module-info.class. Shading will break its strong encapsulation.
[WARNING] Discovered module-info.class. Shading will break its strong encapsulation.
[WARNING] netty-transport-native-unix-common-4.1.96.Final-linux-aarch_64.jar, netty-transport-native-unix-common-4.1.96.Final-linux-x86_64.jar, netty-transport-native-unix-common-4.1.96.Final-osx-aarch_64.jar, netty-transport-native-unix-common-4.1.96.Final-osx-x86_64.jar, netty-transport-native-unix-common-4.1.96.Final.jar define 34 overlapping classes and resources:
[WARNING]   - META-INF/maven/io.netty/netty-transport-native-unix-common/pom.properties
[WARNING]   - META-INF/maven/io.netty/netty-transport-native-unix-common/pom.xml
[WARNING]   - io.netty.channel.unix.Buffer
[WARNING]   - io.netty.channel.unix.DatagramSocketAddress
[WARNING]   - io.netty.channel.unix.DomainDatagramChannel
[WARNING]   - io.netty.channel.unix.DomainDatagramChannelConfig
[WARNING]   - io.netty.channel.unix.DomainDatagramPacket
[WARNING]   - io.netty.channel.unix.DomainDatagramSocketAddress
[WARNING]   - io.netty.channel.unix.DomainSocketAddress
[WARNING]   - io.netty.channel.unix.DomainSocketChannel
[WARNING]   - 24 more...
[WARNING] netty-tcnative-boringssl-static-2.0.61.Final-linux-aarch_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-linux-x86_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-osx-aarch_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-osx-x86_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-windows-x86_64.jar define 6 overlapping resources:
[WARNING]   - META-INF/license/LICENSE.aix-netbsd.txt
[WARNING]   - META-INF/license/LICENSE.boringssl.txt
[WARNING]   - META-INF/license/LICENSE.mvn-wrapper.txt
[WARNING]   - META-INF/license/LICENSE.tomcat-native.txt
[WARNING]   - META-INF/maven/io.netty/netty-tcnative-boringssl-static/pom.properties
[WARNING]   - META-INF/maven/io.netty/netty-tcnative-boringssl-static/pom.xml
[WARNING] armeria-1.25.2.jar, brotli4j-1.12.0.jar, classgraph-4.8.160.jar, jackson-core-2.15.2.jar, jackson-databind-2.15.2.jar, jackson-datatype-jdk8-2.15.2.jar, jackson-datatype-jsr310-2.15.2.jar, jcl-over-slf4j-2.0.9.jar, native-windows-x86_64-1.12.0.jar, picocli-4.7.1.jar, service-1.12.0.jar, slf4j-api-2.0.7.jar, snakeyaml-2.0.jar define 1 overlapping classes:
[WARNING]   - META-INF.versions.9.module-info
[WARNING] attoparser-2.0.7.RELEASE.jar, commons-codec-1.16.0.jar, netty-tcnative-boringssl-static-2.0.61.Final-linux-aarch_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-linux-x86_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-osx-aarch_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-osx-x86_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-windows-x86_64.jar, unbescape-1.1.6.RELEASE.jar define 1 overlapping resource:
[WARNING]   - META-INF/NOTICE.txt
[WARNING] jackson-datatype-jdk8-2.15.2.jar, jackson-datatype-jsr310-2.15.2.jar define 1 overlapping resource:
[WARNING]   - META-INF/services/com.fasterxml.jackson.databind.Module
[WARNING] HdrHistogram-2.1.12.jar, LatencyUtils-2.0.3.jar, accessors-smart-2.4.9.jar, antlr4-runtime-4.11.1.jar, armeria-1.25.2.jar, asm-9.3.jar, attoparser-2.0.7.RELEASE.jar, brotli4j-1.12.0.jar, classgraph-4.8.160.jar, commons-codec-1.16.0.jar, fastcsv-2.2.1.jar, httpclient-4.5.14.jar, httpcore-4.4.16.jar, icu4j-71.1.jar, jackson-annotations-2.15.2.jar, jackson-core-2.15.2.jar, jackson-databind-2.15.2.jar, jackson-datatype-jdk8-2.15.2.jar, jackson-datatype-jsr310-2.15.2.jar, javassist-3.29.0-GA.jar, jcl-over-slf4j-2.0.9.jar, json-path-2.8.0.jar, json-smart-2.4.10.jar, jsr305-3.0.2.jar, karate-0.1.0-SNAPSHOT.jar, karate-core-1.4.1.jar, logback-classic-1.4.11.jar, logback-core-1.4.11.jar, micrometer-commons-1.11.3.jar, micrometer-core-1.11.3.jar, micrometer-observation-1.11.3.jar, native-windows-x86_64-1.12.0.jar, netty-buffer-4.1.96.Final.jar, netty-codec-4.1.96.Final.jar, netty-codec-dns-4.1.96.Final.jar, netty-codec-haproxy-4.1.96.Final.jar, netty-codec-http-4.1.96.Final.jar, netty-codec-http2-4.1.96.Final.jar, netty-codec-socks-4.1.96.Final.jar, netty-common-4.1.96.Final.jar, netty-handler-4.1.96.Final.jar, netty-handler-proxy-4.1.96.Final.jar, netty-resolver-4.1.96.Final.jar, netty-resolver-dns-4.1.96.Final.jar, netty-resolver-dns-classes-macos-4.1.96.Final.jar, netty-resolver-dns-native-macos-4.1.96.Final-osx-aarch_64.jar, netty-resolver-dns-native-macos-4.1.96.Final-osx-x86_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-linux-aarch_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-linux-x86_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-osx-aarch_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-osx-x86_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-windows-x86_64.jar, netty-tcnative-classes-2.0.61.Final.jar, netty-transport-4.1.96.Final.jar, netty-transport-classes-epoll-4.1.96.Final.jar, netty-transport-classes-kqueue-4.1.96.Final.jar, netty-transport-native-epoll-4.1.96.Final-linux-aarch_64.jar, netty-transport-native-epoll-4.1.96.Final-linux-x86_64.jar, netty-transport-native-kqueue-4.1.96.Final-osx-aarch_64.jar, netty-transport-native-kqueue-4.1.96.Final-osx-x86_64.jar, netty-transport-native-unix-common-4.1.96.Final-linux-aarch_64.jar, netty-transport-native-unix-common-4.1.96.Final-linux-x86_64.jar, netty-transport-native-unix-common-4.1.96.Final-osx-aarch_64.jar, netty-transport-native-unix-common-4.1.96.Final-osx-x86_64.jar, netty-transport-native-unix-common-4.1.96.Final.jar, ognl-3.3.4.jar, picocli-4.7.1.jar, reactive-streams-1.0.4.jar, resemble-1.0.2.jar, service-1.12.0.jar, slf4j-api-2.0.7.jar, snakeyaml-2.0.jar, ssim-1.0.0.jar, thymeleaf-3.1.2.RELEASE.jar, truffle-api-22.3.3.jar, unbescape-1.1.6.RELEASE.jar define 1 overlapping resource:
[WARNING]   - META-INF/MANIFEST.MF
[WARNING] netty-resolver-dns-native-macos-4.1.96.Final-osx-aarch_64.jar, netty-resolver-dns-native-macos-4.1.96.Final-osx-x86_64.jar define 2 overlapping resources:
[WARNING]   - META-INF/maven/io.netty/netty-resolver-dns-native-macos/pom.properties
[WARNING]   - META-INF/maven/io.netty/netty-resolver-dns-native-macos/pom.xml
[WARNING] armeria-1.25.2.jar, netty-common-4.1.96.Final.jar define 1 overlapping resource:
[WARNING]   - META-INF/services/reactor.blockhound.integration.BlockHoundIntegration
[WARNING] armeria-1.25.2.jar, httpclient-4.5.14.jar, httpcore-4.4.16.jar, jackson-annotations-2.15.2.jar, jackson-core-2.15.2.jar, jackson-databind-2.15.2.jar, jackson-datatype-jdk8-2.15.2.jar, jackson-datatype-jsr310-2.15.2.jar, micrometer-commons-1.11.3.jar, micrometer-core-1.11.3.jar, micrometer-observation-1.11.3.jar define 1 overlapping resource:
[WARNING]   - META-INF/LICENSE
[WARNING] netty-transport-native-epoll-4.1.96.Final-linux-aarch_64.jar, netty-transport-native-epoll-4.1.96.Final-linux-x86_64.jar define 2 overlapping resources:
[WARNING]   - META-INF/maven/io.netty/netty-transport-native-epoll/pom.properties
[WARNING]   - META-INF/maven/io.netty/netty-transport-native-epoll/pom.xml
[WARNING] netty-transport-native-kqueue-4.1.96.Final-osx-aarch_64.jar, netty-transport-native-kqueue-4.1.96.Final-osx-x86_64.jar define 2 overlapping resources:
[WARNING]   - META-INF/maven/io.netty/netty-transport-native-kqueue/pom.properties
[WARNING]   - META-INF/maven/io.netty/netty-transport-native-kqueue/pom.xml
[WARNING] attoparser-2.0.7.RELEASE.jar, commons-codec-1.16.0.jar, jcl-over-slf4j-2.0.9.jar, netty-tcnative-boringssl-static-2.0.61.Final-linux-aarch_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-linux-x86_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-osx-aarch_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-osx-x86_64.jar, netty-tcnative-boringssl-static-2.0.61.Final-windows-x86_64.jar, slf4j-api-2.0.7.jar, unbescape-1.1.6.RELEASE.jar define 1 overlapping resource:
[WARNING]   - META-INF/LICENSE.txt
[WARNING] netty-buffer-4.1.96.Final.jar, netty-codec-4.1.96.Final.jar, netty-codec-dns-4.1.96.Final.jar, netty-codec-haproxy-4.1.96.Final.jar, netty-codec-http-4.1.96.Final.jar, netty-codec-http2-4.1.96.Final.jar, netty-codec-socks-4.1.96.Final.jar, netty-common-4.1.96.Final.jar, netty-handler-4.1.96.Final.jar, netty-handler-proxy-4.1.96.Final.jar, netty-resolver-4.1.96.Final.jar, netty-resolver-dns-4.1.96.Final.jar, netty-resolver-dns-classes-macos-4.1.96.Final.jar, netty-resolver-dns-native-macos-4.1.96.Final-osx-aarch_64.jar, netty-resolver-dns-native-macos-4.1.96.Final-osx-x86_64.jar, netty-transport-4.1.96.Final.jar, netty-transport-classes-epoll-4.1.96.Final.jar, netty-transport-classes-kqueue-4.1.96.Final.jar, netty-transport-native-epoll-4.1.96.Final-linux-aarch_64.jar, netty-transport-native-epoll-4.1.96.Final-linux-x86_64.jar, netty-transport-native-kqueue-4.1.96.Final-osx-aarch_64.jar, netty-transport-native-kqueue-4.1.96.Final-osx-x86_64.jar, netty-transport-native-unix-common-4.1.96.Final-linux-aarch_64.jar, netty-transport-native-unix-common-4.1.96.Final-linux-x86_64.jar, netty-transport-native-unix-common-4.1.96.Final-osx-aarch_64.jar, netty-transport-native-unix-common-4.1.96.Final-osx-x86_64.jar, netty-transport-native-unix-common-4.1.96.Final.jar define 1 overlapping resource:
[WARNING]   - META-INF/io.netty.versions.properties
[WARNING] netty-transport-native-unix-common-4.1.96.Final-linux-aarch_64.jar, netty-transport-native-unix-common-4.1.96.Final-linux-x86_64.jar, netty-transport-native-unix-common-4.1.96.Final-osx-aarch_64.jar, netty-transport-native-unix-common-4.1.96.Final-osx-x86_64.jar define 10 overlapping resources:
[WARNING]   - META-INF/native/include/netty_jni_util.h
[WARNING]   - META-INF/native/include/netty_unix.h
[WARNING]   - META-INF/native/include/netty_unix_buffer.h
[WARNING]   - META-INF/native/include/netty_unix_errors.h
[WARNING]   - META-INF/native/include/netty_unix_filedescriptor.h
[WARNING]   - META-INF/native/include/netty_unix_jni.h
[WARNING]   - META-INF/native/include/netty_unix_limits.h
[WARNING]   - META-INF/native/include/netty_unix_socket.h
[WARNING]   - META-INF/native/include/netty_unix_util.h
[WARNING]   - META-INF/native/lib/libnetty-unix-common.a
[WARNING] js-22.3.3.jar, regex-22.3.3.jar define 1 overlapping resource:
[WARNING]   - META-INF/services/com.oracle.truffle.api.TruffleLanguage$Provider
[WARNING] httpclient-4.5.14.jar, httpcore-4.4.16.jar define 1 overlapping resource:
[WARNING]   - META-INF/DEPENDENCIES
[WARNING] httpclient-4.5.14.jar, httpcore-4.4.16.jar, jackson-annotations-2.15.2.jar, jackson-core-2.15.2.jar, jackson-databind-2.15.2.jar, jackson-datatype-jdk8-2.15.2.jar, jackson-datatype-jsr310-2.15.2.jar, micrometer-commons-1.11.3.jar, micrometer-core-1.11.3.jar, micrometer-observation-1.11.3.jar define 1 overlapping resource:
[WARNING]   - META-INF/NOTICE
[WARNING] maven-shade-plugin has detected that some files are
[WARNING] present in two or more JARs. When this happens, only one
[WARNING] single version of the file is copied to the uber jar.
[WARNING] Usually this is not harmful and you can skip these warnings,
[WARNING] otherwise try to manually exclude artifacts based on
[WARNING] mvn dependency:tree -Ddetail=true and the above output.
[WARNING] See https://maven.apache.org/plugins/maven-shade-plugin/
[INFO] Replacing C:\developer\sergueik\springboot_study\basic-karate-example3\target\karate-tests.jar.jar with C:\developer\sergueik\springboot_study\basic-karate-example3\target\karate-0.1.0-SNAPSHOT-shaded.jar
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  6.361 s
[INFO] Finished at: 2026-03-20T23:10:23-04:00
[INFO] ------------------------------------------------------------------------
```

```sh
java -cp target\karate-tests.jar.jar com.intuit.karate.Main features\oauth.feature
```
![Html Report](screenshots/capture-karate-oauth2.png)

```text
java -cp target\karate-tests.jar.jar com.intuit.karate.Main features\oauth.feature
00:16:11.622 [main]  INFO  com.intuit.karate - Karate version: 1.4.1
00:16:11.877 [main]  INFO  com.intuit.karate.Suite - backed up existing 'target\karate-reports' dir to: target\karate-reports_1774066571873
00:16:13.704 [main]  DEBUG com.intuit.karate - request:
1 > POST http://localhost:8085/token
1 > Content-Type: application/json; charset=UTF-8
1 > Content-Length: 37
1 > Host: localhost:8085
1 > Connection: Keep-Alive
1 > User-Agent: Apache-HttpClient/4.5.14 (Java/11.0.12)
1 > Accept-Encoding: gzip,deflate
{"username":"test","password":"test"}

00:16:13.737 [main]  DEBUG com.intuit.karate - response time in milliseconds: 30
1 < 200
1 < Content-Type: application/json
1 < Transfer-Encoding: chunked
1 < Date: Sat, 21 Mar 2026 04:16:13 GMT
1 < Keep-Alive: timeout=60
1 < Connection: keep-alive
{"access_token":"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0ZXN0Iiwicm9sZSI6IlVTRVIiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwODUiLCJpYXQiOjE3NzQwNjY1NzMsImV4cCI6MTc3NDA3MDE3M30.rUcXDf6_6_glVvp1PRfAUlY0dLgTzwQN4fchSqEG034","expires_in":3600,"token_type":"Bearer"}

00:16:13.900 [main]  INFO  com.intuit.karate - [print] {"alg":"HS256"}
00:16:13.905 [main]  INFO  com.intuit.karate - [print] {"sub":"test","role":"USER","iss":"http://localhost:8085","iat":1774066573,"exp":1774070173}
---------------------------------------------------------
feature: features/oauth.feature
scenarios:  1 | passed:  1 | failed:  0 | time: 0.9574
---------------------------------------------------------

00:16:14.421 [main]  INFO  com.intuit.karate.Suite - <<pass>> feature 1 of 1 (0 remaining) features/oauth.feature
Karate version: 1.4.1
======================================================
elapsed:   2.63 | threads:    1 | thread time: 0.96
features:     1 | skipped:    0 | efficiency: 0.36
scenarios:    1 | passed:     1 | failed: 0
======================================================

HTML report: (paste into browser to view) | Karate version: 1.4.1
file:///C:/developer/sergueik/springboot_study/basic-karate-example3/target/karate-reports/karate-summary.html
===================================================================

```
### NOTE

Example failed run
```text
2026-03-20T23:08:22.938-04:00  INFO 20012 --- [nio-8085-exec-1] example.controller.AuthController        : token request for test/test
```		
```text
23:08:21.265 [main]  INFO  com.intuit.karate - Karate version: 1.4.1
23:08:22.796 [main]  DEBUG com.intuit.karate - request:
1 > POST http://localhost:8085/token
1 > Content-Type: application/json; charset=UTF-8
1 > Content-Length: 37
1 > Host: localhost:8085
1 > Connection: Keep-Alive
1 > User-Agent: Apache-HttpClient/4.5.14 (Java/11.0.12)
1 > Accept-Encoding: gzip,deflate
{"username":"test","password":"test"}

23:08:22.992 [main]  DEBUG com.intuit.karate - response time in milliseconds: 193
1 < 200
1 < Content-Type: application/json
1 < Transfer-Encoding: chunked
1 < Date: Sat, 21 Mar 2026 03:08:22 GMT
1 < Keep-Alive: timeout=60
1 < Connection: keep-alive
{"access_token":"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0ZXN0Iiwicm9sZSI6IlVTRVIiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwODUiLCJpYXQiOjE3NzQwNjI1MDIsImV4cCI6MTc3NDA2NjEwMn0.2jKQ57kej2ZIF96QeeIuwN255Kg4Mng7dREQXPCpkR0","expires_in":3600,"token_type":"Bearer"}

23:08:23.061 [main]  ERROR com.intuit.karate - features/oauth.feature:24
* match parts.length == 3
match failed: EQUALS
  $ | actual path does not exist (STRING:NUMBER)
  '#notpresent'
  3

features/oauth.feature:24
---------------------------------------------------------
feature: features/oauth.feature
scenarios:  1 | passed:  0 | failed:  1 | time: 0.8510
---------------------------------------------------------

23:08:23.557 [main]  INFO  com.intuit.karate.Suite - <<fail>> feature 1 of 1 (0 remaining) features/oauth.feature
Karate version: 1.4.1
======================================================
elapsed:   2.13 | threads:    1 | thread time: 0.85
features:     1 | skipped:    0 | efficiency: 0.40
scenarios:    1 | passed:     0 | failed: 1
======================================================
>>> failed features:
match failed: EQUALS
  $ | actual path does not exist (STRING:NUMBER)
  '#notpresent'
  3

features/oauth.feature:24
<<<

HTML report: (paste into browser to view) | Karate version: 1.4.1
file:///C:/developer/sergueik/springboot_study/basic-karate-example3/target/karate-reports/karate-summary.html
===================================================================

com.intuit.karate.KarateException: there are test failures !
        at ...(.)

```
