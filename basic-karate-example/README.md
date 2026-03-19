### Info

Assume there is a dummy server running on `8085` responding to a `GET` request of `/cookie` with a `200` and a cookie `question: value=42`	

```sh
pushd app 
mvn -DskipTests -nptB spring-boot:run
```

```text
2026-03-16 20:14:28.896  INFO 16652 --- [           main] o.s.s.concurrent.ThreadPoolTaskExecutor  : Initializing ExecutorService 'applicationTaskExecutor'
2026-03-16 20:14:29.067  INFO 16652 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8085 (http) with context path ''
2026-03-16 20:14:29.077  INFO 16652 --- [           main] example.Application                      : Started Application in 1.99 seconds (JVM running for 2.448)
2026-03-16 20:16:15.137  INFO 16652 --- [nio-8085-exec-1] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring DispatcherServlet 'dispatcherServlet'
2026-03-16 20:16:15.138  INFO 16652 --- [nio-8085-exec-1] o.s.web.servlet.DispatcherServlet        : Initializing Servlet 'dispatcherServlet'
2026-03-16 20:16:15.145  INFO 16652 --- [nio-8085-exec-1] o.s.web.servlet.DispatcherServlet        : Completed initialization in 7 ms
2026-03-16 20:16:15.164  INFO 16652 --- [nio-8085-exec-1] example.controller.CookieController      : data: value=42/dmFsdWU9NDI=
```

```sh
curl -sv http://localhost:8085/cookie
```

```text
< HTTP/1.1 200

< Set-Cookie: question=dmFsdWU9NDI=; Path=/; Domain=localhost; Max-Age=86400; Expires=Wed, 18 Mar 2026 00:16:15 GMT; Secure; HttpOnly
< Content-Length: 0
< Date: Tue, 17 Mar 2026 00:16:15 GMT
<
* Connection #0 to host localhost left intact
```



The [Karate](https://github.com/karatelabs/karate/wiki/Get-Started) test will execute a Karate feature script performing this request, collecting the cookie named "question", base64-decoding it, 
and extracting the value using the regexp "value="
then considering the scenario a success if the value is equal to 42

The feature file will look like this:
```cucumber
Feature: Cookie decode test

  Scenario: Decode cookie and verify value

    Given url 'http://localhost:8085/cookie'
    When method get
    Then status 200

    # get cookie value
    * def encoded = responseCookies.question.value

    # base64 decode using Java
    * def decoded = new java.lang.String(java.util.Base64.getDecoder().decode(encoded))

    # extract number from "value=42"
    * def value = decoded.match(/value=(\\d+)/)[1]

    # validation
    * match value == '42'
```
there is also a dummy Java class 'ExampleTest.java':

```java

package example;

import com.intuit.karate.junit5.Karate;

public class ExampleTest {

  @Karate.Test
  Karate testUi() 
          return Karate.run("classpath:example/feature/Test.feature");
  }
}

```

the test can be run like

```sh
mvn -ntp -B test
```
this will print to the console

```text

---------------------------------------------------------
feature: classpath:example/feature/Test.feature
scenarios:  1 | passed:  1 | failed:  0 | time: 1.6395
---------------------------------------------------------

Karate version: 1.4.1
======================================================
elapsed:   6.14 | threads:    1 | thread time: 1.64
features:     1 | skipped:    0 | efficiency: 0.27
scenarios:    1 | passed:     1 | failed: 0
======================================================

HTML report: (paste into browser to view) | Karate version: 1.4.1

file:///C:/developer/sergueik/springboot\_study/basic-karate-collector/target/karate-reports/karate-summary.html
===================================================================
```
this produces `target/karate-reports`:

```text
target/karate-reports/
├── example.feature.Test.html
├── example.feature.Test.karate-json.txt
├── favicon.ico
├── karate-labs-logo-ring.svg
├── karate-logo.png
├── karate-logo.svg
├── karate-summary.html
├── karate-summary-json.txt
├── karate-tags.html
├── karate-timeline.html
└── res
    ├── bootstrap.min.css
    ├── bootstrap.min.js
    ├── jquery.min.js
    ├── jquery.tablesorter.min.js
    ├── karate-report.css
    ├── karate-report.js
    ├── vis.min.css
    └── vis.min.js
```
The result looks like

```cmd
"c:\\Program Files\\Google\\Chrome\\Application\\chrome.exe" -url file:///C:/developer/sergueik/springboot\_study/basic-karate-collector/target/karate-reports/karate-summary.html
```

```sh
google-chrome -url file://$(pwd)/target/karate-reports/karate-summary.html \&
```

![karate-summary report](screenshots/capture-karate-summary.png)

Alternatively one can run

```sh
./karate.sh src/test/java/example/feature/Test.feature
```

```cmd
karate.bat src\test\java\example\feature\Test.feature
```
or

```sh
docker pull maven:3.9.3-eclipse-temurin-11-alpine
IMAGE=basic-karate-jdk11-maven
docker build -t $IMAGE .
```

```text
Sending build context to Docker daemon  302.6kB
Step 1/8 : FROM  maven:3.9.3-eclipse-temurin-11-alpine
 ---> 8dc958e1f01e
Step 2/8 : WORKDIR /work
 ---> Running in f69ea5fc0fa2
Removing intermediate container f69ea5fc0fa2
 ---> 7e4b432cb213
Step 3/8 : COPY pom.xml .
 ---> 0032aa8dda8f
Step 4/8 : RUN mvn dependency:go-offline -ntp -B
 ---> Running in 7243fc9340ba
[INFO] Scanning for projects...
[INFO]
[INFO] ----------------------< example:karate-collector >----------------------
[INFO] Building karate-collector 3.0-SNAPSHOT
[INFO]   from pom.xml
[INFO] --------------------------------[ jar ]---------------------------------
[INFO]
[INFO] --- dependency:3.6.0:go-offline (default-cli) @ karate-collector ---
[INFO] Resolved plugin: jetty-servlet-9.4.46.v20220331.jar
[INFO] Resolved plugin: maven-shared-utils-3.0.1.jar
[INFO] Resolved plugin: surefire-api-2.22.2.jar
[INFO] Resolved plugin: doxia-module-twiki-1.11.1.jar
[INFO] Resolved plugin: maven-deploy-plugin-3.1.1.jar
[INFO] Resolved plugin: velocity-tools-2.0.jar
[INFO] Resolved plugin: maven-toolchain-2.2.1.jar
[INFO] Resolved plugin: flexmark-ext-gfm-tasklist-0.42.14.jar
[INFO] Resolved plugin: maven-plugin-annotations-3.5.2.jar
[INFO] Resolved plugin: flexmark-ext-admonition-0.42.14.jar
[INFO] Resolved plugin: maven-core-3.2.5.jar
[INFO] Resolved plugin: flexmark-ext-footnotes-0.42.14.jar
[INFO] Resolved plugin: flexmark-ext-gfm-strikethrough-0.42.14.jar
[INFO] Resolved plugin: maven-repository-metadata-3.0.jar
[INFO] Resolved plugin: flexmark-ext-media-tags-0.42.14.jar
[INFO] Resolved plugin: doxia-module-xhtml-1.11.1.jar
[INFO] Resolved plugin: wagon-http-1.0-beta-6.jar
[INFO] Resolved plugin: plexus-utils-2.0.4.jar
[INFO] Resolved plugin: flexmark-util-0.42.14.jar
[INFO] Resolved plugin: flexmark-ext-anchorlink-0.42.14.jar
[INFO] Resolved plugin: aether-api-1.7.jar
[INFO] Resolved plugin: plexus-archiver-3.4.jar
[INFO] Resolved plugin: aether-api-1.0.0.v20140518.jar
[INFO] Resolved plugin: autolink-0.6.0.jar
[INFO] Resolved plugin: maven-filtering-3.3.1.jar
[INFO] Resolved plugin: sisu-guice-2.1.7-noaop.jar
[INFO] Resolved plugin: doxia-skin-model-1.11.1.jar
[INFO] Resolved plugin: dom4j-1.1.jar
[INFO] Resolved plugin: plexus-interpolation-1.11.jar
[INFO] Resolved plugin: maven-shared-utils-3.2.1.jar
[INFO] Resolved plugin: maven-surefire-plugin-2.22.2.jar
[INFO] Resolved plugin: plexus-container-default-1.0-alpha-9-stable-1.jar
[INFO] Resolved plugin: flexmark-youtrack-converter-0.42.14.jar
[INFO] Resolved plugin: maven-aether-provider-3.0.jar
[INFO] Resolved plugin: httpclient-4.5.13.jar
[INFO] Resolved plugin: flexmark-ext-toc-0.42.14.jar
[INFO] Resolved plugin: plexus-interpolation-1.14.jar
[INFO] Resolved plugin: maven-core-2.2.1.jar
[INFO] Resolved plugin: jetty-http-9.4.46.v20220331.jar
[INFO] Resolved plugin: maven-repository-metadata-2.2.1.jar
[INFO] Resolved plugin: wagon-webdav-jackrabbit-1.0-beta-6.jar
[INFO] Resolved plugin: plexus-classworlds-2.2.3.jar
[INFO] Resolved plugin: flexmark-jira-converter-0.42.14.jar
[INFO] Resolved plugin: maven-reporting-api-3.1.1.jar
[INFO] Resolved plugin: maven-plugin-api-3.2.5.jar
[INFO] Resolved plugin: sisu-guice-3.2.3-no_aop.jar
[INFO] Resolved plugin: flexmark-ext-attributes-0.42.14.jar
[INFO] Resolved plugin: commons-lang3-3.8.1.jar
[INFO] Resolved plugin: maven-aether-provider-3.2.5.jar
[INFO] Resolved plugin: aether-util-1.0.0.v20140518.jar
[INFO] Resolved plugin: jetty-security-9.4.46.v20220331.jar
[INFO] Resolved plugin: qdox-2.0-M9.jar
[INFO] Resolved plugin: plexus-compiler-javac-2.8.4.jar
[INFO] Resolved plugin: aether-impl-1.7.jar
[INFO] Resolved plugin: maven-plugin-registry-2.2.1.jar
[INFO] Resolved plugin: org.eclipse.sisu.plexus-0.3.5.jar
[INFO] Resolved plugin: maven-error-diagnostics-2.2.1.jar
[INFO] Resolved plugin: doxia-core-1.11.1.jar
[INFO] Resolved plugin: flexmark-ext-ins-0.42.14.jar
[INFO] Resolved plugin: maven-artifact-3.0.jar
[INFO] Resolved plugin: maven-install-plugin-3.1.1.jar
[INFO] Resolved plugin: maven-archiver-3.1.1.jar
[INFO] Resolved plugin: google-collections-1.0.jar
[INFO] Resolved plugin: flexmark-ext-emoji-0.42.14.jar
[INFO] Resolved plugin: maven-plugin-api-2.2.1.jar
[INFO] Resolved plugin: maven-project-2.2.1.jar
[INFO] Resolved plugin: doxia-integration-tools-1.11.1.jar
[INFO] Resolved plugin: maven-compiler-plugin-3.8.1.jar
[INFO] Resolved plugin: maven-repository-metadata-3.2.5.jar
[INFO] Resolved plugin: flexmark-ext-jekyll-front-matter-0.42.14.jar
[INFO] Resolved plugin: doxia-decoration-model-1.11.1.jar
[INFO] Resolved plugin: flexmark-all-0.42.14.jar
[INFO] Resolved plugin: plexus-component-annotations-1.6.jar
[INFO] Resolved plugin: plexus-utils-3.5.0.jar
[INFO] Resolved plugin: maven-shared-incremental-1.1.jar
[INFO] Resolved plugin: jetty-webapp-9.4.46.v20220331.jar
[INFO] Resolved plugin: plexus-io-3.2.0.jar
[INFO] Resolved plugin: xbean-reflect-3.7.jar
[INFO] Resolved plugin: maven-reporting-exec-1.6.0.jar
[INFO] Resolved plugin: doxia-sink-api-1.11.1.jar
[INFO] Resolved plugin: jetty-xml-9.4.46.v20220331.jar
[INFO] Resolved plugin: plexus-velocity-1.2.jar
[INFO] Resolved plugin: plexus-container-default-2.1.0.jar
[INFO] Resolved plugin: commons-codec-1.11.jar
[INFO] Resolved plugin: snappy-0.4.jar
[INFO] Resolved plugin: plexus-build-api-0.0.7.jar
[INFO] Resolved plugin: doxia-module-fml-1.11.1.jar
[INFO] Resolved plugin: commons-digester-1.8.jar
[INFO] Resolved plugin: jetty-server-9.4.46.v20220331.jar
[INFO] Resolved plugin: flexmark-ext-abbreviation-0.42.14.jar
[INFO] Resolved plugin: maven-archiver-3.5.2.jar
[INFO] Resolved plugin: nekohtml-1.9.6.2.jar
[INFO] Resolved plugin: flexmark-ext-definition-0.42.14.jar
[INFO] Resolved plugin: doxia-logging-api-1.11.1.jar
[INFO] Resolved plugin: slf4j-nop-1.5.3.jar
[INFO] Resolved plugin: flexmark-ext-yaml-front-matter-0.42.14.jar
[INFO] Resolved plugin: maven-site-plugin-3.12.1.jar
[INFO] Resolved plugin: flexmark-ext-superscript-0.42.14.jar
[INFO] Resolved plugin: commons-collections-3.2.2.jar
[INFO] Resolved plugin: doxia-module-markdown-1.11.1.jar
[INFO] Resolved plugin: flexmark-ext-xwiki-macros-0.42.14.jar
[INFO] Resolved plugin: commons-compress-1.20.jar
[INFO] Resolved plugin: velocity-1.7.jar
[INFO] Resolved plugin: maven-resources-plugin-3.3.1.jar
[INFO] Resolved plugin: plexus-utils-3.5.1.jar
[INFO] Resolved plugin: wagon-http-shared-1.0-beta-6.jar
[INFO] Resolved plugin: commons-httpclient-3.1.jar
[INFO] Resolved plugin: wagon-provider-api-1.0-beta-6.jar
[INFO] Resolved plugin: plexus-java-0.9.10.jar
[INFO] Resolved plugin: maven-settings-2.2.1.jar
[INFO] Resolved plugin: aopalliance-1.0.jar
[INFO] Resolved plugin: surefire-logger-api-2.22.2.jar
[INFO] Resolved plugin: maven-model-3.0.jar
[INFO] Resolved plugin: aether-spi-1.0.0.v20140518.jar
[INFO] Resolved plugin: slf4j-api-1.5.6.jar
[INFO] Resolved plugin: plexus-interpolation-1.26.jar
[INFO] Resolved plugin: maven-artifact-manager-2.2.1.jar
[INFO] Resolved plugin: jackrabbit-webdav-1.5.0.jar
[INFO] Resolved plugin: maven-settings-builder-3.2.5.jar
[INFO] Resolved plugin: httpcore-4.4.14.jar
[INFO] Resolved plugin: commons-codec-1.2.jar
[INFO] Resolved plugin: maven-reporting-api-3.0.jar
[INFO] Resolved plugin: commons-lang3-3.12.0.jar
[INFO] Resolved plugin: maven-model-builder-3.0.jar
[INFO] Resolved plugin: maven-artifact-3.2.5.jar
[INFO] Resolved plugin: xz-1.5.jar
[INFO] Resolved plugin: flexmark-ext-enumerated-reference-0.42.14.jar
[INFO] Resolved plugin: plexus-component-annotations-2.1.1.jar
[INFO] Resolved plugin: xercesMinimal-1.9.6.2.jar
[INFO] Resolved plugin: plexus-utils-3.0.24.jar
[INFO] Resolved plugin: commons-logging-1.2.jar
[INFO] Resolved plugin: flexmark-ext-escaped-character-0.42.14.jar
[INFO] Resolved plugin: plexus-utils-1.5.15.jar
[INFO] Resolved plugin: plexus-cipher-1.4.jar
[INFO] Resolved plugin: flexmark-0.42.14.jar
[INFO] Resolved plugin: plexus-utils-3.4.2.jar
[INFO] Resolved plugin: flexmark-ext-aside-0.42.14.jar
[INFO] Resolved plugin: flexmark-ext-wikilink-0.42.14.jar
[INFO] Resolved plugin: commons-compress-1.11.jar
[INFO] Resolved plugin: org.eclipse.sisu.inject-0.3.5.jar
[INFO] Resolved plugin: maven-settings-3.0.jar
[INFO] Resolved plugin: maven-plugin-api-3.0.jar
[INFO] Resolved plugin: flexmark-ext-gfm-tables-0.42.14.jar
[INFO] Resolved plugin: sisu-inject-bean-1.4.2.jar
[INFO] Resolved plugin: jackrabbit-jcr-commons-1.5.0.jar
[INFO] Resolved plugin: classworlds-1.1.jar
[INFO] Resolved plugin: commons-io-2.11.0.jar
[INFO] Resolved plugin: jetty-io-9.4.46.v20220331.jar
[INFO] Resolved plugin: cdi-api-1.2.jar
[INFO] Resolved plugin: commons-chain-1.1.jar
[INFO] Resolved plugin: maven-artifact-2.2.1.jar
[INFO] Resolved plugin: flexmark-html-parser-0.42.14.jar
[INFO] Resolved plugin: maven-plugin-descriptor-2.2.1.jar
[INFO] Resolved plugin: javax.inject-1.jar
[INFO] Resolved plugin: maven-shared-utils-3.3.4.jar
[INFO] Resolved plugin: javax.servlet-api-3.1.0.jar
[INFO] Resolved plugin: jetty-util-ajax-9.4.46.v20220331.jar
[INFO] Resolved plugin: maven-settings-3.2.5.jar
[INFO] Resolved plugin: jsoup-1.10.2.jar
[INFO] Resolved plugin: oro-2.0.8.jar
[INFO] Resolved plugin: xz-1.9.jar
[INFO] Resolved plugin: maven-settings-builder-3.0.jar
[INFO] Resolved plugin: aether-impl-1.0.0.v20140518.jar
[INFO] Resolved plugin: aether-util-1.7.jar
[INFO] Resolved plugin: commons-text-1.3.jar
[INFO] Resolved plugin: maven-core-3.0.jar
[INFO] Resolved plugin: commons-lang-2.4.jar
[INFO] Resolved plugin: slf4j-api-1.7.36.jar
[INFO] Resolved plugin: doxia-module-apt-1.11.1.jar
[INFO] Resolved plugin: flexmark-ext-gfm-issues-0.42.14.jar
[INFO] Resolved plugin: jetty-util-9.4.46.v20220331.jar
[INFO] Resolved plugin: maven-model-builder-3.2.5.jar
[INFO] Resolved plugin: plexus-archiver-4.2.7.jar
[INFO] Resolved plugin: commons-io-2.6.jar
[INFO] Resolved plugin: jcl-over-slf4j-1.5.6.jar
[INFO] Resolved plugin: flexmark-ext-youtube-embedded-0.42.14.jar
[INFO] Resolved plugin: flexmark-formatter-0.42.14.jar
[INFO] Resolved plugin: plexus-component-annotations-1.7.1.jar
[INFO] Resolved plugin: maven-model-3.2.5.jar
[INFO] Resolved plugin: maven-monitor-2.2.1.jar
[INFO] Resolved plugin: hamcrest-core-1.3.jar
[INFO] Resolved plugin: flexmark-profile-pegdown-0.42.14.jar
[INFO] Resolved plugin: doxia-module-docbook-simple-1.11.1.jar
[INFO] Resolved plugin: guava-16.0.1.jar
[INFO] Resolved plugin: qdox-2.0-M8.jar
[INFO] Resolved plugin: flexmark-ext-typographic-0.42.14.jar
[INFO] Resolved plugin: flexmark-ext-autolink-0.42.14.jar
[INFO] Resolved plugin: surefire-booter-2.22.2.jar
[INFO] Resolved plugin: flexmark-ext-jekyll-tag-0.42.14.jar
[INFO] Resolved plugin: flexmark-ext-gitlab-0.42.14.jar
[INFO] Resolved plugin: commons-io-2.5.jar
[INFO] Resolved plugin: doxia-module-xdoc-1.11.1.jar
[INFO] Resolved plugin: maven-surefire-common-2.22.2.jar
[INFO] Resolved plugin: doxia-site-renderer-1.11.1.jar
[INFO] Resolved plugin: sisu-inject-plexus-1.4.2.jar
[INFO] Resolved plugin: commons-beanutils-1.7.0.jar
[INFO] Resolved plugin: plexus-io-2.7.1.jar
[INFO] Resolved plugin: plexus-classworlds-2.5.2.jar
[INFO] Resolved plugin: doxia-module-confluence-1.11.1.jar
[INFO] Resolved plugin: doxia-module-xhtml5-1.11.1.jar
[INFO] Resolved plugin: flexmark-ext-tables-0.42.14.jar
[INFO] Resolved plugin: javax.annotation-api-1.2.jar
[INFO] Resolved plugin: plexus-compiler-api-2.8.4.jar
[INFO] Resolved plugin: maven-jar-plugin-3.0.2.jar
[INFO] Resolved plugin: flexmark-ext-macros-0.42.14.jar
[INFO] Resolved plugin: maven-clean-plugin-3.2.0.jar
[INFO] Resolved plugin: plexus-compiler-manager-2.8.4.jar
[INFO] Resolved plugin: aether-spi-1.7.jar
[INFO] Resolved plugin: plexus-sec-dispatcher-1.3.jar
[INFO] Resolved plugin: maven-profile-2.2.1.jar
[INFO] Resolved plugin: asm-6.2.jar
[INFO] Resolved plugin: plexus-i18n-1.0-beta-10.jar
[INFO] Resolved plugin: maven-plugin-parameter-documenter-2.2.1.jar
[INFO] Resolved plugin: junit-4.12.jar
[INFO] Resolved plugin: flexmark-ext-gfm-users-0.42.14.jar
[INFO] Resolved plugin: maven-model-2.2.1.jar
[INFO] Resolved plugin: backport-util-concurrent-3.1.jar
[INFO] Resolved plugin: slf4j-jdk14-1.5.6.jar
[INFO] Resolved dependency: classgraph-4.8.160.jar
[INFO] Resolved dependency: jackson-annotations-2.12.1.jar
[INFO] Resolved dependency: netty-tcnative-boringssl-static-2.0.61.Final-linux-aarch_64.jar
[INFO] Resolved dependency: jcl-over-slf4j-2.0.9.jar
[INFO] Resolved dependency: js-scriptengine-22.3.3.jar
[INFO] Resolved dependency: owasp-java-html-sanitizer-20211018.2.jar
[INFO] Resolved dependency: ssim-1.0.0.jar
[INFO] Resolved dependency: netty-handler-4.1.96.Final.jar
[INFO] Resolved dependency: jsoup-1.14.2.jar
[INFO] Resolved dependency: jackson-datatype-jsr310-2.15.2.jar
[INFO] Resolved dependency: opentest4j-1.2.0.jar
[INFO] Resolved dependency: commons-lang3-3.12.0.jar
[INFO] Resolved dependency: netty-common-4.1.96.Final.jar
[INFO] Resolved dependency: j2objc-annotations-1.3.jar
[INFO] Resolved dependency: service-1.12.0.jar
[INFO] Resolved dependency: logback-classic-1.4.11.jar
[INFO] Resolved dependency: native-linux-x86_64-1.12.0.jar
[INFO] Resolved dependency: netty-codec-dns-4.1.96.Final.jar
[INFO] Resolved dependency: karate-core-1.4.1.jar
[INFO] Resolved dependency: junit-jupiter-api-5.9.3.jar
[INFO] Resolved dependency: commons-logging-1.1.1.jar
[INFO] Resolved dependency: checker-qual-3.8.0.jar
[INFO] Resolved dependency: js-22.3.3.jar
[INFO] Resolved dependency: junit-jupiter-engine-5.9.3.jar
[INFO] Resolved dependency: velocity-engine-core-2.3.jar
[INFO] Resolved dependency: antlr4-runtime-4.11.1.jar
[INFO] Resolved dependency: netty-tcnative-boringssl-static-2.0.61.Final-windows-x86_64.jar
[INFO] Resolved dependency: jackson-datatype-jsr310-2.12.1.jar
[INFO] Resolved dependency: netty-transport-native-epoll-4.1.96.Final-linux-aarch_64.jar
[INFO] Resolved dependency: netty-transport-classes-kqueue-4.1.96.Final.jar
[INFO] Resolved dependency: picocli-4.7.1.jar
[INFO] Resolved dependency: snakeyaml-2.0.jar
[INFO] Resolved dependency: netty-codec-socks-4.1.96.Final.jar
[INFO] Resolved dependency: netty-tcnative-boringssl-static-2.0.61.Final-osx-aarch_64.jar
[INFO] Resolved dependency: netty-transport-native-unix-common-4.1.96.Final-osx-aarch_64.jar
[INFO] Resolved dependency: netty-handler-proxy-4.1.96.Final.jar
[INFO] Resolved dependency: ognl-3.3.4.jar
[INFO] Resolved dependency: commons-lang-2.6.jar
[INFO] Resolved dependency: netty-resolver-dns-native-macos-4.1.96.Final-osx-aarch_64.jar
[INFO] Resolved dependency: netty-resolver-dns-4.1.96.Final.jar
[INFO] Resolved dependency: jackson-core-2.15.2.jar
[INFO] Resolved dependency: regex-22.3.3.jar
[INFO] Resolved dependency: netty-transport-native-unix-common-4.1.96.Final.jar
[INFO] Resolved dependency: fastcsv-2.2.1.jar
[INFO] Resolved dependency: javassist-3.29.0-GA.jar
[INFO] Resolved dependency: joda-time-2.10.10.jar
[INFO] Resolved dependency: error_prone_annotations-2.5.1.jar
[INFO] Resolved dependency: guava-30.1.1-jre.jar
[INFO] Resolved dependency: jackson-core-2.12.1.jar
[INFO] Resolved dependency: logback-core-1.4.11.jar
[INFO] Resolved dependency: micrometer-commons-1.11.3.jar
[INFO] Resolved dependency: httpclient-4.5.14.jar
[INFO] Resolved dependency: commons-codec-1.16.0.jar
[INFO] Resolved dependency: jackson-databind-2.12.1.jar
[INFO] Resolved dependency: listenablefuture-9999.0-empty-to-avoid-conflict-with-guava.jar
[INFO] Resolved dependency: json-path-2.8.0.jar
[INFO] Resolved dependency: asm-9.3.jar
[INFO] Resolved dependency: netty-resolver-dns-classes-macos-4.1.96.Final.jar
[INFO] Resolved dependency: netty-buffer-4.1.96.Final.jar
[INFO] Resolved dependency: netty-codec-http-4.1.96.Final.jar
[INFO] Resolved dependency: micrometer-core-1.11.3.jar
[INFO] Resolved dependency: netty-codec-4.1.96.Final.jar
[INFO] Resolved dependency: json-smart-2.4.10.jar
[INFO] Resolved dependency: failureaccess-1.0.1.jar
[INFO] Resolved dependency: graal-sdk-22.3.3.jar
[INFO] Resolved dependency: netty-resolver-4.1.96.Final.jar
[INFO] Resolved dependency: httpcore-4.4.16.jar
[INFO] Resolved dependency: thymeleaf-3.1.2.RELEASE.jar
[INFO] Resolved dependency: netty-transport-native-kqueue-4.1.96.Final-osx-x86_64.jar
[INFO] Resolved dependency: junit-platform-engine-1.9.3.jar
[INFO] Resolved dependency: slf4j-api-1.7.30.jar
[INFO] Resolved dependency: attoparser-2.0.7.RELEASE.jar
[INFO] Resolved dependency: commons-io-2.7.jar
[INFO] Resolved dependency: netty-transport-native-unix-common-4.1.96.Final-linux-aarch_64.jar
[INFO] Resolved dependency: cucumber-reporting-5.6.1.jar
[INFO] Resolved dependency: brotli4j-1.12.0.jar
[INFO] Resolved dependency: jsr305-3.0.2.jar
[INFO] Resolved dependency: slf4j-api-2.0.7.jar
[INFO] Resolved dependency: netty-transport-4.1.96.Final.jar
[INFO] Resolved dependency: armeria-1.25.2.jar
[INFO] Resolved dependency: jackson-datatype-jdk8-2.15.2.jar
[INFO] Resolved dependency: commons-configuration-1.10.jar
[INFO] Resolved dependency: icu4j-71.1.jar
[INFO] Resolved dependency: netty-tcnative-boringssl-static-2.0.61.Final-osx-x86_64.jar
[INFO] Resolved dependency: micrometer-observation-1.11.3.jar
[INFO] Resolved dependency: netty-tcnative-classes-2.0.61.Final.jar
[INFO] Resolved dependency: netty-transport-native-unix-common-4.1.96.Final-osx-x86_64.jar
[INFO] Resolved dependency: jackson-annotations-2.15.2.jar
[INFO] Resolved dependency: junit-platform-commons-1.9.3.jar
[INFO] Resolved dependency: netty-resolver-dns-native-macos-4.1.96.Final-osx-x86_64.jar
[INFO] Resolved dependency: netty-transport-native-epoll-4.1.96.Final-linux-x86_64.jar
[INFO] Resolved dependency: unbescape-1.1.6.RELEASE.jar
[INFO] Resolved dependency: karate-junit5-1.4.1.jar
[INFO] Resolved dependency: plexus-utils-3.3.0.jar
[INFO] Resolved dependency: netty-transport-native-unix-common-4.1.96.Final-linux-x86_64.jar
[INFO] Resolved dependency: netty-transport-classes-epoll-4.1.96.Final.jar
[INFO] Resolved dependency: commons-collections-3.2.2.jar
[INFO] Resolved dependency: apiguardian-api-1.1.2.jar
[INFO] Resolved dependency: jackson-databind-2.15.2.jar
[INFO] Resolved dependency: resemble-1.0.2.jar
[INFO] Resolved dependency: LatencyUtils-2.0.3.jar
[INFO] Resolved dependency: HdrHistogram-2.1.12.jar
[INFO] Resolved dependency: netty-tcnative-boringssl-static-2.0.61.Final-linux-x86_64.jar
[INFO] Resolved dependency: truffle-api-22.3.3.jar
[INFO] Resolved dependency: netty-codec-haproxy-4.1.96.Final.jar
[INFO] Resolved dependency: netty-codec-http2-4.1.96.Final.jar
[INFO] Resolved dependency: reactive-streams-1.0.4.jar
[INFO] Resolved dependency: accessors-smart-2.4.9.jar
[INFO] Resolved dependency: netty-transport-native-kqueue-4.1.96.Final-osx-aarch_64.jar
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  02:06 min
[INFO] Finished at: 2026-03-18T22:51:05Z
[INFO] ------------------------------------------------------------------------
Removing intermediate container 7243fc9340ba
 ---> b846a03be6c6
Step 5/8 : COPY dummy/src src
 ---> 439d5b137959
Step 6/8 : RUN mvn -B -ntp -DskipTests surefire:test
 ---> Running in fba7461d8883
[INFO] Scanning for projects...
[INFO]
[INFO] ----------------------< example:karate-collector >----------------------
[INFO] Building karate-collector 3.0-SNAPSHOT
[INFO]   from pom.xml
[INFO] --------------------------------[ jar ]---------------------------------
[INFO]
[INFO] --- surefire:2.22.2:test (default-cli) @ karate-collector ---
[INFO] Tests are skipped.
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  7.686 s
[INFO] Finished at: 2026-03-18T22:51:33Z
[INFO] ------------------------------------------------------------------------
Removing intermediate container fba7461d8883
 ---> e681c0ac94df
Step 7/8 : COPY src src
 ---> e3af44f38563
Step 8/8 : CMD ["mvn", "test"]
 ---> Running in 6aa026af8eea
Removing intermediate container 6aa026af8eea
 ---> 6b27975c1b1d
Successfully built 6b27975c1b1d
Successfully tagged basic-karate-jdk8-maven:latest
SECURITY WARNING: You are building a Docker image from Windows against a non-Windows Docker host. 
All files and directories added to build context will have '-rwxr-xr-x' permissions. 
It is recommended to double check and reset permissions for sensitive files and directories.
```
```sh
docker container rm -f $IMAGE
docker run -d --name $IMAGE $IMAGE
docker cp $IMAGE:/work/target target
```

```sh
jq ".featureSummary[0].failedCount" target\karate-reports\karate-summary-json.txt
```
which will give

```text
0
```

### Background

Karate is an open-source API, performance, and UI test automation framework built on Java and JavaScript, yet designed to remove the entry barriers of either language. It uniquely integrates API testing, UI automation, scientifically accurate performance testing (via Gatling), and service virtualization (mocking) into a single cohesive tool. Inspired by Cucumber, Karate employs a simple Gherkin - based syntax (`.feature` files) reminiscent of early BDD DSLs independently found in the Ruby ecosystem (`spec` files).
Karate feature files achieve the same goals that spec-driven frameworks in Ruby, Puppet, or InSpec do: tests are understandable and even writable by domain experts without Java or JavaScript experience, making automation accessible.

Karate should not be viewed as Cucumber under a different brand, but rather as a specialized toolset designed specifically for API testing.

In the API testing domain, Karate deliberately abandons Cucumber’s full flexibility in favor of a “simple English,” precise scenario subset of Gherkin`

This leads to another advantage: Karate keeps HTTP requests, data and test logic in a single script, with optional embedded JavaScript for more complex scenarios, improving readability and maintainability

```cucumber
* def payload =
"""
{
  sub: 'test-user',
  role: 'admin',
  iss: 'test-suite'
}
"""

* def secret = 'dummy-secret'

* def token = karate.jwtSign(payload, secret)

Given url 'https://httpbin.org/something'
And header Authorization = 'Bearer ' + token
When method get
Then status 200
```
or
```cucumber
Feature: Inline JWT generation demo

Scenario: create a fake JWT with inline JS
    # define payload
    * def payload = { sub: 'test-user', role: 'admin', iss: 'karate-demo' }

    # inline JS function to make a fake JWT
    * def makeJwt =
    """
    function(payload){
        function base64Encode(obj){
            return java.util.Base64.getUrlEncoder().withoutPadding().encodeToString(JSON.stringify(obj).getBytes('UTF-8'));
        }
        var header = { alg: 'HS256', typ: 'JWT' };
        var token = base64Encode(header) + '.' + base64Encode(payload) + '.fake-signature';
        return token;
    }
    """

    # generate the token
    * def token = makeJwt(payload)

    # just print for demo
    * print 'JWT:', token
```
*longer version*:

Karate makes an important sacrifice by dropping Cucumber’s full flexibilityi — originally valuable for acceptance tests describing complex user journeys — in favor of a precise “simple-English” subset of the Gherkin language that aligns naturally with REST API testing
This fully eliminates the need for boilerplate Java "step definitions" plumbing code:

```cucumber
@userStory("U02")
Feature: Login and Logout

  @scenarioID("U02-TS01")
  Scenario: Login with valid credentials
    When the user logs in with username "john" and password "secret123"
    Then the login should succeed
```
```java
   private String username;
    private String password;
    private boolean loginResult;

    @When("^the user logs in with username \"([^\"]*)\" and password \"([^\"]*)\"$")
    public void login_with_credentials(String username, String password) throws Throwable {
        this.username = username;
...
```
* Karate equivalent - note: the regex step-definition plumbing is gone

```
Scenario: Login with valid credentials
  Given url baseUrl + '/login'
  And request { username: 'john', password: 'secret123' }
  When method post
  Then status 200
```
making tests concise, readable, and maintainable without sacrificing clarity or expressiveness.
For UI automation, Karate extends into Selenium-like capabilities, providing a DSL for waits, captures, Shadow DOM, iframe handling, file uploads/downloads, and visual verifications. This addresses all major modern web UI testing needs, from dynamic content synchronization to complex DOM manipulations, offering a comprehensive, low-code solution for both API and UI testing. 

These features place Karate in the same league as advanced JavaScript-based frameworks like 
* Playwright
* Cypress
* TestCafe
* WebDriverIO

but they also make it a clear productivity winner over classic pure Selenium, by reducing boilerplate, simplifying test maintenance, and providing richer built-in support for modern web UI challenges.


Another standout aspect is Karate’s HTML page tree-view for step results and debugging, providing instant, clear, hierarchical visibility into every test step — a level of clarity and maintainability that most modern frameworks do not offer

To get similar functionality with tools like TestNG, SpecFlow, Cucumber, or Selenium, teams typically add external reporting add‑ons that require setup, configuration, and often build tool/plugin work:
* Allure Report – requires explicit setup
* ExtentReports
Combined with its scientifically accurate performance testing, Karate offers a complete, low-code automation ecosystem that is robust, accessible, and highly recognized by its community.

### See Also

  * [REST API Testing with Karate](https://www.baeldung.com/karate-rest-api-testing)
  * standalne karate.jar [download](https://github.com/karatelabs/karate/tree/master/karate-netty#standalone-jar)
  * running karate standalone executable JAR (50 MB) which only requires a JRE to run, without maven or gradle [documentation](https://github.com/karatelabs/karate/blob/master/karate-netty/README.md)
  * [Skipping Tests With Gradle](https://www.baeldung.com/gradle-skip-tests)
  * https://github.com/fertekton/karate-api/blob/main/src/test/java/Yevo/BackEndTests.java
  * https://stackoverflow.com/questions/53272230/could-not-create-service-of-type-scriptpluginfactory-using-buildscopeservices-cr
  * https://github.com/gradle/gradle/issues/8436
  * https://github.com/figroc/tensorflow-serving-client/issues/11
  * [docker image](https://hub.docker.com/r/ptrthomas/karate-chrome) with X server (XVFB), VNC, Google Chrome
  * [jenkins cucumber reports plugin](https://plugins.jenkins.io/cucumber-reports/)
  * [Karate and Cucumber Frameworks: A Comparative Study](https://milestone.tech/tips-and-tricks/karate-framework-and-cucumber-framework-a-comparative-study)
  * [Maven CLI Options Reference](https://maven.apache.org/ref/4.0.0-beta-3/maven-embedder/cli.html)

---
### Author
[Serguei Kouzmine](kouzmine\_serguei@yahoo.com)



