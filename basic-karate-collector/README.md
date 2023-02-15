### Info

replica of [basic Karate tesst](https://github.com/KaterinaUK/automated_API_with_Karate)
with `https://jsonplaceholder.typicode.com/` used as system under test.

### Usage

* run with maven (standalone run and run with gradle are alternatives)
```sh
mvn clean test
```
* after successful run there is a  summary report  `target\karate-reports\karate-summary-json.txt` in JSON format
```JSON
{
  "efficiency": 0.22719805728162273,
  "totalTime": 3181,
  "threads": 1,
  "resultDate": "2023-02-08 10:19:52 PM",
  "env": null,
  "version": "1.3.1",
  "scenariosfailed": 0,
  "featureSummary": [
    {
      "failedCount": 0,
      "packageQualifiedName": "example.feature.jsonplaceholder"
      "relativePath": "example/feature/jsonplaceholder.feature"
      "scenarioCount": 2,
      "name": "Tests for the json placeholder page",
      "description": "",
      "durationMillis": 3180.838618,
      "passedCount": 2,
      "failed": false
    }
  ],
  "featuresPassed": 1,
  "featuresFailed": 0,
  "featuresSkipped": 0,
  "scenariosPassed": 2,
  "elapsedTime": 14001
}
```
and HTML page
![success](https://github.com/sergueik/springboot_study/blob/master/basic-karate-collector/screenshots/capture-success.png)

* after failed run
```JSON
{
  "efficiency": 0.25334247514569763,
  "totalTime": 2956,
  "threads": 1,
  "resultDate": "2023-02-08 10:25:46 PM",
  "env": null,
  "version": "1.3.1",
  "scenariosfailed": 1,
  "featureSummary": [
    {
      "failedCount": 1,
      "packageQualifiedName": "example.feature.jsonplaceholder",
      "relativePath": "example/feature/jsonplaceholder.feature",
      "scenarioCount": 2,
      "name": "Tests for the json placeholder page",
      "description": "",
      "durationMillis": 2956.155631,
      "passedCount": 1,
      "failed": true
    }
  ],
  "featuresPassed": 0,
  "featuresFailed": 1,
  "featuresSkipped": 0,
  "scenariosPassed": 1,
  "elapsedTime": 11668
}
```
![failed run](https://github.com/sergueik/springboot_study/blob/master/basic-karate-collector/screenshots/capture-failure.png)

* the details are in  `target\karate-reports\example.feature.jsonplaceholder.karate-json.txt`:
```JSON
{
  "failedCount": 1,
  "prefixedPath": "classpath:example/feature/jsonplaceholder.feature",
  "packageQualifiedName": "example.feature.jsonplaceholder",
  "loopIndex": -1,
  "relativePath": "example/feature/jsonplaceholder.feature",
  "scenarioResults": [
    {
      "sectionIndex": 0,
      "stepResults": [
        {
          "result": {
            "nanos": 5950378,
            "millis": 5.950378,
            "status": "passed"
          },
          "step": {
            "background": true,
            "line": 4,
            "prefix": "Given",
            "index": 0,
            "text": "url 'https://jsonplaceholder.typicode.com/'"
          },
          "stepLog": "22:25:40.858 karate.env system property was: null \n"
        },
        {
          "result": {
            "nanos": 3020092,
            "millis": 3.020092,
            "status": "passed"
          },
          "step": {
            "line": 7,
            "prefix": "Given",
            "index": 0,
            "text": "path 'todos/1'"
          }
        },
        {
          "result": {
            "nanos": 2599152219,
            "millis": 2599.152219,
            "status": "passed"
          },
          "step": {
            "line": 8,
            "prefix": "When",
            "index": 1,
            "text": "method Get"
          }
        },
        {
          "result": {
            "nanos": 38489,
            "millis": 0.038489,
            "status": "passed"
          },
          "step": {
            "line": 9,
            "prefix": "Then",
            "index": 2,
            "text": "status 200"
          }
        },
        {
          "result": {
            "nanos": 116111479,
            "millis": 116.111479,
            "status": "passed"
          },
          "step": {
            "line": 10,
            "prefix": "And",
            "index": 3,
            "text": "match response.userId == 1"
          }
        },
        {
          "result": {
            "nanos": 3928429,
            "millis": 3.928429,
            "status": "passed"
          },
          "step": {
            "line": 11,
            "prefix": "And",
            "index": 4,
            "text": "match response.title == '#string'"
          }
        }
      ],
      "executorName": "main",
      "line": 6,
      "name": "Get todo",
      "description": "",
      "durationMillis": 2728.201086,
      "startTime": 1675913139926,
      "failed": false,
      "refId": "[1:6]",
      "endTime": 1675913143849,
      "exampleIndex": -1
    },
    {
      "sectionIndex": 1,
      "stepResults": [
        {
          "result": {
            "nanos": 1405271,
            "millis": 1.405271,
            "status": "passed"
          },
          "step": {
            "background": true,
            "line": 4,
            "prefix": "Given",
            "index": 0,
            "text": "url 'https://jsonplaceholder.typicode.com/'"
          },
          "stepLog": "22:25:43.899 karate.env system property was: null \n"
        },
        {
          "result": {
            "nanos": 933569,
            "millis": 0.933569,
            "status": "passed"
          },
          "step": {
            "line": 15,
            "prefix": "Given",
            "index": 0,
            "text": "path 'todos'"
          }
        },
        {
          "result": {
            "nanos": 213293710,
            "millis": 213.29371,
            "status": "passed"
          },
          "step": {
            "line": 16,
            "prefix": "When",
            "index": 1,
            "text": "method Get"
          },
        },
        {
          "result": {
            "nanos": 22238,
            "millis": 0.022238,
            "status": "passed"
          },
          "step": {
            "line": 17,
            "prefix": "Then",
            "index": 2,
            "text": "status 200"
          }
        },
        {
          "result": {
            "nanos": 6248453,
            "millis": 6.248453,
            "status": "passed"
          },
          "step": {
            "line": 18,
            "prefix": "And",
            "index": 3,
            "text": "match response[0].title == '#string'"
          }
        },
        {
          "result": {
            "nanos": 6051304,
            "errorMessage": "match failed: EQUALS\n  $ | not equal (NUMBER:NUMBER)\n  1\n  0\n\nclasspath:example/feature/jsonplaceholder.feature:19",
            "millis": 6.051304,
            "status": "failed"
          },
          "step": {
            "line": 19,
            "prefix": "And",
            "index": 4,
            "text": "match response[0].userId == 0"
          },
          "stepLog": "22:25:44.121 classpath:example/feature/jsonplaceholder.feature:19\nAnd match response[0].userId == 0\nmatch failed: EQUALS\n  $ | not equal (NUMBER:NUMBER)\n  1\n  0\n\nclasspath:example/feature/jsonplaceholder.feature:19\n"
        }
      ],
      "line": 14,
      "description": "",
      "durationMillis": 227.954545,
      "failed": true,
      "error": "match failed: EQUALS\n  $ | not equal (NUMBER:NUMBER)\n  1\n  0\n\nclasspath:example/feature/jsonplaceholder.feature:19",
      "executorName": "main",
      "name": "List todos",
      "startTime": 1675913143888,
      "refId": "[2:14]",
      "endTime": 1675913144121,
      "exampleIndex": -1
    }
  ],
  "callDepth": 0,
  "name": "Tests for the json placeholder page",
  "description": "",
  "resultDate": "2023-02-08 10:25:44 PM",
  "durationMillis": 2956.155631,
  "passedCount": 1
}

```

The processing of the `karate-summary-json.txt` is simple with [jq](https://stedolan.github.io/jq/manual):

```sh
jq '.featureSummary[0].failedCount' karate-summary-json.txt
```
```text
0
```
The Perl version is also possible:
```sh
perl karate-ummary-processor.pl  -input karate-summary.json
```
```text
0
```
the code is:
```Perl

our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
local $@;
my $data = eval { return $json_pp->decode($content); };
$error = $@;

if ( !$error ) {
    print $data->{'featureSummary'}->[0]->{'failedCount'};
}
```


the `"stepLog"` contains actual exhange payload, and the log file can grow quite big


### Testing in Docker Container
#### Basic permission check

will add maven to alpine JDK8 gradle image

```sh
docker pull gradle:5.4.1-jdk8-alpine
```

```sh
IMAGE=basic-karate-maven
docker build -t $IMAGE -f Dockerfile.alpine-jdk8-maven .
```
bas ic test
```sh
mkdir target

(docker container run --rm -v $(pwd)/src:/work/src/ -v $(pwd)/target:/work/target:rw $IMAGE ); ls -l target
```
the `karate-summary-json.txt` will  be in `target/karate-reports`

NOTE: Docker run is time consuming due to the massive amount of dependency jars.

```text
[INFO] Scanning for projects...
[INFO] 
[INFO] ----------------------< example:karate-collector >----------------------
[INFO] Building karate-collector 1.0-SNAPSHOT
[INFO] --------------------------------[ jar ]---------------------------------
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-resources-plugin/2.6/maven-resources-plugin-2.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-plugins/23/maven-plugins-23.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/22/maven-parent-22.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/11/apache-11.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-resources-plugin/2.6/maven-resources-plugin-2.6.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-compiler-plugin/3.8.1/maven-compiler-plugin-3.8.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-plugins/33/maven-plugins-33.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/33/maven-parent-33.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/21/apache-21.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-compiler-plugin/3.8.1/maven-compiler-plugin-3.8.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-surefire-plugin/2.22.2/maven-surefire-plugin-2.22.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire/2.22.2/surefire-2.22.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-surefire-plugin/2.22.2/maven-surefire-plugin-2.22.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/com/intuit/karate/karate-junit5/1.3.1/karate-junit5-1.3.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/intuit/karate/karate-parent/1.3.1/karate-parent-1.3.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/intuit/karate/karate-core/1.3.1/karate-core-1.3.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/graalvm/js/js-scriptengine/22.0.0.2/js-scriptengine-22.0.0.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/graalvm/sdk/graal-sdk/22.0.0.2/graal-sdk-22.0.0.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/graalvm/js/js/22.0.0.2/js-22.0.0.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/graalvm/regex/regex/22.0.0.2/regex-22.0.0.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/graalvm/truffle/truffle-api/22.0.0.2/truffle-api-22.0.0.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/thymeleaf/thymeleaf/3.0.14.RELEASE/thymeleaf-3.0.14.RELEASE.pom
Downloading from central: https://repo.maven.apache.org/maven2/ognl/ognl/3.1.26/ognl-3.1.26.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/oss/oss-parent/9/oss-parent-9.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/javassist/javassist/3.20.0-GA/javassist-3.20.0-GA.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/attoparser/attoparser/2.0.5.RELEASE/attoparser-2.0.5.RELEASE.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/unbescape/unbescape/1.1.6.RELEASE/unbescape-1.1.6.RELEASE.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.25/slf4j-api-1.7.25.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-parent/1.7.25/slf4j-parent-1.7.25.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/linecorp/armeria/armeria/1.18.0/armeria-1.18.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-parent/1.7.36/slf4j-parent-1.7.36.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-core/2.13.3/jackson-core-2.13.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-base/2.13.3/jackson-base-2.13.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-bom/2.13.3/jackson-bom-2.13.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-parent/2.13/jackson-parent-2.13.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/oss-parent/43/oss-parent-43.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.13.3/jackson-annotations-2.13.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.13.3/jackson-databind-2.13.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/micrometer/micrometer-core/1.9.2/micrometer-core-1.9.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/hdrhistogram/HdrHistogram/2.1.12/HdrHistogram-2.1.12.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/latencyutils/LatencyUtils/2.0.3/LatencyUtils-2.0.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/oss/oss-parent/7/oss-parent-7.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-transport/4.1.79.Final/netty-transport-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-parent/4.1.79.Final/netty-parent-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-common/4.1.79.Final/netty-common-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-buffer/4.1.79.Final/netty-buffer-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-resolver/4.1.79.Final/netty-resolver-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-codec-haproxy/4.1.79.Final/netty-codec-haproxy-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-codec/4.1.79.Final/netty-codec-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-codec-http2/4.1.79.Final/netty-codec-http2-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-handler/4.1.79.Final/netty-handler-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-transport-native-unix-common/4.1.79.Final/netty-transport-native-unix-common-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-codec-http/4.1.79.Final/netty-codec-http-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-resolver-dns/4.1.79.Final/netty-resolver-dns-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-codec-dns/4.1.79.Final/netty-codec-dns-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/reactivestreams/reactive-streams/1.0.4/reactive-streams-1.0.4.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/google/code/findbugs/jsr305/3.0.2/jsr305-3.0.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-transport-native-epoll/4.1.79.Final/netty-transport-native-epoll-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-transport-classes-epoll/4.1.79.Final/netty-transport-classes-epoll-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-resolver-dns-native-macos/4.1.79.Final/netty-resolver-dns-native-macos-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-resolver-dns-classes-macos/4.1.79.Final/netty-resolver-dns-classes-macos-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-tcnative-boringssl-static/2.0.53.Final/netty-tcnative-boringssl-static-2.0.53.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-tcnative-classes/2.0.53.Final/netty-tcnative-classes-2.0.53.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-tcnative-parent/2.0.53.Final/netty-tcnative-parent-2.0.53.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-handler-proxy/4.1.79.Final/netty-handler-proxy-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-codec-socks/4.1.79.Final/netty-codec-socks-4.1.79.Final.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/aayushatharva/brotli4j/brotli4j/1.7.1/brotli4j-1.7.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/aayushatharva/brotli4j/brotli4j-parent/1.7.1/brotli4j-parent-1.7.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/aayushatharva/brotli4j/native-linux-x86_64/1.7.1/native-linux-x86_64-1.7.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/aayushatharva/brotli4j/natives/1.7.1/natives-1.7.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpclient/4.5.13/httpclient-4.5.13.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcomponents-client/4.5.13/httpcomponents-client-4.5.13.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcomponents-parent/11/httpcomponents-parent-11.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcore/4.4.13/httpcore-4.4.13.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpcomponents-core/4.4.13/httpcomponents-core-4.4.13.pom
Downloading from central: https://repo.maven.apache.org/maven2/commons-codec/commons-codec/1.11/commons-codec-1.11.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/42/commons-parent-42.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/18/apache-18.pom
Downloading from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-classic/1.2.9/logback-classic-1.2.9.pom
Downloading from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-parent/1.2.9/logback-parent-1.2.9.pom
Downloading from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-core/1.2.9/logback-core-1.2.9.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.32/slf4j-api-1.7.32.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-parent/1.7.32/slf4j-parent-1.7.32.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/jcl-over-slf4j/1.7.32/jcl-over-slf4j-1.7.32.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/antlr/antlr4-runtime/4.9.3/antlr4-runtime-4.9.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/antlr/antlr4-master/4.9.3/antlr4-master-4.9.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/jayway/jsonpath/json-path/2.7.0/json-path-2.7.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/net/minidev/json-smart/2.4.7/json-smart-2.4.7.pom
Downloading from central: https://repo.maven.apache.org/maven2/net/minidev/accessors-smart/2.4.7/accessors-smart-2.4.7.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/ow2/asm/asm/9.1/asm-9.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/ow2/ow2/1.5/ow2-1.5.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.33/slf4j-api-1.7.33.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-parent/1.7.33/slf4j-parent-1.7.33.pom
Downloading from central: https://repo.maven.apache.org/maven2/info/cukes/cucumber-java/1.2.5/cucumber-java-1.2.5.pom
Downloading from central: https://repo.maven.apache.org/maven2/info/cukes/cucumber-jvm/1.2.5/cucumber-jvm-1.2.5.pom
Downloading from central: https://repo.maven.apache.org/maven2/info/cukes/cucumber-core/1.2.5/cucumber-core-1.2.5.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/yaml/snakeyaml/1.32/snakeyaml-1.32.pom
Downloading from central: https://repo.maven.apache.org/maven2/de/siegmar/fastcsv/2.0.0/fastcsv-2.0.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/info/picocli/picocli/4.6.1/picocli-4.6.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/github/classgraph/classgraph/4.8.149/classgraph-4.8.149.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/github/t12y/resemble/1.0.2/resemble-1.0.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/io/github/t12y/ssim/1.0.0/ssim-1.0.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/jupiter/junit-jupiter-api/5.7.2/junit-jupiter-api-5.7.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/junit-bom/5.7.2/junit-bom-5.7.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apiguardian/apiguardian-api/1.1.0/apiguardian-api-1.1.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/opentest4j/opentest4j/1.2.0/opentest4j-1.2.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/platform/junit-platform-commons/1.7.2/junit-platform-commons-1.7.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/jupiter/junit-jupiter-engine/5.7.2/junit-jupiter-engine-5.7.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/platform/junit-platform-engine/1.7.2/junit-platform-engine-1.7.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/github/javafaker/javafaker/1.0.2/javafaker-1.0.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-lang3/3.5/commons-lang3-3.5.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/41/commons-parent-41.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/yaml/snakeyaml/1.23/snakeyaml-1.23.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/github/mifmif/generex/1.0.2/generex-1.0.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/dk/brics/automaton/automaton/1.11-8/automaton-1.11-8.pom
Downloading from central: https://repo.maven.apache.org/maven2/net/masterthought/cucumber-reporting/5.6.1/cucumber-reporting-5.6.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.12.1/jackson-databind-2.12.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-base/2.12.1/jackson-base-2.12.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-bom/2.12.1/jackson-bom-2.12.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/jackson-parent/2.12/jackson-parent-2.12.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/oss-parent/41/oss-parent-41.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.12.1/jackson-annotations-2.12.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-core/2.12.1/jackson-core-2.12.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/datatype/jackson-datatype-jsr310/2.12.1/jackson-datatype-jsr310-2.12.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/module/jackson-modules-java8/2.12.1/jackson-modules-java8-2.12.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/velocity/velocity-engine-core/2.3/velocity-engine-core-2.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/velocity/velocity-engine-parent/2.3/velocity-engine-parent-2.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/velocity/velocity-master/4/velocity-master-4.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/23/apache-23.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-lang3/3.11/commons-lang3-3.11.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/51/commons-parent-51.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.7.30/slf4j-api-1.7.30.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-parent/1.7.30/slf4j-parent-1.7.30.pom
Downloading from central: https://repo.maven.apache.org/maven2/commons-collections/commons-collections/3.2.2/commons-collections-3.2.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/39/commons-parent-39.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/16/apache-16.pom
Downloading from central: https://repo.maven.apache.org/maven2/joda-time/joda-time/2.10.10/joda-time-2.10.10.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/52/commons-parent-52.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/junit-bom/5.7.1/junit-bom-5.7.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.7/commons-io-2.7.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/50/commons-parent-50.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/3.3.0/plexus-utils-3.3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/5.1/plexus-5.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/jsoup/jsoup/1.14.2/jsoup-1.14.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/google/guava/guava/30.1.1-jre/guava-30.1.1-jre.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/google/guava/guava-parent/30.1.1-jre/guava-parent-30.1.1-jre.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/google/guava/failureaccess/1.0.1/failureaccess-1.0.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/google/guava/guava-parent/26.0-android/guava-parent-26.0-android.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/google/guava/listenablefuture/9999.0-empty-to-avoid-conflict-with-guava/listenablefuture-9999.0-empty-to-avoid-conflict-with-guava.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/checkerframework/checker-qual/3.8.0/checker-qual-3.8.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/google/errorprone/error_prone_annotations/2.5.1/error_prone_annotations-2.5.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/google/errorprone/error_prone_parent/2.5.1/error_prone_parent-2.5.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/google/j2objc/j2objc-annotations/1.3/j2objc-annotations-1.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/googlecode/owasp-java-html-sanitizer/owasp-java-html-sanitizer/20211018.2/owasp-java-html-sanitizer-20211018.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/googlecode/owasp-java-html-sanitizer/parent/20211018.2/parent-20211018.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/commons-configuration/commons-configuration/1.10/commons-configuration-1.10.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/32/commons-parent-32.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/13/apache-13.pom
Downloading from central: https://repo.maven.apache.org/maven2/commons-lang/commons-lang/2.6/commons-lang-2.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/17/commons-parent-17.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/7/apache-7.pom
Downloading from central: https://repo.maven.apache.org/maven2/commons-logging/commons-logging/1.1.1/commons-logging-1.1.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-parent/5/commons-parent-5.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/4/apache-4.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/intuit/karate/karate-junit5/1.3.1/karate-junit5-1.3.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/com/intuit/karate/karate-core/1.3.1/karate-core-1.3.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/graalvm/js/js-scriptengine/22.0.0.2/js-scriptengine-22.0.0.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/graalvm/js/js/22.0.0.2/js-22.0.0.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/graalvm/sdk/graal-sdk/22.0.0.2/graal-sdk-22.0.0.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/graalvm/regex/regex/22.0.0.2/regex-22.0.0.2.jar
Downloaded from central: https://repo.maven.apache.org/maven2/ognl/ognl/3.1.26/ognl-3.1.26.jar (262 kB at 175 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/attoparser/attoparser/2.0.5.RELEASE/attoparser-2.0.5.RELEASE.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/unbescape/unbescape/1.1.6.RELEASE/unbescape-1.1.6.RELEASE.jar
Downloading from central: https://repo.maven.apache.org/maven2/io/micrometer/micrometer-core/1.9.2/micrometer-core-1.9.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/latencyutils/LatencyUtils/2.0.3/LatencyUtils-2.0.3.jar
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-transport/4.1.79.Final/netty-transport-4.1.79.Final.jar
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-common/4.1.79.Final/netty-common-4.1.79.Final.jar
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-buffer/4.1.79.Final/netty-buffer-4.1.79.Final.jar
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-codec/4.1.79.Final/netty-codec-4.1.79.Final.jar
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-codec-http2/4.1.79.Final/netty-codec-http2-4.1.79.Final.jar
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-handler/4.1.79.Final/netty-handler-4.1.79.Final.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/reactivestreams/reactive-streams/1.0.4/reactive-streams-1.0.4.jar
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-transport-native-unix-common/4.1.79.Final/netty-transport-native-unix-common-4.1.79.Final.jar
Downloading from central: https://repo.maven.apache.org/maven2/io/netty/netty-handler-proxy/4.1.79.Final/netty-handler-proxy-4.1.79.Final.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/httpcomponents/httpclient/4.5.13/httpclient-4.5.13.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/antlr/antlr4-runtime/4.9.3/antlr4-runtime-4.9.3.jar
Downloaded from central: https://repo.maven.apache.org/maven2/ch/qos/logback/logback-classic/1.2.9/logback-classic-1.2.9.jar (234 kB at 25 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/ow2/asm/asm/9.1/asm-9.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/info/cukes/cucumber-java/1.2.5/cucumber-java-1.2.5.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/yaml/snakeyaml/1.32/snakeyaml-1.32.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/ow2/asm/asm/9.1/asm-9.1.jar (122 kB at 12 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/de/siegmar/fastcsv/2.0.0/fastcsv-2.0.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/io/github/t12y/resemble/1.0.2/resemble-1.0.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/io/github/t12y/ssim/1.0.0/ssim-1.0.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/jupiter/junit-jupiter-api/5.7.2/junit-jupiter-api-5.7.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apiguardian/apiguardian-api/1.1.0/apiguardian-api-1.1.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/opentest4j/opentest4j/1.2.0/opentest4j-1.2.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/jupiter/junit-jupiter-engine/5.7.2/junit-jupiter-engine-5.7.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/com/github/javafaker/javafaker/1.0.2/javafaker-1.0.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/commons/commons-lang3/3.5/commons-lang3-3.5.jar
Downloading from central: https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.12.1/jackson-databind-2.12.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/joda-time/joda-time/2.10.10/joda-time-2.10.10.jar
Downloading from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.7/commons-io-2.7.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/3.3.0/plexus-utils-3.3.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/com/google/guava/listenablefuture/9999.0-empty-to-avoid-conflict-with-guava/listenablefuture-9999.0-empty-to-avoid-conflict-with-guava.jar
Downloading from central: https://repo.maven.apache.org/maven2/com/google/code/findbugs/jsr305/3.0.2/jsr305-3.0.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/checkerframework/checker-qual/3.8.0/checker-qual-3.8.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/com/google/errorprone/error_prone_annotations/2.5.1/error_prone_annotations-2.5.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/com/google/j2objc/j2objc-annotations/1.3/j2objc-annotations-1.3.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/checkerframework/checker-qual/3.8.0/checker-qual-3.8.0.jar (231 kB at 17 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/commons-configuration/commons-configuration/1.10/commons-configuration-1.10.jar
Downloading from central: https://repo.maven.apache.org/maven2/commons-logging/commons-logging/1.1.1/commons-logging-1.1.1.jar
[INFO] 
[INFO] --- maven-resources-plugin:2.6:resources (default-resources) @ karate-collector ---
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-api/2.0.6/maven-plugin-api-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven/2.0.6/maven-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/5/maven-parent-5.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/3/apache-3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-project/2.0.6/maven-project-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-settings/2.0.6/maven-settings-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-model/2.0.6/maven-model-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/1.4.1/plexus-utils-1.4.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/1.0.11/plexus-1.0.11.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-container-default/1.0-alpha-9-stable-1/plexus-container-default-1.0-alpha-9-stable-1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-containers/1.0.3/plexus-containers-1.0.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/1.0.4/plexus-1.0.4.pom
Downloading from central: https://repo.maven.apache.org/maven2/junit/junit/3.8.1/junit-3.8.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/1.0.4/plexus-utils-1.0.4.pom
Downloading from central: https://repo.maven.apache.org/maven2/classworlds/classworlds/1.1-alpha-2/classworlds-1.1-alpha-2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-profile/2.0.6/maven-profile-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-artifact-manager/2.0.6/maven-artifact-manager-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-repository-metadata/2.0.6/maven-repository-metadata-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-artifact/2.0.6/maven-artifact-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-registry/2.0.6/maven-plugin-registry-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-core/2.0.6/maven-core-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-parameter-documenter/2.0.6/maven-plugin-parameter-documenter-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/reporting/maven-reporting-api/2.0.6/maven-reporting-api-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/reporting/maven-reporting/2.0.6/maven-reporting-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/doxia/doxia-sink-api/1.0-alpha-7/doxia-sink-api-1.0-alpha-7.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/doxia/doxia/1.0-alpha-7/doxia-1.0-alpha-7.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-error-diagnostics/2.0.6/maven-error-diagnostics-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/commons-cli/commons-cli/1.0/commons-cli-1.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-descriptor/2.0.6/maven-plugin-descriptor-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-interactivity-api/1.0-alpha-4/plexus-interactivity-api-1.0-alpha-4.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-monitor/2.0.6/maven-monitor-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/classworlds/classworlds/1.1/classworlds-1.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/2.0.5/plexus-utils-2.0.5.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/2.0.6/plexus-2.0.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-filtering/1.1/maven-filtering-1.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-components/17/maven-shared-components-17.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/21/maven-parent-21.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/10/apache-10.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/1.5.15/plexus-utils-1.5.15.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/2.0.2/plexus-2.0.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-interpolation/1.12/plexus-interpolation-1.12.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-components/1.1.14/plexus-components-1.1.14.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-build-api/0.0.4/plexus-build-api-0.0.4.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/spice/spice-parent/10/spice-parent-10.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/forge/forge-parent/3/forge-parent-3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/1.5.8/plexus-utils-1.5.8.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-interpolation/1.13/plexus-interpolation-1.13.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-components/1.1.15/plexus-components-1.1.15.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/2.0.3/plexus-2.0.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-api/2.0.6/maven-plugin-api-2.0.6.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-project/2.0.6/maven-project-2.0.6.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-profile/2.0.6/maven-profile-2.0.6.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-registry/2.0.6/maven-plugin-registry-2.0.6.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-artifact-manager/2.0.6/maven-artifact-manager-2.0.6.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-core/2.0.6/maven-core-2.0.6.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-parameter-documenter/2.0.6/maven-plugin-parameter-documenter-2.0.6.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-profile/2.0.6/maven-profile-2.0.6.jar (35 kB at 187 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/reporting/maven-reporting-api/2.0.6/maven-reporting-api-2.0.6.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-repository-metadata/2.0.6/maven-repository-metadata-2.0.6.jar
Downloading from central: https://repo.maven.apache.org/maven2/commons-cli/commons-cli/1.0/commons-cli-1.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-descriptor/2.0.6/maven-plugin-descriptor-2.0.6.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/reporting/maven-reporting-api/2.0.6/maven-reporting-api-2.0.6.jar (9.9 kB at 32 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/doxia/doxia-sink-api/1.0-alpha-7/doxia-sink-api-1.0-alpha-7.jar (5.9 kB at 19 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/classworlds/classworlds/1.1/classworlds-1.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-interactivity-api/1.0-alpha-4/plexus-interactivity-api-1.0-alpha-4.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-artifact/2.0.6/maven-artifact-2.0.6.jar
Downloaded from central: https://repo.maven.apache.org/maven2/commons-cli/commons-cli/1.0/commons-cli-1.0.jar (30 kB at 84 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-model/2.0.6/maven-model-2.0.6.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-descriptor/2.0.6/maven-plugin-descriptor-2.0.6.jar (37 kB at 98 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-monitor/2.0.6/maven-monitor-2.0.6.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-container-default/1.0-alpha-9-stable-1/plexus-container-default-1.0-alpha-9-stable-1.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-model/2.0.6/maven-model-2.0.6.jar (86 kB at 199 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/junit/junit/3.8.1/junit-3.8.1.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-artifact/2.0.6/maven-artifact-2.0.6.jar (87 kB at 200 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/2.0.5/plexus-utils-2.0.5.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-settings/2.0.6/maven-settings-2.0.6.jar (49 kB at 111 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-filtering/1.1/maven-filtering-1.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-build-api/0.0.4/plexus-build-api-0.0.4.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-filtering/1.1/maven-filtering-1.1.jar (43 kB at 83 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-interpolation/1.13/plexus-interpolation-1.13.jar
Downloaded from central: https://repo.maven.apache.org/maven2/junit/junit/3.8.1/junit-3.8.1.jar (121 kB at 231 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-build-api/0.0.4/plexus-build-api-0.0.4.jar (6.8 kB at 13 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/2.0.5/plexus-utils-2.0.5.jar (223 kB at 413 kB/s)
[INFO] Using 'UTF-8' encoding to copy filtered resources.
[INFO] skip non existing resourceDirectory /work/src/main/resources
[INFO] 
[INFO] --- maven-compiler-plugin:3.8.1:compile (default-compile) @ karate-collector ---
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-api/3.0/maven-plugin-api-3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven/3.0/maven-3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/15/maven-parent-15.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/6/apache-6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-model/3.0/maven-model-3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/2.0.4/plexus-utils-2.0.4.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-artifact/3.0/maven-artifact-3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/sisu/sisu-inject-plexus/1.4.2/sisu-inject-plexus-1.4.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/sisu/inject/guice-plexus/1.4.2/guice-plexus-1.4.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/sisu/inject/guice-bean/1.4.2/guice-bean-1.4.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/sisu/sisu-inject/1.4.2/sisu-inject-1.4.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/sisu/sisu-parent/1.4.2/sisu-parent-1.4.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/forge/forge-parent/6/forge-parent-6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-component-annotations/1.7.1/plexus-component-annotations-1.7.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-containers/1.7.1/plexus-containers-1.7.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/4.0/plexus-4.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/forge/forge-parent/10/forge-parent-10.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-classworlds/2.2.3/plexus-classworlds-2.2.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/sisu/sisu-inject-bean/1.4.2/sisu-inject-bean-1.4.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/sisu/sisu-guice/2.1.7/sisu-guice-2.1.7.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-core/3.0/maven-core-3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-settings/3.0/maven-settings-3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-settings-builder/3.0/maven-settings-builder-3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-interpolation/1.14/plexus-interpolation-1.14.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-components/1.1.18/plexus-components-1.1.18.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/2.0.7/plexus-2.0.7.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-sec-dispatcher/1.3/plexus-sec-dispatcher-1.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/spice/spice-parent/12/spice-parent-12.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/forge/forge-parent/4/forge-parent-4.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/1.5.5/plexus-utils-1.5.5.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-cipher/1.4/plexus-cipher-1.4.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-repository-metadata/3.0/maven-repository-metadata-3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-model-builder/3.0/maven-model-builder-3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-aether-provider/3.0/maven-aether-provider-3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/aether/aether-api/1.7/aether-api-1.7.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/aether/aether-parent/1.7/aether-parent-1.7.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/aether/aether-util/1.7/aether-util-1.7.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/aether/aether-impl/1.7/aether-impl-1.7.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/aether/aether-spi/1.7/aether-spi-1.7.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-utils/3.2.1/maven-shared-utils-3.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-components/30/maven-shared-components-30.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/30/maven-parent-30.pom
Downloading from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.5/commons-io-2.5.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-incremental/1.1/maven-shared-incremental-1.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-components/19/maven-shared-components-19.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/23/maven-parent-23.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-api/2.2.1/maven-plugin-api-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven/2.2.1/maven-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/11/maven-parent-11.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/5/apache-5.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-core/2.2.1/maven-core-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-settings/2.2.1/maven-settings-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-model/2.2.1/maven-model-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-interpolation/1.11/plexus-interpolation-1.11.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-parameter-documenter/2.2.1/maven-plugin-parameter-documenter-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-jdk14/1.5.6/slf4j-jdk14-1.5.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-parent/1.5.6/slf4j-parent-1.5.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.5.6/slf4j-api-1.5.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/jcl-over-slf4j/1.5.6/jcl-over-slf4j-1.5.6.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-profile/2.2.1/maven-profile-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-artifact/2.2.1/maven-artifact-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-repository-metadata/2.2.1/maven-repository-metadata-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-error-diagnostics/2.2.1/maven-error-diagnostics-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-project/2.2.1/maven-project-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-artifact-manager/2.2.1/maven-artifact-manager-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/backport-util-concurrent/backport-util-concurrent/3.1/backport-util-concurrent-3.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-registry/2.2.1/maven-plugin-registry-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-descriptor/2.2.1/maven-plugin-descriptor-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-monitor/2.2.1/maven-monitor-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-utils/0.1/maven-shared-utils-0.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-components/18/maven-shared-components-18.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/google/code/findbugs/jsr305/2.0.1/jsr305-2.0.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-java/0.9.10/plexus-java-0.9.10.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-languages/0.9.10/plexus-languages-0.9.10.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/ow2/asm/asm/6.2/asm-6.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/thoughtworks/qdox/qdox/2.0-M9/qdox-2.0-M9.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-compiler-api/2.8.4/plexus-compiler-api-2.8.4.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-compiler/2.8.4/plexus-compiler-2.8.4.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-components/4.0/plexus-components-4.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/3.0.22/plexus-utils-3.0.22.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus/3.3.1/plexus-3.3.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/spice/spice-parent/17/spice-parent-17.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-compiler-manager/2.8.4/plexus-compiler-manager-2.8.4.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-compiler-javac/2.8.4/plexus-compiler-javac-2.8.4.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-compilers/2.8.4/plexus-compilers-2.8.4.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-api/3.0/maven-plugin-api-3.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-model/3.0/maven-model-3.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/sisu/sisu-guice/2.1.7/sisu-guice-2.1.7-noaop.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/sisu/sisu-inject-plexus/1.4.2/sisu-inject-plexus-1.4.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/sisu/sisu-inject-bean/1.4.2/sisu-inject-bean-1.4.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-core/3.0/maven-core-3.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-repository-metadata/3.0/maven-repository-metadata-3.0.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/aether/aether-spi/1.7/aether-spi-1.7.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/aether/aether-api/1.7/aether-api-1.7.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-interpolation/1.14/plexus-interpolation-1.14.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/sonatype/aether/aether-spi/1.7/aether-spi-1.7.jar (14 kB at 32 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-classworlds/2.2.3/plexus-classworlds-2.2.3.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-sec-dispatcher/1.3/plexus-sec-dispatcher-1.3.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/sonatype/aether/aether-api/1.7/aether-api-1.7.jar (74 kB at 154 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-cipher/1.4/plexus-cipher-1.4.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-classworlds/2.2.3/plexus-classworlds-2.2.3.jar (46 kB at 95 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-utils/3.2.1/maven-shared-utils-3.2.1.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/sonatype/aether/aether-util/1.7/aether-util-1.7.jar (108 kB at 220 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/commons-io/commons-io/2.5/commons-io-2.5.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-component-annotations/1.7.1/plexus-component-annotations-1.7.1.jar (4.3 kB at 8.6 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-incremental/1.1/maven-shared-incremental-1.1.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/sonatype/plexus/plexus-cipher/1.4/plexus-cipher-1.4.jar (13 kB at 25 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/ow2/asm/asm/6.2/asm-6.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/com/thoughtworks/qdox/qdox/2.0-M9/qdox-2.0-M9.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-compiler-javac/2.8.4/plexus-compiler-javac-2.8.4.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-java/0.9.10/plexus-java-0.9.10.jar (39 kB at 60 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-compiler-manager/2.8.4/plexus-compiler-manager-2.8.4.jar (4.7 kB at 7.0 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-compiler-javac/2.8.4/plexus-compiler-javac-2.8.4.jar (21 kB at 31 kB/s)
```
and another batch of dependencies for surefire:

```text
[INFO] 
[INFO] --- maven-surefire-plugin:2.22.2:test (default-test) @ karate-collector ---
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/maven-surefire-common/2.22.2/maven-surefire-common-2.22.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugin-tools/maven-plugin-annotations/3.5.2/maven-plugin-annotations-3.5.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugin-tools/maven-plugin-tools/3.5.2/maven-plugin-tools-3.5.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/31/maven-parent-31.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/apache/19/apache-19.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire-api/2.22.2/surefire-api-2.22.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire-logger-api/2.22.2/surefire-logger-api-2.22.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire-booter/2.22.2/surefire-booter-2.22.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/junit/junit/4.12/junit-4.12.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/hamcrest/hamcrest-parent/1.3/hamcrest-parent-1.3.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/reporting/maven-reporting-api/3.0/maven-reporting-api-3.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/shared/maven-shared-components/15/maven-shared-components-15.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-parent/16/maven-parent-16.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-toolchain/2.2.1/maven-toolchain-2.2.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/com/thoughtworks/qdox/qdox/2.0-M8/qdox-2.0-M8.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/maven-surefire-common/2.22.2/maven-surefire-common-2.22.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/plugin-tools/maven-plugin-annotations/3.5.2/maven-plugin-annotations-3.5.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-api/2.2.1/maven-plugin-api-2.2.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire-logger-api/2.22.2/surefire-logger-api-2.22.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire-api/2.22.2/surefire-api-2.22.2.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-api/2.2.1/maven-plugin-api-2.2.1.jar (12 kB at 106 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-artifact/2.2.1/maven-artifact-2.2.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire-booter/2.22.2/surefire-booter-2.22.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/junit/junit/4.12/junit-4.12.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-project/2.2.1/maven-project-2.2.1.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-utils/1.5.15/plexus-utils-1.5.15.jar (228 kB at 881 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-settings/2.2.1/maven-settings-2.2.1.jar
Downloaded from central: https://repo.maven.apache.org/maven2/junit/junit/4.12/junit-4.12.jar (315 kB at 1.0 MB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-plugin-registry/2.2.1/maven-plugin-registry-2.2.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-model/2.2.1/maven-model-2.2.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-jdk14/1.5.6/slf4j-jdk14-1.5.6.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/codehaus/plexus/plexus-interpolation/1.11/plexus-interpolation-1.11.jar (51 kB at 123 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.5.6/slf4j-api-1.5.6.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-repository-metadata/2.2.1/maven-repository-metadata-2.2.1.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/slf4j/slf4j-api/1.5.6/slf4j-api-1.5.6.jar (22 kB at 48 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-error-diagnostics/2.2.1/maven-error-diagnostics-2.2.1.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-core/2.2.1/maven-core-2.2.1.jar (178 kB at 368 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-monitor/2.2.1/maven-monitor-2.2.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-toolchain/2.2.1/maven-toolchain-2.2.1.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/slf4j/jcl-over-slf4j/1.5.6/jcl-over-slf4j-1.5.6.jar (17 kB at 18 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/com/thoughtworks/qdox/qdox/2.0-M8/qdox-2.0-M8.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-monitor/2.2.1/maven-monitor-2.2.1.jar (10 kB at 11 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/reporting/maven-reporting-api/3.0/maven-reporting-api-3.0.jar (11 kB at 12 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/maven-repository-metadata/2.2.1/maven-repository-metadata-2.2.1.jar (26 kB at 27 kB/s)
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire-junit-platform/2.22.2/surefire-junit-platform-2.22.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire-providers/2.22.2/surefire-providers-2.22.2.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/platform/junit-platform-launcher/1.3.1/junit-platform-launcher-1.3.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/apiguardian/apiguardian-api/1.0.0/apiguardian-api-1.0.0.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/platform/junit-platform-engine/1.3.1/junit-platform-engine-1.3.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/platform/junit-platform-commons/1.3.1/junit-platform-commons-1.3.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/opentest4j/opentest4j/1.1.1/opentest4j-1.1.1.pom
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/platform/junit-platform-commons/1.3.1/junit-platform-commons-1.3.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/platform/junit-platform-engine/1.3.1/junit-platform-engine-1.3.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire-junit-platform/2.22.2/surefire-junit-platform-2.22.2.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/junit/platform/junit-platform-launcher/1.3.1/junit-platform-launcher-1.3.1.jar
Downloading from central: https://repo.maven.apache.org/maven2/org/apiguardian/apiguardian-api/1.0.0/apiguardian-api-1.0.0.jar
Downloaded from central: https://repo.maven.apache.org/maven2/org/opentest4j/opentest4j/1.1.1/opentest4j-1.1.1.jar (7.1 kB at 178 kB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/junit/platform/junit-platform-engine/1.3.1/junit-platform-engine-1.3.1.jar (135 kB at 1.2 MB/s)
Downloaded from central: https://repo.maven.apache.org/maven2/org/apache/maven/surefire/surefire-junit-platform/2.22.2/surefire-junit-platform-2.22.2.jar (66 kB at 545 kB/s)
```

the test concludes with
```text
---------------------------------------------------------
feature: classpath:example/feature/jsonplaceholder.feature
scenarios:  2 | passed:  2 | failed:  0 | time: 2.0639
---------------------------------------------------------

23:44:30.710 [main] INFO  com.intuit.karate.Suite - <<pass>> feature 1 of 1 (0 remaining) classpath:example/feature/jsonplaceholder.feature
Karate version: 1.3.1
======================================================
elapsed:   7.61 | threads:    1 | thread time: 2.06 
features:     1 | skipped:    0 | efficiency: 0.27
scenarios:    2 | passed:     2 | failed: 0
======================================================

HTML report: (paste into browser to view) | Karate version: 1.3.1
file:///work/target/karate-reports/karate-summary.html
===================================================================

[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 10.182 s - in example.ExampleTest
[INFO] 
[INFO] Results:
[INFO] 
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
[INFO] 
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  53.913 s
[INFO] Finished at: 2023-02-09T23:44:33Z
[INFO] ------------------------------------------------------------------------
```
#### Gradle
```sh
IMAGE=basic-karate-gradle
docker build -t $IMAGE -f Dockerfile.alpine-jdk8-gradle .
```
* NOTE: run as root to prevent dealing with error:

```text
Could not create service of type ScriptPluginFactory using BuildScopeServices.createScriptPluginFactory().
```


```sh
docker container run --rm -u root -v $(pwd)/src:/work/src/ -v $(pwd)/build:/work/build:rw -it $IMAGE
```
```
Welcome to Gradle 5.4.1!

Here are the highlights of this release:
 - Run builds with JDK12
 - New API for Incremental Tasks
 - Updates to native projects, including Swift 5 support

For more details see https://docs.gradle.org/5.4.1/release-notes.html

Starting a Gradle Daemon (subsequent builds will be faster)
> Task :compileJava NO-SOURCE
> Task :processResources NO-SOURCE
> Task :classes UP-TO-DATE
> Task :compileTestJava
> Task :processTestResources
> Task :testClasses
> Task :test
BUILD SUCCESSFUL in 3m 2s
3 actionable tasks: 3 executed
```

the `karate-summary-json.txt` will  be in `build/karate-reports`

NOTE: gradle run is quite a bit slower than maven

### Cleanup

```sh
sudo rm -fr target
```
### See Also

  * https://github.com/fertekton/karate-api/blob/main/src/test/java/Yevo/BackEndTests.java
  * https://stackoverflow.com/questions/53272230/could-not-create-service-of-type-scriptpluginfactory-using-buildscopeservices-cr
  * https://github.com/gradle/gradle/issues/8436
  * https://github.com/figroc/tensorflow-serving-client/issues/11

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
