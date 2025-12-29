### Info

### Testing
```sh
docker pull kibana:7.17.7
docker pull elasticsearch:7.17.7
docker pull docker.elastic.co/apm/apm-server:7.17.7
docker pull eclipse-temurin:8-jre-alpine
```
optionally
```sh
docker pull alpine:3.14

```
```sh
for app in app1 app2; do pushd $app; mvn -Pproguard package;popd; done
```
```sh
find . -iname 'example*jar'
```
```text
./app2/target/example.server_proguard_base.jar
./app2/target/example.server.jar
./app1/target/example.relay.jar
./app1/target/example.relay_proguard_base.jar
```

```sh
df -h /
```
```text
Filesystem      Size  Used Avail Use% Mounted on
/dev/sda1        22G   15G  6.1G  71% /
```

```sh
docker-compose up --build --detach
```

```text
Creating elasticsearch ... done
Creating kibana        ... done
Creating apm-server    ... done
Creating app2          ... done
Creating app1          ... done

```
```sh
docker-compose ps
```
```text
--------------------------------------------------------------------------------
apm-server      /usr/bin/tini --          Up (healthy)   0.0.0.0:8200-          
                /usr/loca ...                            >8200/tcp,:::8200-     
                                                         >8200/tcp              
app1            java                      Up (healthy)   0.0.0.0:8080-          
                -javaagent:/home/elas                    >8080/tcp,:::8080-     
                ...                                      >8080/tcp              
app2            /bin/sh -c java           Up (healthy)   8080/tcp               
                -javaagent ...                                                  
elasticsearch   /bin/tini --              Up (healthy)   0.0.0.0:9200-          
                /usr/local/bi ...                        >9200/tcp,:::9200-     
                                                         >9200/tcp, 9300/tcp    
kibana          /bin/tini --              Up (healthy)   0.0.0.0:5601-          
                /usr/local/bi ...                        >5601/tcp,:::5601-     
                                                         >5601/tcp      
```
			
```sh
docker-compose logs app1
```
```text
app1             | 2025-12-27 05:43:34.551  INFO 1 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8080 (http) with context path ''
app1             | 2025-12-27 05:43:34.592  INFO 1 --- [           main] example.Application                      : Started Application in 22.812 seconds (JVM running for 43.943)
```
NOTE: if you see 
```text
Error: Could not find or load main class example.Applciation
```
check the Spring Boot application manifest for entry method

```sh
cd app1
mvn package 
jar tvf target/example.relay.jar  | grep -i META-INF/MANIFEST.MF
```
```text
   403 Fri Dec 26 18:24:04 EST 2025 META-INF/MANIFEST.MF
```
```sh
jar xvf target/example.relay.jar  META-INF/MANIFEST.MF
```
```text
inflated: META-INF/MANIFEST.MF
```
```sh
cat META-INF/MANIFEST.MF
```
```text
Manifest-Version: 1.0
Spring-Boot-Classpath-Index: BOOT-INF/classpath.idx
Implementation-Title: relay
Implementation-Version: 0.6.0-SNAPSHOT
Start-Class: example.Application
Spring-Boot-Classes: BOOT-INF/classes/
Spring-Boot-Lib: BOOT-INF/lib/
Build-Jdk-Spec: 1.8
Spring-Boot-Version: 2.3.4.RELEASE
Created-By: Maven Jar Plugin 3.2.0
Main-Class: org.springframework.boot.loader.JarLauncher
```
if you need to rebuild the cluster after confguration change, it is necessary to remove it first:

```sh
docker-compose stop; docker-compose rm -f; docker-compose up --build --detach
```
### Testing

To introduce the application error, change `app2/src/main/java/example/controller/Controller.java`

```java
@GetMapping("/{id}")
        public ResponseEntity<User> getUser(@PathVariable("id") long id) {

```
```sh
curl -s http://localhost:8080/users/1 | jq '.'
```
```JSON
{
  "email": "alice@example.com",
  "id": 1,
  "name": "Alice"
}

```
to
```java
@GetMapping("/{id}")
        public ResponseEntity<User> getUser(@PathVariable long id) {
```
> NOTE: this is a Java version specific issue 

```sh
curl -v http://localhost:8080/users/1
```
```text
*   Trying 127.0.0.1:8080...
* Connected to localhost (127.0.0.1) port 8080 (#0)
> GET /users/1 HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.81.0
> Accept: */*
> 
* Mark bundle as not supporting multiuse
< HTTP/1.1 502 
< Content-Length: 0
< Date: Sat, 27 Dec 2025 00:03:59 GMT
< 
* Connection #0 to host localhost left intact
```
```sh
curl -s -X POST -H 'Content-type: application/json' -d '{"id":2, "name":"Bob"}' http://localhost:8080/users |jq '.'
```
```JSON
{
  "email": null,
  "id": 2,
  "name": "Bob"
}
```
```sh
curl -s -X PUT -H 'Content-type: application/json' -d '{"id":2, "name":""}' http://localhost:8080/users | jq '.'
```
```JSON
{
  "timestamp": "2025-12-27T06:01:29.718+00:00",
  "status": 405,
  "error": "Method Not Allowed",
  "message": "",
  "path": "/users"
}
```
```sh
curl -s -X PUT -H 'Content-type: application/json' http://localhost:8080/users/1 | jq '.'
```
```JSON
{
  "timestamp": "2025-12-27T06:02:19.164+00:00",
  "status": 400,
  "error": "Bad Request",
  "message": "",
  "path": "/users/1"
}
```
![Transaction - Obfuscated](https://github.com/sergueik/springboot_study/blob/master/basic-proguard-elk-cluster/screenshots/transaction-obfuscated.png)

```sh
df  -h /
```
```text
Filesystem      Size  Used Avail Use% Mounted on
/dev/sda1        22G   15G  5.5G  74% /
```

### Cleanup
```sh
docker-compose stop
```
```text
Stopping app1          ... done
Stopping app2          ... done
Stopping apm-server    ... done
Stopping kibana        ... done
Stopping elasticsearch ... done


```
```sh
docker-compose rm -f
```
```text
Going to remove app1, app2, apm-server, kibana, elasticsearch
Removing app1          ... done
Removing app2          ... done
Removing apm-server    ... done
Removing kibana        ... done
Removing elasticsearch ... done
```
```sh
docker system prune -f
```
```sh
docker image rm basic-proguard-elk-cluster_elasticsearch basic-proguard-elk-cluster_apm-server basic-proguard-elk-cluster_kibana  basic-proguard-elk-cluster_app2 basic-proguard-elk-cluster_app1
```
```sh
docker image ls | grep -E '(temurin|apm|kibana|elastic)'
```
```text
eclipse-temurin                    11-jre-alpine     f135099692b9   6 weeks ago   169MB
eclipse-temurin                    8-jre-alpine      4cfc34e7cef0   6 weeks ago   150MB
elasticsearch                      7.17.7            ec0817395263   3 years ago   619MB
docker.elastic.co/apm/apm-server   7.17.7            79ccb403f58f   3 years ago   258MB
kibana                             7.17.7            47c5b6ca1535   3 years ago   799MB
```

### Trace Clear Text
to reproduce the tracing pattern without code obfuscation, simply rebuild (repackage - proguard operates on jars)

```sh
for app in app1 app2; do pushd $app; mvn clean package;popd; done
```
and recreate cluster
```sh
docker-compose stop; docker-compose rm -f; docker-compose up --build --detach
```

then hit the  endpoint
```sh
curl -v http://localhost:8080/users/1
```

and view traces in Kubana

![Transaction - Clear Text](https://github.com/sergueik/springboot_study/blob/master/basic-proguard-elk-cluster/screenshots/transaction-cleartext.png)


### See Also

  * https://www.elastic.co/docs/reference/apm/agents/java

### Siren Song Part
### Info
  * https://www.wudsn.com/productions/www/site/news/2023/2023-05-08-microservices-01.pdf
  * https://dev.to/indika_wimalasuriya/amazon-prime-videos-90-cost-reduction-throuh-moving-to-monolithic-k4a
  * https://riak.com/posts/technical/microservices-please-dont/

Microservices Aren’t Always Better, theyare almost always Worse

Microservices often introduce unexpected operational and architectural complexity —
higher than what most teams anticipate. Common complaints include:

* Network and orchestration overhead (latency, debugging, serialization cost).
* Testing & deployment complexity across many services.
* Cost and maintenance overhead dwarfs benefits for small/medium teams


* Distributed systems are inherently harder — debugging, performance, consistency, data flow.
* Many teams adopt microservices without clear boundaries or tooling, creating complexity rather than solving problems.
* Some “microservices” simply end up as distributed monoliths — loosely coupled in name but tightly interdependent in reality
* In tightly coupled workloads with high inter-component data exchange (like video processing), in-process communication beats networked workflows
There’s a growing discussion around modular monoliths — internal modularization without distribution — as a balanced approach.


Step Functions are:

* optimized for coarse-grained business workflows
* priced per state transition
* introduce serialization, persistence, and latency at every step

CPU-heavy, stateful, high-fan-out, low-latency, data-intensive pipeline processing - cannot be done through step functions.
Using Step Functions for tight inner loops is like using a BPM engine to schedule CPU instructions - *will* work — but it is slow and expensive by design

S3 is:

* optimized for durable object storage
* priced per PUT/GET
* has ultra high latency relative to memory or IPC or disk io

using S3 as an intermediate bus for video frames analysis

* serialization overhead
* cold data paths
* cost explosion proportional to frame count

S3 was economically optimal for AWS, not for Prime Video

Early success masking scaling failure

At small scale:

* Step Functions look elegant
* costs appear negligible
* operational complexity seems reduced
* cost is under-modeled during design
* architecture reviews validate correctness, not economic efficiency

At production scale:

* state transitions explode
* data movement dominates cost
* latency compound


* distributed ≠ scalable
* managed ≠ cheap
* serverless ≠ efficient


The fix was obvious to systems engineers

What they replaced it with is telling:

* In-process orchestration
* plain control flow
* function calls (no network or storage)
* shared memory
* local scheduling

This is not innovative — it is how high-performance systems have always worked

Parameterized replicas

Instead of:

sophisticated dynamic distributed scheduling


Resolution basically was: *replaced a managed serverless workflow engine / s3 with a for-loop*

he Siren Song of Microservices

Let’s walk through the canonical Tier-2 cases and decode them without vendor spin.

2. Canonical Tier-2 cases (decoded)
🔹 Netflix (early years)

What’s usually told

“Netflix pioneered microservices”

“Independent scaling!”

“Resilience!”

What actually happened

Netflix split because the monolith could not survive rapid team growth

They paid:

extreme observability cost

heavy internal tooling burden

custom reliability engineering

Hidden truth
Netflix didn’t “win” because microservices are cheap or simple —
they won because they could afford:

chaos engineering

bespoke tooling

elite SRE teams

Literal reading is false: microservices didn’t simplify Netflix — they taxed them heavily.

🔹 Twitter (pre-2016)

What’s usually told

“Twitter suffered scaling issues”

What actually happened

Over-fragmented services

Tight coupling over RPC

Latency cascades

Deployment paralysis

They re-centralized aggressively.

This is a pure “distributed monolith” failure.

Lesson
Microservices increased coordination cost faster than scaling benefits.

🔹 Uber (2014–2018)

What’s usually told

“Uber had hundreds of microservices”

What actually happened

Explosive service count

Weak ownership boundaries

Cascading failures

Enormous operational cost

Uber publicly:

killed internal RPC frameworks

reduced service count

invested in “macro-services”

Uber didn’t abandon microservices — they rejected literal micro-ness.

🔹 Spotify

What’s usually told

“Squads + microservices = autonomy”

What actually happened

High coordination overhead

Inconsistent APIs

Duplication everywhere

Spotify’s own internal postmortems admit:

the model works socially

but technical consistency suffered

Microservices solved org problems while creating technical debt.

🔹 Amazon (outside Prime Video)

Even Amazon has internal guidance now that:

service count must be capped

data locality matters

synchronous fan-out is dangerous

Which is ironic, given what AWS markets externally.


