### Info

This directory contains a fragment replica of [javaagent-bytebuddy](https://github.com/ShehanPerera/javaagent-bytebuddy) project by ShehanPerera: bare bones Java Agent agent
that uses [byte-buddy](https://bytebuddy.net/#/) code generation and manipulation library for attaching an agent to the "business application" jar and
Maven [shade plugin](https://maven.apache.org/plugins/maven-shade-plugin/usage.html) `org.apache.maven.plugins.shade.resource.ManifestResourceTransformer` 
to write the main manifest `MANIFEST.MF` entries (to prevent from "no main manifest attribute, in target application jar" error).

This is a work in progress experiment the same with Docker linked to dedicated Docker insance serving for agent messages sink and to replace a vanilla app jar with an equally vanilla Springboot REST server.


### Testing

NOTE: the instrumented application is not assumed to share the code base with agent, so we take them apart.
 * Build agent
```sh
pushd  agent
mvn clean package
popd
```
 * Build application
```sh
pushd application
mvn clean package
popd
```
 * run aplication directly: create a local file:

```sh
cat>index.html
<html>
<body></body>
</html>
^D
```

  * read the file it e.g.
```sh
curl -XGET http://localhost:8085/index.html
```
this will print the file back
```html
<html>
<body></body>
</html>
```
 * run instrumented application bundle:

```sh
java -javaagent:agent/target/agent.jar -jar application/target/application.jar 8085
```


This will invoke the following code that logs detailed instrumentation about a specific method matched my name `processRequest` when that method is called the instrumented class to the console:
```java
private final static String methodName = "processRequest";
private final static String methodMatcher = ".*" + "( |\\.)" + methodName + "\\(" + ".*";
@Advice.OnMethodEnter
static long enter(@Advice.Origin String method) throws Exception {
  if (method.matches(methodMatcher)) {
    long start = System.currentTimeMillis();
```
```java    
@Advice.OnMethodExit
static void exit(@Advice.Origin String method, @Advice.Enter long start) throws Exception {
  if (method.matches(methodMatcher)) {
    long end = System.currentTimeMillis();
    System.out.println(method +  " took " + (end - start) + " milliseconds ");
}
}
```
this will print, together with application own logging messages, the following
```sh
private void example.httpRequestHandler.processRequest() X took 3 milliseconds
```
### Run on Docker
* Deploy both jars into Docker container together (work in progress)

* repackage or use already packaged earlier
```sh
pushd  agent
mvn package
cd  ../appplication
mvn package
popd
```

* containerize
```sh
docker build -t 'application-agent' .
```
* launch and remove container
```sh
docker run -p 8085 --rm 'application-agent'
docker image rm 'application-agent'
```
invoke the web server

```sh
curl http://localhost:8085/index.html
```
```
<html>
<body></body>
</html>
```
* observe the istrumentation taking place:
```sh
Launched agent for time computation
New connection accepted /127.0.0.1:41410
input: GET /index.html HTTP/1.1
requested path: ./index.html
input: Host: localhost:8085
input: User-Agent: curl/7.58.0
input: Accept: */*
input: 
```
and the extra info from the agent:
```sh
private void example.httpRequestHandler.processRequest() X took 23 milliseconds
```
* terminate from another terminal:
```sh
docker container ls -a |  grep 'application-agent' | awk '{print $1}' | xargs -IX docker stop X
```
```sh
2e646c47acd0
```
```sh

docker container prune -f 
docker image prune -f  
docker image rm application-agent
```
### See Also

  * https://docs.oracle.com/javase/7/docs/api/java/lang/instrument/package-summary.html
  * https://docs.appdynamics.com/display/PRO45/Install+the+Java+Agent
  * https://www.oracle.com/technetwork/articles/java/javamanagement-140525.html
  * [multiple services in a Docker container](https://docs.docker.com/config/containers/multi-service_container/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
