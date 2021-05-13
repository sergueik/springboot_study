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
  * start service
```sh
java -jar application/target/application.jar
```
this will respond with
```sh
httpServer running on port 8500
```
  * read the file it e.g.

```sh
curl -XGET http://localhost:8500/index.html
```
this will print the file back
```html
<html>
<body></body>
</html>
```
 * run instrumented application bundle passing a different TCP port to listen:

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
### Adding custom headers

#### Non-Instrumented

* launch
```cmd
java -jar application\target\application.jar 8085
```
* test

```sh
curl -i http://localhost:8500/index.html
```
produces
```sh
Server: Simple Java Http Server
Content-type: text/html
Content-Length: 32
name: some_name
val: some_value
staticinfo: c7ff2b9a-d263-47c6-94f2-cec1d6537f20
class: class example.Header

<html>
<body></body>
</html>

```
#### Instrumented
* launch
```cmd
java -javaagent:agent/target/agent.jar -jar application\target\application.jar 8085
```
* test
```
$ curl -i http://localhost:8085/index.html
```
(note the flags `curl` recongizes are platform-specfic)

prints

```sh
Server: Simple Java Http Server
Content-type: text/html
Content-Length: 32
name: new_name
val: new_value
staticinfo: bfa7d47c-265d-4959-b4d3-1b8beca52890
class: class example.Header
```
```html
<html>
<body></body>
</html>
```
it will also print debugging info as:

```sh

getting value of field: name
This is new name: new_name
received: new_name
getting value of field: val
This is new value: new_value
received: new_value
```

This is achieved through the following method chain:
```java

		new AgentBuilder.Default()

				.with(new AgentBuilder.InitializationStrategy.SelfInjection.Eager())
				.type((ElementMatchers.any()))
				.transform((builder, typeDescription, classLoader, module) -> {

					try {

						return builder
								.defineMethod("myGetVal", String.class, Visibility.PUBLIC)
								.intercept(MethodDelegation.to(AddMethod.class))
								.method(ElementMatchers.nameContains("getVal"))
								.intercept(SuperMethodCall.INSTANCE.andThen(
										MethodCall.invoke(Class.forName("javaagent.AddMethod")
												.getMethod("myGetVal"))));
					} catch (ClassNotFoundException | NoSuchMethodException e) {
						System.err
								.println("Exception in dynamic method call: " + e.toString());
						return null;
					}
				}).installOn(instrumentation);


```

### Passthrough traceid
NOTE: on Windows use regular curl.exe, not the `/mingw64/bin/curl` - the latter is too limited in options
```cmd
c:\tools\curl.exe -I -H "traceid: abcdef" -XGET http://localhost:8500/index.html
```
will reply preserving the caller's `traceid`:
```cmd
HTTP/1.0 200 OK
Server: Simple Java Http Server
Content-type: text/html
Content-Length: 32
traceid: abcdef
staticinfo: e0e4c0fc-4196-4bc2-828f-4aad5a79a54b
class: class example.Header
```
When no `traceid` is received from client, none will be aded by vanilla app:
```cmd
curl -I -XGET http://localhost:8500/index.html
```
```sh
HTTP/1.0 200 OK
Server: Simple Java Http Server
Content-type: text/html
Content-Length: 32
staticinfo: e0e4c0fc-4196-4bc2-828f-4aad5a79a54b
class: class example.Header
```
will log the requst processing details
```sh
httpServer running on port 8500
New connection accepted /127.0.0.1:51803
input: GET /index.html HTTP/1.1 request: GET
requested path: ./index.html
input: User-Agent: curl/7.40.0-DEV request: User-Agent:
input: Host: localhost:8500 request: Host:
input: Accept: */* request: Accept:
getting value of field: staticinfo
Adding to response headers staticinfo: 375e6f09-ec82-4f36-a2fe-df94c89f6807
getting value of field: traceid
Ignoring null value of: traceid
getting value of field: class
Adding to response headers class: class example.Header
Sending custom header: staticinfo: 375e6f09-ec82-4f36-a2fe-df94c89f6807
class: class example.Header
```
### Injecting traceid
```sh
java -javaagent:agent/target/agent.jar -jar application/target/application.jar
```
this time request will have traceid:
```
curl -I -XGET http://localhost:8500/index.html
HTTP/1.0 200 OK
Server: Simple Java Http Server
Content-type: text/html
Content-Length: 32
traceid: new_value
staticinfo: 84d5cd3c-7513-4179-a9ef-77400ede3aa2
class: class example.Header
```
one can pass in the desired `traceid` via java property:
```cmd
java -javaagent:agent/target/agent.jar -Dtraceid=abcdef-1234-5678-90 -jar application/target/application.jar
```

this will lead to response header
```sh
traceid: abcdef-1234-5678-90
```
### See Also

  * https://docs.oracle.com/javase/7/docs/api/java/lang/instrument/package-summary.html
  * https://docs.appdynamics.com/display/PRO45/Install+the+Java+Agent
  * https://www.oracle.com/technetwork/articles/java/javamanagement-140525.html
  * [multiple services in a Docker container](https://docs.docker.com/config/containers/multi-service_container/)
  * about [Mapped Diagnostic Context](http://logback.qos.ch/manual/mdc.html)
  * about [log4j thread context](https://logging.apache.org/log4j/2.x/manual/thread-context.html)
  * [guide to access fields of a java Class using Reflection]https://www.baeldung.com/java-reflection-class-fields
  * [guide to Java instrumentation](https://www.baeldung.com/java-instrumentation)
  * [guide to bytebuddy](https://www.baeldung.com/byte-buddy)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
	
