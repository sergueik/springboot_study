###  Info
Clone of [graphviz-server](https://github.com/omerio/graphviz-server) lightweight Java HTTP server wrapping the locally installed [graphviz](http://www.graphviz.org/) binary installed locally. 

### Usage 

```sh
mvn clean package
```
run in foreground
```sh
java -jar dist/DotGraphics.jar 8080 
```
interact with
```sh
curl -v -X POST http://localhost:8080/ -d @../basic-graphviz/color.dot
```
```text
> POST / HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.81.0
> Accept: */*
> Content-Length: 278
> Content-Type: application/x-www-form-urlencoded
> 
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Date: Fri, 06 Feb 2026 19:57:55 GMT
< Server: DotGraphics/1.1
< Content-Length: 0
< 
* Connection #0 to host localhost left intact
```
NOTICE the response length is 0, investigate.
examine console logs
```text
14:57:18.891 INFO  example.DotGraphics - Listening on port 8080
14:57:21.988 INFO  example.DotGraphics - Incoming connection from /127.0.0.1
14:57:22.012 INFO  example.DotGraphics - New connection thread
14:57:22.038 INFO  o.a.http.protocol.HttpRequestHandler - POST / [Host: localhost:8080, User-Agent: curl/7.81.0, Accept: */*, Content-Length: 27, Content-Type: application/x-www-form-urlencoded]
14:57:22.039 INFO  o.a.http.protocol.HttpRequestHandler - Incoming entity content (27 bytes): ../basic-graphviz/color.dot
14:57:22.050 INFO  o.a.http.protocol.HttpRequestHandler - Responded with Success
14:57:55.978 INFO  example.DotGraphics - Incoming connection from /127.0.0.1
14:57:55.979 INFO  example.DotGraphics - New connection thread
14:57:55.980 INFO  o.a.http.protocol.HttpRequestHandler - POST / [Host: localhost:8080, User-Agent: curl/7.81.0, Accept: */*, Content-Length: 278, Content-Type: application/x-www-form-urlencoded]
14:57:55.980 INFO  o.a.http.protocol.HttpRequestHandler - Incoming entity content (278 bytes): graph {    { rank=same; white}    { rank=same; cyan; yellow; pink}    { rank=same; red; green; blue}    { rank=same; black}    white -- cyan -- blue    white -- yellow -- green    white -- pink -- red    cyan -- green -- black    yellow -- red -- black    pink -- blue -- black}
14:57:55.980 INFO  o.a.http.protocol.HttpRequestHandler - Responded with Success
```

after fixing the code,
```
curl -v -X POST http://localhost:8080/ -d @../basic-graphviz/color.dot -o result.png
```
```text
onnected to localhost (127.0.0.1) port 8080 (#0)
> POST / HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.81.0
> Accept: */*
> Content-Length: 278
> Content-Type: application/x-www-form-urlencoded
> 
} [278 bytes data]
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Date: Fri, 06 Feb 2026 20:46:00 GMT
< Server: DotGraphics/1.1
< Content-Length: 32547
< Content-Type: image/png
< 
{ [20480 bytes data]
100 32825  100 32547  100   278   533k   4663 --:--:-- --:--:-- --:--:--  543k
* Connection #0 to host localhost left intact
```
the `result.png` is written to current directory

the console log shows
```text
15:46:00.094 INFO  example.DotGraphics - Incoming connection from /127.0.0.1
15:46:00.094 INFO  example.DotGraphics - New connection thread
15:46:00.095 INFO  o.a.http.protocol.HttpRequestHandler - POST / [Host: localhost:8080, User-Agent: curl/7.81.0, Accept: */*, Content-Length: 278, Content-Type: application/x-www-form-urlencoded]
15:46:00.095 INFO  o.a.http.protocol.HttpRequestHandler - Incoming entity content (278 bytes): graph {    { rank=same; white}    { rank=same; cyan; yellow; pink}    { rank=same; red; green; blue}    { rank=same; black}    white -- cyan -- blue    white -- yellow -- green    white -- pink -- red    cyan -- green -- black    yellow -- red -- black    pink -- blue -- black}
15:46:00.095 INFO  o.a.http.protocol.HttpRequestHandler - valid dot content
15:46:00.095 INFO  o.a.http.protocol.HttpRequestHandler - requesting graph type: 
15:46:00.095 INFO  o.a.http.protocol.HttpRequestHandler - graph {    { rank=same; white}    { rank=same; cyan; yellow; pink}    { rank=same; red; green; blue}    { rank=same; black}    white -- cyan -- blue    white -- yellow -- green    white -- pink -- red    cyan -- green -- black    yellow -- red -- black    pink -- blue -- black}
15:46:00.096 INFO  example.GraphViz - write image stream to /tmp/graph_10734618452734776524.dot.tmp
15:46:00.096 INFO  example.GraphViz - get image stream from /tmp/graph_10734618452734776524.dot.tmp
15:46:00.152 INFO  o.a.http.protocol.HttpRequestHandler - Responded with Success

```
package
```sh
docker pull eclipse-temurin:11-jre-alpine
export NAME=example-graphviz-java
docker build -t $NAME -f Dockerfile . 
```
```sh
docker run -p 8080:8080 --name $NAME -d $NAME 
```
if the container is hosted locally, run
```sh
curl -sX POST http://localhost:8080/ -d @../basic-graphviz/color.dot -o result.png
```
when the docker is run in a VM updste the host address 

```sh
curl -sX POST http://192.168.99.102:8080/ -d @../basic-graphviz/color.dot -o result.png
```
the resulting file will be saved in local directory.
```sh
docker logs
```
and the server returns back a graph in SVG, PDF or PNG format. Graphviz-server uses the [Graphviz Java API](https://github.com/jabbalaci/graphviz-java-api), a Java wrapper that invokes the dot binary using Runtime.exec.

A typical usage is demonstrated in this sequence diagram:

<img src="http://omerio.com/wp-content/uploads/2013/11/dot_server.png" width="600">

**Source**: [github.com/omerio/graphviz-server](https://github.com/omerio/graphviz-server)

**Author**: [Omer Dawelbeit](http://omerio.com/omer-dawelbeit/)

## Motivation

Graphviz is a powerful open source graph visualization and layout tool, unfortunately no runtime exists for Java, so the only option is to invoke the dot binary from a running Java application. This option might not be possible or practical for many reasons, for example:

* If you are running your app in a Platform as a Service (PaaS) environment like [Google App Engine](https://cloud.google.com/appengine/) where you can't install extra software.
* You can't install the dot binaries in the server running your code, or you need to access Graphviz from more than one server.
* You don't want your application to have a dependency on the dot binary. 

In the cases mentioned above, it does make sense to setup one graphviz-server in a separate environment, and then access it from all your applications that require graphs to be generated.

## Live Demo

Here is a demo running on Google App Engine [http://dot-graphics1.appspot.com/](http://dot-graphics1.appspot.com/). 

The source code for the demo is [here](https://github.com/omerio/graphviz-appengine). The graphviz-server is installed on a [Google Compute Engine](https://cloud.google.com/compute/) VM (see documentation section below).


## Jump start

* Clone the git repository - `git clone https://github.com/omerio/graphviz-server`
* An executable jar with dependencies is included in the dist folder. The port on which the server listens can be configured as a command line parameter to the jar. To change the default port (8080) edit the DotGraphics.sh in the dist directory:
```
#!/bin/sh
java -jar DotGraphics.jar 8080 > /dev/null 2>&1 &
exit 0
```
* If you want to make changes to the code and build your own jar, you need to have Maven installed. Simply run `mvn package` this will create a jar with dependencies inside the **target** directory.

## Usage

Run the graphviz-server:
```
./DotGraphics.sh
```

The graphviz-server uses Log4j for logging. All incoming requests are logged to DotGraphics.log, a sample output is provided [here](https://github.com/omerio/graphviz-server/blob/master/dist/DotGraphics.log).

To use the Graphviz server simply submit a HTTP POST with the dot graph script set as the request body. Optionally an output type can be specified on the URL for example:

* Post to http://localhost:8080/svg to render the graph as SVG
* Post to http://localhost:8080/pdf to render the graph as PDF
* Post to http://localhost:8080 to render the graph as PNG (default)

**Note:** The server will validate that the dot graph starts with `digraph G {`, if this is not your desired behaviour, you can remove the second check on the [HttpDotGraphMessageHandler.java](https://github.com/omerio/graphviz-server/blob/master/src/info/dawelbeit/graphviz/dot/HttpDotGraphMessageHandler.java#L82).

## Docker

If you are using Docker and would like to run graphviz-server in a Docker container, I've created a Ubuntu [Docker image](https://hub.docker.com/r/omerio/graphviz-server/). There is a [Dockerfile](https://github.com/omerio/graphviz-server/blob/master/Dockerfile) in the source if you want to build your own image.


## Documentation

For more details on the implementation of graphviz-server and a detailed guide on how to set it up on Google Compute Engine VM, see this blog post:


[http://omerio.com/2013/11/03/running-a-graphviz-server-on-google-compute-engine/](http://omerio.com/2013/11/03/running-a-graphviz-server-on-google-compute-engine/).

## License

Open Source (Apache License 2.0)

