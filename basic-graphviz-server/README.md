###  Info
Clone of [graphviz-server](https://github.com/omerio/graphviz-server) lightweight Java HTTP server wrapping the locally installed [graphviz](http://www.graphviz.org/) binary installed locally. 

### Usage 

```sh
mvn clean package
```
run in foreground
```sh
java -jar target/example.graphviz-java-fat.jar 8080 
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
docker container stop $NAME
docker container rm $NAME
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
docker logs $NAME
```
```text
23:49:55.252 INFO  example.DotGraphics - Listening on port 8080
23:49:55.225 INFO  example.GraphViz - Initializing Graphviz engine...
23:50:00.251 INFO  example.DotGraphics - Incoming connection from /127.0.0.1
23:50:00.490 INFO  example.DotGraphics - New connection thread
23:50:00.566 INFO  o.a.http.protocol.HttpRequestHandler - GET /health [Host: localhost:8080, User-Agent: curl/8.17.0, Accept: */*]
23:50:00.569 INFO  o.a.http.protocol.HttpRequestHandler - Responded with Success
23:50:08.130 INFO  example.DotGraphics - Incoming connection from /192.168.99.1
23:50:08.138 INFO  example.DotGraphics - New connection thread
23:50:08.154 INFO  o.a.http.protocol.HttpRequestHandler - POST / [Host: 192.168.99.102:8080, User-Agent: curl/8.12.1, Accept: */*, Content-Length: 278, Content-Type: application/x-www-form-urlencoded]
23:50:08.160 INFO  o.a.http.protocol.HttpRequestHandler - Incoming entity content (278 bytes): graph {    { rank=same; white}    { rank=same; cyan; yellow; pink}    { rank=same; red; green; blue}    { rank=same; black}    white -- cyan -- blue    white -- yellow -- green    white -- pink -- red    cyan -- green -- black    yellow -- red -- black    pink -- blue -- black}
23:50:08.456 INFO  o.a.http.protocol.HttpRequestHandler - valid dot content
23:50:08.470 INFO  o.a.http.protocol.HttpRequestHandler - requesting graph type:

23:50:08.471 INFO  o.a.http.protocol.HttpRequestHandler - graph {    { rank=same; white}    { rank=same; cyan; yellow; pink}    { rank=same; red; green; blue}    { rank=same; black}    white -- cyan -- blue    white -- yellow -- green    white -- pink -- red    cyan -- green -- black    yellow -- red -- black    pink -- blue -- black}
23:50:08.647 INFO  example.GraphViz - write image stream to /tmp/graph_5927018251725095829.dot.tmp
23:50:08.655 INFO  example.GraphViz - Rendering graph /tmp/graph_5927018251725095829.dot.tmp using graphviz-java engine, format=png
23:50:11.301 INFO  example.DotGraphics - Incoming connection from /127.0.0.1
23:50:11.315 INFO  example.DotGraphics - New connection thread
23:50:11.322 INFO  o.a.http.protocol.HttpRequestHandler - GET /health [Host: localhost:8080, User-Agent: curl/8.17.0, Accept: */*]
23:50:11.323 INFO  o.a.http.protocol.HttpRequestHandler - Responded with Success
```


### See Also
  * https://github.com/omerio/graphviz-server
  * https://github.com/omerio/graphviz-webapp
  * https://hub.docker.com/r/mejran/graphviz-server
  * https://github.com/blackears/svgSalamander SVG engine for Java
  * https://mvnrepository.com/artifact/guru.nidi/graphviz-java/0.18.1

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


