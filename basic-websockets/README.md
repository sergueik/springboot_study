### Info
 Dockerized example from [chat application with Spring Boot and STOMP WebSocket](https://www.callicoder.com/spring-boot-websocket-chat-example).
 Since the example poject referenced there has heavy parent pom chain, used aalternative compact, standalone project boilerplate code take from [callicoder/spring-boot-websocket-chat-demo](https://github.com/callicoder/spring-boot-websocket-chat-demo), simplified to leave `Dockerfile` part out of Maven scope and modify packaging to include dependencies and to store the main Javascript dependencies within the project resource directory.
### Test

* Local maven run
```sh
mvn spring-boot:run
```
open the chat langing page `http://localhost:8080` in two or more browser instances.
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-websockets/screenshots/handshake-capture.jpg)
* Docker run
```sh
mvn clean package
NAME=basic-websockets
docker build -t $NAME -f Dockerfile .
docker run --name $NAME -d -p 8080:8080 -t $NAME
```
open the chat langing page `http://localhost:8080` in the browser.
connect to interactive session in the container and ispect sockets
```sh
docker exec -it $NAME sh
```
```sh
 # netstat -ant
Active Internet connections (servers and established)
Proto Recv-Q Send-Q Local Address           Foreign Address         State
tcp        0      0 0.0.0.0:8080            0.0.0.0:*               LISTEN
tcp        0      0 172.17.0.2:8080         192.168.0.25:56862      ESTABLISHED
tcp        0      0 172.17.0.2:8080         192.168.0.25:60969      ESTABLISHED
tcp        0      0 172.17.0.2:8080         192.168.0.25:63169      ESTABLISHED
tcp        0      0 172.17.0.2:8080         192.168.0.25:56009      ESTABLISHED
tcp        0      0 172.17.0.2:8080         192.168.0.25:61515      ESTABLISHED
```
```sh
apk add bind-tools
```
```sh
lsof -i -a -p 1
```
note, with `STOMP` protocol there will be 5 or 6 sockets per client session in browser
```sh
docker stop $NAME
docker container prune -f
docker image rm $NAME
docker image prune -f
```
### See Also
 * https://www.baeldung.com/websockets-spring
 * https://www.baeldung.com/spring-websockets-sendtouser
 * https://www.baeldung.com/java-websockets
 * Pluralsigt [Introduction to the Java API for WebSockets](https://app.pluralsight.com/course-player?clipId=048e3bf7-66a9-4240-ac75-d6d5b6df8618)
 * [code](https://github.com/bazzani/pluralsight-java-websockets) for the above
 * https://docs.spring.io/spring-framework/docs/4.1.7.RELEASE/spring-framework-reference/html/websocket.html
 * https://www.devglan.com/spring-boot/spring-boot-websocket-example

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

