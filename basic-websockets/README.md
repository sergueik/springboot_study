### Info
 Dockerized example from
 [chat application with Spring Boot and WebSocket](https://www.callicoder.com/spring-boot-websocket-chat-example).
 Since the example poject referenced there has heavy parent pom chain, used aalternative compact, standalone project boilerplate code take from [callicoder/spring-boot-websocket-chat-demo](https://github.com/callicoder/spring-boot-websocket-chat-demo), simplified to leave `Dockerfile` part out of Maven scope and modify packaging to include dependencies and to store the main Javascript dependencies within the project resource directory.
### Test

* Local maven run
```sh
mvn spring-boot:run
```
open the chat langing page `http://localhost:8080` in the browser.
* Docker run
```sh
mvn clean package
docker build -t basic-websockets -f Dockerfile .
docker run -n basic-websockets -d -p 8080:8080 -t basic-websockets
```
open the chat langing page `http://localhost:8080` in the browser.
```sh
docker stop basic-websockets
docker container prune -f
docker image rm basic-websockets
docker image prune -f
```
### See Also
 * https://www.baeldung.com/websockets-spring
 * https://www.baeldung.com/spring-websockets-sendtouser

