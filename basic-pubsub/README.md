### Info

this directory contains a replica of
[redis-pubsub-demo](https://github.com/ercancelik/redis-pubsub-demo)
demo springboot project for Redis Pub/Sub with some missind depednecnies added to `pom.xml`. the default settings are assumedin this project.


### Usage
```sh
docker pull redis:5.0.5-alpine3.9
```
```sh

docker run --name redis -d -p 6379:6379 redis:5.0.5-alpine3.9
```
```sh
mvn spring-boot:run
```
```sh
curl -X POST -d '{"body":"test message"}' -H "Content-type: application/json" http://localhost:8080/messages
```
this will respond with
```json
{
  "body": "test message"
}
```
and the application console log will show
```text
2023-03-12 23:37:53.333  INFO 29643 --- [enerContainer-2] example.service.DemoMessageListener      : Channel: channel1, Message: test message

```
if the redis container is not running will see error in console when processing the request
```text
2023-03-12 18:26:15.094 ERROR 5940 --- [nio-8080-exec-2] o.a.c.c.C.[.[.[/].[dispatcherServlet]    : 
Servlet.service() for servlet [dispatcherServlet] in context
 with path [] threw exception [Request processing failed; nested exception is or
g.springframework.data.redis.RedisConnectionFailureException: Unable to connect
to Redis; nested exception is io.lettuce.core.RedisConnectionException: Unable t
o connect to localhost:6379] with root cause
java.net.ConnectException: Connection refused: no further information
```
and the error in curl call:
```json
{
  "timestamp": "2023-03-12T23:07:07.076+00:00",
  "status": 500,
  "error": "Internal Server Error",
  "message": "",
  "path": "/messages"
}

```
NOTE: if redis is removed while appliction is running, it will hange
### Cleanup

```sh
CONTAINER=redis
docker stop $CONTAINER
docker container rm $CONTAINER
```
### See Also
   * [intro to Jedis – the Java Redis](https://www.baeldung.com/jedis-java-redis-client-library)
   * [introduction to Spring Data Redis](https://www.baeldung.com/spring-data-redis-tutorial)

### Author
