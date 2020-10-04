### Info

This directory contains practice Java Redis basic code
### Usage
```sh
docker run -it --rm --name redis -p 6379:6379 redis:6.0-alpine
```
```sh
mvn sring-boot:run
```
```sh
curl http://localhost:8080/redis
```
```sh
Connection to Redis server 127.0.0.1 6379 sucessful
Server is running: PONG
Stored string: Redis tutorial
Stored range of numbers: 3.0,2.0,1.0
List of stored keys: title,numbers
```
### Cleanup
```sh
docker container prune -f
```
### See Also

  * https://github.com/itxwnet/redis-springboot (uses `spring-boot-starter-data-redis` dependency, has a featured `RedisUtils` class implementing all basic Redis operations.
  * https://www.tutorialspoint.com/redis/redis_java.htm
  * https://github.com/abhirockzz/fn-redis-example

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
