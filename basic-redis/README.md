### Info

This directory contains practice Java Redis basic code

### Usage
```sh
IMAGE=redis:6.0-alpine
docker pull $IMAGE
docker run -it --rm --name redis -p 6379:6379 $IMAGE
```
in separate console
```sh
mvn sring-boot:run
```
```sh
curl -s http://localhost:8080/redis
```
```sh
Connection to Redis server 127.0.0.1 6379 sucessful
Server is running: PONG
Stored string: Redis tutorial
Stored range of numbers: 3.0,2.0,1.0
List of stored keys: title,numbers
```
```sh
cusl -s http://192.168.0.64:8080/redis?ex=10
```
```text
Connection to Redis server 127.0.0.1 6379 sucessful 
Server is running: PONG 
Stored string: Redis tutorial  with expiration of 10 second
Stored range of numbers: 3.0,2.0,1.0,3.0,2.0,1.0,3.0,2.0,1.0,3.0,2.0 
sleep 11 second
List of stored keys: numbers

```
### Cleanup
```sh
docker container prune -f
```
### See Also
  * [Intro to Jedis - the Java Redis Client Library](https://www.baeldung.com/jedis-java-redis-client-library)
  * https://github.com/itxwnet/redis-springboot (uses `spring-boot-starter-data-redis` dependency, has a featured `RedisUtils` class implementing all basic Redis operations.

  * https://www.tutorialspoint.com/redis/redis_java.htm
  * https://github.com/abhirockzz/fn-redis-example
  * https://stackoverflow.com/questions/49376096/how-to-set-a-key-with-value-along-with-expiry-using-java-jediscluster
  * https://www.geeksforgeeks.org/redis-cache-in-java-using-jedis/

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)



