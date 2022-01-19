### Info

Springboot Docker mongo project extracted from basic springboot embedded mongodb project [sample-spring-boot-data-mongodb-embedded](https://github.com/alexbt/sample-spring-boot-data-mongodb-embedded) converted to run on alpine openjdk jre mongo base images and connect using docker-compose

### Setup

* download the base image
```sh
docker pull mvertes/alpine-mongo
```
* run mongo standalone
```sh
IMAGE='mvertes/alpine-mongo'
CONTAINER=mongo-serviceo:14
docker container prune -f
# docker run -d --name $CONTAINER -i $IMAGE
docker run -d --name $CONTAINER -p 27717:27017 -i $IMAGE
```
* alternatively
```sh
IMAGE=mongodb
CONTAINER=mongo-service
docker build -t $IMAGE -f Dockerfile.$IMAGE .
docker container prune -f
docker run -d --name $CONTAINER -p 27717:27017 -i $IMAGE
docker logs $CONTAINER
```
* verify ports
```sh
netstat -ant | grep LISTEN | grep 27717
```
```text
tcp6       0      0 :::27717                :::*                    LISTEN
```
* drop into mongo shell on the service container
```sh
docker exec -it $CONTAINER mongo
```
* simply quit the shell
```sh
> quit()
```
### Testing Plain Java Application in a Developer host
```sh
mvn package
java -Dmongo_host=127.0.0.1 -Dmongo_db=test -Dmongo_port=27717 -jar target/example.basic-mongo-app.jar
```
alternatively
```sh
java -Dspring.data.mongodb.uri=mongodb://127.0.0.1:27717/test -jar target/example.basic-mongo-app.jar
```
alternatively,
```sh
java -Dspring.data.mongo.host=localhost -Dspring.data.mongo.port=27717 -Dspring.data.mongo.database=test -jar target/example.basic-mongo-app.jar
```
then after a sucessful launch run CRUD commands through curl



### Testing SpringBoot Application in a Linked Docker Container

* configure Springboot  `spring/src/main/resources/application.properties` to use default port `27017` on the node named `mongo`:
change from
```java
mongo_host=mongo-service
mongo_db=mydb
spring.data.mongodb.uri=mongodb://${mongo_host}:27017/${mongo_db}
spring.data.mongo.repositories.enabled=true
```
to
```java
mongo_host=127.0.0.1
spring.data.mongodb.uri=mongodb://127.0.0.1:27717/test
```
* package the app
```sh
cd spring
mvn clean package
```
* build run the application node
```sh
docker build -f Dockerfile -t mongo-example .
```
Note: if the `delayed_shart.sh` is set to run via `CMD` 
```sh
CMD delayed_start.sh
```
followed by vanilla `ENTRYPOINT`
```
ENTRYPOINT ["java", "-jar", "app.jar"]
```
the shell script will be silently skipped having no effect on blocking the 
`ENRYPOINT` command until `SERVICE_PORT` is available  on the `SERVICE_HOST` node. 
The only way to make it work is to embed (or pass through argument) the original `ENTRYPOINT` command in the `delayed_start.sh`
```sh
docker run -e DEBUG_DELAYED_START=true -e SERVICE_PORT=27017 -e SERVICE_HOST=mongo-service -p 8085:8085 --link mongo-service -d mongo-example
```
```sh
docker container ls | grep mongo-example | awk '{print $1}' |xargs -IX docker attach X
```
or use `docker-compose`:
* remove the loose container
```sh
docker container stop mongo-service
docker container rm mongo-service
```

* run the cluster
```sh
docker-compose -f docker-compose.yaml up
```
* test
```sh
curl http://localhost:8085/mongo/all
```
this will respond with an empty collection.
```
[]
```
the error
```sh
curl: (56) Recv failure: Connection reset by peer
```
indicates the app has not launched yet
the error
```json
{
  "timestamp": 1583505069867,
  "status": 500,
  "error": "Internal Server Error",
  "exception": "org.springframework.dao.DataAccessResourceFailureException",
  "message": "Timed out after 30000 ms while waiting for a server that matches ReadPreferenceServerSelector{readPreference=primary}. Client view of cluster state is {type=UNKNOWN, servers=[{address=mongo:27017, type=UNKNOWN, state=CONNECTING, exception={com.mongodb.MongoSocketException: mongo}, caused by {java.net.UnknownHostException: mongo}}]; nested exception is com.mongodb.MongoTimeoutException: Timed out after 30000 ms while waiting for a server that matches ReadPreferenceServerSelector{readPreference=primary}. Client view of cluster state is {type=UNKNOWN, servers=[{address=mongo:27017, type=UNKNOWN, state=CONNECTING, exception={com.mongodb.MongoSocketException: mongo}, caused by {java.net.UnknownHostException: mongo}}]",
  "path": "/mongo/all"
}
```
indicate the problem with inter container networking

Alternatively run with `DEBUG_DELAED_START` enabled
```sh
docker run -e DEBUG_DELAYED_START=true -e SERVICE_PORT=27017 -p 8085:8085 --link mongo-service -d mongo-example

```
and once started inspect the log file:
```sh
docker container ls | grep 'mongo-example' | awk '{print $1}' | xargs -IX docker exec X ls /tmp
```
and see
```
debug.log
hsperfdata_root
tomcat-docbase.3966690493895183356.8085
tomcat.8344236035599407589.8085
```
and
```sh
docker container ls | grep 'mongo-example' | awk '{print $1}' | xargs -IX docker exec X cat /tmp/debug.log
```
and read logged execution details:
```sh
Waiting on the mongo-service 27017
Got Response
```

* add few values
```sh
for VALUE in test1 test2 test3 ; do curl -s http://localhost:8085/mongo/insert1/$VALUE; done
```
get it back
```sh
curl -s http://localhost:8085/mongo/all |jq '.' -
```
this will respond with
```js
[
  {
    "id": 1581805810247,
    "value": "test1"
  },
  {
    "id": 1581805810283,
    "value": "test2"
  },
  {
    "id": 1581805810312,
    "value": "test3"
  },
]
```
```sh
for VALUE in test4 test5 test6 ; do curl -s http://localhost:8085/mongo/insert2/$VALUE; done
```

Note:
there may be replicas if the insert was run multiple times -  the application assigns a unique `id` in every insert.
More realistic support of CRUD is a WIP.

Note:

the REST call to get value back
```sh
ID=$(curl -s  http://localhost:8085/mongo/all| jq -r  '.[0]|.id')
curl  -s http://localhost:8085/mongo/get/$ID
```
is currently prnting nothing (an empty page is returned
though the server response status is OK:
```sh
curl  -v -s http://localhost:8085/mongo/get/$ID
```
```text
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 8085 (#0)
> GET /mongo/get/1642535883618 HTTP/1.1
> Host: localhost:8085
> User-Agent: curl/7.47.0
> Accept: */*
>
< HTTP/1.1 200
< Content-Length: 0
< Date: Tue, 18 Jan 2022 20:00:11 GMT
<
* Connection #0 to host localhost left intact

```
it is WIP to debug what is causing empry response body
```sh
docker exec -it 'mongo-service' mongo
```
followed by
```sh
> use mydb
switched to db mydb
> 
> db.model.find({"_id":1583701210532}).pretty();
```
returns 
```json
{
	"_id" : NumberLong("1583701210532"),
	"_class" : "example.Model",
	"value" : "test5"
}
```

### Testing SpringBoot Application with credentials
```sh
IMAGE=mongodb
CONTAINER=mongo-service
docker build -t $IMAGE -f Dockerfile.$IMAGE .
docker container prune -f
docker run -d --name $CONTAINER -p 27017:27017 -e MONGO_USERNAME=test -e MONGO_PASSWORD=test -e MONGODB_DATABASE=admin $IMAGE
docker logs $CONTAINER
```
uncomment the lines in `src/main/resources/application.properties` of the springboot app project
```java
spring.data.mongodb.username=${MONGODB_USERNAME:test}
spring.data.mongodb.password=${MONGODB_PASSWORD:test}
```
### Cleanup

destroy all started containers and image afterwards
```sh
docker container ls -a | grep mongo | awk '{print $1}' |xargs -IX docker container stop X
docker container prune -f
docker image prune -f
docker image rm mongo-example
```
when rebuilding the application, force the application image recycle (name is assigned by docker-compose)
```sh
docker image rm 'basic-mongo_app'
```

### Full Cleanup

```sh
docker image ls | grep  mongo | awk '{print $3}' |xargs -IX docker image rm X -f 
```
### See Also

  * [original post](https://qna.habr.com/q/714443)(in Russian)
  * [custom code to sync wait for lagging containers](https://qna.habr.com/q/726237)(also in Russian, not considered the answer acceptable possibly too complex
  * Docker [variables](https://docs.docker.com/compose/environment-variables/)
### Author
  * https://github.com/trishagee/mongodb-getting-started 
  * https://www.baeldung.com/java-mongodb (plain)
  * https://www.baeldung.com/spring-data-mongodb-guide
  * https://www.codejava.net/java-se/jdbc/java-connecting-to-mongodb-database-examples
  * https://github.com/BunnyAndOak0/MongoDB
  * https://www.baeldung.com/java-mongodb-aggregations
  * https://www.baeldung.com/queries-in-spring-data-mongodb
  * https://mongodb.github.io/mongo-java-driver/3.9/driver/getting-started/quick-start/  

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
