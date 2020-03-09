### Info

Springboot Docker mongo project extracted from basic springboot embedded mongodb project [sample-spring-boot-data-mongodb-embedded](https://github.com/alexbt/sample-spring-boot-data-mongodb-embedded) converted to run on alpine openjdk jre mongo base images and connect using docker-compose

### Setup

* download the base image
```sh
docker pull mvertes/alpine-mongo
```
* run mongo standalone
```sh
docker run -d --name 'mongo-service' -i 'mvertes/alpine-mongo'
```
* drop into mongo shell on the service container
```sh
docker exec -it 'mongo-service' mongo
```
* simply quit the shell
```sh
> quit()
```
### Test
* configure Springboot `application.properties` to use default port `27017` on the node named `mongo`

```java
mongo_host=mongo
mongo_db=mydb
spring.data.mongodb.uri=mongodb://${mongo_host}:27017/${mongo_db}
spring.data.mongo.repositories.enabled=true
```
* package the app
```sh
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
and once started inspec the log file:
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
for VALUE in test1 test2 test3 ; do curl http://localhost:8085/mongo/insert1/$VALUE; done
```
get it back
```sh
curl http://localhost:8085/mongo/all |jq '.' -
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
for VALUE in test4 test5 test6 ; do curl http://localhost:8085/mongo/insert2/$VALUE; done
```

Note:
there may be replicas if the insert was run multiple times -  the application assigns a unique `id` in every insert.
More realistic support of CRUD is a WIP.

Note:

the REST call to get value back
```sh
curl http://localhost:8085/mongo/get/1583701210532
```
is currently failing (nothing is returned)
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
```
docker image ls | grep  mongo | awk '{print $3}' |xargs -IX docker image rm X -f 
```
### See Also
  * [original post](https://qna.habr.com/q/714443)(in Russian)
  * [custom code to sync wait for lagging containers](https://qna.habr.com/q/726237)(also in Russian, not considered the answer acceptable possibly too complex
  * Docker [variables](https://docs.docker.com/compose/environment-variables/)
### Author
 
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
