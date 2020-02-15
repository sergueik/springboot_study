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
docker exec -it mongo-service mongo
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
* remove the loose container
```sh
docker container stop mongo-service
docker container rm mongo-service
```
* run the cluster
```sh
docker-compose -f docker-compose.yaml up
```
test 
```sh
curl http://localhost:8085/mongo/repo
```
this will respond with an empty collection. 
```
[]
```

* add few values
```
for VALUE in test1 test2  test3 ; do curl http://localhost:8085/mongo/repo/$VALUE; done
```
get it back
```sh
curl http://localhost:8085/mongo/repo |jq '.' -
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
Note: 
there may be replicas if the insert was run multiple times -  the application assigns a unique `id` in every insert.
More realistic support of CRUD is a WIP.

### Cleanup

destroy all started containers and image afterwards
```sh
docker container ls -a | awk '{print $1}' |xargs -IX docker container stop X
docker container prune -f
docker image prune -f
```
when rebuilding the application, force the application image recycle (name is assigned by docker-compose)
```sh
docker image rm 'basic-mongo_app'
```

### See Also
  * [original post](https://qna.habr.com/q/714443)(in Russian)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
