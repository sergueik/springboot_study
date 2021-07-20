### Info

This directory contains a rearranged example
[basic reactive CRUD](https://github.com/rbarbioni/spring-boot-microservices-docker) example project
converted to maven. Note: there is no reactive programming in this project, only direect `Controller` =&gt; `Service` =&gt; `Reposiory`

### TODO

NOTE:
still uses the original version of springboot parent __2.0.4.RELEASE__
due to compilation problems after attempting to ugrade to __2.3.4.RELEASE__:
```sh

```
### Usage

####  Without relying on docker compose
  * pull a vanilla alpine mongodb container with auth support
```sh
docker pull clutteredcode/mongo-alpine
```
 * build the backend server
```sh
export MONGODB_SERVER=mongodb-server
sudo rm -fr data/*
mkdir data
docker run -d --name $MONGODB_SERVER -e MONGO_USERNAME=test -e MONGO_PASSWORD=test -p 27017:27017 -v data:/data/db clutteredcode/mongo-alpine
```
* build application
```sh
mvn clean package
IMAGE=app
docker build -t $IMAGE -f Dockerfile.$IMAGE .
docker run --name $IMAGE --link $MONGODB_SERVER -p 8080:8080 -e MONGO_USERNAME=test -e MONGO_PASSWORD=test -e MONGODB_HOST=$MONGODB_SERVER -e MONGODB_DATABASE=admin -d $IMAGE
docker logs $IMAGE
```
wait until see the launch to happen:
```text
Started Launcher in 11.74 seconds (JVM running for 13.109)
```
* NOTE if changing credentials make sure to also remove the `$MONGODB_SERVER` container and `clutteredcode/mongo-alpine` image

#### with `docker-compose.yml`
 * add another vanilla [mongo container](https://github.com/bitnami/bitnami-docker-mongodb)

```sh
docker pull 'bitnami/mongodb'
```
 - the `clutteredcode/mongo-alpine` appears to have problems with environments, leading it to failures in run time
```text
mongodb_1  | 2021-07-20T21:34:30.634+0000 I ACCESS   [conn6] SASL SCRAM-SHA-1 authentication failed for test on admin from client 172.21.0.3:37770 ; UserNotFound: Could not find user test@admin
```
and eventually fail to operate at the database level:
```text
{
  "timestamp": "2021-07-20T21:36:13.714+0000",
  "path": "/api/user",
  "status": 500,
  "error": "Internal Server Error",
  "message": "Timed out after 30000 ms while waiting for a server that matches WritableServerSelector. Client view of cluster state is
  {type=UNKNOWN, servers=
  [{address=mongodb:27017, type=UNKNOWN, state=CONNECTING, exception=
  {com.mongodb.MongoSecurityException: Exception authenticating MongoCredential
  {mechanism=null, userName='test', source='admin', password=<hidden>, mechanismProperties={}}},
  caused by { com.mongodb.MongoCommandException: Command failed with error 18: 'Authentication failed.' on server mongodb:27017.
  The full response is { \"ok\" : 0.0, \"errmsg\" : \"Authentication failed.\", \"code\" : 18, \"codeName\" : \"AuthenticationFailed\" }}}];
  nested exception is com.mongodb.MongoTimeoutException: Timed out after 30000 ms while waiting for a server that matches WritableServerSelector. Client view of cluster state is
  {type=UNKNOWN, servers=[{address=mongodb:27017, type=UNKNOWN, state=CONNECTING, exception={com.mongodb.MongoSecurityException: Exception authenticating MongoCredential{mechanism=null, userName='test', source='admin', password=<hidden>, mechanismProperties={}}}, caused by {com.mongodb.MongoCommandException: Command failed with error 18: 'Authentication failed.' on server mongodb:27017. The full response is { \"ok\" : 0.0, \"errmsg\" : \"Authentication failed.\", \"code\" : 18, \"codeName\" : \"AuthenticationFailed\" }}}]"
}

```
```sh
mvn clean package
docker-compose -f docker-compose.yml up --build
```

### Testing the Application
* create new user
```sh
curl -X POST http://localhost:8080/api/user -H 'Content-Type: application/json' -d '{ "name":"demouser", "email": "user@email.com"}'
```
* get back the user

```sh
ID=$(curl http://localhost:8080/api/user | jq -cr '.[0].id')
curl http://localhost:8080/api/user/$ID | jq '.'
```
this will return
```json
{
  "id": "60f5f71e6344590001f6706b",
  "name": "demouser",
  "email": "user@email.com"
}
```
* modify user
```sh
curl -X PUT http://localhost:8080/api/user/$ID -H 'Content-Type: application/json' -d '{"email": "user@other.email.com", "name": "demouser"}'
```
this will create additional rows (it will print back the created object):
```json
{"id":"60f5f8726344590001f6706d","name":"demouser","email":"user@other.email.com"}
```
### Cleanup
#### Plain Dockerfile case
```sh
docker container stop $IMAGE MONGODB_SERVER
docker container rm $IMAGE MONGODB_SERVER
docker image rm -f $IMAGE
```
#### docker-compose.yml case
```sh
docker-compose -f docker-compose.yml stop
docker-compose -f docker-compose.yml rm  -f
docker image rm spring-boot-microservices-docker
docker image rm 'bitnami/mongodb'
```
### Work in progress

#### Switch to alpine mongo cluster member `mvertes/alpine-mongo` or to the one
in the sibling project (the user and database name are unclear)

### See Also

   * another [spring-webflux-reactive-rest-api-demo](https://github.com/callicoder/spring-webflux-reactive-rest-api-demo)
   * https://www.baeldung.com/spring-boot-reactor-netty
   * https://github.com/spring-projects/spring-boot/issues/9690
	
