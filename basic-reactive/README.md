### Info

This directory contains a rearranged example
[basic reactive CRUD](https://github.com/rbarbioni/spring-boot-microservices-docker) example project
converted to maven. 

### TODO

NOTE:
still uses the original version of springboot parent __2.0.4.RELEASE__ 
due to compilation problems after attempting to ugrade to __2.3.4.RELEASE__:
```sh

```
and a vanilla mongo [container](https://github.com/bitnami/bitnami-docker-mongodb)
### Usage

build
```sh
mvn clean package
docker build -f Dockerfile.mongodb .
docker-compose -f docker-compose.yml up --build
```
* create user
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
modify
```sh
curl -X PUT http://localhost:8080/api/user/$ID -H 'Content-Type: application/json' -d '{"email": "user@other.email.com", "name": "demouser"}'
```
this will create additional rows (it will print back the created object):
```json
{"id":"60f5f8726344590001f6706d","name":"demouser","email":"user@other.email.com"}
```
### Cleanup

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
	
