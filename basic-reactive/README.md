### Info

This directory contains a replica of
[basic reactive CRUD](https://github.com/rbarbioni/spring-boot-microservices-docker) example project converted to maven and password-less mongodb.

### Usage

* build and launch mongod container
```sh
MONGODB_SERVER=mongodb
docker build -t $MONGODB_SERVER -f Dockerfile.$MONGODB_SERVER .
docker container rm -f $MONGODB_SERVER
docker run -d --name $MONGODB_SERVER -p 27717:27017 -i $MONGODB_SERVER
docker logs $MONGODB_SERVER
```
* build application
```sh
mvn clean package
APPLICATION=app

docker build -t $APPLICATION -f Dockerfile.$APPLICATION .
docker run --name $APPLICATION --link $MONGODB_SERVER -p 8080:8080 -e MONGODB_HOST=$MONGODB_SERVER -e MONGODB_DATABASE=test -e MONGODB_PORT=27017 -d $APPLICATION
docker logs $APPLICATION
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
curl hurl -X PUT http://localhost:8080/api/user/$ID -H 'Content-Type: application/json' -d "{\"id\": \"$ID\", \"email\": \"user@other.email.com\", \"name\": \"demousernewname\"}"
```

Note, the original code
```java

public Mono<ServerResponse> update(ServerRequest serverRequest) {
	
	return userRepository.findById(serverRequest.pathVariable("id"))
			.flatMap(user -> ServerResponse.ok()
					.body(userRepository.save(serverRequest.bodyToMono(User.class).block()), User.class))
			.switchIfEmpty(ServerResponse.notFound().build());
}
	
```
was leading to the following problem: if the `id` is lacking from the body of the PUT request:

```sh
curl -X PUT http://localhost:8080/api/user/$ID -H 'Content-Type: application/json' -d '{"email": "user@other.email.com", "name": "demouser"}'
```
the command will result in creation of  additional rows (it will print back the created object):
```json
{"id":"60f5f8726344590001f6706d","name":"demouser","email":"user@other.email.com"}
```
the version which sets all fields excwept the id:
```java
public Mono<ServerResponse> update(ServerRequest serverRequest) {
	String id = serverRequest.pathVariable("id");
	User user = serverRequest.bodyToMono(User.class).toProcessor().peek();
;
	return userRepository.findById(id).flatMap(o -> {
		o.setName(user.getName());
		o.setEmail(user.getEmail());
		return userRepository.save(o);
	}).flatMap(o -> ServerResponse.ok().body(Mono.just(o), User.class))
		.switchIfEmpty(ServerResponse.notFound().build());
}
```
is free from that error.
* if no user is found it will return
```sh
HTTP/1.1 404 Not Found
```
ocker container stop $APPLICATION- can be seen by providing `-v` option in curl command
### Cleanup
```sh
docker container stop $APPLICATION $MONGODB_SERVER
docker container rm $APPLICATION $MONGODB_SERVER
docker image rm -f $APPLICATION $MONGODB_SERVER
```

### TODO

* project still uses the original version of springboot parent __2.0.4.RELEASE__
due to compilation problems after attempting to ugrade to __2.3.4.RELEASE__:

* the property default notation used in `application.properties`:
```java
spring.data.mongodb.port=${MONGODB_PORT:27017}
```
 misbehaves when property is not set:
```text
Failed to bind properties under 'spring.data.mongodb.port' to java.lang.Integer:
  Property: spring.data.mongodb.port
  Value: ${MONGODB_PORT:27017}
  Reason: failed to convert java.lang.String to java.lang.Integer
```
### See Also
   * [Spring WebFlux: Getting Started](https://app.pluralsight.com/library/courses/getting-started-spring-webflux/)
   * another [spring-webflux-reactive-rest-api-demo](https://github.com/callicoder/spring-webflux-reactive-rest-api-demo)
   * https://www.baeldung.com/spring-boot-reactor-netty
   * https://github.com/spring-projects/spring-boot/issues/9690
   * https://github.com/netifi/webflux-rxjava2-jdbc-example - looks good
   * https://github.com/chang-chao/spring-webflux-reactive-jdbc-sample
   * https://github.com/whnqwe/spring-boot-jdbc (incomplete?)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
