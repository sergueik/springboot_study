### Info

Spring Boot on Docker basic extracted from [Springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example)
upgraded to MySQL __8.0__

### Setup
Edit `pom.xml` and specify the __8.0__ version of mysql-connector-java jar:

```xml
<dependency>
  <groupId>mysql</groupId>
  <artifactId>mysql-connector-java</artifactId>
  <version>8.0.18</version>
</dependency>
```
Pull the collaborator Docker image:

```sh
docker pull mysql:8.0
```
and run it with environments matching the `application.properties`:
```sh
docker run --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=password -d mysql:8.0
```
observe the successful start log message in `mysql-server` container:
```sh
docker logs mysql-server
```
```sh
[Server] /usr/sbin/mysqld: ready for connections. Version: '8.0.18'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server - GPL.
```
NOTE: the mysqld and java processes will be visible on host
### Test
Build java project to package the jar
```sh
mvn clean package
```
build the `mysql-example` Docker image
```sh
docker build -f Dockerfile -t mysql-example . 
```
lanch the `mysql-example` backed Docker container
```sh
docker run -p 8086:8086 --link mysql-server -d mysql-example
```
verify basic CRUD operations
```sh
curl http://localhost:8086/all/
[]
```
Check application logs 
```sh
docker logs $(docker container ls | grep mysql-example | awk '{print $1}')
```
```sh
Hibernate: select users0_.id as id1_0_, users0_.name as name2_0_, users0_.salary as salary3_0_, users0_.team_name as team_nam4_0_ from users users0_
```
```sh
curl http://localhost:8086/all/create
```
### Custom Error Handler Exercise


Add error handler
```java
@GetMapping("/user/{id}")
public Users getOne(@PathVariable("id") int id) {
	try {
		return usersRepository.getOne(id);
	} catch (Exception e) {
		throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Id Not Found", e);
	}
}

```
and a simplified version since the basic-mysql project repository does not have enough features:

```java
public Users getUsers(@PathVariable("id") int id) {
  // https://github.com/spring-projects/spring-framework/blob/master/spring-web/src/main/java/org/springframework/http/HttpStatus.java
  throw new ResponseStatusException(HttpStatus.NO_CONTENT,
      String.format("this is what actually happened with id %d.", id),
      new Exception(""));
}

```
Rebuild package and image and test the route:

```sh
curl -I http://localhost:8086/users/42
```
```sh
HTTP/1.1 404 
Content-Type: application/json;charset=UTF-8
Transfer-Encoding: chunked
Date: Wed, 06 Nov 2019 17:22:34 GMT
```

responds with custom HTTP status exception details:
```sh
curl http://localhost:86/all/users/123 2>/dev/null|jq '.'
{
  "timestamp": "2019-11-06T17:16:51.703+0000",
  "status": 404,
  "error": "Not Found",
  "message": "This is what actually happened with that id 123.",
  "path": "/all/users/123"
}
```

Note: one cannot use an successful HTTP statuses,
E.g. when set to `HttpStatus.NO_CONTENT` (204)

The application returns with  the custom HTTP status 
```sh
curl -I http://localhost:8086/all/users/123
HTTP/1.1 204 
Content-Type: application/json;charset=UTF-8
Date: Wed, 06 Nov 2019 18:14:29 GMT
```

but no details will be available
```sh
curl http://localhost:8086/all/users/123
```
(no output)
### Cleanup
```sh
docker stop mysql-server
```
```sh
ID=$(docker container ls | grep mysql-example | awk '{print $1}')
docker stop $ID
docker rm $ID
```
```sh
docker stop mysql-server
```
