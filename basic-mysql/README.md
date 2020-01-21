### Info

Spring Boot on Docker basic extracted from [Springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example) converted too use openjdk jre alpine base image, upgraded to MySQL __8.0__

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
[Server] /usr/sbin/mysqld (mysqld 8.0.18) initializing of server in progress as process 45
...
[Server] /usr/sbin/mysqld: ready for connections. Version: '8.0.18'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server - GPL.

[Server] /usr/sbin/mysqld: ready for connections. Version: '8.0.18'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server - GPL.
[Server] X Plugin ready for connections. Socket: '/var/run/mysqld/mysqlx.sock' bind-address: '::' port: 33060
```

verify the console connection:
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e "set @var = '1'; select @var ;"
```
```sh
mysql: [Warning] Using a password on the command line interface can be insecure.
+------+
| @var |
+------+
| 1    |
+------+
```
NOTE: the mysqld and java processes will be visible on host

### Basic Test
* Build java project to package the jar
```sh
mvn clean package
```
* Build the `mysql-example` Docker image
```sh
docker build -f Dockerfile -t mysql-example .
```
* Lanch the `mysql-example` backed Docker container
```sh
docker run -p 8086:8086 --link mysql-server -d mysql-example
```

* Confirm the app started through the log
```sh
docker logs $(docker container ls | grep mysql-example | awk '{print $1}')
```
will display usual Spring launch logs and eventually show
```sh
Started ExampleApplication in 21.678 seconds (JVM running for 23.016)
```
* Verify basic CRUD operations
```sh
curl http://localhost:8086/all/
```
would respond with the list of users added so far, starting with an empty list
```sh
[]
```
```sh
curl http://localhost:8086/create/10
```
would respond with

```json
[
  {
    "id": 10,
    "name": "Smith",
    "salary": 30,
    "teamName": "Development"
  }
]
```
* Check the backend operation log
```sh
Hibernate: select users0_.id as id1_0_, users0_.name as name2_0_, users0_.salary as salary3_0_, users0_.team_name as team_nam4_0_ from users users0_
```
### Exercise Custom Error Handler

* Add error handler. Note that current implementation is very basic and in particular it fails to show the already added user:
```sh
 curl http://localhost:8086/user/10/
```
would  respond with
```json
{
  "timestamp": "2019-11-06T20:24:15.802+0000",
  "status": 500,
  "error": "Internal Server Error",
  "message": "Type definition error: [simple type, class org.hibernate.proxy.pojo.bytebuddy.ByteBuddyInterceptor]; nested exception is com.fasterxml.jackson.databind.exc.InvalidDefinitionException: No serializer found for class org.hibernate.proxy.pojo.bytebuddy.ByteBuddyInterceptor and no properties discovered to create BeanSerializer (to avoid exception, disable SerializationFeature.FAIL_ON_EMPTY_BEANS) (through reference chain: example.resource.Users$HibernateProxy$jp3QM2bB[\"hibernateLazyInitializer\"])",
  "path": "/user/10/"
}
```
we like to intercept and customize this exception.
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
Since the basic-mysql project repository does not have enough features, it is quicker to add and test a simplified version which will not even try to
call `getOne()`:

```java
@GetMapping("/fail/{id}")
public Users getUsers(@PathVariable("id") int id) {
  throw new ResponseStatusException(HttpStatus.NO_CONTENT,
      String.format("this is what actually happened with id %d.", id),
      new Exception(""));
}
```
* Rebuild package and image and test the route:

```sh
curl -I http://localhost:8086/demo_error/42
```
respond with a HTTP status 404 now
```sh
HTTP/1.1 404
Content-Type: application/json;charset=UTF-8
Transfer-Encoding: chunked
Date: Wed, 06 Nov 2019 17:22:34 GMT
```

and allows one to pass down the exception details:
```sh
curl http://localhost:86/all/users/123 2>/dev/null|jq '.'
```
will show
```json
{
  "timestamp": "2019-11-06T17:16:51.703+0000",
  "status": 404,
  "error": "Not Found",
  "message": "This is what actually happened with that id 42.",
  "path": "/all/demo_error/42"
}
```

Note: one cannot use an successful [HTTP status](https://github.com/spring-projects/spring-framework/blob/master/spring-web/src/main/java/org/springframework/http/HttpStatus.java),
e.g. when set to `HttpStatus.NO_CONTENT` (204)

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
docker container prune -f
```
If the `prune` command is not desirable, stop and clean individual container by name
```sh
CONTAINER='mysql-example'
ID=$(docker ps | grep $CONTAINER | awk '{print $1}')
docker stop $ID
docker rm $ID
```
```sh
CONTAINER='mysql-server'
docker stop $CONTAINER
ID=$(docker container ls -a| grep $CONTAINER | awk '{print $1}')
docker rm $ID
```
