### Info

SpringBoot Docker basic example extracted from [Springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example) converted too use alpine openjdk jre base image, upgraded to MySQL __8.0__

### Setup
Edit `pom.xml` and specify the __8.0__ version of mysql-connector-java jar:
.
```xml
<dependency>
  <groupId>mysql</groupId>
  <artifactId>mysql-connector-java</artifactId>
  <version>8.0.18</version>
</dependency>
```.p
Pull the collaborator Docker image:

```sh
docker pull mysql:8.0.18
```
and run it with environments matching the `application.properties`:
```sh
docker run --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=password -d mysql:8.0.18
```
The enviroment entries `MYSQL_ROOT_PASSWORD`, `MYSQL_USER`,`MYSQL_DATABASE`, `MYSQL_PASSWORD` are required by Mysql docker image.
It will take the Docker instance  quite some time to launch. 
One can safely start building and runing Spring app container while database initializes itself.
Eventually
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
# docker build -f Dockerfile -t mysql-example .
docker build -f Dockerfile.with_delayed_start -t mysql-example .
```
* Lanch the `mysql-example` backed Docker container
```sh
docker run -p 8086:8086 -e "SERVICE-PORT=3306" --link mysql-server -d mysql-example
```
It will execute the delayed launch script:
```sh
#!/bin/sh

SERVICE_HOST='mysql-server'
SERVICE_PORT='3306'
APP='app.jar'
while true
do
nc -z $SERVICE_HOST $SERVICE_PORT
if [ $? -eq 0 ]
then
break
fi
echo "Waiting on the ${SERVICE_HOST} ${SERVICE_PORT}"
sleep 10
done

java -jar $APP
```
thus preventing it from failing to launch Spring when no connection bean dependency is not yet ready.
```sh
docker logs mysql-server
```
```sh
Waiting on the mysql-server 3306
Waiting on the mysql-server 3306
Waiting on the mysql-server 3306

  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v2.1.2.RELEASE)


```

NOTE:
One cannot modify the [stock image](https://github.com/docker-library/mysql/blob/master/8.0/Dockerfile) `CMD` or `ENTRYPOINT` - 
 to establish some synhronization betwen slowly bootstrapping docker-hosted
applications - they are already finished vendor commands and would attempt to interpret the added command is an argument e.g.:
```
# [ERROR] [MY-010147] [Server] Too many arguments (first extra is 'sh -c "while true; do netstat -ant | grep -q 3036; if [ $$? -eq 0  ] ; then break ;  fi ;  echo \"x\"; sleep 10;  done"').
``

* Confirm the app started through the log
```sh
docker logs $(docker container ls | grep mysql-example | awk '{print $1}')
```
will display usual verbose Spring launch logs
```sh
org.hibernate.dialect.Dialect            : HHH000400: Using dialect: org.hibernate.dialect.MySQL5Dialect
```
and eventually show
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
* Alternatively bring both containers up via `docker-compose.yaml`(slower and less reliable):
```sh
mvn clean package
export COMPOSE_HTTP_TIMEOUT=600
docker-compose  -f docker-compose.yaml up --build
```
The ordering does not appear to work well:
```sh
caused by: java.net.ConnectException: Connection refused (Connection refused)
```
This is sometimes cured by the `docker-compose` rerun, alterntively one may launch and attache the exited container:
```sh
ID=$(docker container ls -a | grep 'basic-mysql_app'| grep -i exited| awk '{print $1}')
docker start $ID
docker attach $ID
```
at which point the console running the `docker-compose` will show the console  log of the `basic-mysql_app_1` container  that exited earlier.
NOTE: this process is prone to fail and leave behind the unfinished images blocking its repeated runs:

```
```
the only proven way to troubleshoot those is
```sh
docker-compose stop
docker container ls -a | awk '{print $1}' | xargs -IX docker container stop X
docker container  prune -f
docker image prune -f -a
sudo killall docker-compose
sudo systemctl stop docker
sudo systemctl start docker
```
NOTE: the disk usage seems to show some shortge after this cycle.

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


### Build Docker Image Locally

* build the alpine based  mysql 5.x Docker image:

```sh
export SERVER_IMAGE=alpine-mysql
docker build -f Dockerfile.alpine-mysql -t $SERVER_IMAGE .
```
* run it with environments matching the `application.properties`:
```sh
export SERVER_NAME=mysql-server
docker run --name $SERVER_NAME -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=password -d $SERVER_IMAGE
```
* observe the successful start log message in `mysql-server` container:
```sh
docker logs $SERVER_NAME
```
* verify the console connection:
```sh
docker exec -it $SERVER_NAME mysql -P 3306 -h localhost -u java -ppassword -e "set @var = '1'; select @var ;"
```
### Plain docker-compose test


```sh
docker-compose -f docker-compose.plain.yaml  stop


export COMPOSE_HTTP_TIMEOUT=600
docker-compose -f  docker-compose.plain.yaml up --build
docker-compose -f docker-compose.plain.yaml up
```

then observe only one node to be running
```sh
docker ps
```
observe the error logs
```
ID=$(docker container ls -a | grep '_app_' | awk '{print $1}')
docker logs  $ID
docker container rm $ID
```

followed
by re-running compose
```sh
docker-compose -f  docker-compose.plain.yaml up --build
```
- this will rebuild the `_app_` node with Java app 
and  seeing healty response to REST API
```sh
curl http://localhost:8086/all/
```
```sh
[]
```
### Cleanup

```sh
docker container prune -f
``
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
### Dedicated Node

One can compose `docker-compose.yaml` with separate dependency waiter node e.g. based on bare alpine:
```yaml
  delayed_start:
    container_name: delayed_start
    image: dadarek/wait-for-dependencies
    depends_on:
      - mysql-server
    # NOTE: when node does not recognize hostname, check the networks
    # nc: bad address 'mysql-server'
    command: mysql-server:3306
    networks:
      - example
  app:
    depends_on:
      # the db server name needs to match setting in application.properties
      - delayed_start
```
or one may use an alpine-jre base image which is likely to be used by appplication image anyway and be able to perform data specific probe, e.g. through JDBC:
```yaml
  delayed_start:
    container_name: delayed_start
    build:
      context: .
      dockerfile: Dockerfile.delayed_start
    image: openjdk:8-jre-alpine3.9
    depends_on:
      - mysql-server
    environment:
      SERVICE_PORT: 3306
      SERVICE_HOST: 'mysql-server'
    networks:
      - example
  app:
    depends_on:
      # NOTE: db server name has to match application.properties
      - delayed_start
```
with the `Dockerfile.delayed_start` hosting the same operations as before:
```sh
FROM openjdk:8-jre-alpine3.9
ADD "target/${app_jar}" app.jar
ENTRYPOINT ["sh", "/delayed_start.sh"]
```
### See also
  * mysql docker image `Dockerfile` based on [alpine](https://hub.docker.com/r/wangxian/alpine-mysql)
  * best practices of [startup/shutdown order](https://docs.docker.com/compose/startup-order/) and [wait for dependencies](https://8thlight.com/blog/dariusz-pasciak/2016/10/17/docker-compose-wait-for-dependencies.html) in __docker-compose__
  * [k8n](https://kubernetes.io/docs/tasks/configure-pod-container/configure-liveness-readiness-startup-probes/) __liveness__, __readiness__ and __startup__ Probes
  * standalone [dependency waiter](dadarek/wait-for-dependencies) image.
