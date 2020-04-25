### Info

This directory contains a basic springboot jdbc on postgresql project based on
[KominfoPemudaPersis/springboot-jdbc-postgres](https://github.com/KominfoPemudaPersis/springboot-jdbc-postgres)
and alpine postgres basic Dockerfile from [github repository](https://hub.docker.com/r/kiasaki/alpine-postgres/dockerfile)

### Run application

* switch to password auth locally
```sh
sudo -u postgres psql
```
```sh
ALTER USER postgres PASSWORD 'postgres';
ALTER ROLE
```
* verify the credentials
```sh
psql -h localhost -p 5432 --username postgres --password
```
```sh
postgres=# \c
```
* create database and table locally
```sh
sudo -u postgres psql
```
```sh
postgres=# create database example;
CREATE DATABASE
postgres=# \c example
You are now connected to database "example" as user "postgres".
example=# CREATE TABLE rest ( id serial PRIMARY KEY NOT NULL, key varchar(100) NOT NULL, value varchar(250) NOT NULL, rand smallint NOT NULL);
```
* run locally
```sh
mvn -Dmaven.test.skip=true clean spring-boot:run
```
* test 
```sh
curl -X POST -H "Content-Type: application/json" -d '{"key":"some example", "value":"some data"}' http://127.0.0.1:8080/rest | jq '.'
```
will respond with
```json
{
  "id": 1,
  "rand": 21,
  "key": "some example",
  "value": "some data"
}
```
and then
```sh
curl http://127.0.0.1:8080/rest/ | jq '.'
```
shows
```json
[
  {
    "id": 1,
    "rand": 21,
    "key": "some example",
    "value": "some data"
  }
]
```
and
```sh
\c example
Password for user postgres: 
SSL connection (protocol: TLSv1.3, cipher: TLS_AES_256_GCM_SHA384, bits: 256, compression: off)
You are now connected to database "example" as user "postgres".
example=# select *  from rest;
```
returns
```sh
 id | key          | value      | rand 
----+--------------+------------+------
  1 | some example | some data  |   21
```
and
```sh
curl -X PUT -H "Content-Type: application/json" -d '{"key":"example", "value":"new data"}' http://127.0.0.1:8080/rest/1 | jq '.'
```
will update:
```json
{
  "id": 1,
  "rand": 12,
  "key": "example",
  "value": "new data"
}
```
### Run in Docker
* package the jar 
```sh
mvn -Dmaven.test.skip=true clean package
```
__NOTE:__ This currenlty is failing with apk specific dependency conflict

* pull smallest possible postgresql container image 
```
docker pull kiasaki/alpine-postgres
```
launch the database named container
```sh
SERVER_NAME=postgres-database
docker run --name $SERVER_NAME -e POSTGRES_PASSWORD=postgres -d kiasaki/alpine-postgres
```
- will fail

rebuild the container
```sh
SERVER_IMAGE=alpine-postgres
docker build -f Dockerfile.alpine-postgres -t $SERVER_IMAGE .
```
run contained from the built image
```sh
SERVER_NAME=postgres-database
docker run --name $SERVER_NAME -e POSTGRES_PASSWORD=postgres -d $SERVER_IMAGE
```

* create database
```sh
docker exec -it $SERVER_NAME psql -h localhost -p 5432 --username postgres -c "create database example"
```
* drop table
```sh
docker exec -it $SERVER_NAME psql -h localhost -p 5432 --username postgres --dbname example -c "drop table rest"
```
* optionlly rebuild the java container
```
IMAGE=postgres-example
docker build -f Dockerfile -t $IMAGE .
```
* run aplication
```sh
NAME=example-postgres
docker run --name $NAME --link $SERVER_NAME -p 8080:8080 -d $IMAGE
```
* monitor logs:
```sh
docker logs $NAME
```
* run aplication linked to postgres container
* repeat the curl checks
### TODO:
The bolierplate test class is incomplete:

```sh
java.lang.IllegalStateException: Unable to find a @SpringBootConfiguration, you need to use @ContextConfiguration or @SpringBootTest(classes=...) with your test
```

The current configuration  does now allow `application/x-www-form-urlencoded` form posts
```sh
curl -X POST -H "application/x-www-form-urlencoded" -d "key=example&value=data&rand=123" 127.0.0.1:8080/rest/
```
raises the exception
```json
{
  "timestamp": "2020-04-13T14:39:52.197+0000",
  "status": 415,
  "error": "Unsupported Media Type",
  "message": "Content type 'application/x-www-form-urlencoded;charset=UTF-8' not supported",
  "trace": "org.springframework.web.HttpMediaTypeNotSupportedException: 
  Content type 'application/x-www-form-urlencoded;charset=UTF-8' not supported
  at org.springframework.web.servlet.mvc.method.annotation.AbstractMessageConverterMethodArgumentResolver.readWithMessageConverters(AbstractMessageConverterMethodArgumentResolver.java:224)
  at org.springframework.web.servlet.mvc.method.annotation.RequestResponseBodyMethodProcessor.readWithMessageConverters(RequestResponseBodyMethodProcessor.java:157)
  at org.springframework.web.servlet.mvc.method.annotation.RequestResponseBodyMethodProcessor.resolveArgument(RequestResponseBodyMethodProcessor.java:130)
  at org.springframework.web.method.support.HandlerMethodArgumentResolverComposite.resolveArgument(HandlerMethodArgumentResolverComposite.java:126)
  at org.springframework.web.method.support.InvocableHandlerMethod.getMethodArgumentValues(InvocableHandlerMethod.java:166)
  at org.springframework.web.method.support.InvocableHandlerMethod.invokeForRequest(InvocableHandlerMethod.java:134)
  at org.springframework.web.servlet.mvc.method.annotation.ServletInvocableHandlerMethod.invokeAndHandle(ServletInvocableHandlerMethod.java:102)
  at org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerAdapter.invokeHandlerMethod(RequestMappingHandlerAdapter.java:895)
  at org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerAdapter.handleInternal(RequestMappingHandlerAdapter.java:800)
  at org.springframework.web.servlet.mvc.method.AbstractHandlerMethodAdapter.handle(AbstractHandlerMethodAdapter.java:87)
  at org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:1038)
  at org.springframework.web.servlet.DispatcherServlet.doService(DispatcherServlet.java:942)
  at org.springframework.web.servlet.FrameworkServlet.processRequest(FrameworkServlet.java:1005)
  at org.springframework.web.servlet.FrameworkServlet.doPost(FrameworkServlet.java:908)
  at javax.servlet.http.HttpServlet.service(HttpServlet.java:660)
  at org.springframework.web.servlet.FrameworkServlet.service(FrameworkServlet.java:882)
  at javax.servlet.http.HttpServlet.service(HttpServlet.java:741)
        ...
  at java.lang.Thread.run(Thread.java:748)
",
  "path": "/rest/"
}
```
### Cleanup
```sh
psql -h localhost -p 5432 --username postgres --dbname example --command "drop table rest;"
```
```
docker stop $NAME
docker container rm $NAME
docker container prune -f
docker image rm $IMAGE -f
docker image prune -f

```
### See also

 * another basic [jdbc postgress example](https://github.com/christosperis/spring-jdbctemplate-postgresql-example)
 * [initialize default user and password in postgresql](https://chartio.com/resources/tutorials/how-to-set-the-default-user-password-in-postgresql/)
 * postgresql [command rederence](https://www.tutorialspoint.com/postgresql/postgresql_select_database.htm)
 * Docker [image](https://hub.docker.com/r/kiasaki/alpine-postgres/) and [github repository](https://hub.docker.com/r/kiasaki/alpine-postgres/dockerfile)
 * configuring "sticky" versions with [apk](https://superuser.com/questions/1055060/how-to-install-a-specific-package-version-in-alpine)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
