### Info

This directory contains a basic springboot jdbc on postgresql project based on
[KominfoPemudaPersis/springboot-jdbc-postgres](https://github.com/KominfoPemudaPersis/springboot-jdbc-postgres)
and alpine postgres basic Dockerfile from [github repository](https://hub.docker.com/r/kiasaki/alpine-postgres/dockerfile)

### Run application
* install postgres locally 
```sh
sudo apt-get -qy install postgresql
```
* switch to password auth locally
```sh
sudo -u postgres psql
```
```sh
ALTER USER postgres PASSWORD 'postgres';
```
```sh
ALTER ROLE
```
```sh
\q
```
* verify the credentials
```sh
psql -h localhost -p 5432 --username postgres --password
```
```sh
postgres=# \c
```

```sh
Password:
```
```sh
SSL connection (protocol: TLSv1.3, cipher: TLS_AES_256_GCM_SHA384, bits: 256, compression: off)
You are now connected to database "postgres" as user "postgres"
```
close the connection

* create database and table locally
```sh
sudo -u postgres psql
```
```text
psql (10.22 (Ubuntu 10.22-0ubuntu0.18.04.1))
Type "help" for help.

postgres=#
```
```sql
select datname from pg_database;
```
```text
  datname
-----------
 postgres
 template1
 template0
(3 rows)

```
if you do not see the `example` database in the output proceeed with (see [stackoverflow](https://stackoverflow.com/questions/18389124/simulate-create-database-if-not-exists-for-postgresql)):

```sql
create database example;
```
```sh
CREATE DATABASE
```
```sh
postgres=# \c example
```
```
You are now connected to database "example" as user "postgres".
example=# 
```
```sh
CREATE TABLE rest ( id serial PRIMARY KEY NOT NULL, key varchar(100) NOT NULL, value varchar(250) NOT NULL, rand smallint NOT NULL);
```
* run locally
```sh
mvn clean spring-boot:run
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

if there is an exception make sure that onnection is using the `localhost` in the `application.properties`:


```sh
sed -i 's|//\(.*\):5432|//localhost:5432|' src/main/resources/application.properties
```
```sh
cat src/main/resources/application.properties
```
and rebuild the app
```sh
mvn -Dmaven.test.skip=true clean spring-boot:run
```
NOTE: the command line option `-Dspring.datasource.url='jdbc:postgresql://localhost:5432/example'` will have no effect

and if the exception
```sh
nested exception is org.postgresql.util.PSQLException: ERROR: relation "rest" already exists
```
make  sure to drop the table:
```sh
psql -h localhost -p 5432 --username postgres --password example
```
```sh
example=# drop table rest;
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
  },
  {
    "id": 2,
    "rand": 0,
    "key": "another example",
    "value": "some more data"
  }  
]
```
and in the postgres console,
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
can now uninstall postgresql

### Run in Docker
* package the jar 
```sh
mvn clean package
```

* pull the smallest possible postgresql container image 
```
docker pull kiasaki/alpine-postgres
```
launch the database named container
```sh
SERVER_NAME=postgres-database
docker run --name $SERVER_NAME -e POSTGRES_PASSWORD=postgres -d kiasaki/alpine-postgres
```
- this will fail

change `SERVER_IMAGE` and rebuild the container from plain alpine:
```sh
SERVER_IMAGE=alpine-postgres
docker build -f Dockerfile.$SERVER_IMAGE -t $SERVER_IMAGE .
```
run container from the built image. NOTE: not exposing the port `5432`, will be connecting containrs over Docker network.
```sh
SERVER_NAME=postgres-database
docker run --name $SERVER_NAME -e POSTGRES_PASSWORD=postgres -d $SERVER_IMAGE
```
* run
```sh
docker logs -f $SERVER_NAME
```
wait for a few minutes until seeing the connect invite 
NORE: the log entry from PGAdmin (PGAdmin III on Xenial) is unrelated

```text
database system is ready to accept connections
```
confirm the warning dialog

![PG Admin III](https://github.com/sergueik/springboot_study/blob/master/basic-postgresql/screenshots/capture_pgadmin_iii.png)

- turns out one cannot use PG Admin III with Postresql __12.x__, so for desktop testing one needs __bionic__ __18.04__ or later release of Ubuntu

* create database
```sh
docker exec -it $SERVER_NAME psql -h localhost -p 5432 --username postgres -c "create database example"
```
this will print 
```text
CREATE DATABASE
```
* drop table
```sh
docker exec -it $SERVER_NAME psql -h localhost -p 5432 --username postgres --dbname example -c "drop table rest"
```
NOTE: the table may not exist yet.
* update the `application.properties` to use docker dns hostname:
```java
# for container deployed app
# uncomment the following line
spring.datasource.url=jdbc:postgresql://postgres-database:5432/example
# for local run
# uncomment the following line
# spring.datasource.url=jdbc:postgresql://localhost:5432/example
```
and repackage:
```sh
mvn clean package
```
* optionlly rebuild the java container
```sh
IMAGE=postgres-example
docker build -f Dockerfile -t $IMAGE .
```
* run application in continer linked to the postgres container `$SERVER_NAME`:
```sh
NAME=example-postgres
docker run --name $NAME --link $SERVER_NAME -p 8080:8080 -d $IMAGE
```
NOTE: do not forget to stop the spring-boot run one earlier or get an error:
```text
Error response from daemon: driver failed programming external connectivity on endpoint example-postgres (4d734f6c5e503d0011591b22f027d77f537a3c97bd835bd0d339bc9c06e0054d): Error starting userland proxy: listen tcp 0.0.0.0:8080: bind: address already in use.
```
* monitor logs:
```sh
docker logs $NAME
```
* run aplication linked to postgres container
* repeat the curl checks

```
curl -s -X PUT -H "Content-Type: application/json" -d '{"key":"example", "value":"new data"}' http://127.0.0.1:8080/rest/1 | jq '.'
```
```js
{
  "timestamp": 1665182943733,
  "status": 500,
  "error": "Internal Server Error",
  "exception": "org.springframework.dao.EmptyResultDataAccessException",
  "message": "Incorrect result size: expected 1, actual 0",
  "path": "/rest/1"
}
```
```sh
curl -s http://127.0.0.1:8080/rest/ | jq '.'
```
```js
[]
```

```sh
curl -s -X POST -H "Content-Type: application/json" -d '{"key":"another example", "value":"some data"}' http://127.0.0.1:8080/rest | jq '.'
```

```js
{
  "id": 1,
  "rand": 29,
  "key": "another example",
  "value": "some data"
}

```
```sh
curl -s http://127.0.0.1:8080/rest/ | jq '.'
```
```js
[
  {
    "id": 1,
    "rand": 29,
    "key": "another example",
    "value": "some data"
  }
]

```
```sh
curl -s -X PUT -H "Content-Type: application/json" -d '{"key":"example", "value":"new data"}' http://127.0.0.1:8080/rest/1 | jq '.'
```
```js
{
  "id": 1,
  "rand": 38,
  "key": "example",
  "value": "new data"
}

```
### TODO:

The current configuration of java app does now accept the `application/x-www-form-urlencoded` content-type POST requests:
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
```sh
docker stop $NAME
docker container rm $NAME
docker container prune -f
docker image rm $IMAGE -f
docker image prune -f
```
### Testing Qeury
* dummy run (echo)
```sh
curl "http://localhost:8080/rest/queryparam?ids=1,2,3,4,5&keys=example"
```
```text
appids: foo,bar ids: 1,2,3,4,5
```
in the application log will see the query result:
```txt
query returned: 5 rows
```
insert more data to query:
```sh
for CNT in $(seq 1 1 10); do curl -X POST -H "Content-Type: application/json" -d "{\"key\":\"example $CNT\", \"value\":\"some data $CNT\"}" http://127.0.0.1:8080/rest | jq '.' ; done
```
this will print a series of entries inserted to the database
```json
{
  "id": 14,
  "rand": 13,
  "key": "some example 9",
  "value": "some data 9"
}
```
```json
{
  "id": 15,
  "rand": 7,
  "key": "some example 10",
  "value": "some data 10"
}

```
by providing matching / out of range `ids` and observing the returned row counts one can be certain the condition is taken into account:
```SQL
"select * from rest  " + String.format("where id in (%s)", String.join(",", Arrays.asList(marks)));
```

### Multiple Condition Query

```sh
curl "http://localhost:8080/rest/queryparam?ids=1,2,3,5,6,7&keys=example,example+1"
```
will echo to the curl console:
```text
keys: example,example 1 ids: 1,2,3,5,6,7
```
the springboot application server console log will show:
```text
query by ids returned: 6 rows
args [1, 2, 3, 5, 6, 7, example, other]
query by ids and keys returned: 6 rows
```

### Troubleshooting

```text
	 org.springframework.jdbc.BadSqlGrammarException: 
	 PreparedStatementCallback; 
	 bad SQL grammar 
	 [select id,key,value from rest where id in (?,?,?) and key in (?)]; 
	 nested exception is org.postgresql.util.PSQLException: 
	 ERROR: operator does not exist: integer = character varying
	 Hint: No operator matches the given name and argument types. 
	 You might need to add explicit type casts.
	 Position: 40

```
### Install PG Admin
* on Bionic can 
```sh
sudo apt-get install pgadmin4-desktop
```
this will install version __6.14__

![PG Admin 4 Splash Screen](https://github.com/sergueik/springboot_study/blob/master/basic-postgresql/screenshots/capture-pg4_splash.png)

if the application was installed, you may need to find out the last usedmaster password and unlock it:
![PG Admin 4 Splash Screen](https://github.com/sergueik/springboot_study/blob/master/basic-postgresql/screenshots/capture-pg4_unlock_password.png)

Follow the Debian system install [steps](https://www.pgadmin.org/download/pgadmin-4-apt/)
```sh
curl -l -k -o - https://www.pgadmin.org/static/packages_pgadmin_org.pub | sudo apt-key add
```
```sh
sudo sh -c 'echo "deb https://ftp.postgresql.org/pub/pgadmin/pgadmin4/apt/$(lsb_release -cs) pgadmin4 main" > /etc/apt/sources.list.d/pgadmin4.list && apt-get update'
```
```sh
sudo apt install pgadmin4-desktop
```
![PG Admin 4](https://github.com/sergueik/springboot_study/blob/master/basic-postgresql/screenshots/capture_pgadmin_4.png)

ignore the warning on __Xenial__
```text
W: The repository 'https://ftp.postgresql.org/pub/pgadmin/pgadmin4/apt/xenial pgadmin4 Release' does not have a Release file.
N: Data from such a repository can't be authenticated and is therefore potentially dangerous to use.
N: See apt-secure(8) manpage for repository creation and user configuration details.
E: Failed to fetch https://ftp.postgresql.org/pub/pgadmin/pgadmin4/apt/xenial/dists/pgadmin4/main/binary-amd64/Packages  server certificate verification failed. CAfile: /etc/ssl/certs/ca-certificates.crt CRLfile: none
E: Some index files failed to download. They have been ignored, or old ones used instead.
```
if the error remains, install via pip following the [steps](https://wpcademy.com/how-to-install-pgadmin-on-ubuntu-16-04-lts/)

alternatively install legacy version
```sh
apt-get download pgadmin3
sudo apt-get install libpq5 pgadmin3-data postgresql-client postgresql-client-9.5 postgresql-client-common
sudo dpkg -i pgadmin3*
```
turns out one cannot connect to a newer PostgreSQL from older PG Admin
#### TODO 
One can install [docker container](https://hub.docker.com/r/dpage/pgadmin4/tags) with based web client pg admin 4 on xenial. Alternatiely pick a smalleralpine based [image](https://hub.docker.com/layers/huggla/sam-pgadmin/4.20/images/sha256-8867f605c49e9ccdbb52858decd15258c144c7b183dd2532964bed48bcb0dcc5?context=explore)	

### See also

 * another basic [jdbc postgress example](https://github.com/christosperis/spring-jdbctemplate-postgresql-example)
 * [initialize default user and password in postgresql](https://chartio.com/resources/tutorials/how-to-set-the-default-user-password-in-postgresql/)
 * postgresql [command rederence](https://www.tutorialspoint.com/postgresql/postgresql_select_database.htm)
 * Docker [image](https://hub.docker.com/r/kiasaki/alpine-postgres/) and [github repository](https://hub.docker.com/r/kiasaki/alpine-postgres/dockerfile)
 * configuring "sticky" versions with [apk](https://superuser.com/questions/1055060/how-to-install-a-specific-package-version-in-alpine)
  * [basics of installing postgresql in ubuntu](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-18-04)
  * [pgadmin](https://www.pgadmin.org/download/)
  * Cluster conriguration [instructions](https://linuxhint.com/postgresql_docker/)  for `dpage/pgadmin4` and `postgres`
  * misc. PostgreSQL hint link [post](https://habr.com/ru/post/667428/) (in Russian)  

  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
