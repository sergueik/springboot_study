### Info

derivative ot plain Servlet based REST service backed by PostgreSQL JDBC [project](https://github.com/markuszver/skytecapp). Replaced `jakarta.servlet-api` (which was not working for me)  with `javax.servlet.javax.servlet-api`. Added multiparam JDBC method

### Usage
* package application
```sh
mvn package
```
* stop the database server possibly running on host
```sh
sudo /etc/init.d/postgresql stop
```
```text
[ ok ] Stopping postgresql (via systemctl): postgresql.service.
```
* start container with alpine postgres
```sh
DATABASE=postgres-database
docker container start $DATABASE
```
* create database and table

```sh
psql -h localhost -p 5432 --username postgres --password example
```
```SQL
CREATE TABLE IF NOT EXISTS data ( id serial PRIMARY KEY NOT NULL, key varchar(100) NOT NULL, value varchar(250) NOT NULL, rand smallint NOT NULL);
```
* insert some data

```SQL
INSERT INTO data (key,value,rand) VALUES ('foo 1', 'bar',42), ('foo 2', 'bat, 42), ('foo 3', 'baz',42) ;
```
```SQL
SELECT * FROM data;
```
```text
 id |  key  | value | rand
----+-------+-------+------
  1 | foo 1 | bar   |   42
  2 | foo 2 | bat   |   42
  3 | foo 3 | baz   |   42
```
* package the war into tomcat __8.5__
```sh
IMAGE=postgres-servlet
docker build -t $IMAGE -f Dockerfile .
```
* run linked
```sh
NAME=postgres-servlet
docker run --name $NAME --link $DATABASE -p 8080:8080 -d $IMAGE
```

* test
```sh
curl http://192.168.0.64:8080/demo/hello-servlet
```
* monitor container logs

```sh
docker logs -f $NAME
```
### Cleanup
```sh
docker container rm -f $NAME
docker image rm -f $IMAGE
docker image prune -f
```
### See Also

  * https://github.com/Kroshkazavr/data_base_service
  * java 7 (tomcat 8) spring mvc, jdbc, thymeleaf, postgresql [example project](https://github.com/whoscared/LibraryDemo)
  * springboot thymeleaf postgresql [example project](https://github.com/arkenidar/spring-demo1)
  * https://github.com/anicetkeric/Java-stored-procedures-postgresql

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
