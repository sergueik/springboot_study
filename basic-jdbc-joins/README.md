### Info

This directory contains a skeleton MySQL Jdbc Spring Boot web application [spring-boot-jdbc-join](https://github.com/Java-Gyan-Mantra/spring-boot-jdbc-join) exercising basic SQL Joins
working with [alpine-mysql](https://github.com/epicallan/alpine-mysql)
a docker image base on alpine with mysql


### Testing

Pull the collaborator Docker image:

```sh
docker pull mysql:8.0.18
```
and run it with environments matching the `application.properties`:
```sh
export MYSQL_USER='java'
export MYSQL_PASSWORD='password'
docker run -p 3306:3306 --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=$MYSQL_USER -e MYSQL_DATABASE=join_check -e MYSQL_PASSWORD=$MYSQL_PASSWORD -d mysql:8.0.18
```
### build Mysql Server Image

* with Alpine based image
```sh
docker build -t mysql-server-alpine -f Dockerfile.mysql-server-alpine .
```
make sure to not provide volume argument `-v $(pwd):/app` (in the orginal project the database was used for interactive work):
```sh
export MYSQL_USER='java'
export MYSQL_PASSWORD='password'
docker run -d --name mysql-server-alpine -p 3306:3306 -e MYSQL_DATABASE=join_check -e MYSQL_USER=$MYSQL_USER -e MYSQL_PASSWORD=${MYSQL_PASSWORD} -e MYSQL_ROOT_PASSWORD=password mysql-server-alpine
```
connect and [check it remote accesss allowed](https://mariadb.com/kb/en/configuring-mariadb-for-remote-client-access/):
```sh
docker exec -it mysql-server-alpine sh
```
```sh
mysql --host=localhost --protocol=tcp --port=3306
```
```text
Welcome to the MariaDB monitor.
```
Note: one does not need to specify the `-p 3306:3306` argument.  Note: same user and password is used in `DB/users.sql` and java `database.properties`
The enviroment entries `MYSQL_ROOT_PASSWORD`, `MYSQL_USER`,`MYSQL_DATABASE`, `MYSQL_PASSWORD` are required by Mysql docker image.
Note, it will take the Docker instance  quite some time to launch.
Run
```sh
SERVER_ID=$(docker container ls -a | grep mysql-server | awk '{print $1}')
docker start $SERVER_ID
```
if this is the first time, then
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u root -ppassword
```
paste the `src/main/resources/users.sql`


* verify
```sql
SELECT User, Host, authentication_string from mysql.user;
```
then paste `src/main/resources/tables.sql
```sql
CREATE DATABASE join_check;
USE join_check;
CREATE TABLE t1 ( id INT PRIMARY KEY, pattern VARCHAR(50) NOT NULL, name varchar(250) );

CREATE TABLE t2 ( id VARCHAR(50) PRIMARY KEY, pattern VARCHAR(50) NOT NULL, data varchar(250) );

INSERT INTO t1(id, pattern,name) VALUES(1,'Divot','Basant'), (2,'Brick','Santosh'), (3,'Grid','Chinmaya');

INSERT INTO t2(id, pattern,data) VALUES('A','Brick','B'), ('B','Grid','S'), ('C','Diamond','C');

```
* verify join query
```sql

SELECT t1.id, t2.id as new_id,t1.name,t2.data FROM t1 JOIN t2 ON t1.pattern = t2.pattern;
```
* Build the `jdbc-example` Docker image
* uncomment the localhost connection:
```java
```
then run
```sh
mvn test
```
* uncomment the following line in `src/main.resources/datasource.properties`:
```sh
jdbc.server=mysql-server
```
or
```sh
jdbc.server=mysql-server-alpine
```
Note: maven command line option
```sh
mvn clean -Djdbc.server=mysql-server -Dmaven.test.skip=true clean package
```
is ignored during packaging
```sh
IMAGE=jdbc-example
docker build -f Dockerfile -t $IMAGE .
```
* launch the Docker container linked to the `mysql-server` 
```sh
CONTAINER=jdbc-example
docker container rm -f $CONTAINER
docker run  --name $CONTAINER -p 8080:8080 --link mysql-server -d $IMAGE
docker logs $CONTAINER
```
or with `mysql-server-alpine`
```sh
CONTAINER=jdbc-example
docker container rm -f $CONTAINER
docker run --name $CONTAINER -p 8080:8080 --link mysql-server-alpine -d $IMAGE
docker logs $CONTAINER
```
CONTAINER=jdbc-exampleCONTAINER=jdbc-exampleCONTAINER=jdbc-examplethis will show , along with other logs,
```sh
INFO  example.config.JdbcConfiguration - Datasource URL: jdbc:mysql://mysql-server:3306/cardb?characterEncoding=UTF-8&rewriteBatchedStatements=true
```
NOTE: with __mysql 8.1__ the application fails to start. The following is logged to console
```text
java.lang.IllegalStateException: Failed to execute
Caused by: java.sql.SQLException: Access denied for user 'java'@'172.17.0.3' (using password: YES)
```
* test
```sh
curl -k http://localhost:8080/data | jq '.'
```
```json
[
  {
    "id": 2,
    "new_id": "A",
    "name": "Santosh",
    "data": "B"
  },
  {
    "id": 3,
    "new_id": "B",
    "name": "Chinmaya",
    "data": "S"
  }
]

```
### See Also

  * https://www.toolsqa.com/rest-assured/deserialize-json-array-to-an-array/
  * https://github.com/rest-assured/rest-assured/issues/805
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


