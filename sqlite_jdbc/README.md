### Ifo

This directory contains a skeleton spring/sqlite JDBC project based on
[Spring Boot MVC Demo with JDBC and SqlServer](https://github.com/wuwei1024/SpringBoot-MVC-JDBC-SqlServer) but with SQLite
instead  of MS SQL Server accessed through JDBC.


### Prerequisites

This project operates the SQLite database through straight SQL - not using JPA
plain JDBC static methods `JDBCUtils.getConnection()`, `JDBCUtils.TranverseToList()` etc.

Create the sqlite database directory

```sh
pushd ~
mkdir sqlite
```
and create database `~/sqlite/springboot.db` with a table
```sql
DROP TABLE IF EXISTS `student`;
CREATE TABLE IF NOT EXISTS `student` (
	`id`	INTEGER PRIMARY KEY AUTOINCREMENT,
	`name`	TEXT NOT NULL,,
	`course`	TEXT NOT NULL,,
	`addtime`	datetime NOT NULL DEFAULT current_timestamp
);
```
alternatively create table at desktop and update `src/main/resources/application.properties`
and insert some data
```sql
INSERT INTO student(name,course) VALUES ('Jack','Chinese');
INSERT INTO student(name,course) VALUES ('Tom','Computer');
``` 
and build and start project as regular springboot application
```cmd
mvn -Dmaven.test.skip=true clean  spring-boot:run
```

### Testing

Verify it works via Postman or curl (one will need to specify POST method in all requests).
The application was originally designed with Spring 4 and is being convered to Spring 5.x to be able to test some packages which did not exist prior.


#### On Spring 5.x

```sh
curl -X POST http://127.0.0.1:8181/student/findAllStudent |jq
```
returns
```json
{
    "status": 1,
    "data": [{
        "id": 2,
        "name": "John",
        "course": "Guitar"
    }, {
        "id": 3,
        "name": "Ringo",
        "course": "Drums"
    }]
}
```
and
```sh
curl -X POST -H 'application/x-www-form-urlencoded' -d 'id=2' http://127.0.0.1:8181/student/findStudentById | jq
```
returns
```json
{
  "status":0,
  "data":{
    "id":2,
    "name":"John",
    "course":"Music"
  }
}
```


#### On Spring 4.X
```sh
curl -X POST http://127.0.0.1:8181/test/student/findAllStudent | jq
```
returns
```json
{
    "status": 1,
    "data": [{
        "id": 2,
        "name": "John",
        "course": "Guitar"
    }, {
        "id": 3,
        "name": "Ringo",
        "course": "Drums"
    }]
}
```
```sh
curl -X POST -H "application/x-www-form-urlencoded" -d "id=2" http://127.0.0.1:8181/test/student/findStudentById |jq
```
returns
```json
{
  "status":0,
  "data":{
    "id":2,
    "name":"John",
    "course":"Music"
  }
}
```
and

```sh
curl -X POST -H "Content-Type: application/json" -d '{"name":"John"}' http://127.0.0.1:8181/test/student/findStudentByName
returns
```json
{
  "status":0,
  "data":{
    "id":2,
    "name":"John",
    "course":"Music"
  }
}
```

Other supported routes are `updateStudent`, `delStudentById`, `addStudent`.
### Docker Exercise

### Shell Version

* update `src/main/resources/application.properties` and repackage
```sh
docker build -f Dockerfile.shell -t sqlite-shell .
```
 
```sh
docker run -it -v ${HOME}/Desktop/:/db sqlite-shell
```
```sh
sqlite> .tables
student  user   

sqlite> .schema user
CREATE TABLE user 
  ( 
     id          INTEGER, 
     nick_name   VARCHAR, 
     user_sex    INTEGER, 
     user_gender INTEGER, 
     password    VARCHAR, 
     name        VARCHAR, 
     PRIMARY KEY (id) 
  );
sqlite> select count(1) from user;
3
.quit
```
#### Java Version
* uncoment the relevant path in `spring.datasource.url` in `src/main/resources/application.properties`:
```
spring.datasource.url=jdbc:sqlite:/db/springboot.db

```
* repackage
```sh
mvn clean package
```
```sh
docker build -f Dockerfile.jdbc -t sqlite-jdbc .
```
* run Docker container
```sh
docker run -it -v ${HOME}/Desktop/:/db -p 8181:8181 sqlite-jdbc
```
* verify
```sh
curl -X POST http://127.0.0.1:8181/student/findAllStudent 2>/dev/null |jq '.'
```
returns
```json
{
  "status": 1,
  "data": [
    {
      "id": 2,
      "name": "John",
      "course": "Music"
    },
    {
...
```
* cleanup
```sh
docker container ls -a | grep sqlite | cut -d ' ' -f 1 | xargs -IX docker container rm X
```
```sh
docker image rm -f sqlite-shell sqlite-jdbc
docker image ls |  grep '<none>' | awk '{print $3}' | xargs -IX  docker image rm -f X
```
### See Also

For SQLite Hibernate project example, see [restart1025/Spring-Boot-SQLite](https://github.com/restart1025/Spring-Boot-SQLite)
or [CherryYu/springboot-sqlite](https://github.com/CherryYu/springboot-sqlite)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
