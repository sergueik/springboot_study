### Info

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
CREATE TABLE `student` (
  `id`  INTEGER,
  `name`  TEXT NOT NULL,
  `course`  TEXT NOT NULL,
  PRIMARY KEY(`id`)
);
```
and build and start project as regular springboot application
```cmd
mvn clean spring-boot:run
```

### Testing

Verify it works via Postman or curl (needs the POST)
```sh
curl -X POST http://127.0.0.1:8181/test/student/findAllStudent
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
curl -X POST -H "application/x-www-form-urlencoded" -d "id=2" http://127.0.0.1:8181/test/student/findStudentById
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

### Note

This project seems to require springboot version __1.5.4.RELEASE__.
With Springboot __2.1.2.RELEASE__ found all tests to fail with the server status 404 response
with errors like
```json
{
  "timestamp":"2019-11-09T01:19:42.256+0000",
  "status":404,
  "error":"Not Found",
  "message":"No message available",
  "path":"/test/student/findStudentByName"
}
```
which seems to indicate incomplete springboot application initialization due to possibly poor jar versions. No console log of interest is coming from applicatiion itself.

### See Also

For SQLite Hibernate project example, see [restart1025/Spring-Boot-SQLite](https://github.com/restart1025/Spring-Boot-SQLite)
or [CherryYu/springboot-sqlite](https://github.com/CherryYu/springboot-sqlite)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
