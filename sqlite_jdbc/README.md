### Info

This directory contains a skeleton spring/sqlite JDBC project based on
[Spring Boot MVC Demo with JDBC and SqlServer](https://github.com/wuwei1024/SpringBoot-MVC-JDBC-SqlServer)

It operates the database via plain JDBC static methods `JDBCUtils.getConnection()`, `JDBCUtils.TranverseToList()` and straight SQL - not using JPA.

To build, create the sqlite database directory
```sh
pushd ~
mkdir sqlite
```
and create database `~/sqlite/springboot.db` with table
```sql
CREATE TABLE `student` (
  `id`  INTEGER,
  `name`  TEXT NOT NULL,
  `course`  TEXT NOT NULL,
  PRIMARY KEY(`id`)
);
```
and build and start as a regular spring-boot application
```cmd
mvn clean spring-boot:run
```
Verify it works via Postman or curl (needs the POST)
```sh
curl -X POST http://127.0.0.1:8181/test/student/findAllStudent
```
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
The other supported routes are `updateStudent`, `delStudentById`, `addStudent`.


For SQLite Hibernate project example, see [restart1025/Spring-Boot-SQLite](https://github.com/restart1025/Spring-Boot-SQLite)
or [CherryYu/springboot-sqlite](https://github.com/CherryYu/springboot-sqlite)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
