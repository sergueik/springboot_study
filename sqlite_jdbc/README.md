### Ifo

This directory contains a skeleton spring/sqlite JDBC project based on
[Spring Boot MVC Demo with JDBC and SqlServer](https://github.com/wuwei1024/SpringBoot-MVC-JDBC-SqlServer) but with SQLite
instead  of MS SQL Server accessed through JDBC.


### Prerequisites

This project operates the SQLite database through straight SQL - not using JPA
plain JDBC static methods `JDBCUtils.getConnection()`, `JDBCUtils.TranverseToList()` etc.

Create the sqlite database on Desktop `springboot.db` 

```sh
pushd ~
sqlite3 Desktop/springboot.db
```
with a table
```sql
DROP TABLE IF EXISTS `student`;
CREATE TABLE IF NOT EXISTS `student` (
	`id`	INTEGER PRIMARY KEY AUTOINCREMENT,
	`name`	TEXT NOT NULL,
	`course`	TEXT NOT NULL,
	`addtime`	datetime NOT NULL DEFAULT current_timestamp
);
.quit
```
update `src/main/resources/application.properties` to point to it:
```java
spring.datasource.url=jdbc:sqlite:${HOME}/Desktop/springboot.db
```
for Linux host
and with

```java
spring.datasource.url=jdbc:sqlite:${USERPROFILE}\\Desktop\\springboot.db
```
for Windows host
and insert some data
```sql
INSERT INTO student(name,course) VALUES ('Jack','Chinese');
INSERT INTO student(name,course) VALUES ('Tom','Computer');
.quit
``` 
alternatively use [SQLIteBrowser](https://sqlitebrowser.org)
* verify 
```sh
sqlite3 ~/Desktop/springboot.db
SQLite version 3.22.0 2018-01-22 18:45:57
Enter ".help" for usage hints.
sqlite> .table student
```
```text
student
```
```sh
.schema student
```
```sql
CREATE TABLE `student` (
        `id`    INTEGER PRIMARY KEY AUTOINCREMENT,
        `name`  NVARCHAR(30) NOT NULL,
        `course`        NVARCHAR(30) NOT NULL,
        `addtime`       datetime NOT NULL DEFAULT current_timestamp
);
```
```cmd
sqlite3.exe %userprofile%\Desktop\springboot.db
```

```text
SQLite version 3.11.1 2016-03-03 16:17:53
Enter ".help" for usage hints.
sqlite>
```
enter
```sql
select * from student;
```
it will print something like
```text
2|John|Music|
3|Ringo|Drums|
```

```sql
.quit
```
and build and start project as regular springboot application

```cmd
mvn -Dmaven.test.skip=true clean spring-boot:run
```
* NOTE: The `test` goal is failing on Windows host when run in foreground:
```text
[ERROR] org.apache.maven.surefire.booter.SurefireBooterForkException: The forked
 VM terminated without properly saying goodbye. VM crash or System.exit called?
[ERROR] Process Exit Code: 0
[ERROR] Crashed tests:
[ERROR] example.controller.FailingTest
```
while pass on Linux host:
```sh
mvn clean test
```
```text
[WARNING] Tests run: 7, Failures: 0, Errors: 0, Skipped: 1
```

On Windows,  redirection of the output to file, also makes the test pass:
```cmd
mvn test > a.log
```

```cmd
type a.log
```
```text
[WARNING] Tests run: 7, Failures: 0, Errors: 0, Skipped: 1
[INFO]
[INFO] ---------------------------------------------------
[INFO] BUILD SUCCESS
```
### Running the App

```sh
mvn -Dmaven.test.skip=true spring-boot:run
```
Verify it works via Postman or curl (one will need to specify POST method in all requests).
The application was originally designed with Spring 4 and is being convered to Spring 5.x to be able to test some packages which did not exist prior.


#### On Spring 5.x

```sh
curl -X POST http://127.0.0.1:8181/student/findAllStudent |jq
```
alternatively can use the external IP address (`$(hostname -i)`) of the host the maven is run:
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

if the error is returned

```json
{"status":1,"data":null}
```

check the console log - most likely the database is not present in the specific path the connection is attempted
#### On Spring 4.X

the only difference is in the endpoint 
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

Other supported routes are `updateStudent`, `delStudentById`, `addStudent`.
### Docker Exercise

### Shell Version

* update `src/main/resources/application.properties` to point to database under absolute path `/db` and repackage
```sh
docker build -f Dockerfile.shell -t sqlite-shell .
```
 
```sh
docker run -it -v ${HOME}/Desktop/:/db sqlite-shell
```
```sql
sqlite> .tables
```
```text
student  user   
```
```sql
sqlite> .schema user
```
```text
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
```
```sql
sqlite> select count(1) from user;
```
```text
3
```
```sql
.quit
```
#### Testing in Docker
* uncoment the relevant path in `spring.datasource.url` in `src/main/resources/application.properties`:
```java
spring.datasource.url=jdbc:sqlite:/db/springboot.db
```
* repackage
```sh
mvn clean -Dmaven.test.skip=true package
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
### Testing Common Table Expression (CTE)
the `` endpoint supports a boolean `cte` parameter to switch to JDBC query using `WITH` clause:

```sh
curl -X POST -H 'application/x-www-form-urlencoded' -d 'id=2&cte=true' http://127.0.0.1:8181/student/findStudentById
```

The JDBC will run CTE query:
```SQL
WITH s AS (
SELECT *, 'dummy' as extracolumn FROM student
)  SELECT * FROM s WHERE id = ?
```
and log the extra column 
```java
ResultSet resultSet = preparedStatement.executeQuery();
logger.info("findStudentByIdWithCTE: extracolumn: "
	+ resultSet.getString("extracolumn"));
```
to apppliction console:
```text
2022-09-16 12:17:21.771 logback [http-nio-8181-exec-1] INFO  example.dao.JDBCDao
 - findStudentByIdWithCTE: extracolumn: dummy
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

  * for SQLite Hibernate project example, see [restart1025/Spring-Boot-SQLite](https://github.com/restart1025/Spring-Boot-SQLite)
or [CherryYu/springboot-sqlite](https://github.com/CherryYu/springboot-sqlite)
  * [documentation](https://dev.mysql.com/doc/refman/8.0/en/with.html) on MySQL CTE - amed temporary result set that exists within the scope of a single statement and that can be referred to later within that statement, possibly multiple times
  * [documentation](https://www.sqlite.org/lang_with.html) on SQLite CTE - a little dry
  * detailed [guide](https://www.baeldung.com/spring-request-param) to __Spring__ `@RequestParam` Annotation
  * the simplest SQLite CTE [Tutorial](https://blog.expensify.com/2015/09/25/the-simplest-sqlite-common-table-expression-tutorial/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

