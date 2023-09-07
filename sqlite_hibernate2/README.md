### Info

This directory contains a basic springboot hibernate on sqlite project based on
[restart1025/Spring-Boot-SQLite](https://github.com/restart1025/Spring-Boot-SQLite) and using in memory cache to measure performance impact of
reducing the JDBC calls


### Run application

Compile and start as a regular spring-boot appplication
```sh
mvn clean spring-boot:run
```
To verify it works, access application in Postman or curl
```sh
curl http://localhost:8080/springboot/getUsers
```

will output something like

```json
[{
  "id": 5,
  "userName ": null,
  "passWord ": null,
  "
  "userGender ": null,
  "nickName ": null
}]
```
the cached version of the same call:
```sh
curl http://localhost:8080/springboot/getCachedUsers
```
will return data that was retrieved only once

```sh
curl http://localhost:8080/springboot/getUser?id=1
```

```sh
curl http://localhost:8080/springboot/getCachedUser?id=1
```

```sh
curl -X POST -H "application/x-www-form-urlencoded" -d "userName=Michael&nickName=michaeljackson&gender=MAN&password=thriller&confirmPassword=thriller" http://localhost:8080/springboot/addUser
```
will reply with
```sh
User added
```
while the
```sh
curl -X POST -H "Content-Type: application/json" -d '{"userName":"John", "password":"beatles", "gender":"MAN"}' http://localhost:8080/springboot/addUserObject
```
would response with
```
{"id":10,"userName":"John","password":"beatles","gender":"MAN","nickName":null}

```
```sh
for CNT in $(seq 1 1 1000); do curl -sX POST -H "application/x-www-form-urlencoded" -d "userName=Michael&nickName=michaeljackson&gender=MAN&password=thriller&confirmPassword=thriller" http://localhost:8080/springboot/addUser ; done
```

* add shell scripts to evaluate timing of the run (serially)

`test1.sh`:

```sh
HOST=192.168.0.25
MAX_COUNT=1000
for cnt in $(seq 1 1 $MAX_COUNT); do curl -s http://$HOST:8080/springboot/getCachedUser?id=10 > /dev/null; don
```
```sh
time ./test1.sh
```
with caching
```text
real    0m32.533s
user    0m11.619s
sys     0m7.024s
```
`test2.sh`:

```sh
HOST=192.168.0.25
MAX_COUNT=1000
for cnt in $(seq 1 1 $MAX_COUNT); do curl -s http://$HOST:8080/springboot/getUser?id=10 > /dev/null; done
```
```sh
time ./test2.sh
```
without caching
```text
real    1m11.333s
user    0m11.010s
sys     0m7.357s

```

With MySQL connection instead of SQLite the timings:
```

```sh
time ./test1.sh
```
with caching
```text
real    1m49.866s
user    0m10.927s
sys     0m27.960s
```


```sh
time ./test2.sh
```
without caching
```text
real    3m7.313s
user    0m10.693s
sys     0m28.451s
```

#### Gatling Results

* with caching

```text
---- Global Information --------------------------------------------------------

> request count                                       1000 (OK=1000   KO=0     )

> min response time                                      2 (OK=2      KO=-     )

> max response time                                   3372 (OK=3372   KO=-     )

> mean response time                                   407 (OK=407    KO=-     )

> std deviation                                        605 (OK=605    KO=-     )

> response time 50th percentile                         59 (OK=59     KO=-     )

> response time 75th percentile                        737 (OK=737    KO=-     )

> response time 95th percentile                       1618 (OK=1618   KO=-     )

> response time 99th percentile                       2722 (OK=2722   KO=-     )

> mean requests/sec                                    100 (OK=100    KO=-     )

---- Response Time Distribution ------------------------------------------------

> t < 800 ms                                           767 ( 77%)
> 800 ms <= t < 1200 ms                                103 ( 10%)
> t >= 1200 ms                                         130 ( 13%)
> failed                                                 0 (  0%)
================================================================================

```
* without caching

```text
---- Global Information --------------------------------------------------------

> request count                                       1000 (OK=1000   KO=0     )

> min response time                                   2842 (OK=2842   KO=-     )

> max response time                                  57917 (OK=57917  KO=-     )

> mean response time                                 30909 (OK=30909  KO=-     )

> std deviation                                      15149 (OK=15149  KO=-     )

> response time 50th percentile                      31062 (OK=31062  KO=-     )

> response time 75th percentile                      43782 (OK=43782  KO=-     )

> response time 95th percentile                      54385 (OK=54385  KO=-     )

> response time 99th percentile                      57208 (OK=57208  KO=-     )

> mean requests/sec                                 14.925 (OK=14.925 KO=-     )

---- Response Time Distribution ------------------------------------------------

> t < 800 ms                                             0 (  0%)
> 800 ms <= t < 1200 ms                                  0 (  0%)
> t >= 1200 ms                                        1000 (100%)
> failed                                                 0 (  0%)
================================================================================


```
#### Database Settings

To run with persistent database, set in `application.yml`
```yaml
  datasource:
    driver-class-name: org.sqlite.JDBC
    url: jdbc:sqlite:${USERPROFILE}\\sqlite\\springboot.db
    username:
    password:
  jpa:
    database-platform: org.utils.sqlite.SQLiteDialect
    hibernate:
      ddl-auto: update
    show-sql: true
```

then create the sqlite database directory
```sh
pushd ~
mkdir sqlite
```
of
```cmd
cd /d %USERPROFILE%
MKDIR sqlite
cd sqlite
DEL /q springboot.db
sqlite3.exe springboot.db -cmd "CREATE TABLE `user` ( `id`integer, `nick_name`varchar, `pass_word`varchar, `user_gender`integer,  PRIMARY KEY(`id`) ); " ""
```
This command will create database file `~/sqlite/springboot.db` with table
```sql
CREATE TABLE `user` (
	`id`	integer,
	`nick_name`	varchar,
	`pass_word`	varchar,
	`user_name`	varchar,
	`user_gender`	integer,
	PRIMARY KEY(`id`)
);
```
alternarively can create table in [SQLite browser](https://sqlitebrowser.org).
NOTE: the `sqlite3.exe` does not work from [git bash shell](https://gitforwindows.org) on some Windows platforms.

To run database in-memory, modify settings in `application.yaml` like:
```yaml
  datasource:
    driver-class-name: org.sqlite.JDBC
    url: 'jdbc:sqlite::memory:'
    username:
    password:
```

If you like the java code to create schema right before starting the app, add the `src/main/resources/hibernate.cfg.xml`
```xml
<hibernate-configuration>
  <session-factory>
    <property name="hibernate.dialect">org.utils.sqlite.SQLiteDialect</property>
    <property name="hibernate.connection.driver_class">org.sqlite.JDBC</property>
    <property name="hibernate.connection.url">jdbc:sqlite::memory:</property>
    <property name="hibernate.connection.username"/>
    <property name="hibernate.connection.password"/>
    <property name="hibernate.hbm2ddl.auto">create</property>
    <property name="hibernate.show_sql">true</property>
    <mapping class="org.utils.springboot.User "/>
  </session-factory>
</hibernate-configuration>
```
and uncomment the DDL code in `SpringbootApplication.java`:

```java
SessionFactory sessionFactory = new Configuration().configure().buildSessionFactory();

Session session = sessionFactory.openSession();
session.beginTransaction();
String query = String.format(
    "CREATE TABLE `user` ( `id`	integer, `nick_name` varchar, `pass_word` varchar, `user_name` varchar, `user_gender` integer, PRIMARY KEY(`id`));");
session.createSQLQuery(query);
Transaction transaction = session.getTransaction();
transaction.commit();
session.close();
```
NOTE: this initialization code is not working well when bundled with a Hibernate application
due to a race condition with Spring attempting to load the `User` class and need to be moved into stadalond application (this is Work in progress).

### Cleanup

```sh
docker rm -v $(docker ps -aq -f status=exited)
```

#### 

https://github.com/alicankustemur/spring-boot-jpa-hibernate-mysql-example

### See also

  * [Hibernate/DAO basics](https://habrahabr.ru/post/255829/) (in russian)
  * [diyfr/sqlite-dialect](https://github.com/diyfr/sqlite-dialect)
  * [Spring Boot Reference Guide](https://docs.spring.io/spring-boot/docs/current/reference/html/howto-build.html)
  * [xerial/sqlite-jdbc](https://bitbucket.org/xerial/sqlite-jdbc)
  * [tools and libraries download](https://www.sqlite.org/download.html)
  * sqlite3 [command syntax](https://www.sqlite.org/cli.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
