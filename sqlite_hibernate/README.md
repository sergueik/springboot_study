### Info

This directory contains a basic springboot hibernate on sqlite project based on
[restart1025/Spring-Boot-SQLite](https://github.com/restart1025/Spring-Boot-SQLite)

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
    },
}]
```
```sh
$ curl -X POST -H "" -d "userName=Alex&password=secret&confirmPassword=secret&gender=MAN" http://localhost:8080/springboot/addUser
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
and create database file `~/sqlite/springboot.db` with table
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

To run in-memory set in `application.yaml`
```yaml
  datasource:
    driver-class-name: org.sqlite.JDBC
    url: 'jdbc:sqlite::memory:'
    username:
    password:
```

If you need to create schema before starting the app, add the `src/main/resources/hibernate.cfg.xml`
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
and uncomment DDL code in `SpringbootApplication.java`

### See also

* [Hibernate/DAO basics](https://habrahabr.ru/post/255829/) (in russian)
* [diyfr/sqlite-dialect](https://github.com/diyfr/sqlite-dialect)
* [Spring Boot Reference Guide](https://docs.spring.io/spring-boot/docs/current/reference/html/howto-build.html)
* [xerial/sqlite-jdbc](https://bitbucket.org/xerial/sqlite-jdbc)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
