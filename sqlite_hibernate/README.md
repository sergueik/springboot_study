### Info

This directory contains a basic springboot hibernate on sqlite project based on
[restart1025/Spring-Boot-SQLite](https://github.com/restart1025/Spring-Boot-SQLite)

### Run application

Compile and start appplication
```sh
mvn clean spring-boot:run
```
Access application in Postman or command line
```sh
curl http://localhost:8080/springboot/getUsers
```
```sh
$ curl -X POST -H "" -d 'userName=Alex&password=secret&confirmPassword=secret&gender=MAN' http://localhost:8080/springboot/addUser
```
#### Database Settings

To run with persistent database, set in `application.yaml`
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
* [diyfr/sqlite-dialect](https://github.com/diyfr/sqlite-dialect)
* [Spring Boot Reference Guide](https://docs.spring.io/spring-boot/docs/current/reference/html/howto-build.html)
