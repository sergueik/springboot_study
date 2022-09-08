### Info

a fragment of [](https://github.com/RameshMF/spring-mvc-tutorial)


### Usage

```
mvn -Dmaven.test.skip=true spring-boot:run
```


In the first run it will log the error:
```text
Hibernate: create table students (id bigint not null, email varchar(255), name v
archar(255), phone_no bigint, primary key (id)) engine=InnoDB
2022-09-06 12:28:27.371  WARN 7952 --- [         task-1] o.h.t.s.i.ExceptionHand
lerLoggedImpl     : GenerationTarget encountered exception accepting command : E
rror executing DDL "create table students (id bigint not null, email varchar(255
), name varchar(255), phone_no bigint, primary key (id)) engine=InnoDB" via JDBC
 Statement

org.hibernate.tool.schema.spi.CommandAcceptanceException: Error executing DDL "c
reate table students (id bigint not null, email varchar(255), name varchar(255),
 phone_no bigint, primary key (id)) engine=InnoDB" via JDBC Statement
```

construct the database manually in SQLite browser, dropping invalid `engine=InnoDB` modifier, switch the configuration to 
```java
spring.jpa.hibernate.ddl-auto = none
```
and restart the Spring boot app


if seeing the exception
```text
java.lang.NoSuchMethodError: org.springframework.orm.jpa.JpaTransactionManager$JpaTransactionObject.setReadOnly(Z)V
```
 this is not solved in this project yet
 https://github.com/spring-projects/spring-framework/issues/24422


If seeing the error

```text
Hibernate: select next_val as id_val from hibernate_sequence for update
2022-09-06 22:38:14.026 ERROR 6116 --- [nio-8080-exec-4] o.hibernate.id.enhanced.TableStructure   : 
could not read a hi value
org.sqlite.SQLiteException: [SQLITE_ERROR] SQL error or missing database (near "update": syntax error)
        at org.sqlite.core.DB.newSQLException(DB.java:941) ~[sqlite-jdbc-3.28.0.jar:na]
        at org.sqlite.core.DB.newSQLException(DB.java:953) ~[sqlite-jdbc-3.28.0.jar:na]
        at org.sqlite.core.DB.throwex(DB.java:918) ~[sqlite-jdbc-3.28.0.jar:na]
        at org.sqlite.core.NativeDB.prepare_utf8(Native Method) ~[sqlite-jdbc-3.28.0.jar:na]
        at org.sqlite.core.NativeDB.prepare(NativeDB.java:134) ~[sqlite-jdbc-3.2
```
```SQL
CREATE TABLE "students" (
	"id"	INTEGER NOT NULL,
	"email"	varchar(255),
	"name"	varchar(255),
	"phone_no"	bigint,
	PRIMARY KEY("id" AUTOINCREMENT)
);
```

https://stackoverflow.com/questions/27381781/java-spring-boot-how-to-map-my-app-root-to-index-html