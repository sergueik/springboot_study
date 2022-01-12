### Info

This directory contains a replica of [Yugabyte Spring Data Rest/JPA](https://github.com/MikeQin/yugabyte-spring-data-rest)  demo
project. The setup is documented in [YugabyteDB Documentation](https://docs.yugabyte.com/latest/quick-start/create-local-cluster/docker/)
### Usage

#### Local Run

* install yugabyte fillowing the [steps](https://docs.yugabyte.com/latest/quick-start/install/linux)

```sh
wget https://downloads.yugabyte.com/releases/2.11.1.0/yugabyte-2.11.1.0-b305-el8-aarch64.tar.gz
tar xzvf /yugabyte-2.11.1.0-b305-el8-aarch64.tar.gz
cd ./yugabyte-2.11.1.0-b305-el8-aarch64

```

```text
./bin/yugabyted start
```
is somewhat confusing, since it is not obvious when the application is connection ready

#### Docker run 

using release [yugabytedb/yugabyte:2.5.0.0-b2](https://hub.docker.com/layers/yugabytedb/yugabyte/2.5.0.0-b2/images/sha256-dd9ca0eb1647d635df6469ef97edad884c77b4fca2edb4d11f72961d3d730a25?context=explore)
follow [create cluster](https://docs.yugabyte.com/latest/quick-start/create-local-cluster/docker) document:

```sh
 docker run -d --name yugabyte  -p7000:7000 -p9000:9000 -p5433:5433 -p9042:9042 yugabytedb/yugabyte:2.5.0.0-b2 bin/yugabyted start --daemon=false
```
```sh
docker logs -f yugabyte
```
shows after a while
```text
Starting yugabyted...
✅ System checks
```
There is no "completed" message 

Connecting to `yugabyte` container and tailing the log
`/home/yugabyte/var/data/yb-data/tserver/logs/yb-tserver.INFO` doe not provide any new information


the maven test is still failing with Database availability
```sh
mvn test
```
```text
org.postgresql.util.PSQLException: FATAL: database "yb_db" does not exist	
```
* open an extra shell in `yugabyte` container and create database:
```sh
docker exec -it yugabyte /home/yugabyte/bin/ysqlsh --echo-queries
```
```sh
CREATE DATABASE yb_db;
```
wait till it  returns to the prompt
```sh
yugabyte=#

```

now maven test will succeed:
```text
03:51:55.138 [main] DEBUG org.springframework.test.context.support.TestPropertySourceUtils - Adding inlined properties to environment: {spring.jmx.enabled=false, org.springframework.boot.test.context.SpringBootTestContextBootstrapper=true}

  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v2.3.4.RELEASE)

2022-01-12 03:51:55.911  INFO 2895 --- [           main] c.e.datarest.DataRestApplicationTests    : Starting DataRestApplicationTests on sergueik71 with PID 2895 (started by sergueik in /home/sergueik/src/springboot_study/basic-yugabytedb)
2022-01-12 03:51:55.920  INFO 2895 --- [           main] c.e.datarest.DataRestApplicationTests    : No active profile set, falling back to default profiles: default
2022-01-12 03:51:58.306  INFO 2895 --- [           main] .s.d.r.c.RepositoryConfigurationDelegate : Bootstrapping Spring Data JPA repositories in DEFERRED mode.
2022-01-12 03:51:58.563  INFO 2895 --- [           main] .s.d.r.c.RepositoryConfigurationDelegate : Finished Spring Data repository scanning in 197ms. Found 1 JPA repository interfaces.
2022-01-12 03:52:00.609  INFO 2895 --- [           main] o.s.s.concurrent.ThreadPoolTaskExecutor  : Initializing ExecutorService 'applicationTaskExecutor'
2022-01-12 03:52:00.830  INFO 2895 --- [         task-1] o.hibernate.jpa.internal.util.LogHelper  : HHH000204: Processing PersistenceUnitInfo [name: default]
2022-01-12 03:52:01.092  INFO 2895 --- [         task-1] org.hibernate.Version                    : HHH000412: Hibernate ORM core version 5.4.21.Final
2022-01-12 03:52:01.610  INFO 2895 --- [         task-1] o.hibernate.annotations.common.Version   : HCANN000001: Hibernate Commons Annotations {5.1.0.Final}
2022-01-12 03:52:01.873  INFO 2895 --- [         task-1] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Starting...
2022-01-12 03:52:02.892  INFO 2895 --- [         task-1] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Start completed.
2022-01-12 03:52:02.930  INFO 2895 --- [         task-1] org.hibernate.dialect.Dialect            : HHH000400: Using dialect: org.hibernate.dialect.PostgreSQLDialect
Hibernate: create table person (id int8 not null, first_name varchar(255), last_name varchar(255), primary key (id))
Hibernate: create sequence hibernate_sequence start 1 increment 1
2022-01-12 03:52:10.776  INFO 2895 --- [         task-1] o.h.e.t.j.p.i.JtaPlatformInitiator       : HHH000490: Using JtaPlatform implementation: [org.hibernate.engine.transaction.jta.platform.internal.NoJtaPlatform]
2022-01-12 03:52:10.795  INFO 2895 --- [         task-1] j.LocalContainerEntityManagerFactoryBean : Initialized JPA EntityManagerFactory for persistence unit 'default'
Hibernate: select count(*) as col_0_0_ from person person0_
Hibernate: select nextval ('hibernate_sequence')
Hibernate: select nextval ('hibernate_sequence')
Hibernate: select nextval ('hibernate_sequence')
Hibernate: insert into person (first_name, last_name, id) values (?, ?, ?)
Hibernate: insert into person (first_name, last_name, id) values (?, ?, ?)
Hibernate: insert into person (first_name, last_name, id) values (?, ?, ?)
2022-01-12 03:52:14.206  WARN 2895 --- [           main] JpaBaseConfiguration$JpaWebConfiguration : spring.jpa.open-in-view is enabled by default. Therefore, database queries may be performed during view rendering. Explicitly configure spring.jpa.open-in-view to disable this warning
2022-01-12 03:52:15.767  INFO 2895 --- [           main] DeferredRepositoryInitializationListener : Triggering deferred initialization of Spring Data repositories…
2022-01-12 03:52:15.768  INFO 2895 --- [           main] DeferredRepositoryInitializationListener : Spring Data repositories initialized!
2022-01-12 03:52:15.804  INFO 2895 --- [           main] c.e.datarest.DataRestApplicationTests    : Started DataRestApplicationTests in 20.638 seconds (JVM running for 23.211)
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 22.082 s - in com.example.datarest.DataRestApplicationTests
2022-01-12 03:52:16.249  INFO 2895 --- [extShutdownHook] j.LocalContainerEntityManagerFactoryBean : Closing JPA EntityManagerFactory for persistence unit 'default'
2022-01-12 03:52:16.263  INFO 2895 --- [extShutdownHook] o.s.s.concurrent.ThreadPoolTaskExecutor  : Shutting down ExecutorService 'applicationTaskExecutor'
2022-01-12 03:52:16.271  INFO 2895 --- [extShutdownHook] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Shutdown initiated...
2022-01-12 03:52:16.308  INFO 2895 --- [extShutdownHook] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Shutdown completed.
[INFO]
[INFO] Results:
[INFO]
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
```

![yb-master Admin UI](https://github.com/sergueik/springboot_study/blob/master/basic-yugabytedb/screenshots/yb_master_admin_capture.png)
run the app
```sh
mvn -Dmaven.test.skip=true spring-boot:run
```
	
and obsevre console test successful:
```sh
curl localhost:8080/people
```
```json
{
  "_embedded" : {
    "people" : [ {
      "firstName" : "Joe",
      "lastName" : "Doe",
      "_links" : {
        "self" : {
          "href" : "http://localhost:8080/people/3"
        },
        "person" : {
          "href" : "http://localhost:8080/people/3"
        }
      }
    }, {
      "firstName" : "Amy",
      "lastName" : "Brown",
      "_links" : {
        "self" : {
          "href" : "http://localhost:8080/people/2"
        },
        "person" : {
          "href" : "http://localhost:8080/people/2"
        }
      }
    }, {
      "firstName" : "John",
      "lastName" : "Smith",
      "_links" : {
        "self" : {
          "href" : "http://localhost:8080/people/1"
        },
        "person" : {
          "href" : "http://localhost:8080/people/1"
        }
      }
    } ]
  },
  "_links" : {
    "self" : {
      "href" : "http://localhost:8080/people"
    },
    "profile" : {
      "href" : "http://localhost:8080/profile/people"
    },
    "search" : {
      "href" : "http://localhost:8080/people/search"
    }
  },
  "page" : {
    "size" : 20,
    "totalElements" : 3,
    "totalPages" : 1,
    "number" : 0
  }
}
```

NOTE, there is no CRUD functionality in this project, the JSON above is provided by the 
```java
@RepositoryRestResource(collectionResourceRel = "people", path = "people")
public interface PersonRepository extends PagingAndSortingRepository<Person, Long> {

  List<Person> findByLastName(@Param("lastName") String lastName);
  
  List<Person> findByFirstName(@Param("firstName") String firstName);

}
```
the standard Spring Controller example can be found  in [MikeQin/yugabyte-spring](https://github.com/MikeQin/yugabyte-spring)

### See Also

 * [introduction to HikariCP](https://www.baeldung.com/hikaricp)
 * [configuring a Hikari JDBC Connection Pool with Spring Boot](https://www.baeldung.com/spring-boot-hikari)
 
 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
