### Info

A replica of Spring Boot Application with Embeded PostgreSQL database [example](https://github.com/skpk24/springboot_embeded_postgresql)
sans devtools plugin since we do not intend to Dockerize it
### Usage
```cmd
mvn spring-boot:run
```
* note: maven downloads a full 200 Mb Postgresql installer into
```cmd
~/.embedpostgresql
```

in first run fails with
```text
org.postgresql.util.PSQLException: Connection to localhost:63340 refused. Check that the hostname and port are correct and that the postmaster is accepting TCP/IP connections.
unable to open JDBC Connection for DDL execution
```
* in the second run fails with

```text
2021-11-28 20:07:50.441  INFO 11256 --- [           main] d.f.embed.process.runtime.Executable     : start AbstractPostgresConfig{storage=Storage{dbDir=C:\Users\Serguei\AppData\Local\Temp\postgresql-embed-36d90118-e071-43d5-b688-886260410419\db-content-85181475-027a-4481-9372-f61610856401, dbName='test', isTmpDir=true}, network=Net{host='localhost', port=63644}, timeout=Timeout{startupTimeout=15000}, credentials=Credentials{username='user', password='pass'}, args=[], additionalInitDbParams=[--nosync, --locale=en_US]}2021-11-28 20:07:52.444  WARN 11256 --- [           main] r.y.q.embed.postgresql.PostgresProcess   : Possibly failed to run initdb:

The files belonging to this database system will be owned by user "Serguei".

This user must also own the server process.

initdb: invalid locale name "en_US"


2021-11-28 20:08:12.459 ERROR 11256 --- [           main] r.y.q.embed.postgresql.PostgresProcess   : Failed to read PID file (File 'C:\Users\Serguei\AppData\Local\Temp\postgresql-embed-36d90118-e071-43d5-b688-886260410419\db-content-85181475-027a-4481-9372-f61610856401\postmaster.pid' does not exist)java.io.FileNotFoundException: File 'C:\Users\Serguei\AppData\Local\Temp\postgresql-embed-36d90118-e071-43d5-b688-886260410419\db-content-85181475-027a-4481-9372-f61610856401\postmaster.pid' does not exist
2021-11-28 20:08:17.373  WARN 11256 --- [           main] r.y.q.embed.postgresql.PostgresProcess   : Could not create database first time (0 of 3 trials)
2021-11-28 20:08:18.005  INFO 11256 --- [           main] d.f.embed.process.runtime.Executable     : start AbstractPostgresConfig{storage=Storage{dbDir=C:\Users\Serguei\AppData\Local\Temp\postgresql-embed-36d90118-e071-43d5-b688-886260410419\db-content-85181475-027a-4481-9372-f61610856401, dbName='test', isTmpDir=true}, network=Net{host='localhost', port=63644}, timeout=Timeout{startupTimeout=15000}, credentials=Credentials{username='user', password='pass'}, args=[test], additionalInitDbParams=[]}2021-11-28 20:08:22.115  WARN 11256 --- [           main] r.y.q.embed.postgresql.PostgresProcess   : Could not create database first time (1 of 3 trials)
```
(fails all 3 trials)
 - removing 
 fixes this issue.

Testing the REST API urls
`http://localhost:8080/api/db/v1/data` and `http://localhost:8080/api/v1/ms` is successful

### See Also
 * https://github.com/xcwy/springboot-embedded-postgresql
 * https://github.com/Romeh/springboot-postgres-embedded-dao-testing 
