### Info

This directory contains a basic springboot jdbc on postgresql project based on
[KominfoPemudaPersis/springboot-jdbc-postgres](https://github.com/KominfoPemudaPersis/springboot-jdbc-postgres)
and alpine postgres basic Dockerfile from [kiasaki/alpine-postgres](https://hub.docker.com/r/kiasaki/alpine-postgres/dockerfile) github repository

### Run application
* install postgres locally 
```sh
sudo apt-get -qy install postgresql
```
* switch to password auth locally
```sh
sudo -u postgres psql
```
```sh
ALTER USER postgres PASSWORD 'postgres';
```
```sh
ALTER ROLE
```
```sh
\q
```
* verify the credentials
```sh
psql -h localhost -p 5432 --username postgres --password
```
```sh
postgres=# \c
```

```sh
Password:
```
```sh
SSL connection (protocol: TLSv1.3, cipher: TLS_AES_256_GCM_SHA384, bits: 256, compression: off)
You are now connected to database "postgres" as user "postgres"
```
close the connection

* create database and table locally
```sh
sudo -u postgres psql
```
```text
psql (10.22 (Ubuntu 10.22-0ubuntu0.18.04.1))
Type "help" for help.

postgres=#
```
```sql
select datname from pg_database;
```
```text
  datname
-----------
 postgres
 template1
 template0
(3 rows)

```
if you do not see the `example` database in the output proceeed with (see [stackoverflow](https://stackoverflow.com/questions/18389124/simulate-create-database-if-not-exists-for-postgresql)):

```sql
create database example;
```
```sh
CREATE DATABASE
```
```sh
postgres=# \c example
```
```
You are now connected to database "example" as user "postgres".
example=# 
```
```sh
CREATE TABLE rest ( id serial PRIMARY KEY NOT NULL, key varchar(100) NOT NULL, value varchar(250) NOT NULL, rand smallint NOT NULL);
```
* run locally
```sh
mvn clean spring-boot:run
```
* test 
```sh
curl -X POST -H "Content-Type: application/json" -d '{"key":"some example", "value":"some data"}' http://127.0.0.1:8080/rest | jq '.'
```
will respond with
```json
{
  "id": 1,
  "rand": 21,
  "key": "some example",
  "value": "some data"
}
```

if there is an exception make sure that onnection is using the `localhost` in the `application.properties`:


```sh
sed -i 's|//\(.*\):5432|//localhost:5432|' src/main/resources/application.properties
```
```sh
cat src/main/resources/application.properties
```
and rebuild the app
```sh
mvn -Dmaven.test.skip=true clean spring-boot:run
```
NOTE: the command line option `-Dspring.datasource.url='jdbc:postgresql://localhost:5432/example'` will have no effect

and if the exception
```sh
nested exception is org.postgresql.util.PSQLException: ERROR: relation "rest" already exists
```
make  sure to drop the table:
```sh
psql -h localhost -p 5432 --username postgres --password example
```
```sh
example=# drop table rest;
```
and then
```sh
curl http://127.0.0.1:8080/rest/ | jq '.'
```
shows
```json
[
  {
    "id": 1,
    "rand": 21,
    "key": "some example",
    "value": "some data"
  },
  {
    "id": 2,
    "rand": 0,
    "key": "another example",
    "value": "some more data"
  }  
]
```
and in the postgres console,
```sh
\c example
Password for user postgres: 
SSL connection (protocol: TLSv1.3, cipher: TLS_AES_256_GCM_SHA384, bits: 256, compression: off)
You are now connected to database "example" as user "postgres".
example=# select *  from rest;
```
returns
```sh
 id | key          | value      | rand 
----+--------------+------------+------
  1 | some example | some data  |   21
```
and
```sh
curl -X PUT -H "Content-Type: application/json" -d '{"key":"example", "value":"new data"}' http://127.0.0.1:8080/rest/1 | jq '.'
```
will update:
```json
{
  "id": 1,
  "rand": 12,
  "key": "example",
  "value": "new data"
}
```
can now uninstall postgresql

### Run in Docker

stop postgresql running on host:
```sh
sudo /etc/init.d/postgresql  stop
```
```text
[ ok ] Stopping postgresql (via systemctl): postgresql.service.
```
alternat	ively
```sh
sudo systemctl stop postgresql
sudo systemctl status postgresql
```
```text
* postgresql.service - PostgreSQL RDBMS
   Loaded: loaded (/lib/systemd/system/postgresql.service; enabled; vendor prese
   Active: inactive (dead) since Tue 2022-10-18 22:21:07 CEST; 5s ago
  Process: 1491 ExecStart=/bin/true (code=exited, status=0/SUCCESS)
 Main PID: 1491 (code=exited, status=0/SUCCESS)

Oct 17 14:59:06 sergueik71 systemd[1]: Starting PostgreSQL RDBMS...
Oct 17 14:59:06 sergueik71 systemd[1]: Started PostgreSQL RDBMS.
Oct 18 22:21:07 sergueik71 systemd[1]: Stopped PostgreSQL RDBMS.

```
* package the jar 
```sh
mvn clean package
```
if the container was saved from earlier, start

```sh
SERVER_NAME=postgres-database
docker container start $SERVER_NAME
```
```text
postgres-database
```
* pull the [smallest possible postgresql container image](https://hub.docker.com/r/kiasaki/alpine-postgres/)
```
docker pull kiasaki/alpine-postgres
```
launch the database in container named $SERVER_NAME
```sh
SERVER_NAME=postgres-database
docker stop $SERVER_NAME
docker container rm $SERVER_NAME
docker run --name $SERVER_NAME -e POSTGRES_PASSWORD=postgres -d kiasaki/alpine-postgres
```
- this will fail on __Docker Toolbox__ with
```text
PostgreSQL stand-alone backend 9.6.5
backend> statement: ALTER USER postgres WITH SUPERUSER PASSWORD 'postgres';

backend>
waiting for server to start....
FATAL:  could not create lock file "/run/postgresql/.s.PGSQL.5432.lock": No such file or directory
LOG:  database system is shut down
 stopped waiting
pg_ctl: could not start server
Examine the log output.

/docker-entrypoint.sh: ignoring /docker-entrypoint-initdb.d/*

pg_ctl: PID file "/var/lib/postgresql/data/postmaster.pid" does not exist Is server running?
FATAL:  could not create lock file "/run/postgresql/.s.PGSQL.5432.lock": No such file or directory
LOG:  database system is shut down
```

on a Linux host it will succeed with:
```text
2023-06-17 14:48:41.201 UTC [31] LOG:  shutting down
2023-06-17 14:48:41.674 UTC [29] LOG:  database system is shut down
 done
server stopped
2023-06-17 14:48:41.712 UTC [1] LOG:  listening on IPv4 address "0.0.0.0", port 5432
2023-06-17 14:48:41.712 UTC [1] LOG:  listening on IPv6 address "::", port 5432
2023-06-17 14:48:41.752 UTC [1] LOG:  listening on Unix socket "/run/postgresql/.s.PGSQL.5432"
2023-06-17 14:48:41.795 UTC [1] LOG:  listening on Unix socket "/tmp/.s.PGSQL.5432"
2023-06-17 14:48:41.864 UTC [40] LOG:  database system was shut down at 2023-06-17 14:48:41 UTC
2023-06-17 14:48:41.884 UTC [1] LOG:  database system is ready to accept connections
```
stop the container and focus on client code in selected language
```sh
docker stop $SERVER_NAME
docker container rm $SERVER_NAME
```
Alternatively
change `SERVER_IMAGE` and rebuild the container from plain alpine using `Dockerfile.alpine-postgres`:
```sh
SERVER_IMAGE=alpine-postgres
docker build -f Dockerfile.$SERVER_IMAGE -t $SERVER_IMAGE .
```
run container from the built image. NOTE: one is not required to expose the port `5432`, application will be connecting containers over Docker network.
```sh
SERVER_NAME=postgres-database
docker run --name $SERVER_NAME -e POSTGRES_PASSWORD=postgres -p 5432:5432 -d $SERVER_IMAGE
```
* run
```sh
docker logs -f $SERVER_NAME
```
wait for a few minutes until seeing the connect invite 
NORE: the log entry from PGAdmin (PGAdmin III on Xenial) is unrelated

```text
database system is ready to accept connections
```
confirm the warning dialog

![PG Admin III](https://github.com/sergueik/springboot_study/blob/master/basic-postgresql/screenshots/capture_pgadmin_iii.png)

- turns out one cannot use PG Admin III with Postresql __12.x__, so for desktop testing one needs __bionic__ __18.04__ or later release of Ubuntu

* create the database
```sh
docker exec -it $SERVER_NAME psql -h localhost -p 5432 --username postgres -c "create database example"
```
this will print 
```text
CREATE DATABASE
```
* drop table
```sh
docker exec -it $SERVER_NAME psql -h localhost -p 5432 --username postgres --dbname example -c "drop table rest"
```
* create table `rest` by running the script noninteractively:
```sh
docker exec -it $SERVER_NAME psql -h localhost -p 5432 --username postgres --dbname example -c "CREATE TABLE IF NOT EXISTS rest ( id serial PRIMARY KEY NOT NULL, key varchar(100) NOT NULL, value varchar(250) NOT NULL, timestamp TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp(), rand smallint NOT NULL );"
```

* create function in database `example` by running interactively:
```sh
docker exec -it $SERVER_NAME psql -h localhost -p 5432 --username postgres --dbname example
```
paste
```SQL
CREATE OR REPLACE FUNCTION update_row_modified_function_()
RETURNS TRIGGER 
AS 
$$
BEGIN
    -- ASSUMES the table has a column named exactly "row_modified_".
    -- Fetch date-time of actual current moment from clock, rather than start of statement or start of transaction.
    NEW.row_modified_ = clock_timestamp(); 
    RETURN NEW;
END;
$$ 
language 'plpgsql';
```
this will respond with 
```text
CREATE FUNCTION
```
paste
```SQL
drop function update_row_modified_function_;
```
this will respond with 
```text
DROP FUNCTION
```
paste
```SQL
CREATE OR REPLACE FUNCTION update_row_modified_function_()
RETURNS TRIGGER 
AS 
$$
BEGIN
    NEW.timestamp = clock_timestamp(); 
    RETURN NEW;
END;
$$ 
language 'plpgsql';

```
this will respond with 
```text
CREATE FUNCTION
```
paste 
```SQL
CREATE TRIGGER row_mod_on_customer_trigger_
BEFORE UPDATE
ON rest
FOR EACH ROW 
EXECUTE PROCEDURE update_row_modified_function_();
```
this will respond with
```text
CREATE TRIGGER

```
run check
```SQL
\d rest
```
```text
                                      Table "public.rest"
  Column   |           Type           | Collation | Nullable |             Default              
-----------+--------------------------+-----------+----------+----------------------------------
 id        | integer                  |           | not null | nextval('rest_id_seq'::regclass)
 key       | character varying(100)   |           | not null | 
 value     | character varying(250)   |           | not null | 
 timestamp | timestamp with time zone |           | not null | clock_timestamp()
 rand      | smallint                 |           | not null | 
Indexes:
    "rest_pkey" PRIMARY KEY, btree (id)
Triggers:
    row_mod_on_customer_trigger_ BEFORE UPDATE ON rest FOR EACH ROW EXECUTE PROCEDURE update_row_modified_function_()
```

inspect the table columns by querying the `information_schema`:
```SQL
select * from information_schema.columns where table_name = 'rest';
```
```text
 table_catalog | table_schema | table_name | column_name | ordinal_position |          column_default          | is_nullable |        data_type         | character_maximum_length | character_octet_length | numeric_precision | numeric_precision_radix | numeric_scale | datetime_precision | interval_type | interval_precision | character_set_catalog | character_set_schema | character_set_name | collation_catalog | collation_schema | collation_name | domain_catalog | domain_schema | domain_name | udt_catalog | udt_schema |  udt_name   | scope_catalog | scope_schema | scope_name | maximum_cardinality | dtd_identifier | is_self_referencing | is_identity | identity_generation | identity_start | identity_increment | identity_maximum | identity_minimum | identity_cycle | is_generated | generation_expression | is_updatable 
---------------+--------------+------------+-------------+------------------+----------------------------------+-------------+--------------------------+--------------------------+------------------------+-------------------+-------------------------+---------------+--------------------+---------------+--------------------+-----------------------+----------------------+--------------------+-------------------+------------------+----------------+----------------+---------------+-------------+-------------+------------+-------------+---------------+--------------+------------+---------------------+----------------+---------------------+-------------+---------------------+----------------+--------------------+------------------+------------------+----------------+--------------+-----------------------+--------------
 example       | public       | rest       | id          |                1 | nextval('rest_id_seq'::regclass) | NO          | integer                  |                          |                        |                32 |                       2 |             0 |                    |               |                    |                       |                      |                    |                   |                  |                |                |               |             | example     | pg_catalog | int4        |               |              |            |                     | 1              | NO                  | NO          |                     |                |                    |                  |                  | NO             | NEVER        |                       | YES
 example       | public       | rest       | key         |                2 |                                  | NO          | character varying        |                      100 |                    400 |                   |                         |               |                    |               |                    |                       |                      |                    |                   |                  |                |                |               |             | example     | pg_catalog | varchar     |               |              |            |                     | 2              | NO                  | NO          |                     |                |                    |                  |                  | NO             | NEVER        |                       | YES
 example       | public       | rest       | value       |                3 |                                  | NO          | character varying        |                      250 |                   1000 |                   |                         |               |                    |               |                    |                       |                      |                    |                   |                  |                |                |               |             | example     | pg_catalog | varchar     |               |              |            |                     | 3              | NO                  | NO          |                     |                |                    |                  |                  | NO             | NEVER        |                       | YES
 example       | public       | rest       | timestamp   |                4 | clock_timestamp()                | NO          | timestamp with time zone |                          |                        |                   |                         |               |                  6 |               |                    |                       |                      |                    |                   |                  |                |                |               |             | example     | pg_catalog | timestamptz |               |              |            |                     | 4              | NO                  | NO          |                     |                |                    |                  |                  | NO             | NEVER        |                       | YES
 example       | public       | rest       | rand        |                5 |                                  | NO          | smallint                 |                          |                        |                16 |                       2 |             0 |                    |               |                    |                       |                      |                    |                   |                  |                |                |               |             | example     | pg_catalog | int2        |               |              |            |                     | 5              | NO                  | NO          |                     |                |                    |                  |                  | NO             | NEVER        |                       | YES
```

inspect the triggers by querying the `information_schema`:


```SQL
SELECT trigger_catalog, trigger_name, action_timing,  event_manipulation, event_object_catalog, event_object_table, action_statement FROM information_schema.triggers where event_object_table = 'rest';
```

```text
 trigger_catalog |         trigger_name         | action_timing | event_manipulation | event_object_catalog | event_object_table |                 action_statement
-----------------+------------------------------+---------------+--------------------+----------------------+--------------------+---------------------------------------------------
 example         | row_mod_on_customer_trigger_ | BEFORE        | UPDATE             | example              | rest               | EXECUTE PROCEDURE update_row_modified_function_()
```

```SQL
SELECT event_object_table, trigger_name FROM information_schema.triggers   GROUP BY event_object_table,trigger_name ORDER BY event_object_table,trigger_name;
```
```text
 event_object_table |         trigger_name
--------------------+------------------------------
 rest               | row_mod_on_customer_trigger_

```
* check with direct inserts and updates

```SQL
insert into rest (key,value,rand) values ('a','A',12345);
```
```SQL
select * from rest;
```
```text
 id | key | value |           timestamp           | rand  
----+-----+-------+-------------------------------+-------
  2 | a   | A     | 2024-03-13 17:31:52.916959+00 | 12345

```

```SQL
update rest set rand = 23456 where key = 'a';

```
```SQL
select * from rest;
```
```text
id | key | value |           timestamp           | rand  
----+-----+-------+-------------------------------+-------
  2 | a   | A     | 2024-03-13 17:32:14.014161+00 | 23456

```

NOTE: the table may not exist yet.
* update the `application.properties` to use docker dns hostname:
```java
# for container deployed app
# uncomment the following line
spring.datasource.url=jdbc:postgresql://postgres-database:5432/example
# for local run
# uncomment the following line
# spring.datasource.url=jdbc:postgresql://localhost:5432/example
```
and repackage:
```sh
mvn clean package
```
* optionlly rebuild the java container
```sh
IMAGE=postgres-example
docker build -f Dockerfile -t $IMAGE .
```
* run application in container linked to the postgres container `$SERVER_NAME`:
```sh
NAME=example-postgres
docker container stop $NAME
docker container rm $NAME
docker run --name $NAME --link $SERVER_NAME -p 8080:8080 -d $IMAGE
```
NOTE: do not forget to stop the spring-boot run one earlier or get an error:
```text
Error response from daemon: 
driver failed programming external connectivity on endpoint example-postgres (4d734f6c5e503d0011591b22f027d77f537a3c97bd835bd0d339bc9c06e0054d): 
Error starting userland proxy: listen tcp 0.0.0.0:8080: 
bind: address already in use.
```
* monitor logs:
```sh
docker logs $NAME
```
observe the success message
```text
ns for JMX exposure on startup
2024-03-13 17:23:19.386  INFO 1 --- [           main] s.b.c.e.t.TomcatEmbeddedServletContainer : Tomcat started on port(s): 8080 (http)
2024-03-13 17:23:19.393  INFO 1 --- [           main] example.Launcher                         : Started Launcher in 9.426 seconds (JVM running for 11.163)

```
* run aplication linked to postgres container
* repeat the curl checks

```
curl -s -X PUT -H "Content-Type: application/json" -d '{"key":"example", "value":"new data"}' http://127.0.0.1:8080/rest/1 | jq '.'
```
NOTE: will see the error, and code does not does process the exceptions
```js
{
  "timestamp": 1665182943733,
  "status": 500,
  "error": "Internal Server Error",
  "exception": "org.springframework.dao.EmptyResultDataAccessException",
  "message": "Incorrect result size: expected 1, actual 0",
  "path": "/rest/1"
}
```
- probably need a `POST`
```sh
curl -s http://127.0.0.1:8080/rest/ | jq '.'
```
```js
[
  {
    "id": 2,
    "rand": 41,
    "key": "a",
    "value": "new data",
    "timestamp": 1710351617167
  }
]

```
```sh
curl -s -X PUT -H "Content-Type: application/json" -d '{"key":"a", "value":"new data"}' http://127.0.0.1:8080/rest/2 
```
```
{
  "id": 2,
  "rand": 48,
  "key": "a",
  "value": "new data",
  "timestamp": 1710351746834
}

```
```sh
curl -s -X POST -H "Content-Type: application/json" -d '{"key":"another example", "value":"some data"}' http://127.0.0.1:8080/rest | jq '.'
```

```js
{
  "id": 1,
  "rand": 29,
  "key": "another example",
  "value": "some data"
}

```
```sh
curl -s http://127.0.0.1:8080/rest/ | jq '.'
```
```js
[
  {
    "id": 1,
    "rand": 29,
    "key": "another example",
    "value": "some data"
  }
]

```
```sh
curl -s -X PUT -H "Content-Type: application/json" -d '{"key":"example", "value":"new data"}' http://127.0.0.1:8080/rest/1 | jq '.'
```
```js
{
  "id": 1,
  "rand": 38,
  "key": "example",
  "value": "new data"
}

```
### TODO:

The current configuration of java app does not accept the `application/x-www-form-urlencoded` content-type POST requests:
```sh
curl -X POST -H "application/x-www-form-urlencoded" -d "key=example&value=data&rand=123" 127.0.0.1:8080/rest/
```
raises the exception
```json
{
  "timestamp": "2020-04-13T14:39:52.197+0000",
  "status": 415,
  "error": "Unsupported Media Type",
  "message": "Content type 'application/x-www-form-urlencoded;charset=UTF-8' not supported",
  "trace": "org.springframework.web.HttpMediaTypeNotSupportedException: 
  Content type 'application/x-www-form-urlencoded;charset=UTF-8' not supported
  at org.springframework.web.servlet.mvc.method.annotation.AbstractMessageConverterMethodArgumentResolver.readWithMessageConverters(AbstractMessageConverterMethodArgumentResolver.java:224)
  at org.springframework.web.servlet.mvc.method.annotation.RequestResponseBodyMethodProcessor.readWithMessageConverters(RequestResponseBodyMethodProcessor.java:157)
  at org.springframework.web.servlet.mvc.method.annotation.RequestResponseBodyMethodProcessor.resolveArgument(RequestResponseBodyMethodProcessor.java:130)
  at org.springframework.web.method.support.HandlerMethodArgumentResolverComposite.resolveArgument(HandlerMethodArgumentResolverComposite.java:126)
  at org.springframework.web.method.support.InvocableHandlerMethod.getMethodArgumentValues(InvocableHandlerMethod.java:166)
  at org.springframework.web.method.support.InvocableHandlerMethod.invokeForRequest(InvocableHandlerMethod.java:134)
  at org.springframework.web.servlet.mvc.method.annotation.ServletInvocableHandlerMethod.invokeAndHandle(ServletInvocableHandlerMethod.java:102)
  at org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerAdapter.invokeHandlerMethod(RequestMappingHandlerAdapter.java:895)
  at org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerAdapter.handleInternal(RequestMappingHandlerAdapter.java:800)
  at org.springframework.web.servlet.mvc.method.AbstractHandlerMethodAdapter.handle(AbstractHandlerMethodAdapter.java:87)
  at org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:1038)
  at org.springframework.web.servlet.DispatcherServlet.doService(DispatcherServlet.java:942)
  at org.springframework.web.servlet.FrameworkServlet.processRequest(FrameworkServlet.java:1005)
  at org.springframework.web.servlet.FrameworkServlet.doPost(FrameworkServlet.java:908)
  at javax.servlet.http.HttpServlet.service(HttpServlet.java:660)
  at org.springframework.web.servlet.FrameworkServlet.service(FrameworkServlet.java:882)
  at javax.servlet.http.HttpServlet.service(HttpServlet.java:741)
        ...
  at java.lang.Thread.run(Thread.java:748)
",
  "path": "/rest/"
}
```
### Cleanup
```sh
psql -h localhost -p 5432 --username postgres --dbname example --command "drop table rest;"
```
```sh
docker stop $NAME
docker container rm $NAME
docker container prune -f
docker image rm $IMAGE -f
docker image prune -f
```
### Testing Qeury
* dummy run (echo)
```sh
curl "http://localhost:8080/rest/queryparam?ids=1,2,3,4,5&keys=example"
```
```text
appids: foo,bar ids: 1,2,3,4,5
```
in the application log will see the query result:
```txt
query returned: 5 rows
```
insert more data to query:
```sh
for CNT in $(seq 1 1 10); do curl -X POST -H "Content-Type: application/json" -d "{\"key\":\"example $CNT\", \"value\":\"some data $CNT\"}" http://127.0.0.1:8080/rest | jq '.' ; done
```
this will print a series of entries inserted to the database
```json
{
  "id": 14,
  "rand": 13,
  "key": "some example 9",
  "value": "some data 9"
}
```
```json
{
  "id": 15,
  "rand": 7,
  "key": "some example 10",
  "value": "some data 10"
}

```
by providing matching / out of range `ids` and observing the returned row counts one can be certain the condition is taken into account:
```SQL
"select * from rest  " + String.format("where id in (%s)", String.join(",", Arrays.asList(marks)));
```

### Multiple Condition Query

```sh
curl "http://localhost:8080/rest/queryparam?ids=1,2,3,5,6,7&keys=example,example+1"
```
will echo to the curl console:
```text
keys: example,example 1 ids: 1,2,3,5,6,7
```
the springboot application server console log will show:
```text
query by ids returned: 6 rows
args [1, 2, 3, 5, 6, 7, example, other]
query by ids and keys returned: 6 rows
```

### Troubleshooting

```text
	 org.springframework.jdbc.BadSqlGrammarException: 
	 PreparedStatementCallback; 
	 bad SQL grammar 
	 [select id,key,value from rest where id in (?,?,?) and key in (?)]; 
	 nested exception is org.postgresql.util.PSQLException: 
	 ERROR: operator does not exist: integer = character varying
	 Hint: No operator matches the given name and argument types. 
	 You might need to add explicit type casts.
	 Position: 40

```
### Install PG Admin
* on Ubuntu Bionic one can 
```sh
sudo apt-get install pgadmin4-desktop
```
this will install version __6.14__

![PG Admin 4 Splash Screen](https://github.com/sergueik/springboot_study/blob/master/basic-postgresql/screenshots/capture-pg4_splash.png)

if the application was installed, you may need to find out the last used master password and unlock it:
![PG Admin 4 Splash Screen](https://github.com/sergueik/springboot_study/blob/master/basic-postgresql/screenshots/capture-pg4_unlock_password.png)

Follow the Debian system install [steps](https://www.pgadmin.org/download/pgadmin-4-apt/)
```sh
curl -l -k -o - https://www.pgadmin.org/static/packages_pgadmin_org.pub | sudo apt-key add
```
```sh
sudo sh -c 'echo "deb https://ftp.postgresql.org/pub/pgadmin/pgadmin4/apt/$(lsb_release -cs) pgadmin4 main" > /etc/apt/sources.list.d/pgadmin4.list && apt-get update'
```
```sh
sudo apt install pgadmin4-desktop
```
![PG Admin 4](https://github.com/sergueik/springboot_study/blob/master/basic-postgresql/screenshots/capture_pgadmin_4.png)

ignore the warning on __Xenial__
```text
W: The repository 'https://ftp.postgresql.org/pub/pgadmin/pgadmin4/apt/xenial pgadmin4 Release' does not have a Release file.
N: Data from such a repository can't be authenticated and is therefore potentially dangerous to use.
N: See apt-secure(8) manpage for repository creation and user configuration details.
E: Failed to fetch https://ftp.postgresql.org/pub/pgadmin/pgadmin4/apt/xenial/dists/pgadmin4/main/binary-amd64/Packages  server certificate verification failed. CAfile: /etc/ssl/certs/ca-certificates.crt CRLfile: none
E: Some index files failed to download. They have been ignored, or old ones used instead.
```
if the error remains, install via pip following the [steps](https://wpcademy.com/how-to-install-pgadmin-on-ubuntu-16-04-lts/)

alternatively install legacy version
```sh
apt-get download pgadmin3
sudo apt-get install libpq5 pgadmin3-data postgresql-client postgresql-client-9.5 postgresql-client-common
sudo dpkg -i pgadmin3*
```
turns out one cannot connect to a newer PostgreSQL from older PG Admin

### Vendor-Specific Query DSL

#### Postgresql
the request
`http://localhost:8080/rest/similar/keys?keys=example+1,example+2,example+3`
get queried as
```java
jdbcTemplate.query("select * from rest where id SIMILAR TO ?",
                   new String[]{ String.format("(%s)", String.join("|", keys)) },
                   new BackendDataMapper());
```
produces 
```text
keys: example 1,example 2,example 3
[
id=21,key=example 1,value=some data 1, 
id=22,key=example 2,value=some data 2, 
id=23,key=example 3,value=some data 3
]
```
assuming that entries with the `key` `example 1`, `example 2`, `example 3` were added to the database

NOTE: no way to use similar query against numeric column:
the attempt to
`http://localhost:8080/rest/similar/ids?ids=1,2,3,4,5`
leads to exception
```text
 org.springframework.jdbc.BadSqlGrammarException: 
 PreparedStatementCallback; 
 bad SQL grammar [select * from rest where key SIMILAR TO ?]; 
 nested exception is org.postgresql.util.PSQLException: 
 ERROR: operator does not exist: integer ~ text Hint: No operator matches the given name and argument types. You might need to add explicit type casts.

```
#### MySQL

the request
`http://localhost:8080/rest/similar/keys?keys=example+1,example+5,example+3`
get fulfilled as
```java
jdbcTemplate.query("select * from rest where `key` REGEXP ?",
                   new String[]{ String.format("(%s)", String.join("|", keys)) },
                   new BackendDataMapper());
```
after traversing the usual call chain: `example.controller.RestController`, `example.service.RestService`, `example.service.RestServiceImp`,`example.dao.BackendDataDao`, `example.dao.BackendDataDaoImp`


and produces the result:
```text
keys: example 1,example 5,example 3
[id=5,key=example 1,value=product 1, id=7,key=example 3,value=product 3]
```
#### TODO 
One can install [docker container](https://hub.docker.com/r/dpage/pgadmin4/tags) with based web client pg admin 4 on xenial. Alternatiely pick a smalleralpine based [image](https://hub.docker.com/layers/huggla/sam-pgadmin/4.20/images/sha256-8867f605c49e9ccdbb52858decd15258c144c7b183dd2532964bed48bcb0dcc5?context=explore)	

### Notes
```sh
psql -h 172.17.0.2 -p 5432 --username postgres --password
Password:
psql: FATAL:  password authentication failed for user "postgres"
``` 
```sh
 psql -h 127.0.0.1 -p 5432 --username postgres --password
Password:
psql (11.11)
Type "help" for help.

postgres=# \l
                                 List of databases
   Name    |  Owner   | Encoding |  Collate   |   Ctype    |   Access privileges
-----------+----------+----------+------------+------------+-----------------------
 example   | postgres | UTF8     | en_US.utf8 | en_US.utf8 |

...
```


```sh
psql -h postgres-database -p 5432 --username postgres --password
```
https://www.bigbinary.com/blog/configure-postgresql-to-allow-remote-connection
```sh

 nc -z postgres-database 5432
/tmp # echo $?
0

pip install py-postgresql
python
```
```
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/usr/local/lib/python3.8/site-packages/postgresql/__init__.py", line 92, in open
    c.connect()
  File "/usr/local/lib/python3.8/site-packages/postgresql/driver/pq3.py", line 2441, in connect
    self._establish()
  File "/usr/local/lib/python3.8/site-packages/postgresql/driver/pq3.py", line 2567, in _establish
    self.typio.raise_client_error(could_not_connect, creator = self, cause = exc)
  File "/usr/local/lib/python3.8/site-packages/postgresql/driver/pq3.py", line 517, in raise_client_error
    raise client_error
postgresql.exceptions.ClientCannotConnectError: could not establish connection to server
  CODE: 08001
  LOCATION: CLIENT
CONNECTION: [failed]
  failures[0]:
    NOSSL socket('172.17.0.2', 5432)
    postgresql.exceptions.AuthenticationSpecificationError: password authentication failed for user "postgres"
      CODE: 28P01
      LOCATION: File 'auth.c', line 337, in auth_failed from SERVER
CONNECTOR: [Host] pq://postgres:***@postgres-database:5432/example
  category: None
DRIVER: postgresql.driver.pq3.Driver
```
```sh
apk add postgresql
```
```sh
psql -h postgres-database -p 5432 --username postgres --password
Password:
psql: error: FATAL:  password authentication failed for user "postgres"

```

```sh
docker pull postgres:9.6-alpine3.13

docker run -d --name postgres-database -e POSTGRES_PASSWORD=password postgres:9.6-alpine3.13
docker run --link postgres-database -it python:3.8.2-alpine sh

```


```sh
 psql -h $(hostname -i) -p 5432 --username postgres --password
Password for user postgres:
psql (9.6.22)
Type "help" for help.
```
```SQL
postgres=# \l
                                 List of databases
   Name    |  Owner   | Encoding |  Collate   |   Ctype    |   Access privileges
-----------+----------+----------+------------+------------+-----------------------
 postgres  | postgres | UTF8     | en_US.utf8 | en_US.utf8 |
 template0 | postgres | UTF8     | en_US.utf8 | en_US.utf8 | =c/postgres          +
           |          |          |            |            | postgres=CTc/postgres
 template1 | postgres | UTF8     | en_US.utf8 | en_US.utf8 | =c/postgres          +
           |          |          |            |            | postgres=CTc/postgres

CREATE DATABASE
postgres=#
postgres=# \c example
Password:
You are now connected to database "example" as user "postgres".
CREATE TABLE rest ( id serial PRIMARY KEY NOT NULL, key varchar(100) NOT NULL, value varchar(250) NOT NULL, rand smallint NOT NULL);


example=# \d
             List of relations
 Schema |    Name     |   Type   |  Owner
--------+-------------+----------+----------
 public | rest        | table    | postgres
 public | rest_id_seq | sequence | postgres
(2 rows)

```
```python
pip install
python
import postgresql
db = postgresql.open('pq://postgres:password@postgres-database:5432/example')
>>> db
<postgresql.driver.pq3.Connection[pq://postgres:***@postgres-database:5432/example] b'I'>

make_emp = db.prepare("INSERT INTO rest (key,value,rand) VALUES ($1, $2, $3)")
make_emp("key1", "value1", 32)

```

on database side

```
 \d rest
                                 Table "public.rest"
 Column |          Type          |                     Modifiers                
--------+------------------------+---------------------------------------------------
 id     | integer                | not null default nextval('rest_id_seq'::regclass)
 key    | character varying(100) | not null
 value  | character varying(250) | not null
 rand   | smallint               | not null
Indexes:
    "rest_pkey" PRIMARY KEY, btree (id)

example=# select * from rest;
 id | key  | value  | rand
----+------+--------+------
  1 | key1 | value1 |   32

```

```sql
CREATE TABLE cache ( id serial PRIMARY KEY NOT NULL, hostname varchar(100) NOT NULL, value varchar(250) NOT NULL, timestamp bigint NOT NULL);
```
```text
CREATE TABLE
```
```
CREATE INDEX cache_hostname ON cache ( hostname);
```
* the [multi column index](https://www.tutlane.com/tutorial/sqlite/sqlite-indexes) is relevant for SQLite cache, not needed for PostgreSQL one
```sql
CREATE INDEX cache_hostname_timestamp ON cache ( hostname,timestamp);
```

```
db.close()
from datetime import datetime,timedelta

db = postgresql.open('pq://postgres:password@postgres-database:5432/example')
make_emp = db.prepare('INSERT INTO cache (hostname,value,timestamp) VALUES ($1, $2, $3)')
timestamp = int(datetime.now().strftime('%s'))
make_emp("host1", "value1", timestamp)

query_timestamp = int((datetime.now() - timedelta(seconds=60)).strftime('%s'))
res = db.query('select * from cache where timestamp > $1' , query_timestamp)
res
[]
make_emp = db.query('DELETE FROM cache where timestamp < $1', query_timestamp)
db.close()
```

```sh
docker container stop postgres-database
```
```sh
docker container ls -a | grep -i postgres-database | awk '{print $1}'| xargs -IX docker container rm X
```
### See also

  * another basic [jdbc postgress example](https://github.com/christosperis/spring-jdbctemplate-postgresql-example)
  * [initialize default user and password in postgresql](https://chartio.com/resources/tutorials/how-to-set-the-default-user-password-in-postgresql/)
  * postgresql [command rederence](https://www.tutorialspoint.com/postgresql/postgresql_select_database.htm)
  * Docker [image](https://hub.docker.com/r/kiasaki/alpine-postgres/) and [github repository](https://hub.docker.com/r/kiasaki/alpine-postgres/dockerfile)
  * configuring "sticky" versions with [apk](https://superuser.com/questions/1055060/how-to-install-a-specific-package-version-in-alpine)
  * [basics of installing postgresql in ubuntu](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-18-04)
  * [pgadmin](https://www.pgadmin.org/download/)
  * Cluster conriguration [instructions](https://linuxhint.com/postgresql_docker/)  for `dpage/pgadmin4` and `postgres`
  * misc. PostgreSQL hint link [post](https://habr.com/ru/post/667428/) (in Russian)  
  * [Docker inventory inspection for Dockerizer Postgres](https://habr.com/ru/companies/piter/articles/736332/) (in Russian)
  * [py-postgresql](https://pypi.org/project/py-postgresql/)  
  * https://stackoverflow.com/questions/769683/how-to-show-tables-in-postgresql
  * https://stackoverflow.com/questions/9556474/automatically-populate-a-timestamp-field-in-postgresql-when-a-new-row-is-inserte
  * https://www.reddit.com/r/PostgreSQL/comments/10shxej/how_to_update_a_timestamp_automatically/
  * https://x-team.com/blog/automatic-timestamps-with-postgresql/  
  * https://www.postgresql.org/message-id/6150FC42.5070207@anastigmatix.net mentions `track_commit_timestamp`
  * https://stackoverflow.com/questions/9488640/how-to-find-out-when-data-was-inserted-to-postgres/61788447#61788447
  * https://gist.github.com/brianmed/0e73292da11940a95b98
  * https://aviyadav231.medium.com/automatically-updating-a-timestamp-column-in-postgresql-using-triggers-98766e3b47a0
  * http://crafted-software.blogspot.com/2014/10/track-date-time-of-row-creation.html 
  * https://tembo.io/docs/postgres_guides/how-to-describe-tables-in-postgres
  * https://soft-builder.com/how-to-list-triggers-in-postgresql-database/


### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


