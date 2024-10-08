### Info

This directory contains a skeleton MySQL JdbcTemplate Spring Boot web application project based on tehe article [Using JdbcTemplate in a Spring Boot Web Application](https://www.codeproject.com/Articles/1269020/Using-JdbcTemplate-in-a-Spring-Boot-Web-Applicatio).

### Testing

Pull the collaborator Docker image:

```sh
docker pull mysql:8.0.18
```
and run it with environments matching the `application.properties`:
```sh
export MYSQL_USER='java'
export MYSQL_PASSWORD='password'
docker run --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=$MYSQL_USER -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=$MYSQL_PASSWORD -d mysql:8.0.18
```
Note: one does not need to specify the `-p 3306:3306` argument.  Note: same user and password is used in `DB/users.sql` and java `database.properties`
The enviroment entries `MYSQL_ROOT_PASSWORD`, `MYSQL_USER`,`MYSQL_DATABASE`, `MYSQL_PASSWORD` are required by Mysql docker image.
Note, it will take the Docker instance  quite some time to launch.
Run
```sh
SERVER_ID=$(docker container ls -a | grep mysql-server | awk '{print $1}')
docker start $SERVER_ID
```
if this is the first time, then
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u root -ppassword
```
paste the `src/main/resources/users.sql`, then paste `src/main/resources/tables.sql`.

* Build the `mysql-example` Docker image

* uncomment the following line in `src/main.resources/datasource.properties`:
```sh
jdbc.server=mysql-server
```
Note: maven command line option
```sh
mvn clean -Djdbc.server=mysql-server -Dmaven.test.skip=true clean package
```
is ignored
```sh
IMAGE=jdbc-example
docker build -f Dockerfile -t $IMAGE .
```
* launch the Docker container linked to the `mysql-example`
```sh
CONTAINER=jdbc-example
docker container rm -f $CONTAINER
docker run  --name $CONTAINER -p 8080:8080 --link mysql-server -d $IMAGE
docker logs $CONTAINER
```
this will show , along with other logs,
```sh
INFO  example.config.JdbcConfiguration - Datasource URL: jdbc:mysql://mysql-server:3306/cardb?characterEncoding=UTF-8&rewriteBatchedStatements=true
```
if the following is shown
```sh
INFO  example.config.JdbcConfiguration - Datasource URL: jdbc:mysql://127.0.0.1:3306/cardb?characterEncoding=UTF-8&rewriteBatchedStatements=true
```
this will lead to `com.mysql.cj.jdbc.exceptions.CommunicationsException`
 exception connecting to the server.
* testing
```sh
curl "http://$(hostname -i):8080/public/getCars?make=VW&startYear=2020&endYear=2020" |jq -r "." | tee result.json
```
this should return no exceptions - initially will print back an empty array:
```sh
[]
```
when data is available, it is printed :
```json
[
  {
    "yearOfManufacturing": 2020,
    "model": "jetta",
    "maker": "VW",
    "suggestedRetailPrice": 0,
    "fullPrice": 0,
    "rebateAmount": 0
  },
  {
    "yearOfManufacturing": 2020,
    "model": "passat",
    "maker": "VW",
    "suggestedRetailPrice": 0,
    "fullPrice": 0,
    "rebateAmount": 0
  }
]
```
The proper csv conversion is a work in progress:
```sh
jq -r '. | map(.maker), map(.model),map(.yearOfManufacturing) | @csv' result.json
```
```sh
"VW","VW"
"jetta","passat"
2020,2020
```
* add rows to database table:
```sh
curl -d "model=passat&maker=VW&yearOfManufacturing=2020" -H "Content-Type: application/x-www-form-urlencoded" -X POST "http://$(hostname -i):8080/public/addCar" 2>/dev/null
```
this will respond with
```sh
"success":true,"statusMsg":"Operation is successful."}
```
alternatively

```sh
curl -d "{\"model\":\"jetta\",\"maker\":\"VW\",\"yearOfManufacturing\":\"2020\"}" -H "Content-Type: application/json" -X POST "http://$(hostname -i):8080/public/addCarJSON" 2>/dev/null
```
NOTE: with regular form, possibly throws an exception:
```sh
springboot http status 415 error":"Unsupported Media Type",
"exception":"org.springframework.web.HttpMediaTypeNotSupportedException",
"message":"Content type 'application/x-www-form-urlencoded;charset=UTF-8' not supported"
```

### Debugging the SQL

* connect to `mysql_server` Docker container Mysql shell as root
```sh
SERVER_ID=$(docker container ls -a | grep mysql-server | awk '{print $1}')
docker exec -it $SERVER_ID mysql -P 3306 -h localhost -u root -ppassword
```
* enable file logging:
```sh
SET GLOBAL log_output = "FILE";
SET GLOBAL general_log_file = "/tmp/mysql.log";
SET GLOBAL general_log = 'ON';
```
* connect to `mysql_server` Docker container ash shell as root
```sh
SERVER_ID=$(docker container ls -a | grep mysql-server | awk '{print $1}')
docker exec -it $SERVER_ID sh
```
and inspect the logs
```sh
more /tmp/mysql.log
```
(NOTE: base image has no standard editors installed)
### Cleanup
```sh
docker container stop $CONTAINER
docker container rm $CONTAINER
docker image prune -f
```
### Troubleshooting

Some revisions of the project were unstable against creating the database and tables even if exist during application startup:
```sh
org.springframework.jdbc.datasource.init.ScriptStatementFailedException: 
Failed to execute SQL script statement #1 of class path resource [tables.sql]: 
CREATE DATABASE cardb; nested exception is 
java.sql.SQLException: 
Can't create database 'cardb'; database exists
```
and at the same time complaining the same does not exist:
```sh
java.lang.IllegalStateException: Failed to execute CommandLineRunner
        at org.springframework.boot.SpringApplication.callRunner(SpringApplication.java:735) [spring-boot-1.5.4.RELEASE.jar!/:1.5.4.RELEASE]
...
        at org.springframework.boot.loader.JarLauncher.main(JarLauncher.java:51) [app.jar:0.6.1-SNAPSHOT]
Caused by: java.sql.SQLSyntaxErrorException: Unknown database 'cardb'
        at com.mysql.cj.jdbc.exceptions.SQLError.createSQLException(SQLError.java:120) ~[mysql-connector-java-8.0.18.jar!/:8.0.18]
```
### See Also

 * https://github.com/simplechen/SpringJdbcTemplateExample
 * https://unix.stackexchange.com/questions/163845/using-jq-to-extract-values-and-format-in-csv/227950
 * https://stackoverflow.com/questions/32960857/how-to-convert-arbitrary-simple-json-to-csv-using-jq
 * JetBrains IDE-heavy docker-compose.yaml [project](https://github.com/IdeaUJetBrains/SpringBootDockerDemoDebug)
 * basic classic Spring MVC/JPA [demo](https://github.com/gaussic/SpringMVCDemo  (not excessively heavy) 
 * https://qna.habr.com/q/825303
 * netbeans spring jdbc mvc [example](https://github.com/hendrosteven/springmvc-jdbc-sample)
  * [JPA and JDBC SQL Generarion Logging](https://www.baeldung.com/sql-logging-spring-boot)
  * stackoverflow disscussion on possibility to see the generated SQL of [prepared statement](https://stackoverflow.com/questions/2382532/how-can-i-get-the-sql-of-a-preparedstatement)
  * JDBC driver that logs all SQL queries [source repository](https://github.com/rbloom/jdbcLogDriver) - not deployed to Maven central or other public repository
  * enable MySQL Query Log [on the server](https://stackoverflow.com/questions/6479107/how-to-enable-mysql-query-log)
  * is it possible to get [SQL of a PreparedStatement](https://stackoverflow.com/questions/2382532/how-can-i-get-the-sql-of-a-preparedstatement)
  * https://www.journaldev.com/2509/java-datasource-jdbc-datasource-example
  * https://www.baeldung.com/spring-boot-configure-data-source-programmatic
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


