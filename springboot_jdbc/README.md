### Info

This directory contains a skeleton MySQL JdbcTemplate Spring Boot web application project based on tehe article [Using JdbcTemplate in a Spring Boot Web Application](https://www.codeproject.com/Articles/1269020/Using-JdbcTemplate-in-a-Spring-Boot-Web-Applicatio).

### Testing

Pull the collaborator Docker image:

```sh
docker pull mysql:8.0.18
```
and run it with environments matching the `application.properties`:
```sh
export MYSQL_USER='cardbuser'
export MYSQL_PASSWORD='123test321'
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
paste the `DB/users.sql`, then paste `DB/tables.sql`.

* Build the `mysql-example` Docker image

* uncomment the following line in `src/main.resources/datasource.properties`:
```sh
jdbc.server=mysql-server
```
Note: maven command line option
```sh
mvn clean -Djdbc.server=mysql-server package
```
is ignored
```sh
docker build -f Dockerfile -t jdbc-example .
```
* launch the `mysql-example` backed Docker container
```sh
NAME=jdbc-example
docker run  --name $NAME -p 8080:8080 --link mysql-server -d jdbc-example
docker logs $NAME
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
curl "http://192.168.0.64:8080/public/getCars?make=VM&startYear=2020&endYear=2020"
```
this should return no excceptions - it will likely print back an empty array:
```sh
[]
```
* add rows to database table:
```sh
curl -d "model=jetta&maker=VW&yearOfManufacturing=2020" -H "Content-Type: application/x-www-form-urlencoded" -X POST "http://192.168.0.64:8080/public/addCar" 2>/dev/null
```
this will respond with
```sh
"success":true,"statusMsg":"Operation is successful."}
```
alternatively

```sh
curl -d "{\"model\":\"jetta\",\"maker\":\"VW\",\"yearOfManufacturing\":\"2020\"}" -H "Content-Type: application/json" -X POST "http://192.168.0.64:8080/public/addCarJSON" 2>/dev/null
```
NOTE: with regular form, possibly throws an exception:
```sh
springboot http status 415 error":"Unsupported Media Type",
"exception":"org.springframework.web.HttpMediaTypeNotSupportedException",
"message":"Content type 'application/x-www-form-urlencoded;charset=UTF-8' not supported"
```
### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
docker image prune -f
```
### See Also
 * https://github.com/simplechen/SpringJdbcTemplateExample

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
