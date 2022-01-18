### Info 

this directory contains replica of skeleton project [multiple-db-connection](https://github.com/Java-Gyan-Mantra/multiple-db-connection) demonstrating how to configure  Springboot application to operate Mongo DB and SQLite simultaneously 

### Testing
 * create mongo server container
```sh
IMAGE=mongodb
CONTAINER=mongo-server
docker build -t $IMAGE -f Dockerfile.$IMAGE .
docker container prune -f
docker run -d --name $CONTAINER -p 27717:27017 -i $IMAGE
docker logs $CONTAINER
```

```sh
docker build -t mysql-server-alpine -f Dockerfile.mysql-server-alpine .
```
and run it with environments matching the `application.properties`:
```sh
export MYSQL_USER='java'
export MYSQL_PASSWORD='password'
docker run --name mysql-server-alpine -p 3306:3306 -e MYSQL_DATABASE=join_check -e MYSQL_USER=$MYSQL_USER -e MYSQL_PASSWORD=${MYSQL_PASSWORD} -e MYSQL_ROOT_PASSWORD=password -d mysql-server-alpine
```
* running app
```sh
mvn spring-boot:run
```
* testing
```sh
curl http://localhost:8080/products?db=zzz
```
```text
invalid operation: zzz
```
```sh
curl http://localhost:8080/products?db=mysql
```
```text
[]
```
```sh
curl http://localhost:8080/products?db=mongo
```
```text
Cluster description not yet available. Waiting for 30000 ms before timing out


{java.net.ConnectException: Connection refused (Connection refused)}}]","path":"/products"}s

{
"timestamp":"2022-01-18T02:41:27.739+00:00","status":500,"error":"Internal Server Error",
"trace":"org.springframework.dao.DataAccessResourceFailureException: Timed out after 30000 ms while waiting to connect. Client view of cluster state is 
{
type=UNKNOWN, 
servers=[
{
address=localhost:27017, type=UNKNOWN, state=CONNECTING, exception={com.mongodb.MongoSocketOpenException: Exception opening socket}, 
caused by {java.net.ConnectException: Connection refused (Connection refused)}}]
```
- need configuration
### See Also


  * https://www.javacodegeeks.com/2018/12/java-streaming-jdbc-resultset-csv.html
  * https://narayanatutorial.com/java-tutorial/java-opencsv/csv-file-writing-resultset
  * [MySQL alpine](https://github.com/ipburger/mysql-alpine.docker/blob/master/Dockerfile)
