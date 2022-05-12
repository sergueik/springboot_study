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
[]
```
* add product
```sh
curl -X POST -H 'Content-Type: application/json'  http://localhost:8080/products -d '{
  "id": 123,
  "qty": 1,
  "price": 1000,
  "name": "product"
}

```

repeat queries:
```ssh
curl http://localhost:8080/products?db=mongo
```
```json
[{"id":123,"name":"product","qty":1,"price":1000.0}]
```
```sh
curl http://localhost:8080/products?db=mysql
```
```json
[{"id":0,"name":null,"qty":0,"price":0.0}]
```
-  need a fix
### See Also

  * [discussion of multi-database Hibernate App fix](https://qna.habr.com/q/1104464) (in Russian)
  * https://www.javacodegeeks.com/2018/12/java-streaming-jdbc-resultset-csv.html
  * https://narayanatutorial.com/java-tutorial/java-opencsv/csv-file-writing-resultset
  * [MySQL alpine](https://github.com/ipburger/mysql-alpine.docker/blob/master/Dockerfile)
