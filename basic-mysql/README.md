### Info

Spring Boot on Docker basic extracted from [Springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example)
upgraded to try access MySQL 8.0(not installed locally, has differnt shell etc).

### Setup

One has to pull the collaborator Docker image ahead of time:

```sh
docker pull mysql:5.7
```
```sh
docker run --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=password -d mysql:5.7
```
```sh
docker logs mysql-server
```
```sh
2019-10-30T01:35:36.948121Z 0 [Note] mysqld: ready for connections.
Version: '5.7.28'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server (GPL)
```
NOTE: the mysqld and java processes will be visible on host
### Test

package jar to run in container
```sh
mvn clean package
docker build -f Dockerfile -t mysql-example . 
docker run -p 8086:8086 --link mysql-server -d mysql-example
```
```sh
curl http://localhost:8086/all/
[]
```
Check action logs 
```sh
docker logs $(docker container ls | grep mysql-example | awk '{print $1}')
```
```sh
Hibernate: select users0_.id as id1_0_, users0_.name as name2_0_, users0_.salary as salary3_0_, users0_.team_name as team_nam4_0_ from users users0_
```
```sh
curl http://localhost:8086/all/create
```
### Cleanup
```sh
docker stop mysql-server
```
```sh
ID=$(docker ls $(docker container ls | grep mysql-example | awk '{print $1}'))
docker stop $ID
docker rm $ID
```
